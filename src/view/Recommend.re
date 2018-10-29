module S = Recommend_State;
module A = Recommend_Action;

let evtValue = evt => ReactEvent.Form.target(evt)##value;

type state('err, 'a) = S.t('err, 'a);
type action('err, 'a) = A.t('err, 'a);

let createMake = () => {
  module RecommendMenu = {
    let make = RecommendMenu.createMake();
  };
  let component = ReasonReact.reducerComponent(__MODULE__);

  let make =
      (
        ~fetchSuggestions,
        ~renderSuggestion,
        ~eqSuggestion,
        ~minCharCount=0,
        _children: array(string),
      ) => {
    ...component,
    initialState: () => S.initial,
    reducer: (action: A.t('x, 'sugg), state: S.t('x, 'sugg)) => {
      let insufficientFilter = Js.String.length(state.filter) < minCharCount;
      let loadSuggestions = (h, filter, send) =>
        fetchSuggestions(filter)
        ->(Future.map(res => A.suggestionsFromResult(h, res) |> send))
        ->ignore;

      switch (action, state.menuState) {
      /* Update filter and set menu to "insufficient" */
      | (SetFilter(filter), _) when Js.String.length(filter) < minCharCount =>
        Update({filter, menuState: Closed(InsufficientFilter)})

      /* Update filter and open the menu */
      | (SetFilter(filter), _) =>
        UpdateWithSideEffects(
          {filter, menuState: Open(Loading, None)},
          (({send}) => loadSuggestions(None, filter, send)),
        )

      /* When told to open a closed menu, but the filter doesn't have enough
         characters, reject that request and update the menu state */
      | (OpenMenu, Closed(Inactive)) when insufficientFilter =>
        Update({...state, menuState: Closed(InsufficientFilter)})

      /* When told to update an inactive menu in all other cases, do it, and
         kick off the request for suggestions */
      | (OpenMenu, Closed(Inactive)) =>
        UpdateWithSideEffects(
          {...state, menuState: Open(Loading, None)},
          (({send, state}) => loadSuggestions(None, state.filter, send)),
        )

      /* When told to update an already-opened or closed-for-a-good-reason menu,
         ignore those requests */
      | (OpenMenu, Closed(InsufficientFilter))
      | (OpenMenu, Open(_)) => NoUpdate

      /* When told to close the menu, just do it */
      | (CloseMenu, _) => Update({...state, menuState: Closed(Inactive)})

      /* If we've loaded suggestions (or suggestions failed) but the menu is now
         closed for any reason, ignore that request and leave the menu closed */
      | (SetSuggestions(_), Closed(_))
      | (FailSuggestions(_), Closed(_)) => NoUpdate

      /* When suggestions complete loading, but are empty, keep the menu open,
         but in a "no results" state */
      | (SetSuggestions([], _), Open(_)) =>
        Update({...state, menuState: Open(NoResults, None)})

      /* TODO: deal with matching highlight later */
      | (SetSuggestions([head, ...tail], _), Open(_)) =>
        Update({
          ...state,
          menuState: Open(Loaded(NonEmptyList.make(head, tail)), None),
        })

      /* When suggestions fail and the menu is still open, show the failed state
         and clear any highlight */
      | (FailSuggestions(err), Open(_)) =>
        Update({...state, menuState: Open(Failed(err), None)})

      /* If the menu is closed, loading, or has no results, and we receive an
         action telling us to change the lighlight, ignore it */
      | (ChangeHighlight(_), Closed(_))
      | (ChangeHighlight(_), Open(Loading, _))
      | (ChangeHighlight(_), Open(Failed(_), _))
      | (ChangeHighlight(_), Open(NoResults, _)) => NoUpdate

      | (ChangeHighlight(ClearHighlight), Open(Loaded(results), _)) =>
        Update({...state, menuState: Open(Loaded(results), None)})

      | (ChangeHighlight(Exact(sugg)), Open(Loaded(results), _)) =>
        Update({...state, menuState: Open(Loaded(results), Some(sugg))})

      | _ => NoUpdate
      };
    },
    render: ({send, state}) =>
      <div>
        <input
          value={state.S.filter}
          onChange={evt => send(A.SetFilter(evtValue(evt)))}
        />
        <RecommendMenu
          menuState={state.S.menuState}
          renderSuggestion={renderSuggestion(state.S.filter)}
          onHoverSuggestion={sugg => send(ChangeHighlight(Exact(sugg)))}
          onClearSuggestions={() => send(ChangeHighlight(ClearHighlight))}
          eqSuggestion
        />
      </div>,
  };

  make;
};
