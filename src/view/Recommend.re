module State = Recommend_State;
module Action = Recommend_Action;

type state('err, 'a) = State.t('err, 'a);
type action('err, 'a) = Action.t('err, 'a);

let createMake = () => {
  let component = ReasonReact.reducerComponent(__MODULE__);

  let make = (~fetchSuggestions, ~minCharCount=0, _children) => {
    ...component,
    initialState: () => State.initial,
    reducer: (action: Action.t('x, 'sugg), state: State.t('x, 'sugg)) => {
      let insufficientFilter = Js.String.length(state.filter) < minCharCount;
      let loadSuggestions = (h, filter, send) =>
        fetchSuggestions(filter)
        |. Future.map(res => Action.suggestionsFromResult(h, res) |> send)
        |. ignore;

      switch (action, state.menuState) {
      /* Update filter and set menu to "insufficient" */
      | (SetFilter(filter), _) when insufficientFilter =>
        Update({filter, menuState: Closed(InsufficientFilter)})

      /* Update filter and open the menu */
      | (SetFilter(filter), _) =>
        UpdateWithSideEffects(
          {filter, menuState: Open(Loading, None)},
          ({send, state}) => loadSuggestions(None, state.filter, send)
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

      /* If we've loaded suggestions but the menu is now closed for any reason,
         ignore that request and leave the menu closed */
      | (SetSuggestions(_), Closed(_)) => NoUpdate

      /* When suggestions complete loading, but are empty, keep the menu open,
         but in a "no results" state */
      | (SetSuggestions([], _), Open(_)) => Update(
        {...state, menuState: Open(NoResults, None)}
      )

      | _ => NoUpdate
      };
    },
    render: _self => <div />,
  };

  (make);
};
