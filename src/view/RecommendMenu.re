let createMake = () => {
  let component = ReasonReact.statelessComponent(__MODULE__);

  let make =
      (
        ~menuState: Recommend_State.MenuState.t('err, 'sugg),
        ~renderSuggestion: 'sugg => ReasonReact.reactElement,
        ~eqSuggestion,
        ~onHoverSuggestion: 'sugg => unit,
        ~onClearSuggestions: unit => unit,
        _children,
      ) => {
    ...component,
    render: _self =>
      switch (menuState) {
      | Closed(Inactive) => ReasonReact.null
      | Closed(InsufficientFilter) =>
        <div> {ReasonReact.string("Type more characters")} </div>

      | Open(Loading, _) => <div> {ReasonReact.string("Loading")} </div>
      | Open(Failed(_), _) => <div> {ReasonReact.string("Failed")} </div>
      | Open(NoResults, _) => <div> {ReasonReact.string("No results")} </div>
      | Open(Loaded(suggs), highlight) =>
        let eqHighlight = sugg =>
          Belt.Option.mapWithDefault(highlight, false, eqSuggestion(sugg));

        let suggs =
          NonEmptyList.toT(suggs)
          |> Array.of_list
          |> Array.map(v => {
               let className =
                 "reccommend-suggestion-item"
                 ++ (eqHighlight(v) ? " is-highlighted" : "");
               <li
                 className
                 onMouseOver=(_ => onHoverSuggestion(v))
                 onMouseOut=(_ => onClearSuggestions())>
                 {renderSuggestion(v)}
               </li>;
             });

        <ul> ...suggs </ul>;
      },
  };
  make;
};
