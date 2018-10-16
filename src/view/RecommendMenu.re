let createMake = () => {
  let component = ReasonReact.statelessComponent(__MODULE__);

  let make =
      (
        ~menuState: Recommend_State.MenuState.t('err, 'sugg),
        ~renderSuggestion: 'sugg => ReasonReact.reactElement,
        _children,
      ) => {
    ...component,
    render: _self =>
      switch (menuState) {
      | Closed(Inactive) => ReasonReact.null
      | Closed(InsufficientFilter) =>
        <div> (ReasonReact.string("Type more characters")) </div>
      | Open(Loading, _) => <div> (ReasonReact.string("Loading")) </div>
      | Open(Failed(_), _) => <div> (ReasonReact.string("Failed")) </div>
      | Open(NoResults, _) => <div> (ReasonReact.string("No results")) </div>
      | Open(Loaded(suggestions), highlight) =>
        let suggs =
          NonEmptyList.toT(suggestions)
          |> Array.of_list
          |> Array.map(renderSuggestion);
        <ul> ...suggs </ul>;
      },
  };
  make;
};
