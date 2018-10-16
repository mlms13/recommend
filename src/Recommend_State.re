module MenuState = {
  type closedState =
    | Inactive
    | InsufficientFilter;

  type openState('err, 'sugg) =
    | Loading
    | Failed('err)
    | NoResults
    | Loaded(NonEmptyList.t('sugg));

  type t('err, 'sugg) =
    | Closed(closedState)
    | Open(openState('err, 'sugg), option('sugg));
};

type t('err, 'sugg) = {
  menuState: MenuState.t('err, 'sugg),
  filter: string,
};

let initial = {menuState: Closed(Inactive), filter: ""};
