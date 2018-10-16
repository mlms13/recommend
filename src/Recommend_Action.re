module State = Recommend_State;

type highlight('sugg) =
  | ClearHighlight
  | Exact('sugg)
  | MoveUp
  | MoveDown;

type t('err, 'sugg) =
  | SetFilter(string)
  | OpenMenu
  | CloseMenu
  | SetSuggestions(list('sugg), option('sugg))
  | FailSuggestions('err)
  | ChangeHighlight(highlight('sugg))
  | ChooseCurrent;

let suggestionsFromResult = (highlight, res) => switch res {
| Belt.Result.Ok(v) => SetSuggestions(v, highlight)
| Belt.Result.Error(x) => FailSuggestions(x)
};
