module Recommend = {
  let make = Recommend.createMake();
};

let fetchSuggestions:
  string => Future.t(Belt.Result.t(list(string), string)) =
  _str => Future.value(Belt.Result.Ok(["Apple", "Pear", "Orange"]));

let renderSuggestion = str => <div> (ReasonReact.string(str)) </div>;

ReactDOMRe.renderToElementWithId(
  <Recommend fetchSuggestions renderSuggestion minCharCount=3 />,
  "app",
);
