module D = Decode.AsOption;

let resultFromOpt = (note, opt) =>
  Belt.Option.mapWithDefault(opt, Belt.Result.Error(note), ok =>
    Belt.Result.Ok(ok)
  );

let resultFlatMap = (fn, v) => Belt.Result.flatMap(v, fn);

let listAny = (pred, lst) =>
  switch (Belt.List.keep(lst, pred)) {
  | [] => false
  | _ => true
  };

module Food = {
  type t = {
    name: string,
    aliases: list(string),
    category: string,
  };

  let make = (name, aliases, category) => {name, aliases, category};

  let matches = (filter, {name, aliases}) => {
    let containsFilter = Js.String.includes(filter);
    containsFilter(name) || listAny(containsFilter, aliases);
  };
  let decode = json =>
    D.Pipeline.succeed(make)
    |> D.Pipeline.field("name", D.string)
    |> D.Pipeline.field("aliases", D.list(D.string))
    |> D.Pipeline.field("category", D.string)
    |> D.Pipeline.run(json);
};

module Recommend = {
  let make = Recommend.createMake();
};

let url = "https://raw.githubusercontent.com/mlms13/foods/master/src/data/foods.yml";

let foodsFromYaml = yamlStr =>
  ReYaml.safeLoad(yamlStr)->(Belt.Result.map(D.list(Food.decode)))
  |> resultFlatMap(resultFromOpt("Decode error"));

let fetchSuggestions = filter =>
  Fetch.fetch(url)
  ->Js.Promise.then_(Fetch.Response.text, _)
  ->FutureJs.fromPromise(_ => "Network Error")
  ->Future.map(resultFlatMap(foodsFromYaml))
  ->Future.mapOk(v => Belt.List.keep(v, Food.matches(filter)));

let renderSuggestion = ({ Food.name }) => <div> {ReasonReact.string(name)} </div>;

ReactDOMRe.renderToElementWithId(
  <Recommend fetchSuggestions renderSuggestion minCharCount=3 />,
  "app",
);
