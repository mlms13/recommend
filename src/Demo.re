module D = Decode.AsResult.OfParseError;

let resultFromOpt = (note, opt) =>
  Belt.Option.mapWithDefault(opt, Belt.Result.Error(note), ok =>
    Belt.Result.Ok(ok)
  );

let resultFlatMap = (fn, v) => Belt.Result.flatMap(v, fn);

let listAny = (lst, pred) =>
  switch (Belt.List.keep(lst, pred)) {
  | [] => false
  | _ => true
  };

module Food = {
  type t = {
    name: string,
    description: option(string),
    aliases: list(string),
    category: string,
  };

  let make = (name, description, aliases, category) => {
    name,
    description,
    aliases,
    category,
  };

  let eq = (a, b) =>
    a.name == b.name
    && a.description == b.description
    && a.category == b.category;

  let matches = (filter, {name, aliases}) => {
    let lc = Js.String.toLowerCase;
    let containsFilter = Js.String.includes(lc(filter));
    containsFilter(lc(name))
    || aliases->Belt.List.map(lc)->listAny(containsFilter);
  };

  let decode = json =>
    D.Pipeline.succeed(make)
    |> D.Pipeline.field("name", D.string)
    |> D.Pipeline.optionalField("description", D.string)
    |> D.Pipeline.field("aliases", D.list(D.string))
    |> D.Pipeline.field("category", D.string)
    |> D.Pipeline.run(json);
};

module Recommend = {
  let make = Recommend.createMake();
};

let url = "https://raw.githubusercontent.com/mlms13/foods/master/src/data/foods.yml";

let foodsFromYaml = yamlStr =>
  ReYaml.safeLoad(yamlStr)
  ->(
      Belt.Result.flatMap(json =>
        D.list(Food.decode, json)
        |> D.ResultUtil.mapErr(
             Decode.ParseError.toDebugString(DecodeBase.failureToString),
           )
      )
    );

let fetchSuggestions = filter =>
  Fetch.fetch(url)
  ->Js.Promise.then_(Fetch.Response.text, _)
  ->FutureJs.fromPromise(_ => "Network Error")
  ->Future.map(resultFlatMap(foodsFromYaml))
  ->Future.mapOk(v => Belt.List.keep(v, Food.matches(filter)))
  ->Future.tapError(Js.log);

let renderSuggestion = (_filter, {Food.name, Food.description}) =>
  <li>
    <strong> {ReasonReact.string(name)} </strong>
    {
      Belt.Option.mapWithDefault(description, ReasonReact.null, d =>
        <span> {ReasonReact.string(" (" ++ d ++ ")")} </span>
      )
    }
  </li>;

ReactDOMRe.renderToElementWithId(
  <Recommend
    eqSuggestion=Food.eq
    fetchSuggestions
    renderSuggestion
    minCharCount=3
  />,
  "app",
);
