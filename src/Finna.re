let apiUrl = "https://api.finna.fi";
let baseUrl = "https://finna.fi";
let recordBaseUrl = baseUrl ++ "/Record/";

type translated = {
  value: string,
  label: string,
};

type facetLabelVariant =
  | String(string)
  | Int(int);

let val2Str = v =>
  switch (v) {
  | String(v) => v
  | Int(v) => string_of_int(v)
  };

type facetType =
  | Normal
  | Boolean;

type facetValue =
  | None
  | Value(string);

type facetItem = {
  value: string,
  label: string,
  count: int,
};
type facet = {
  key: string,
  facetType,
  value: facetValue,
  items: array(facetItem),
};
type filter = {
  key: string,
  value: string,
};

let getInitialFacets = () => {
  let facets = Js.Dict.empty();
  let facet: facet = {
    key: "format",
    facetType: Normal,
    items: [||],
    value: None,
  };
  Js.Dict.set(facets, "format", facet);

  let facet: facet = {
    key: "building",
    facetType: Normal,
    items: [||],
    value: None,
  };
  Js.Dict.set(facets, "building", facet);

  let facet: facet = {
    key: "online_boolean",
    facetType: Boolean,
    items: [||],
    value: None,
  };
  Js.Dict.set(facets, "online_boolean", facet);

  facets;
};

type author = {
  name: string,
  role: string,
};
type record = {
  id: string,
  title: string,
  formats: array(translated),
  buildings: array(translated),
  images: array(string),
  authors: array(string),
  year: option(string),
};

type result = {
  records: option(array(record)),
  facets: option(Js.Dict.t(array(facetItem))),
  resultCount: int,
  status: string,
};

type resultProcessed = {
  records: option(array(record)),
  facets: Js.Dict.t(facet),
  resultCount: int,
  status: string,
};

let translated = json =>
  Json.Decode.{
    value: json |> field("value", string),
    label: json |> field("translated", string),
  };

let decodeFacetLabel =
  Json.Decode.(
    either(string |> map(s => String(s)), int |> map(i => Int(i)))
  );

let decodeFacetItem = json =>
  Json.Decode.{
    count: json |> field("count", int),
    value: json |> field("value", decodeFacetLabel) |> val2Str,
    label: json |> field("translated", decodeFacetLabel) |> val2Str,
  };

let record = json =>
  Json.Decode.{
    id: json |> field("id", string),
    title: json |> field("title", string),
    formats: json |> field("formats", array(translated)),
    buildings: json |> field("buildings", array(translated)),
    images: json |> field("images", array(string)),
    authors:
      json
      |> [%bs.raw
        {| (json) => { return(Object.keys(json.authors.primary)); } |}
      ],
    year: json |> optional(field("year", string)),
  };

let result = (json: Js.Json.t): result =>
  Json.Decode.{
    records: json |> optional(field("records", array(record))),
    resultCount: json |> field("resultCount", int),
    facets:
      json |> optional(field("facets", dict(array(decodeFacetItem)))),
    status: json |> field("status", string),
  };

let processFacets = facets =>
  switch (facets) {
  | Some(facets) =>
    let newDict = Js.Dict.empty();
    Array.iter(
      facetKey =>
        switch (Js.Dict.get(facets, facetKey)) {
        | Some(items) =>
          let facetType =
            switch (Array.length(items)) {
            | 1 => Boolean
            | _ => Normal
            };
          let facet: facet = {key: facetKey, facetType, items, value: None};
          Js.Dict.set(newDict, facetKey, facet);
        | None => ()
        },
      Js.Dict.keys(facets),
    );
    newDict;
  | None => Js.Dict.empty()
  };

let processResults = (results: result) => {
  let res: resultProcessed = {
    facets: processFacets(results.facets),
    records: results.records,
    resultCount: results.resultCount,
    status: results.status,
  };
  res;
};

let search = (~lookfor, ~filters, ~page, ~limit, ~onResults, ~facetKey=?, ()) => {
  let filterStr =
    filters
    |> Array.map((f: filter) => {
         let key = f.key;
         let value = f.value;
         {j|filter[]=$key:"$value"|j};
       })
    |> Js.Array.joinWith("&");

  let lng = "fi";
  let sort = "relevance";

  let facetStr =
    switch (facetKey) {
    | None => ""
    | _ => {j|&facet[]=$facetKey&facetFilter[]=$facetKey%3A0%2F.*|j}
    };

  let url = {j|$apiUrl/api/v1/search?lookfor=$lookfor&type=AllFields&field[]=id&field[]=formats&field[]=title&field[]=buildings&field[]=images&field[]=authors&field[]=year&sort=$sort%2Cid%20asc&page=$page&limit=$limit&prettyPrint=false&lng=$lng$facetStr&$filterStr|j};

  Js.log(url);
  Js.Promise.(
    Fetch.fetch(url)
    |> then_(Fetch.Response.text)
    |> then_(text =>
         text
         |> Json.parseOrRaise
         |> result
         |> processResults
         |> onResults
         |> resolve
       )
    |> ignore
  );
};

let getFacets = (~lookfor, ~filters, ~page, ~facetKey, ~onResults) =>
  search(~lookfor, ~filters, ~page, ~limit=0, ~onResults, ~facetKey, ());
