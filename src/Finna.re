let apiUrl = "https://api.finna.fi";
let baseUrl = "https://finna.fi";
let recordBaseUrl = baseUrl ++ "/Record/";

type facettyLabelVariant =
  | String(string)
  | Int(int);

let val2Str = v =>
  switch (v) {
  | String(v) => v
  | Int(v) => string_of_int(v)
  };

/* API types */
type translated = {
  value: string,
  label: string,
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
  label: option(string),
};

type author = {
  name: string,
  role: string,
};

type record = {
  id: string,
  title: string,
  formats: array(translated),
  buildings: option(array(translated)),
  images: array(string),
  authors: array(string),
  publishers: option(array(string)),
  year: option(string),
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

/* API responses */
type recordResult = {
  records: option(array(record)),
  resultCount: option(int),
  status: string,
};
type result = {
  records: option(array(record)),
  facets: option(Js.Dict.t(array(facetItem))),
  resultCount: option(int),
  status: string,
  statusMessage: option(string),
};

/* Cleaned API responses */
type recordResultProcessed = {record: option(record)};
type resultProcessed = {
  records: option(array(record)),
  facets: Js.Dict.t(facet),
  resultCount: int,
};

/* Result wrappers */
type recordResponse = {
  error: bool,
  record: option(record),
};
type searchResponse = {
  error: bool,
  results: option(resultProcessed),
};

/* Decoders */
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
    buildings: json |> optional(field("buildings", array(translated))),
    images: json |> field("images", array(string)),
    authors:
      json
      |> [%bs.raw
        {| (json) => { return(Object.keys(json.authors.primary)); } |}
      ],
    publishers: json |> optional(field("publishers", array(string))),
    year: json |> optional(field("year", string)),
  };

let result = (json: Js.Json.t): result =>
  Json.Decode.{
    records: json |> optional(field("records", array(record))),
    resultCount: json |> optional(field("resultCount", int)),
    facets:
      json |> optional(field("facets", dict(array(decodeFacetItem)))),
    status: json |> field("status", string),
    statusMessage: json |> optional(field("statusMessage", string)),
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

/* Search result decoder */
let processResults = (results: result): searchResponse =>
  switch (results.status) {
  | "OK" =>
    let res: resultProcessed = {
      facets: processFacets(results.facets),
      records: results.records,
      resultCount:
        switch (results.resultCount) {
        | Some(count) => count
        | None => 0
        },
    };
    {error: false, results: Some(res)};
  | "ERROR" => {error: true, results: None}
  };

/* Record decoders */
let processRecordResults = (result: recordResult): recordResponse =>
  switch (result.status) {
  | "OK" =>
    let records =
      switch (result.records) {
      | Some(records) => records
      | None => [||]
      };
    {error: false, record: Some(records[0])};
  | _ => {error: true, record: None}
  };

let recordResult = (json: Js.Json.t): recordResult =>
  Json.Decode.{
    records: json |> optional(field("records", array(record))),
    resultCount: json |> optional(field("resultCount", int)),
    status: json |> field("status", string),
  };

/* API calls */
let search =
    (~lookfor, ~filters, ~page, ~limit, ~onResults, ~facetKey=?, ~lng, ()) => {
  let filterStr =
    filters
    |> Array.map((f: filter) => {
         let key = f.key;
         let value = f.value;
         {j|filter[]=$key:"$value"|j};
       })
    |> Js.Array.joinWith("&");

  let sort = "relevance";

  let facetStr =
    switch (facetKey) {
    | None => ""
    | _ => {j|&facet[]=$facetKey&facetFilter[]=$facetKey%3A0%2F.*|j}
    };

  let url = {j|$apiUrl/api/v1/search?lookfor=$lookfor&type=AllFields&field[]=id&field[]=formats&field[]=buildings&field[]=title&field[]=images&field[]=authors&field[]=year&field[]=publishers&sort=$sort%2Cid%20asc&page=$page&limit=$limit&prettyPrint=false&lng=$lng$facetStr&$filterStr|j};

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
    |> catch(_err => resolve(onResults({error: true, results: None})))
  );
};

let record = (~id, ~onResults, ~lng, ()) => {
  let id = Js_global.encodeURIComponent(id);
  let url = {j|$apiUrl/api/v1/record?id=$id&field[]=id&field[]=formats&field[]=title&field[]=buildings&field[]=images&field[]=authors&field[]=year&prettyPrint=false&lng=$lng|j};

  Js.log(url);
  Js.Promise.(
    Fetch.fetch(url)
    |> then_(Fetch.Response.text)
    |> then_(text =>
         text
         |> Json.parseOrRaise
         |> recordResult
         |> processRecordResults
         |> onResults
         |> resolve
       )
    |> catch(_err => resolve(onResults({error: true, record: None})))
  );
};

let getFacets = (~lookfor, ~filters, ~page, ~facetKey, ~onResults, ~lng) => {
  let filtersWithoutFacet =
    List.filter(f => f.key != facetKey, Array.to_list(filters))
    |> Array.of_list;
  search(
    ~lookfor,
    ~filters=filtersWithoutFacet,
    ~page,
    ~limit=0,
    ~onResults,
    ~lng,
    ~facetKey,
    (),
  );
};
