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

let facetLabel = label =>
  switch (label) {
  | String(label) => label
  | Int(label) => string_of_int(label)
  };

type facetType =
  | Normal
  | Boolean;

type facet = {
  value: facetLabelVariant,
  label: facetLabelVariant,
  count: int,
  facetType,
};

type filter = {
  key: string,
  facet,
};

let getFilter = (~key, ~label, ~value) => {
  key,
  facet: {
    label: String(label),
    value: String(value),
    count: 0,
    facetType: Normal,
  },
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
  facets: option(Js.Dict.t(array(facet))),
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
let decodeFacetType =
  Json.Decode.(either(string |> map(_ => Normal), int |> map(_ => Boolean)));

let facet = json => {
  Js.log(json);
  Json.Decode.{
    count: json |> field("count", int),
    value: json |> field("value", decodeFacetLabel),
    label: json |> field("translated", decodeFacetLabel),
    facetType: json |> field("translated", decodeFacetType),
  };
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

let result = json =>
  Json.Decode.{
    records: json |> optional(field("records", array(record))),
    resultCount: json |> field("resultCount", int),
    facets: json |> optional(field("facets", dict(array(facet)))),
    status: json |> field("status", string),
  };

let search = (lookfor, filters, page, limit, onResults) => {
  Js.log(filters);
  let filterStr =
    Array.map(
      f => {
        let key = f.key;
        let value = facetLabel(f.facet.value);
        {j|filter[]=$key:"$value"|j};
      },
      filters,
    )
    |> Js.Array.joinWith("&");
  let facetStr = "&facet[]=format&facet[]=building&facet[]=online_boolean";

  let url = {j|$apiUrl/api/v1/search?lookfor=$lookfor&type=AllFields&field[]=id&field[]=formats&field[]=title&field[]=buildings&field[]=images&field[]=authors&field[]=year&sort=relevance%2Cid%20asc&page=$page&limit=$limit&prettyPrint=false&lng=fi&$filterStr$facetStr&facetFilter[]=building%3A0%2F.*&facetFilter[]=format%3A0%2F.*|j};

  Js.log(url);
  Js.Promise.(
    Fetch.fetch(url)
    |> then_(Fetch.Response.text)
    |> then_(text =>
         text |> Json.parseOrRaise |> result |> onResults |> resolve
       )
    |> ignore
  );
};
