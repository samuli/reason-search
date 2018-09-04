let apiUrl = "https://api.finna.fi";
let baseUrl = "https://finna.fi";
let recordBaseUrl = baseUrl ++ "/Record/";

type translated = {
  value: string,
  label: string,
};
type facet = {
  value: string,
  label: string,
  count: int,
};
type filter = {
  key: string,
  facet,
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
let facet = json =>
  Json.Decode.{
    value: json |> field("value", string),
    label: json |> field("translated", string),
    count: json |> field("count", int),
  };
/*
 authors: {
   primary: "lorem ipsum": { role: "jee"}
 }
 */
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
        let value = f.facet.value;
        {j|filter[]=$key:"$value"|j};
      },
      filters,
    )
    |> Js.Array.joinWith("&");
  let facetStr = "&facet[]=format&facet[]=building";

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
