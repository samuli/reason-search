open Util;

let component = ReasonReact.statelessComponent("Record");

let make =
    (~record: Finna.record, ~onSelectFacet, ~filters, ~showImages, _children) => {
  ...component,
  render: _self => {
    let imgs =
      switch (showImages) {
      | false => [||]
      | true => Array.map(img => Finna.apiUrl ++ img, record.images)
      };

    let authors =
      switch (record.authors) {
      | [||] => ReasonReact.null
      | authors =>
        <span className="authors mr-2">
          {str(Js.Array.joinWith(", ", authors))}
        </span>
      };

    let year =
      switch (record.year) {
      | Some(year) => <span className="year ml-2"> {str(year)} </span>
      | None => ReasonReact.null
      };

    let isFacetActive = key =>
      switch (
        List.find((f: Finna.filter) => key == f.key, Array.to_list(filters))
      ) {
      | _f => true
      | exception Not_found => false
      };

    let facetLink = (key, facets: array(Finna.translated)) =>
      switch (Array.to_list(facets)) {
      | [facet, ..._rest] =>
        <FacetLink
          label={facet.label}
          facetKey=key
          value={facet.value}
          isActive={isFacetActive(key)}
          onSelect=onSelectFacet
        />
      | [] => ReasonReact.null
      };
    <li
      key={record.id}
      className="record pb-1 mb-1 border-b border-solid border-grey">
      <a
        className="link font-hairline no-underline"
        target="_finna"
        href={Finna.recordBaseUrl ++ record.id}>
        {str(record.title)}
      </a>
      year
      /* <FormatIcon record /> */
      <p>
        authors
        {facetLink("format", record.formats)}
        {facetLink("building", record.buildings)}
        {
          switch (Array.to_list(imgs)) {
          | [] => <span />
          | [img, ..._rest] => <img className="w-1/4" src=img />
          }
        }
      </p>
    </li>;
  },
};
