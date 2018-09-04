open Util;

let component = ReasonReact.statelessComponent("Record");

let make = (~record: Finna.record, ~onSelectFacet, ~showImages, _children) => {
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
        <FacetLink facetKey="format" facets={record.formats} onSelectFacet />
        <FacetLink
          facetKey="building"
          facets={record.buildings}
          onSelectFacet
        />
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
