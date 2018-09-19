open Util;

type details =
  | List
  | Full;
let component = ReasonReact.statelessComponent("Record");

let make =
    (
      ~details,
      ~record: Finna.record,
      ~onClick,
      ~onSelectFacet,
      ~filters,
      ~showImages,
      ~isVisited=false,
      _children,
    ) => {
  ...component,
  render: _self => {
    let imgs =
      switch (showImages) {
      | false => [||]
      | true =>
        Array.map(img => Finna.apiUrl ++ img ++ "&size=medium", record.images)
      };

    let authors =
      switch (record.authors) {
      | [||] => ReasonReact.null
      | authors =>
        <span className=Style.recordAuthors>
          {str(Js.Array.joinWith(", ", authors))}
        </span>
      };

    let publishers =
      switch (record.publishers) {
      | Some(publishers) when Array.length(publishers) > 0 =>
        <span className=Style.recrodPublisher>
          {str(Js.Array.joinWith(", ", publishers))}
        </span>
      | _ => ReasonReact.null
      };

    let year =
      switch (record.year) {
      | Some(year) => <span className=Style.recordYear> {str(year)} </span>
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
          isActive={!isFacetActive(key)}
          onSelect=onSelectFacet
        />
      | [] => ReasonReact.null
      };

    switch (details) {
    | List =>
      <li
        onClick
        key={record.id}
        className={Style.recordList(~visited=isVisited)}>
        <a target="_finna"> <h2> {str(record.title)} </h2> </a>
        <p>
          authors
          {facetLink("format", record.formats)}
          {
            switch (record.buildings) {
            | Some(buildings) => facetLink("building", buildings)
            | None => ReasonReact.null
            }
          }
        </p>
        <p> publishers year </p>
      </li>
    | Full =>
      <div className=Style.recordFull>
        <h1> {str(record.title)} </h1>
        <div>
          authors
          {
            switch (record.buildings) {
            | Some(buildings) => facetLink("building", buildings)
            | None => ReasonReact.null
            }
          }
        </div>
        <div className=Style.recordImages>
          {
            ReasonReact.array(
              Array.map(
                img =>
                  <div> <img className=Style.recordImage href=img /> </div>,
                imgs,
              ),
            )
          }
        </div>
        <p>
          <a href={Finna.recordBaseUrl ++ record.id}> {str("Finna")} </a>
        </p>
      </div>
    };
  },
};
