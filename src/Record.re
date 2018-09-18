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
      | true => Array.map(img => Finna.apiUrl ++ img, record.images)
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
        className={
          Style.recordList(~visited=isVisited)
          ++ " record py-1 border-b border-solid border-grey hover:bg-grey-light cursor-pointer"
        }>
        <a
          className="link font-hairline no-underline cursor-pointer"
          target="_finna">
          <h2 className="font-normal text-base"> {str(record.title)} </h2>
        </a>
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
    /* <FormatIcon record /> */
    | Full =>
      <div className=Style.recordFull>
        <h1> {str(record.title)} </h1>
        <div className="my-2">
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
                  <div> <img className=Style.recordImage src=img /> </div>,
                Array.sub(imgs, 0, min(Array.length(imgs), 5)),
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
