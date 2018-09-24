let component = ReasonReact.statelessComponent("Facets");

let titles =
  Js.Dict.fromArray([|("building", "location"), ("format", "format")|]);

let getTitle = key =>
  "All "
  ++ (
    switch (Js.Dict.get(titles, key)) {
    | Some(value) => value
    | _ => key
    }
  )
  ++ "s";

let make =
    (
      ~onGetFacets,
      ~onSelectFacet,
      ~onClearFacet,
      ~facets,
      ~filters,
      _children,
    ) => {
  ...component,
  render: _self =>
    <div>
      {
        ReasonReact.array(
          facets
          |> Js.Dict.values
          |> Array.mapi((ind, facet: Finna.facet) =>
               <Facet
                 key={facet.key}
                 ind
                 onGetFacets
                 onSelectFacet
                 onClearFacet
                 facet
                 filters
                 title={getTitle(facet.key)}
               />
             ),
        )
      }
    </div>,
  /* <button onClick=onClearFilters> {str("Clear filters")} </button> */
};
