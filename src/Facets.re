let component = ReasonReact.statelessComponent("Facets");

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
    <div className="flex sm:flex-row flex-col">
      {
        ReasonReact.array(
          facets
          |> Js.Dict.values
          |> Array.mapi((ind, facet: Finna.facet) =>
               <Facet
                 ind
                 onGetFacets
                 onSelectFacet
                 onClearFacet
                 facet
                 filters
                 title={"All " ++ facet.key ++ "s"}
               />
             ),
        )
      }
    </div>,
  /* <button onClick=onClearFilters> {str("Clear filters")} </button> */
};
