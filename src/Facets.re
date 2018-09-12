let component = ReasonReact.statelessComponent("Facets");

let make =
    (
      ~onGetFacets,
      ~onSelectFacet,
      ~onClearFacet,
      /* ~onClearFilters, */
      ~facets,
      _children,
    ) => {
  ...component,
  render: _self =>
    <div>
      {
        ReasonReact.array(
          facets
          |> Js.Dict.values
          |> Array.map((facet: Finna.facet) =>
               <Facet onGetFacets onSelectFacet onClearFacet facet />
             ),
        )
      }
    </div>,
  /* <button onClick=onClearFilters> {str("Clear filters")} </button> */
};
