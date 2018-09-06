let component = ReasonReact.statelessComponent("Facets");

let make =
    (
      ~onGetFacets,
      ~onSelectFacet,
      ~onClearFacet,
      ~onClearFilters,
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
  /*   { */
  /*   ReasonReact.array( */
  /*     filtersWithFacet */
  /*     |> Js.Dict.keys */
  /*     |> Js.Array.map(facetKey => */
  /*          switch (Js.Dict.get(facets, facetKey)) { */
  /*          | Some(items) => */
  /*            let activeFacet = */
  /*              switch ( */
  /*                List.find( */
  /*                  (f: Finna.filter) => f.key === facetKey, */
  /*                  Array.to_list(activeFacets), */
  /*                ) */
  /*              ) { */
  /*              | activeFacet => Some(activeFacet) */
  /*              | exception Not_found => None */
  /*              }; */
  /*            let dummy: Finna.facet = { */
  /*              value: "", */
  /*              label: facetKey, */
  /*              count: 0, */
  /*              facetType: Normal, */
  /*            }; */
  /*            let hd: Finna.facet = List.hd(Array.to_list(items)); */
  /*            let facetType = hd.facetType; */
  /*            let items = */
  /*              Array.of_list([dummy, ...Array.to_list(items)]); */
  /*            <Facet */
  /*              onGetFacets */
  /*              onSelectFacet */
  /*              onClearFacet */
  /*              facetKey */
  /*              facetType */
  /*              items */
  /*              activeFacet */
  /*            />; */
  /*          | None => ReasonReact.null */
  /*          } */
  /*        ), */
  /*   ) */
  /* } */
  /* /> */
  /* <button onClick=onClearFilters> {str("Clear filters")} </button> */
};
