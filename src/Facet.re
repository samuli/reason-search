open Util;

type state =
  | ();

type action =
  | FacetClick(string);

let component = ReasonReact.reducerComponent("Facet");

let make =
    (
      ~onSelectFacet,
      ~onClearFacet,
      ~facetKey,
      ~items,
      ~activeFacet: option(Finna.filter),
      _children,
    ) => {
  ...component,
  initialState: () => (),
  reducer: (action: action, _state: state) =>
    switch (action) {
    | FacetClick(value) =>
      switch (
        List.find(
          (f: Finna.facet) => f.label == value,
          Array.to_list(items),
        )
      ) {
      | facet =>
        let filter: Finna.filter = {key: facetKey, facet};
        onSelectFacet(filter);
        ReasonReact.NoUpdate;
      | exception Not_found => ReasonReact.NoUpdate
      }
    },
  render: self =>
    switch (activeFacet) {
    | Some(activeFacet) =>
      <div
        className="pointer mr-2 text-xs uppercase p-2 bg-grey-light rounded cursor-pointer w-auto inline-block hover:bg-red-light"
        onClick=(_e => onClearFacet(activeFacet))>
        {str(activeFacet.facet.label)}
      </div>
    | None =>
      <select
        onChange=(
          e => self.send(FacetClick(ReactEvent.Form.target(e)##value))
        )
        className={"w-1/3 mr-2 p-1 facet " ++ facetKey}>
        {
          ReasonReact.array(
            Array.map(
              (facet: Finna.facet) =>
                <option key={facet.value}> {str(facet.label)} </option>,
              items,
            ),
          )
        }
      </select>
    },
};
