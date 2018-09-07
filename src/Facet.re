open Util;

type state =
  | Closed
  | Loading
  | Open;

type action =
  | FacetClick(string)
  | Focus;

let component = ReasonReact.reducerComponent("Facet");

let boolFacet = (onClick, value, label) =>
  <span>
    <label>
      <input type_="checkbox" onClick name="foo" value />
      {str(label)}
    </label>
  </span>;

let make =
    (
      ~onGetFacets,
      ~onSelectFacet,
      ~onClearFacet,
      ~facet: Finna.facet,
      _children,
    ) => {
  ...component,
  initialState: () => Closed,
  reducer: (action: action, state: state) =>
    switch (action) {
    | Focus =>
      switch (state) {
      | Closed =>
        ReasonReact.UpdateWithSideEffects(
          Loading,
          (_self => onGetFacets(facet.key)),
        )
      | _ => ReasonReact.NoUpdate
      }
    | FacetClick(value) =>
      switch (
        List.find(
          (f: Finna.facetItem) => f.label == value,
          Array.to_list(facet.items),
        )
      ) {
      | selected =>
        onSelectFacet(facet.key, selected.value);
        ReasonReact.NoUpdate;
      | exception Not_found => ReasonReact.NoUpdate
      }
    },
  render: self =>
    switch (facet.value) {
    | Value(selectedValue) =>
      switch (facet.facetType) {
      | Normal =>
        <div
          className="pointer mr-2 text-xs uppercase p-2 bg-grey-light rounded cursor-pointer w-auto inline-block hover:bg-red-light"
          onClick=(_e => onClearFacet(facet.key))>
          {str(selectedValue)}
        </div>
      | Boolean =>
        <div>
          {boolFacet(_ => onClearFacet(facet.key), "0", selectedValue)}
        </div>
      }

    | None =>
      switch (facet.facetType) {
      | Normal =>
        <div>
          <p> {str(facet.key)} </p>
          <select
            onFocus=(
              _ => {
                self.send(Focus);
                Js.log("focus");
              }
            )
            onClick=(_ => Js.log("click"))
            onChange=(
              e => self.send(FacetClick(ReactEvent.Form.target(e)##value))
            )
            className={"w-1/3 mr-2 p-1 facet " ++ facet.key}>
            {
              ReasonReact.array(
                Array.map(
                  (facet: Finna.facetItem) =>
                    <option key={facet.value}> {str(facet.label)} </option>,
                  facet.items,
                ),
              )
            }
          </select>
        </div>
      | Boolean =>
        <div>
          {boolFacet(_ => self.send(FacetClick("1")), "1", facet.key)}
        </div>
      }
    },
};
