open Util;

type state =
  | Closed
  | Loading
  | Open;

type action =
  | FacetClick(ReactSelect.selectOption, string)
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
    | FacetClick(obj, _action) =>
      onSelectFacet(facet.key, ReactSelect.valueGet(obj));
      ReasonReact.Update(Closed);
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
        let options =
          Array.map(
            (facet: Finna.facetItem) => {
              let label = facet.label;
              let _count = facet.count;
              let value = facet.value;
              ReactSelect.selectOption(~label, ~value);
            },
            facet.items,
          );
        <div>
          <p> {str(facet.key)} </p>
          <ReactSelect
            options
            onFocus=((_a, _b) => self.send(Focus))
            onChange=(
              (obj: ReactSelect.selectOption, action) =>
                self.send(FacetClick(obj, action))
            )
          />
        </div>;
      | Boolean =>
        <div
          /* {boolFacet(_ => self.send(FacetClick("1")), "1", facet.key)} */
        />
      }
    },
};

/* className={"w-1/3 mr-2 p-1 facet " ++ facet.key} */
/* onFocus=( */
/*   _ => { */
/*     self.send(Focus); */
/*     Js.log("focus"); */
/*   } */
/* ) */
/* onClick=(_e => Js.log("click")) */
/* onChange=(e => Js.log(ReactEvent.Form.target(e)##label)) */
