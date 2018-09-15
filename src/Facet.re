open Util;

type state =
  | Closed
  | Loading
  | Loaded;

type action =
  | FacetClick(ReactSelect.selectOption, string)
  | BooleanFacetClick(bool)
  | Focus
  | FacetsLoaded;

let component = ReasonReact.reducerComponent("Facet");

let boolFacet = (onClick, value, label) =>
  <span>
    <label>
      <input type_="checkbox" onClick value checked={value == "0"} />
    </label>
  </span>;
let make =
    (
      ~ind,
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
          (self => onGetFacets(facet.key, self.send)),
        )
      | _ => ReasonReact.NoUpdate
      }
    | FacetClick(obj, _action) =>
      onSelectFacet(facet.key, ReactSelect.valueGet(obj));
      ReasonReact.Update(Closed);
    | BooleanFacetClick(selected) =>
      onSelectFacet(facet.key, selected ? "1" : "0");
      ReasonReact.NoUpdate;
    | FacetsLoaded => ReasonReact.Update(Loaded)
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
              let label =
                facet.label ++ " (" ++ string_of_int(facet.count) ++ ")";

              let value = facet.value;
              ReactSelect.selectOption(~label, ~value);
            },
            facet.items,
          );
        <div className={"mb-2 sm:w-1/2" ++ (ind == 0 ? " sm:mr-2" : "")}>
          <ReactSelect
            options
            onFocus=((_a, _b) => self.send(Focus))
            onChange=(
              (obj: ReactSelect.selectOption, action) =>
                self.send(FacetClick(obj, action))
            )
            isLoading={self.state == Loading}
            loadingMessage=(_s => "Loading...")
            placeholder={facet.key}
          />
        </div>;
      | Boolean =>
        <div>
          {
            boolFacet(
              _ => self.send(BooleanFacetClick(true)),
              "1",
              facet.key,
            )
          }
        </div>
      }
    },
};
