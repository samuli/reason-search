[%%debugger.chrome];

open Util;

type menuMode =
  | NotLoaded
  | Closed
  | Loading
  | Loaded
  | Open;

type state = {
  mode: menuMode,
  filters: array(Finna.filter),
  facet: Finna.facet,
};
type action =
  | FacetClick(string)
  | BooleanFacetClick(bool)
  | Focus
  | FacetsLoaded(Finna.facet)
  | MenuClose;

let component = ReasonReact.reducerComponent("Facet");

let boolFacet = (onClick, value, _label) =>
  <span>
    <label>
      <input type_="checkbox" onClick value checked={value == "true"} />
    </label>
  </span>;

let make =
    (
      ~ind,
      ~onGetFacets,
      ~onSelectFacet,
      ~onClearFacet,
      ~facet: Finna.facet,
      ~filters,
      ~title,
      _children,
    ) => {
  ...component,
  initialState: () => {mode: NotLoaded, facet, filters: [||]},
  reducer: (action: action, state: state) =>
    switch (action) {
    | Focus =>
      switch (state.mode) {
      | NotLoaded =>
        ReasonReact.UpdateWithSideEffects(
          {
            ...state,
            mode: Loading,
            facet: {
              ...state.facet,
              items: [||],
            },
          },
          (self => onGetFacets(facet.key, self.send)),
        )
      | Loaded => ReasonReact.Update({...state, mode: Open})
      | _ => ReasonReact.NoUpdate
      }
    | FacetClick(value) =>
      switch (value) {
      | "*" => onClearFacet(facet.key)
      | _ => onSelectFacet(facet.key, value, "label")
      };
      ReasonReact.Update({...state, mode: Closed});
    | BooleanFacetClick(selected) =>
      if (selected) {
        onSelectFacet(facet.key, "1", "");
      } else {
        onClearFacet(facet.key);
      };
      ReasonReact.NoUpdate;
    | FacetsLoaded(facet) =>
      ReasonReact.Update({...state, mode: Loaded, facet})
    | MenuClose => ReasonReact.Update({...state, mode: Closed})
    },
  willReceiveProps: self => {
    let changed = self.state.filters != filters;
    let mode = changed ? NotLoaded : self.state.mode;
    {...self.state, mode, filters};
  },
  render: self =>
    switch (facet.facetType) {
    | Normal =>
      <div className=Style.facetMenu>
        {
          ReasonReact.array([|
            <input
              list={facet.key}
              name={facet.key}
              onKeyUp={e => Js.log("key")}
              onFocus={e => self.send(Focus)}
              placeholder={facet.key}
              onChange={
                e => {
                  Js.log("change");
                  Js.log(ReactEvent.Form.target(e)##value);
                  self.send(FacetClick(ReactEvent.Form.target(e)##value));
                }
              }
            />,
            <datalist id={facet.key}>
              {
                let all: Finna.facetItem = {
                  value: "*",
                  label: title,
                  count: 0,
                };

                ReasonReact.array(
                  List.map(
                    (f: Finna.facetItem) => <option value={f.value} />,
                    [all, ...Array.to_list(self.state.facet.items)],
                  )
                  |> Array.of_list,
                );
              }
            </datalist>,
            /* <ReactSelect */
            /*   options */
            /*   selected */
            /*   onMenuOpen=((_a, _b) => self.send(Focus)) */
            /*   onChange=((obj, action) => self.send(FacetClick(obj, action))) */
            /*   onMenuClose=(() => self.send(MenuClose)) */
            /*   isLoading={self.state.mode == Loading} */
            /*   loadingMessage=(_s => "Loading...") */
            /*   placeholder={facet.key} */
            /* />; */
          |])
        }
      </div>
    | Boolean =>
      <div>
        {
          let selected =
            switch (facet.value) {
            | Value(value) => true
            | None => false
            };
          boolFacet(
            _ => self.send(BooleanFacetClick(!selected)),
            string_of_bool(selected),
            facet.key,
          );
        }
      </div>
    },
};
