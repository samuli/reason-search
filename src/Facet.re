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
  | FacetClick(ReactSelect.selectOption, string)
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
    | FacetClick((obj: ReactSelect.selectOption), _action) =>
      switch (ReactSelect.valueGet(obj)) {
      | "*" => onClearFacet(facet.key)
      | _ =>
        onSelectFacet(
          facet.key,
          ReactSelect.valueGet(obj),
          ReactSelect.labelGet(obj),
        )
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
      let all: Finna.facetItem = {value: "*", label: title, count: 0};
      let options =
        List.mapi(
          (ind, facet: Finna.facetItem) => {
            let label = facet.label;
            let label =
              ind > 0 ?
                label ++ " (" ++ string_of_int(facet.count) ++ ")" : label;

            let value = facet.value;
            ReactSelect.selectOption(~label, ~value);
          },
          [all, ...Array.to_list(self.state.facet.items)],
        )
        |> Array.of_list;

      let (selected, first) =
        switch (facet.value) {
        | Value(value) =>
          Array.length(options) == 1 ?
            switch (
              List.find(
                (f: Finna.filter) => f.key == facet.key,
                Array.to_list(filters),
              )
            ) {
            | exception Not_found => (options[0], true)
            | f =>
              let label =
                switch (f.label) {
                | Some(l) => l
                | None => ""
                };
              (ReactSelect.selectOption(~label, ~value=f.value), true);
            } :
            (
              switch (
                List.find(
                  (opt: ReactSelect.selectOption) =>
                    ReactSelect.valueGet(opt) == value,
                  Array.to_list(options),
                )
              ) {
              | exception Not_found => (options[0], true)
              | f => (f, false)
              }
            )
        | None => (options[0], true)
        };
      <div
        className={
          Style.facetMenu ++ " mb-2 sm:w-1/2" ++ (ind == 0 ? " sm:mr-2" : "")
        }>
        {
          let label = ReactSelect.labelGet(selected);
          let label =
            first ?
              label :
              Js.String.substring(
                ~from=0,
                ~to_=Js.String.indexOf("(", label) - 1,
                label,
              );

          let value = ReactSelect.valueGet(selected);
          let selected = ReactSelect.selectOption(~label, ~value);
          <ReactSelect
            options
            selected
            onMenuOpen=((_a, _b) => self.send(Focus))
            onChange=((obj, action) => self.send(FacetClick(obj, action)))
            onMenuClose=(() => self.send(MenuClose))
            isLoading={self.state.mode == Loading}
            loadingMessage=(_s => "Loading...")
            placeholder={facet.key}
          />;
        }
      </div>;
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
