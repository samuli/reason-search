[%%debugger.chrome];

open Util;

type searchState =
  | Loading
  | NoResults
  | Done;

type state = {
  loading: bool,
  text: string,
  result: Finna.resultProcessed,
  records: array(Finna.record),
  page: int,
  pageCnt: int,
  showImages: bool,
  limit: int,
  facets: Js.Dict.t(Finna.facet),
};

type action =
  | Search(string, bool)
  | Results(Finna.resultProcessed)
  | ToggleImages
  | NextPage
  | GetFacets(string)
  | ReceiveFacets(string, Finna.resultProcessed)
  | FacetResults(string, string)
  | ClearFacet(string)
  | ClearFilters;

let component = ReasonReact.reducerComponent("App");

let setFacet =
    (facets: Js.Dict.t(Finna.facet), facetKey, facetValue: Finna.facetValue) =>
  switch (Js.Dict.get(facets, facetKey)) {
  | Some(facet) =>
    Js.Dict.set(facets, facetKey, {...facet, value: facetValue});
    facets;
  | None => facets
  };

let getActiveFilters = facets => {
  let values = Js.Dict.values(facets);
  let filterDummy: Finna.filter = {key: "", value: ""};
  let filters = ArrayLabels.make(Array.length(values), filterDummy);
  Array.iteri(
    (i, facet: Finna.facet) =>
      switch (facet.value) {
      | None => ()
      | Value(value) =>
        let filter: Finna.filter = {key: facet.key, value};
        filters[i] = filter;
      },
    values,
  );

  Js.log(filters);

  let activeFilters =
    List.filter(
      (f: Finna.filter) =>
        switch (f.key) {
        | "" => false
        | _ => true
        },
      Array.to_list(filters),
    );
  Js.log(activeFilters);

  Array.of_list(activeFilters);
};

let make = _children => {
  ...component,
  initialState: () => {
    loading: true,
    text: "mauri kunnas",
    result: {
      records: Some([||]),
      facets: Js.Dict.empty(),
      resultCount: 0,
      status: "foo",
    },
    page: 1,
    limit: 50,
    pageCnt: 0,
    showImages: false,
    records: [||],
    facets: Finna.getInitialFacets(),
  },
  reducer: (action: action, state: state) =>
    switch (action) {
    | Search(text, newSearch) =>
      /* let values = Js.Dict.values(state.facets); */
      /* let filterDummy: Finna.filter = {key: "", value: ""}; */
      /* let filters = ArrayLabels.make(Array.length(values), filterDummy); */
      /* Array.iteri( */
      /*   (i, facet: Finna.facet) => */
      /*     switch (facet.value) { */
      /*     | None => () */
      /*     | Value(value) => */
      /*       let filter: Finna.filter = {key: facet.key, value}; */
      /*       filters[i] = filter; */
      /*     }, */
      /*   values, */
      /* ); */

      /* Js.log(filters); */

      /* let activeFilters = */
      /*   List.filter( */
      /*     (f: Finna.filter) => */
      /*       switch (f.key) { */
      /*       | "" => false */
      /*       | _ => true */
      /*       }, */
      /*     Array.to_list(filters), */
      /*   ); */
      /* Js.log(activeFilters); */

      ReasonReact.UpdateWithSideEffects(
        {
          ...state,
          text,
          page: newSearch ? 1 : state.page,
          records: newSearch ? [||] : state.records,
          loading: true,
        },
        (
          self =>
            Finna.search(
              ~lookfor=self.state.text,
              ~filters=getActiveFilters(state.facets),
              ~page=self.state.page,
              ~limit=self.state.limit,
              /* ~facetKey=None, */
              ~onResults=results => self.send(Results(results)),
              (),
            )
        ),
      )
    | Results(result) =>
      let newFacets = Finna.getInitialFacets();

      let _foo =
        state.facets
        |> Js.Dict.values
        |> Array.iter((facet: Finna.facet) =>
             switch (facet.value) {
             | Value(_value) => Js.Dict.set(newFacets, facet.key, facet)
             | None => ()
             }
           );

      ReasonReact.Update({
        ...state,
        pageCnt: int_of_float(float_of_int(result.resultCount) /. 50.0) + 1,
        result,
        records:
          switch (result.records) {
          | Some(records) => Array.append(state.records, records)
          | None => state.records
          },
        facets: newFacets,
        loading: false,
      });
    | ToggleImages =>
      ReasonReact.Update({...state, showImages: !state.showImages})
    | NextPage =>
      ReasonReact.UpdateWithSideEffects(
        {...state, page: state.page + 1},
        (self => self.send(Search(state.text, false))),
      )
    | GetFacets(facetKey) =>
      Js.log("get: " ++ facetKey);
      /* ReasonReact.NoUpdate; */
      ReasonReact.UpdateWithSideEffects(
        state,
        (
          self =>
            Finna.getFacets(
              ~lookfor=self.state.text,
              ~filters=getActiveFilters(state.facets),
              ~page=self.state.page,
              ~facetKey=Some(facetKey),
              ~onResults=results =>
              self.send(ReceiveFacets(facetKey, results))
            )
        ),
      );
    | ReceiveFacets(facetKey, results) =>
      Js.log("receive");
      Js.log(results.facets);
      switch (Js.Dict.get(results.facets, facetKey)) {
      | Some(facetItem) =>
        let facets = state.facets;
        Js.Dict.set(facets, facetKey, facetItem);
        ReasonReact.Update({...state, facets});
      | None => ReasonReact.NoUpdate
      };
    | FacetResults(facetKey, value) =>
      ReasonReact.UpdateWithSideEffects(
        {
          ...state,
          facets: setFacet(state.facets, facetKey, Finna.Value(value)),
        },
        (self => self.send(Search(state.text, true))),
      )
    | ClearFacet(facetKey) =>
      ReasonReact.UpdateWithSideEffects(
        {...state, facets: setFacet(state.facets, facetKey, Finna.None)},
        (self => self.send(Search(state.text, true))),
      )
    | ClearFilters =>
      ReasonReact.UpdateWithSideEffects(
        {...state, facets: Finna.getInitialFacets()},
        (self => self.send(Search(state.text, true))),
      )
    },
  render: self => {
    let resultCnt = self.state.result.resultCount;
    let opt = ReactSelect.selectOption(~label="labeli", ~value="valuee");
    Js.log([|opt, opt, opt, opt, opt, opt|]);
    <div className="p-5">
      <ReactSelect options=[|opt, opt, opt, opt, opt, opt|] />
      <SearchField onSearch={text => self.send(Search(text, true))} />
      <input
        checked={self.state.showImages}
        type_="checkbox"
        onChange={_ => self.send(ToggleImages)}
      />
      <Facets
        facets={self.state.facets}
        onGetFacets={facetKey => self.send(GetFacets(facetKey))}
        onSelectFacet={
          (facetKey, facetValue) =>
            self.send(FacetResults(facetKey, facetValue))
        }
        onClearFacet={filter => self.send(ClearFacet(filter))}
        onClearFilters={_ => self.send(ClearFilters)}
      />
      {
        self.state.loading ?
          ReasonReact.null :
          <div className="info mt-2 mb-2">
            {str("Results: " ++ string_of_int(resultCnt))}
          </div>
      }
      <ul className="results mt-5 list-reset">
        {
          switch (self.state.result.resultCount) {
          | 0 => <div> <p> {str("No results")} </p> </div>
          /* { */
          /*   switch (Array.length(self.state.filters)) { */
          /*   | 0 => ReasonReact.null */
          /*   | _ => */
          /*     <div onClick=(_ => self.send(ClearFilters))> */
          /*       {str("Remove filters")} */
          /*     </div> */
          /*   } */
          /* } */
          | _ =>
            ReasonReact.array(
              Array.map(
                (r: Finna.record) =>
                  <Record
                    record=r
                    onSelectFacet={
                      (facetKey, facetValue) =>
                        self.send(FacetResults(facetKey, facetValue))
                    }
                    showImages={self.state.showImages}
                  />,
                self.state.records,
              ),
            )
          }
        }
      </ul>
      <NextPage
        loading={self.state.loading}
        pageCnt={self.state.pageCnt}
        page={self.state.page}
        onNextPage={_ => self.send(NextPage)}
      />
    </div>;
  },
  didMount: self => {
    /* focus search field on keypress */
    Webapi.Dom.Element.addKeyDownEventListener(
      e => {
        let code = Webapi.Dom.KeyboardEvent.code(e);
        Js.Re.fromString("Key.*")
        |> Js.Re.exec(code)
        |> (
          fun
          | Some(_result) => {
              let _x = [%bs.raw
                {| document.getElementById("search").focus() |}
              ];
              ();
            }
          | None => ()
        );
      },
      Webapi.Dom.Document.documentElement(Webapi.Dom.document),
    );

    /* initial search */
    self.send(Search(self.state.text, true));
  },
};
