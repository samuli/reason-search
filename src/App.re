[%%debugger.chrome];

type iw;
type el = Webapi.Dom.Element.t;
[@bs.module] external inview: string => iw = "in-view";
[@bs.send] external on: (iw, string, el => unit) => unit = "";

let registerInview = () => {
  let i = inview(".record-image");
  on(i, "enter", el =>
    switch (Webapi.Dom.Element.getAttribute("data-inview", el)) {
    | Some(_val) => ()
    | _ =>
      Webapi.Dom.Element.setAttribute("data-inview", "1", el);
      switch (Webapi.Dom.Element.getAttribute("href", el)) {
      | Some(url) => Webapi.Dom.Element.setAttribute("src", url, el)
      | _ => ()
      };
    }
  );
};

open Types;

Style.init();

type route =
  | Search
  | Record(string);

type state = {
  lng: string,
  searchStatus: searchState,
  text: string,
  result: option(Finna.searchResponse),
  records: array(Finna.record),
  recordResult: option(Finna.recordResponse),
  record: option(Finna.record),
  page: int,
  pageCnt: int,
  showImages: bool,
  limit: int,
  facets: Js.Dict.t(Finna.facet),
  filters: array(Finna.filter),
  route,
};

let urlChange = (send, _state, url: ReasonReact.Router.url) => {
  let hash = url.hash;
  switch (hash) {
  | "/"
  | "" => send(CloseRecordCmd)
  | _ =>
    let hash = String.sub(url.hash, 1, String.length(url.hash) - 1);
    switch (Js.String.split("/", hash)) {
    | [|"Record", id|] => send(RecordCmd(id))
    | [|"Search", params|] =>
      switch (Js.String.split("=", params)) {
      | [|"lookfor", ""|] => ()
      | [|"lookfor", lookfor|] =>
        let lookfor = Js_global.decodeURIComponent(lookfor);
        send(SearchCmd(lookfor, Search));
      | _ => send(CloseRecordCmd)
      }
    | _ => ()
    };
    ();
  };
};
let openUrl = url => ReasonReact.Router.push("/#" ++ url);

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
  let filterDummy: Finna.filter = {key: "", value: "", label: None};
  let filters = ArrayLabels.make(Array.length(values), filterDummy);
  Array.iteri(
    (i, facet: Finna.facet) =>
      switch (facet.value) {
      | None => ()
      | Value(value) =>
        let filter: Finna.filter = {key: facet.key, value, label: None};
        filters[i] = filter;
      },
    values,
  );

  let activeFilters =
    List.filter(
      (f: Finna.filter) =>
        switch (f.key) {
        | "" => false
        | _ => true
        },
      Array.to_list(filters),
    );

  Array.of_list(activeFilters);
};

let component = ReasonReact.reducerComponent("App");

let make = _children => {
  ...component,
  initialState: () => {
    lng: "fi",
    searchStatus: NotLoadedStatus,
    text: "",
    page: 1,
    limit: 30,
    pageCnt: 0,
    showImages: false,
    records: [||],
    record: None,
    result: None,
    recordResult: None,
    facets: Finna.getInitialFacets(),
    filters: [||],
    route: Search,
  },
  reducer: (action: action, state: state) =>
    switch (action) {
    | OnSearch(text) =>
      /* openUrl("/Search/lookfor=" ++ text); */
      ReasonReact.UpdateWithSideEffects(
        {...state, text: "", searchStatus: LoadingStatus},
        (_self => openUrl("/Search/lookfor=" ++ text)),
      )
    | SearchCmd(text, searchType) =>
      switch (searchType) {
      | BackToResults
      | Search when text == state.text =>
        ReasonReact.Update({
          ...state,
          text,
          route: Search,
          searchStatus: ResultsStatus,
        })
      | _ =>
        let newSearch =
          switch (searchType) {
          | NewSearch => true
          | MoreResults => false
          | Search when text != state.text => true
          | _ => false
          };
        ReasonReact.UpdateWithSideEffects(
          {
            ...state,
            text,
            page: newSearch ? 1 : state.page,
            records: newSearch ? [||] : state.records,
            searchStatus: newSearch ? LoadingStatus : LoadingMoreStatus,
            route: Search,
          },
          (
            self =>
              Finna.search(
                ~lookfor=text,
                ~filters=getActiveFilters(state.facets),
                ~page=self.state.page,
                ~limit=self.state.limit,
                ~lng=state.lng,
                ~onResults=results => self.send(ResultsCmd(results)),
                (),
              )
              |> ignore
          ),
        );
      }
    | ResultsCmd((response: Finna.searchResponse)) =>
      Js.log("results");
      response.error ?
        ReasonReact.Update({...state, searchStatus: ErrorStatus}) :
        {
          let newFacets = Finna.getInitialFacets();
          switch (response.results) {
          | None => ReasonReact.NoUpdate
          | Some(result) =>
            let resultCount = result.resultCount;
            let _foo =
              state.facets
              |> Js.Dict.values
              |> Array.iter((facet: Finna.facet) =>
                   switch (facet.value) {
                   | Value(_value) =>
                     Js.Dict.set(newFacets, facet.key, facet)
                   | None => ()
                   }
                 );

            ReasonReact.Update({
              ...state,
              pageCnt: int_of_float(float_of_int(resultCount) /. 50.0) + 1,
              result: Some(response),
              records:
                switch (result.records) {
                | Some(records) => Array.append(state.records, records)
                | None => state.records
                },
              facets: newFacets,
              searchStatus: resultCount > 0 ? ResultsStatus : NoResultsStatus,
            });
          };
        };
    | ToggleImagesCmd =>
      ReasonReact.Update({...state, showImages: !state.showImages})
    | NextPageCmd =>
      ReasonReact.UpdateWithSideEffects(
        {...state, page: state.page + 1},
        (self => self.send(SearchCmd(state.text, MoreResults))),
      )
    | GetFacetsCmd(facetKey, onLoaded) =>
      ReasonReact.UpdateWithSideEffects(
        state,
        (
          self =>
            Finna.getFacets(
              ~lookfor=self.state.text,
              ~filters=getActiveFilters(state.facets),
              ~page=self.state.page,
              ~facetKey,
              ~lng=self.state.lng,
              ~onResults=results =>
              self.send(ReceiveFacetsCmd(facetKey, results, onLoaded))
            )
            |> ignore
        ),
      )
    | ReceiveFacetsCmd(facetKey, (response: Finna.searchResponse), onLoaded) =>
      response.error ?
        ReasonReact.NoUpdate :
        (
          switch (response.results) {
          | Some(results) =>
            switch (Js.Dict.get(results.facets, facetKey)) {
            | Some(facetItem) =>
              let facets = state.facets;
              Js.Dict.set(facets, facetKey, facetItem);
              ReasonReact.UpdateWithSideEffects(
                {...state, facets},
                (_s => onLoaded(FacetsLoaded(facetItem))),
              );
            | None => ReasonReact.NoUpdate
            }
          | None => ReasonReact.NoUpdate
          }
        )
    | FacetResultsCmd(facetKey, value, label) =>
      let filter: Finna.filter = {key: facetKey, value, label: Some(label)};
      let filters =
        List.filter(
          (f: Finna.filter) => f.key != facetKey,
          Array.to_list(state.filters),
        )
        |> Array.of_list;
      let filters = Array.append(filters, [|filter|]);
      ReasonReact.UpdateWithSideEffects(
        {
          ...state,
          facets: setFacet(state.facets, facetKey, Finna.Value(value)),
          filters,
        },
        (self => self.send(SearchCmd(state.text, NewSearch))),
      );
    | ClearFacetCmd(facetKey) =>
      let filters =
        List.filter(
          (f: Finna.filter) => f.key != facetKey,
          Array.to_list(state.filters),
        )
        |> Array.of_list;
      let facets = setFacet(state.facets, facetKey, Finna.None);
      ReasonReact.UpdateWithSideEffects(
        {...state, facets, filters},
        (self => self.send(SearchCmd(state.text, NewSearch))),
      );
    | RecordCmd(id) =>
      ReasonReact.UpdateWithSideEffects(
        {...state, route: Record(id), searchStatus: LoadingStatus},
        (
          self =>
            Finna.record(
              ~id,
              ~onResults=result => self.send(RecordResultCmd(result)),
              ~lng=state.lng,
              (),
            )
            |> ignore
        ),
      )
    | RecordResultCmd((result: Finna.recordResponse)) =>
      Js.log(result);
      let newState = {
        ...state,
        recordResult: Some(result),
        searchStatus: result.error ? NoResultsStatus : ResultsStatus,
      };
      switch (result.record) {
      | Some(record) =>
        let newState = {...newState, record: Some(record)};
        ReasonReact.Update(newState);
      | None => ReasonReact.Update(newState)
      };
    | CloseRecordCmd => ReasonReact.Update({...state, route: Search})
    },
  didUpdate: ({oldSelf: _self, newSelf: _newSelf}) => registerInview(),
  render: self =>
    <Ui.Fragment>
      <section role="search">
        <SearchField
          onSearch={text => self.send(OnSearch(text))}
          lookfor={self.state.text}
        />
        <div className=Style.facets>
          <Facets
            facets={self.state.facets}
            filters={self.state.filters}
            onGetFacets={
              (facetKey, onLoaded) =>
                self.send(GetFacetsCmd(facetKey, onLoaded))
            }
            onSelectFacet={
              (facetKey, facetValue, label) =>
                self.send(FacetResultsCmd(facetKey, facetValue, label))
            }
            onClearFacet={filter => self.send(ClearFacetCmd(filter))}
          />
        </div>
      </section>
      <Ui.Fragment>
        {
          switch (self.state.route) {
          | Search =>
            let resultCnt =
              switch (self.state.result) {
              | Some(reponse) =>
                switch (reponse.results) {
                | Some(results) => results.resultCount
                | None => 0
                }
              | None => 0
              };

            <Ui.Results
              dispatch={self.send}
              openUrl
              searchStatus={self.state.searchStatus}
              showImages={self.state.showImages}
              facets={self.state.facets}
              filters={self.state.filters}
              activeFilters={self.state.filters}
              resultCnt
              records={self.state.records}
              pageCnt={self.state.pageCnt}
              page={self.state.page}
            />;
          | Record(id) =>
            switch (self.state.recordResult) {
            | Some(response) =>
              response.error ?
                <Ui.Error message={"Failded to load record " ++ id} /> :
                (
                  switch (self.state.record) {
                  | Some((record: Finna.record)) =>
                    <Ui.RecordPage
                      dispatch={self.send}
                      openUrl
                      record
                      searchStatus={self.state.searchStatus}
                      activeFilters={getActiveFilters(self.state.facets)}
                    />
                  | None => ReasonReact.null
                  }
                )
            | None => ReasonReact.null
            }
          }
        }
      </Ui.Fragment>
    </Ui.Fragment>,
  didMount: self => {
    let watcherID =
      ReasonReact.Router.watchUrl(url =>
        urlChange(self.send, self.state, url)
      );
    let url = ReasonReact.Router.dangerouslyGetInitialUrl();
    urlChange(self.send, self.state, url);
    self.onUnmount(() => ReasonReact.Router.unwatchUrl(watcherID));
    /* focus search field on keypress */
    /*   Webapi.Dom.Element.addKeyDownEventListener( */
    /*     e => { */
    /*       let code = Webapi.Dom.KeyboardEvent.code(e); */
    /*       Js.Re.fromString("Key.*") */
    /*       |> Js.Re.exec(code) */
    /*       |> ( */
    /*         fun */
    /*         | Some(_result) => { */
    /*             let _x = [%bs.raw */
    /*               {| document.getElementById("search").focus() |} */
    /*             ]; */
    /*             (); */
    /*           } */
    /*         | None => () */
    /*       ); */
    /*     }, */
    /*     Webapi.Dom.Document.documentElement(Webapi.Dom.document), */
    /*   ), */
  },
  /* ) */
};
