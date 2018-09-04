open Util;

type searchState =
  | Loading
  | NoResults
  | Done;

type state = {
  loading: bool,
  text: string,
  result: Finna.result,
  records: array(Finna.record),
  page: int,
  pageCnt: int,
  showImages: bool,
  limit: int,
  filters: array(Finna.filter),
};

type action =
  | Search(string, bool)
  | Results(Finna.result)
  | ToggleImages
  | NextPage
  | FacetResults(Finna.filter)
  | ClearFacet(Finna.filter)
  | ClearFilters;

let component = ReasonReact.reducerComponent("App");

let make = _children => {
  ...component,
  initialState: () => {
    loading: true,
    text: "mauri kunnas",
    result: {
      records: Some([||]),
      resultCount: 0,
      status: "foo",
      facets: None,
    },
    page: 1,
    limit: 50,
    pageCnt: 0,
    showImages: false,
    records: [||],
    filters: [||],
  },
  reducer: (action: action, state: state) =>
    switch (action) {
    | Search(text, newSearch) =>
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
              self.state.text,
              self.state.filters,
              self.state.page,
              self.state.limit,
              results =>
              self.send(Results(results))
            )
        ),
      )
    | Results(result) =>
      ReasonReact.Update({
        ...state,
        pageCnt: int_of_float(float_of_int(result.resultCount) /. 50.0) + 1,
        result,
        records:
          switch (result.records) {
          | Some(records) => Array.append(state.records, records)
          | None => state.records
          },
        loading: false,
      })
    | ToggleImages =>
      ReasonReact.Update({...state, showImages: !state.showImages})
    | NextPage =>
      ReasonReact.UpdateWithSideEffects(
        {...state, page: state.page + 1},
        (self => self.send(Search(state.text, false))),
      )
    | FacetResults(filter) =>
      ReasonReact.UpdateWithSideEffects(
        {...state, filters: Array.append(state.filters, [|filter|])},
        (self => self.send(Search(state.text, true))),
      )
    | ClearFacet(filter) =>
      ReasonReact.UpdateWithSideEffects(
        {
          ...state,
          filters:
            Array.of_list(
              List.filter(
                (f: Finna.filter) => f.key != filter.key,
                Array.to_list(state.filters),
              ),
            ),
        },
        (self => self.send(Search(state.text, true))),
      )
    | ClearFilters =>
      ReasonReact.UpdateWithSideEffects(
        {...state, filters: [||]},
        (self => self.send(Search(state.text, true))),
      )
    },
  render: self => {
    let resultCnt = self.state.result.resultCount;
    <div className="p-5">
      <SearchField onSearch={text => self.send(Search(text, true))} />
      <input
        checked={self.state.showImages}
        type_="checkbox"
        onChange={_ => self.send(ToggleImages)}
      />
      {
        let facets =
          switch (self.state.result.facets) {
          | Some(facets) => facets
          | None => Js.Dict.empty()
          };

        <Facets
          facets
          activeFacets={self.state.filters}
          onSelectFacet={filter => self.send(FacetResults(filter))}
          onClearFacet={filter => self.send(ClearFacet(filter))}
          onClearFilters={_ => self.send(ClearFilters)}
        />;
      }
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
          | 0 =>
            <div>
              <p> {str("No results")} </p>
              {
                switch (Array.length(self.state.filters)) {
                | 0 => ReasonReact.null
                | _ =>
                  <div onClick=(_ => self.send(ClearFilters))>
                    {str("Remove filters")}
                  </div>
                }
              }
            </div>
          | _ =>
            ReasonReact.array(
              Array.map(
                (r: Finna.record) =>
                  <Record
                    record=r
                    onSelectFacet={filter => self.send(FacetResults(filter))}
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
