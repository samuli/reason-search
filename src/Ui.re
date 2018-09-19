open Util;
open Types;

/* reference: https://reactjs.org/docs/fragments.html */
module Fragment = {
  [@bs.module "react"] external fragment: ReasonReact.reactClass = "Fragment";
  let make = children =>
    ReasonReact.wrapJsForReason(
      ~reactClass=fragment,
      ~props=Js.Obj.empty(),
      children,
    );
};

module Error = {
  let component = ReasonReact.statelessComponent("Error");

  let make = (~message, _children) => {
    ...component,
    render: _self => <div className=Style.error> {str(message)} </div>,
  };
};

module Results = {
  let isVisited = (history, id) =>
    switch (List.find(recId => recId == id, Array.to_list(history))) {
    | exception Not_found => false
    | _ => true
    };

  type state = {history: array(string)};

  type action =
    | Init(state)
    | OpenRecord(string);

  let component = ReasonReact.reducerComponent("Results");

  let stateMemory: ref(state) = ref({history: [||]});

  let make =
      (
        ~dispatch,
        ~openUrl,
        ~showImages,
        ~facets,
        ~filters,
        ~activeFilters,
        ~searchStatus,
        ~resultCnt,
        ~records,
        ~pageCnt,
        ~page,
        _children,
      ) => {
    ...component,
    initialState: () => {history: [||]},
    didMount: self => self.send(Init(stateMemory^)),
    willUnmount: self => stateMemory := self.state,
    reducer: (action: action, state: state) =>
      switch (action) {
      | OpenRecord(id) =>
        openUrl("/Record/" ++ id);
        ReasonReact.Update({history: Array.append(state.history, [|id|])});
      | Init(state) => ReasonReact.Update(state)
      | _ => ReasonReact.NoUpdate
      },
    render: self =>
      <div>
        {
          switch (searchStatus) {
          | ResultsStatus
          | LoadingStatus
          | LoadingMoreStatus =>
            <Fragment>
              <div className=Style.container>
                {
                  switch (searchStatus) {
                  | ResultsStatus
                  | LoadingMoreStatus =>
                    <p className=Style.searchResultsInfo>
                      {str("Results: " ++ string_of_int(resultCnt))}
                    </p>
                  | _ => ReasonReact.null
                  }
                }
                <ul className=Style.searchResults>
                  {
                    ReasonReact.array(
                      Array.map(
                        (r: Finna.record) =>
                          <Record
                            details=Record.List
                            record=r
                            onClick={_e => self.send(OpenRecord(r.id))}
                            /* onClick={_e => openUrl("/Record/" ++ r.id)} */
                            onSelectFacet={
                              (facetKey, facetValue, label) =>
                                dispatch(
                                  FacetResultsCmd(
                                    facetKey,
                                    facetValue,
                                    label,
                                  ),
                                )
                            }
                            filters=activeFilters
                            showImages
                            isVisited={isVisited(self.state.history, r.id)}
                          />,
                        records,
                      ),
                    )
                  }
                </ul>
                {
                  searchStatus == LoadingStatus
                  || searchStatus == LoadingMoreStatus ?
                    <Loading padding=1 /> :
                    <NextPage
                      loading={searchStatus == LoadingMoreStatus}
                      pageCnt
                      page
                      onNextPage=(_ => dispatch(NextPageCmd))
                    />
                }
              </div>
            </Fragment>
          | NoResultsStatus =>
            <div className=Style.pad> {str("No results")} </div>
          | _ => ReasonReact.null
          }
        }
      </div>,
  };
};

module RecordPage = {
  let component = ReasonReact.statelessComponent("Record");

  let make =
      (
        ~searchStatus,
        ~dispatch,
        ~openUrl,
        ~record: Finna.record,
        ~activeFilters,
        _children,
      ) => {
    ...component,
    render: _self =>
      <div>
        {
          switch (searchStatus) {
          | LoadingStatus => <Loading padding=5 />
          | ResultsStatus =>
            <Record
              details=Record.Full
              record
              onClick=(_e => openUrl("/Record/" ++ record.id))
              onSelectFacet=(
                (facetKey, facetValue, label) =>
                  dispatch(FacetResultsCmd(facetKey, facetValue, label))
              )
              filters=activeFilters
              showImages=true
            />
          | _ => ReasonReact.null
          }
        }
      </div>,
  };
};

module Ui = {
  type screen =
    | Results
    | Record;

  let component = ReasonReact.statelessComponent("Ui");

  let make =
      (
        ~screen,
        ~dispatch,
        ~openUrl,
        ~searchStatus,
        ~facets,
        ~filters,
        ~activeFilters,
        ~resultCnt,
        ~pageCnt,
        ~records,
        ~page,
        _children,
      ) => {
    ...component,
    render: _self => <div> {str("ui")} </div>,
  };
};
