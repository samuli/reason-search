open Util;
open Types;

module Error = {
  let component = ReasonReact.statelessComponent("Error");

  let make = (~message, _children) => {
    ...component,
    render: _self => <div className=Style.error> {str(message)} </div>,
  };
};

module Results = {
  let component = ReasonReact.statelessComponent("Results");

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
        ~isVisited,
        _children,
      ) => {
    ...component,
    render: _self =>
      <div>
        {
          switch (searchStatus) {
          | ResultsStatus
          | LoadingStatus
          | LoadingMoreStatus =>
            <div>
              <div className=Style.facets>
                <Facets
                  facets
                  filters
                  onGetFacets=(
                    (facetKey, onLoaded) =>
                      dispatch(GetFacetsCmd(facetKey, onLoaded))
                  )
                  onSelectFacet=(
                    (facetKey, facetValue, label) =>
                      dispatch(FacetResultsCmd(facetKey, facetValue, label))
                  )
                  onClearFacet=(filter => dispatch(ClearFacetCmd(filter)))
                />
              </div>
              <div className=Style.container>
                {
                  searchStatus == ResultsStatus ?
                    <p className=Style.searchResultsInfo>
                      {str("Results: " ++ string_of_int(resultCnt))}
                    </p> :
                    ReasonReact.null
                }
                <ul className=Style.searchResults>
                  {
                    ReasonReact.array(
                      Array.map(
                        (r: Finna.record) =>
                          <Record
                            details=Record.List
                            record=r
                            onClick={_e => openUrl("/Record/" ++ r.id)}
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
                            isVisited={isVisited(r.id)}
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
            </div>
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
