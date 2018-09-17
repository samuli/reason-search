open Util;
open Types;

module Error = {
  let component = ReasonReact.statelessComponent("Error");

  let make = (~message, _children) => {
    ...component,
    render: _self =>
      <div className="error mt-3 p-5 bg-red"> {str(message)} </div>,
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
      <div className="">
        {
          switch (searchStatus) {
          | ResultsStatus
          | LoadingStatus
          | LoadingMoreStatus =>
            <div>
              <div className={Style.facets ++ " px-5 pb-5 bg-grey-lighter"}>
                <Facets
                  facets
                  filters
                  onGetFacets=(
                    (facetKey, onLoaded) =>
                      dispatch(GetFacetsCmd(facetKey, onLoaded))
                  )
                  onSelectFacet=(
                    (facetKey, facetValue) =>
                      dispatch(FacetResultsCmd(facetKey, facetValue))
                  )
                  onClearFacet=(filter => dispatch(ClearFacetCmd(filter)))
                />
              </div>
              <div className={Style.container ++ " info p-5 mb-2"}>
                <p className=Style.searchResultsInfo>
                  {str("Results: " ++ string_of_int(resultCnt))}
                </p>
                <ul
                  className={Style.searchResults ++ " results mt-5 list-reset"}>
                  {
                    ReasonReact.array(
                      Array.map(
                        (r: Finna.record) =>
                          <Record
                            details=Record.List
                            record=r
                            onClick={_e => openUrl("/Record/" ++ r.id)}
                            onSelectFacet={
                              (facetKey, facetValue) =>
                                dispatch(
                                  FacetResultsCmd(facetKey, facetValue),
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
                    <Loading padding=1 /> : ReasonReact.null
                }
                <NextPage
                  loading={searchStatus == LoadingMoreStatus}
                  pageCnt
                  page
                  onNextPage=(_ => dispatch(NextPageCmd))
                />
              </div>
            </div>
          | NoResultsStatus =>
            <div className="mt-3"> {str("No results")} </div>
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
      <div className="p-5">
        {
          switch (searchStatus) {
          | LoadingStatus => <Loading padding=5 />
          | ResultsStatus =>
            <Record
              details=Record.Full
              record
              onClick=(_e => openUrl("/Record/" ++ record.id))
              onSelectFacet=(
                (facetKey, facetValue) =>
                  dispatch(FacetResultsCmd(facetKey, facetValue))
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
