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
        ~activeFilters,
        ~searchStatus,
        ~resultCnt,
        ~records,
        ~pageCnt,
        ~page,
        _children,
      ) => {
    ...component,
    render: _self =>
      <div className="">
        <input
          checked=showImages
          type_="checkbox"
          onChange={_ => dispatch(ToggleImagesCmd)}
        />
        <Facets
          facets
          onGetFacets={
            (facetKey, onLoaded) =>
              dispatch(GetFacetsCmd(facetKey, onLoaded))
          }
          onSelectFacet={
            (facetKey, facetValue) =>
              dispatch(FacetResultsCmd(facetKey, facetValue))
          }
          onClearFacet={filter => dispatch(ClearFacetCmd(filter))}
        />
        {
          switch (searchStatus) {
          | ResultsStatus =>
            <div className="info mt-2 mb-2">
              {str("Results: " ++ string_of_int(resultCnt))}
              <ul className="results mt-5 list-reset">
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
                              dispatch(FacetResultsCmd(facetKey, facetValue))
                          }
                          filters=activeFilters
                          showImages
                        />,
                      records,
                    ),
                  )
                }
              </ul>
              <NextPage
                loading={searchStatus == LoadingStatus}
                pageCnt
                page
                onNextPage=(_ => dispatch(NextPageCmd))
              />
            </div>
          | NoResultsStatus => str("No results")
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
        <a onClick={_e => openUrl("/")}> {str("close")} </a>
        {
          switch (searchStatus) {
          | LoadingStatus => <p> {str("Loading...")} </p>
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
