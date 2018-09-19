type searchActionType =
  | New
  | MoreResults
  | FacetResults
  | BackToResults;

type action =
  | SearchCmd(string, bool, bool)
  | ResultsCmd(Finna.searchResponse)
  | ToggleImagesCmd
  | NextPageCmd
  | GetFacetsCmd(string, Facet.action => unit)
  | ReceiveFacetsCmd(string, Finna.searchResponse, Facet.action => unit)
  | FacetResultsCmd(string, string, string)
  | ClearFacetCmd(string)
  | RecordCmd(string)
  | CloseRecordCmd
  | RecordResultCmd(Finna.recordResponse);

type searchState =
  | NotLoadedStatus
  | LoadingStatus
  | LoadingMoreStatus
  | NoResultsStatus
  | ResultsStatus;
