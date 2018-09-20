type searchActionType =
  | Search
  | NewSearch
  | MoreResults
  | BackToResults;

type action =
  | OnSearch(string)
  | SearchCmd(string, searchActionType)
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
  | ResultsStatus
  | ErrorStatus;
