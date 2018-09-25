open Types;

type actions =
  | SetRecord(remoteData(Finna.record));

type state = {
  lng: string,
  searchStatus: searchState,
  lookfor: string,
  result: option(Finna.searchResponse),
  records: array(Finna.record),
  recordResult: option(Finna.recordResponse),
  /* record: option(Finna.record), */
  record: remoteData(Finna.record),
  page: int,
  pageCnt: int,
  showImages: bool,
  limit: int,
  facets: Js.Dict.t(Finna.facet),
  filters: array(Finna.filter),
};
type store = {
  state,
  dispatch: actions => unit,
};
let component = ReasonReact.reducerComponent("Store");
let make = (~render, _children) => {
  ...component,
  initialState: () => {
    lng: "fi",
    searchStatus: NotLoadedStatus,
    lookfor: "",
    page: 1,
    limit: 30,
    pageCnt: 0,
    showImages: false,
    records: [||],
    record: NotAsked,
    result: None,
    recordResult: None,
    facets: Finna.getInitialFacets(),
    filters: [||],
    /* route: Search, */
  },
  reducer: (action, state) =>
    switch (action) {
    | SetRecord(record) => ReasonReact.Update({...state, record})
    },
  render: self =>
    render({state: self.state, dispatch: action => self.send(action)}),
};
