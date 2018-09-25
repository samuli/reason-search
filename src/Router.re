open Types;
open Util;

type route =
  | Main
  | Search(string, searchActionType)
  | Record(string)
  | NotFound;

type action =
  | SwitchRoute(route);

type routerState = {activeRoute: route};

let component = ReasonReact.reducerComponent("Router");
let make = _children => {
  ...component,
  initialState: () => {activeRoute: Main},
  reducer: (action, _state) =>
    switch (action) {
    | SwitchRoute(r) => ReasonReact.Update({activeRoute: r})
    },
  didMount: self => {
    let urlChange = (url: ReasonReact.Router.url) => {
      let hash = Js.String.substr(~from=1, url.hash);
      Js.log(hash);
      switch (hash) {
      | "/"
      /* | "" => send(CloseRecordCmd) */
      | _ =>
        Js.log(hash);
        if (Js.String.indexOf("Record/", hash) == 0) {
          switch (Js.String.split("/", hash)) {
          | [|"Record", id|] => self.send(SwitchRoute(Record(id)))
          | _ => self.send(SwitchRoute(NotFound))
          };
        };
        if (Js.String.indexOf("Search?", hash) == 0) {
          let hash = Js.String.substr(~from=7, hash);
          switch (Js.String.split("=", hash)) {
          | [|"lookfor", ""|] => ()
          | [|"lookfor", lookfor|] =>
            let lookfor = Js_global.decodeURIComponent(lookfor);
            self.send(SwitchRoute(Search(lookfor, Search)));
          | _ => self.send(SwitchRoute(NotFound))
          };
        };
      };
    };
    let watcherID = ReasonReact.Router.watchUrl(url => urlChange(url));
    self.onUnmount(() => ReasonReact.Router.unwatchUrl(watcherID));

    let url = ReasonReact.Router.dangerouslyGetInitialUrl();
    urlChange(url);
  },
  render: self =>
    <Store
      render={
        store =>
          <div>
            {
              switch (self.state.activeRoute) {
              | Main => <p> {str("main")} </p>
              | Search(_lookfor, _searchType) => <Search store />

              | Record(_id) => <Record store />
              /* switch (store.state.record) { */
              /* | Loading(id) => <p> {str("loading: " ++ id)} </p> */
              /* | _ => <p> {str("rec route: " ++ id)} </p> */
              /* } */
              | NotFound => <p> {str("404")} </p>
              }
            }
          </div>
      }
    />,
};
