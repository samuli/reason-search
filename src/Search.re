open Util;
open Types;

let component = ReasonReact.statelessComponent("Search");

let make = (~store: Store.store, _children) => {
  ...component,
  render: _self => {
    store.dispatch(SetRecord(Loading("foo")));
    ReasonReact.Router.push("/#/Record/bar");

    <Ui.Fragment>
      <SearchField lookfor={store.state.lookfor} />
      <Facets
        facets={store.state.facets}
        filters={store.state.filters}
        /* store.send(ReceiveFacetsCmd(facetKey, results, onLoaded)) */
        onSelectFacet={
          (_facetKey, _facetValue, _label) =>
            Js.log("facet")
            /* self.send(FacetResultsCmd(facetKey, facetValue, label)) */
        }
        onClearFacet={_filter => Js.log("clear facet")}
        /* self.send(ClearFacetCmd(filter))} */
      />
      <p> {str("res")} </p>
    </Ui.Fragment>;
  },
};
