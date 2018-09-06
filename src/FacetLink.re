open Util;

let component = ReasonReact.statelessComponent("FacetLink");

let make = (~label, ~facetKey, ~value, ~onSelect, _children) => {
  ...component,
  render: _self =>
    /* let facet: Finna.translated = List.hd(Array.to_list(facets)); */
    /* let facet: Finna.facet = { */
    /*   count: 0, */
    /*   value: facet.value, */
    /*   label: facet.label, */
    /*   facetType: Normal, */
    /* }; */
    /* let filter: Finna.filter = {key: facetKey, facet}; */
    <span
      onClick={_ => onSelect(facetKey, value)}
      className="cursor-pointer mr-2 text-xs formats uppercase pl-1 pr-1 bg-grey-light hover:bg-grey">
      {str(label)}
    </span>,
};
