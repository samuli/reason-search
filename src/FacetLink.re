open Util;

let component = ReasonReact.statelessComponent("FacetLink");

let make = (~label, ~facetKey, ~value, ~onSelect, ~isActive, _children) => {
  ...component,
  render: _self => {
    let className =
      "cursor-pointer mr-2 text-xs formats uppercase pl-1 pr-1 bg-grey-light"
      ++ (isActive ? " hover:bg-grey" : "");
    <span onClick={_ => isActive ? onSelect(facetKey, value) : ()} className>
      {str(label)}
    </span>;
  },
};
