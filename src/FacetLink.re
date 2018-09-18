open Util;

let component = ReasonReact.statelessComponent("FacetLink");

let make = (~label, ~facetKey, ~value, ~onSelect, ~isActive, _children) => {
  ...component,
  render: _self => {
    let className =
      Style.facetLink
      ++ " cursor-pointer mr-2 text-xs formats uppercase pl-1 pr-1 bg-grey-light"
      ++ (isActive ? " hover:bg-grey" : "");
    <div
      onClick={
        ev =>
          isActive ?
            {
              ReactEventRe.Mouse.stopPropagation(ev);
              onSelect(facetKey, value, label);
            } :
            ()
      }
      className>
      {str(label)}
    </div>;
  },
};
