open Util;

let component = ReasonReact.statelessComponent("FacetLink");

let make = (~label, ~facetKey, ~value, ~onSelect, ~isActive, _children) => {
  ...component,
  render: _self =>
    <span
      onClick={
        ev =>
          isActive ?
            {
              ReactEventRe.Mouse.stopPropagation(ev);
              onSelect(facetKey, value, label);
            } :
            ()
      }
      className={Style.facetLink(isActive)}>
      {str(label)}
    </span>,
};
