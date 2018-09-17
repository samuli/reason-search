open Util;

let component = ReasonReact.statelessComponent("Loading");

let make = (~padding=0, _children) => {
  ...component,
  render: _self => <div className=Style.loader> {str("Loading...")} </div>,
};
