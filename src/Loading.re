open Util;

let component = ReasonReact.statelessComponent("Loading");

let make = (~padding=0, _children) => {
  ...component,
  render: _self =>
    <div className={"loading mt-3 p-" ++ string_of_int(padding)}>
      {str("Loading...")}
    </div>,
};
