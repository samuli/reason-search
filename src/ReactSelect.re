/* ReactJS used by ReasonReact */
/* This component wraps a ReactJS one, so that ReasonReact components can consume it */
/* Typing the myBanner.js component's output as a `reactClass`. */
/* [@bs.module] external reactSelect: ReasonReact.reactClass = "./ReactSelect"; */

[@bs.deriving abstract]
type selectOption = {
  value: string,
  label: string,
};

[@bs.deriving abstract]
type jsProps = {options: array(selectOption)};
/* [@bs.obj] external makeProps: (~options: array(selectOption), unit) => _ = ""; */

/* This is like declaring a normal ReasonReact component's `make` function, except the body is a the interop hook wrapJsForReason */
/* let make = (~options, children) => */
/*   ReasonReact.wrapJsForReason( */
/*     ~reactClass=reactSelect, */
/*     ~props=makeProps(~options), */
/*     children, */
/*   ); */

/* module ReactSelect = { */
/*   [@bs.module "react-select"] */
/*   external reactSelect: ReasonReact.reactClass = "Select"; */
/*   let make = children => */
/*     ReasonReact.wrapJsForReason( */
/*       ~reactClass=reactSelect, */
/*       ~props=Js.Obj.empty(), */
/*       children, */
/*     ); */
/* }; */

[@bs.module "react-select"]
external reactClass: ReasonReact.reactClass = "default";
/* [@bs.module "react-select/Select"] */
/* external reactClass: ReasonReact.reactClass = "default"; */
let make = (~options, children) => {
  Js.log(jsProps(~options));
  ReasonReact.wrapJsForReason(
    ~reactClass,
    /*     ~props=[%bs.raw */
    /*       {| { options: [ */
             /*   { value: 'chocolate', label: 'Chocolate' }, */
             /*   { value: 'strawberry', label: 'Strawberry' }, */
             /*   { value: 'vanilla', label: 'Vanilla' } */
             /* ]}|} */
    /*     ], */
    ~props=jsProps(~options),
    children,
  );
};
