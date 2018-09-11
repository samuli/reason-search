[@bs.deriving abstract]
type selectOption = {
  value: string,
  label: string,
};

[@bs.deriving abstract]
type jsProps = {
  options: array(selectOption),
  onFocus: (string, string) => unit,
  onChange: (selectOption, string) => unit,
  loadingMessage: string => string,
  isLoading: bool,
};

[@bs.module "react-select"]
external reactClass: ReasonReact.reactClass = "default";
let make = (~options, ~onFocus, ~onChange, children) =>
  ReasonReact.wrapJsForReason(
    ~reactClass,
    ~props=
      jsProps(
        ~options,
        ~onFocus,
        ~onChange,
        ~loadingMessage=_string => "Hetki...",
        ~isLoading=true,
      ),
    children,
  );
