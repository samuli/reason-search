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
  onMenuClose: unit => unit,
  loadingMessage: string => string,
  isLoading: bool,
  placeholder: string,
  [@bs.as "value"]
  selected: selectOption,
};

[@bs.module "react-select"]
external reactClass: ReasonReact.reactClass = "default";
let make =
    (
      ~options,
      ~selected,
      ~onFocus,
      ~onChange,
      ~onMenuClose,
      ~isLoading,
      ~loadingMessage,
      ~placeholder,
      children,
    ) =>
  ReasonReact.wrapJsForReason(
    ~reactClass,
    ~props=
      jsProps(
        ~options,
        ~selected,
        ~onFocus,
        ~onChange,
        ~onMenuClose,
        ~loadingMessage,
        ~isLoading,
        ~placeholder,
      ),
    children,
  );
