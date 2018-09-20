[@bs.deriving abstract]
type selectOption = {
  value: string,
  label: string,
};

[@bs.deriving abstract]
type jsProps = {
  options: array(selectOption),
  onMenuOpen: (string, string) => unit,
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
      ~onMenuOpen,
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
        ~onMenuOpen,
        ~onChange,
        ~onMenuClose,
        ~loadingMessage,
        ~isLoading,
        ~placeholder,
      ),
    children,
  );
