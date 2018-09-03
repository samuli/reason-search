let component = ReasonReact.statelessComponent("FormatIcon");

let make = (~record: Finna.record, _children) => {
  ...component,
  render: _self => {
    let formatIconClass =
      Js.Re.fromString("^[0-9]\/([a-zA-Z]*)\/$")
      |> Js.Re.exec(record.formats[0].value)
      |> (
        fun
        | Some(result) => Js.Nullable.toOption(Js.Re.captures(result)[1])
        | None => None
      );

    let formatIcon =
      switch (formatIconClass) {
      | Some(format) => <i className={"fa icon " ++ format} />
      | None => ReasonReact.null
      };

    formatIcon;
  },
};
