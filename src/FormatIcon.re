let component = ReasonReact.statelessComponent("FormatIcon");

let make = (~format, _children) => {
  ...component,
  render: _self =>
    switch (format) {
    | Some(format) =>
      Js.Re.fromString("^[0-9]\/([a-zA-Z]*)\/$")
      |> Js.Re.exec(format)
      |> (
        fun
        | Some(result) => {
            let format = Js.Nullable.toOption(Js.Re.captures(result)[1]);
            switch (format) {
            | Some(format) =>
              let icon =
                switch (Js.String.toLowerCase(format)) {
                | "book" => "book"
                | "sound" => "volume-up"
                | "image" => "image"
                | "workofart" => "palette"
                | "place" => "map-marker-alt"
                | _ => "other"
                };
              <i className={Style.recordIcon ++ " fa fa-" ++ icon} />;
            | _ => ReasonReact.null
            };
          }

        | _ => ReasonReact.null
      )
    | _ => ReasonReact.null
    },
};
