type state = {text: string};

type action =
  | Change(string);

let component = ReasonReact.reducerComponent("SearchField");

let make = (~lookfor, _children) => {
  ...component,
  initialState: () => {text: lookfor},
  reducer: (action: action, _state: state) =>
    switch (action) {
    | Change(text) => ReasonReact.Update({text: text})
    },
  render: self =>
    <div className=Style.searchBox>
      <input
        type_="search"
        id="search"
        placeholder="Search..."
        value={self.state.text}
        onChange={
          ev => self.send(Change(ReactEvent.Form.target(ev)##value))
        }
        onKeyDown={
          ev =>
            if (ReactEvent.Keyboard.keyCode(ev) === 13) {
              ReactEvent.Keyboard.preventDefault(ev);
              let _ = [%bs.raw {| document.getElementById("search").blur() |}];
              ReasonReact.Router.push(
                "/#/Search?lookfor=" ++ self.state.text,
              );
            }
        }
      />
    </div>,
  willReceiveProps: _self => {text: lookfor},
  didMount: _ =>
    /* focus search field on keypress */
    Webapi.Dom.Element.addKeyDownEventListener(
      e => {
        let code = Webapi.Dom.KeyboardEvent.code(e);
        Js.Re.fromString("Key.*")
        |> Js.Re.exec(code)
        |> (
          fun
          | Some(_result) => {
              let _x = [%bs.raw
                {| document.getElementById("search").focus() |}
              ];
              ();
            }
          | None => ()
        );
      },
      Webapi.Dom.Document.documentElement(Webapi.Dom.document),
    ),
};
