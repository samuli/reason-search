type state = {text: string};

type action =
  | Change(string)
  | Search;

let component = ReasonReact.reducerComponent("SearchField");

let make = (~lookfor, ~openUrl, ~onSearch, _children) => {
  ...component,
  initialState: () => {text: lookfor},
  reducer: (action: action, state: state) =>
    switch (action) {
    | Change(text) => ReasonReact.Update({text: text})
    | Search =>
      onSearch(state.text);
      ReasonReact.NoUpdate;
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
              openUrl("/Search/lookfor=" ++ self.state.text);
            }
        }
        autoFocus=true
      />
    </div>,
  willReceiveProps: _self => {text: lookfor},
  didMount: _self => {
    let _x = [%bs.raw {| document.getElementById("search").focus() |}];
    ();
  },
};
