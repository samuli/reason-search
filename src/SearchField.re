type state = {text: string};

type action =
  | Change(string)
  | Search;

let component = ReasonReact.reducerComponent("SearchField");

let make = (~onSearch, _children) => {
  ...component,
  initialState: () => {text: "mauri kunnas"},
  reducer: (action: action, state: state) =>
    switch (action) {
    | Change(text) => ReasonReact.Update({text: text})
    | Search =>
      onSearch(state.text);
      ReasonReact.NoUpdate;
    },
  render: self =>
    <div>
      <input
        id="search"
        className="search edit border border-gray border-solid p-2 w-full"
        value={self.state.text}
        onChange={
          event => self.send(Change(ReactEvent.Form.target(event)##value))
        }
        onKeyDown={
          event =>
            if (ReactEvent.Keyboard.keyCode(event) === 13) {
              ReactEvent.Keyboard.preventDefault(event);
              self.send(Search);
            }
        }
        autoFocus=true
      />
    </div>,
  didMount: _self => {
    let _x = [%bs.raw {| document.getElementById("search").focus() |}];
    ();
  },
};
