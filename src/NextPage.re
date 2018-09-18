open Util;

let component = ReasonReact.statelessComponent("NextPage");

let make = (~loading, ~pageCnt, ~page, ~onNextPage, _children) => {
  ...component,
  render: _self =>
    !loading && pageCnt > page ?
      <div onClick=onNextPage className=Style.nextPage>
        {str("More results")}
      </div> :
      ReasonReact.null,
};
