open Util;

let component = ReasonReact.statelessComponent("NextPage");

let make = (~loading, ~pageCnt, ~page, ~onNextPage, _children) => {
  ...component,
  render: _self =>
    <div className=Style.nextPage>
      {
        !loading && pageCnt > page ?
          <div
            onClick=onNextPage
            className="paginate-next bg-grey-light p-2 rounded cursor-pointer hover:bg-grey">
            {str("next")}
          </div> :
          ReasonReact.null
      }
    </div>,
};
