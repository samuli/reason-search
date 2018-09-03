open Util;

let component = ReasonReact.statelessComponent("NextPage");

let make = (~loading, ~pageCnt, ~page, ~onNextPage, _children) => {
  ...component,
  render: _self => {
    let nextPage =
      loading ?
        <div> {str("Loading...")} </div> :
        pageCnt > page ?
          <div
            onClick=onNextPage
            className="paginate-next bg-grey-light p-2 rounded cursor-pointer">
            {str("next")}
          </div> :
          ReasonReact.null;
    nextPage;
  },
};
