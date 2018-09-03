open Util;

let component = ReasonReact.statelessComponent("Record");

let make = (~record: Finna.record, ~showImages, _children) => {
  ...component,
  render: _self => {
    let formats =
      switch (Array.to_list(record.formats)) {
      | [] => "&nbsp;"
      | [translated, ..._rest] => translated.label
      };

    let buildings =
      switch (Array.to_list(record.buildings)) {
      | [] => "&nbsp;"
      | [translated, ..._rest] => translated.label
      };
    let imgs =
      switch (showImages) {
      | false => [||]
      | true => Array.map(img => Finna.apiUrl ++ img, record.images)
      };

    let authors =
      switch (record.authors) {
      | [||] => ReasonReact.null
      | authors =>
        <span className="authors mr-2">
          {str(Js.Array.joinWith(", ", authors))}
        </span>
      };

    let year =
      switch (record.year) {
      | Some(year) => <span className="year ml-2"> {str(year)} </span>
      | None => ReasonReact.null
      };

    <li
      key={record.id}
      className="record pb-1 mb-1 border-b border-solid border-grey">
      <a
        className="link font-hairline no-underline"
        target="_finna"
        href={"https://finna.fi/Record/" ++ record.id}>
        {str(record.title)}
      </a>
      year
      /* <FormatIcon record /> */
      <p>
        authors
        <span
          className="mr-2 text-xs formats uppercase pl-1 pr-1 bg-grey-light">
          {str(formats)}
        </span>
        <span className="text-grey-darker buildings"> {str(buildings)} </span>
        {
          switch (Array.to_list(imgs)) {
          | [] => <span />
          | [img, ..._rest] => <img className="w-1/4" src=img />
          }
        }
      </p>
    </li>;
  },
};
