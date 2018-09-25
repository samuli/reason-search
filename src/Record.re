open Util;

type details =
  | List
  | Full;

type action =
  | LoadRecord(string);

type state = unit;

let component = ReasonReact.reducerComponent("Record");

let make =
    (
      ~store: Store.store,
      /* ~details, */
      /* ~record: Finna.record, */
      /* ~onClick, */
      /* ~onSelectFacet, */
      /* ~filters, */
      /* ~showImages, */
      /* ~isVisited=false, */
      _children,
    ) => {
  ...component,
  initialState: () => (),
  reducer: (action: action, state: state) =>
    switch (action) {
    | LoadRecord(id) =>
      ReasonReact.UpdateWithSideEffects(
        state,
        (
          _self =>
            Finna.record(
              ~id,
              /* ~onResults=result => self.send(RecordResultCmd(result)), */
              ~onResults=_result => Js.log("loaded"),
              ~lng=store.state.lng,
              (),
            )
            |> ignore
        ),
      )
    },
  render: _self =>
    switch (store.state.record) {
    | Failure(_err) => <p> {str("err")} </p>
    | Loading(id) => <p> {str("Loading: " ++ id)} </p>
    | Success(record) =>
      let imgs =
        switch (store.state.showImages) {
        | false => [||]
        | true =>
          Array.map(img => Finna.apiUrl ++ img ++ "&w=500", record.images)
        };

      let authors =
        switch (record.authors) {
        | [||] => ReasonReact.null
        | authors =>
          <span className=Style.recordAuthors>
            {str(Js.Array.joinWith(", ", authors) ++ ":")}
          </span>
        };

      let publishers =
        switch (record.publishers) {
        | Some(publishers) when Array.length(publishers) > 0 =>
          <span className=Style.recrodPublisher>
            {str(Js.Array.joinWith(", ", publishers))}
          </span>
        | _ => ReasonReact.null
        };

      let year =
        switch (record.year) {
        | Some(year) => <span className=Style.recordYear> {str(year)} </span>
        | None => ReasonReact.null
        };

      let isFacetActive = key =>
        switch (
          List.find(
            (f: Finna.filter) => key == f.key,
            Array.to_list(store.state.filters),
          )
        ) {
        | _f => true
        | exception Not_found => false
        };

      let facetLink = (key, facets: array(Finna.translated)) =>
        switch (Array.to_list(facets)) {
        | [facet, ..._rest] =>
          <FacetLink
            key={facet.value}
            label={facet.label}
            facetKey=key
            value={facet.value}
            isActive={!isFacetActive(key)}
            onSelect=onSelectFacet
          />
        | _ => ReasonReact.null
        };

      switch (details) {
      | List =>
        let format = Finna.getFormat(record);
        <li
          key={record.id}
          className={Style.recordList(~visited=isVisited, ~format)}>
          <div className=Style.recordListBkg onClick>
            <p> authors </p>
            <a target="_finna"> <h2> {str(record.title)} </h2> </a>
            <p>
              {
                switch (format) {
                | Some(format) =>
                  <span className=Style.recordFormat>
                    {str(format.label)}
                  </span>
                | None => ReasonReact.null
                }
              }
              <span className=Style.recordPublished> publishers year </span>
              {
                switch (record.buildings) {
                | Some(buildings) => facetLink("building", buildings)
                | None => ReasonReact.null
                }
              }
            </p>
          </div>
          <div />
        </li>;
      | Full =>
        <div className=Style.recordFull>
          <h1> {str(record.title)} </h1>
          <div>
            authors
            {
              switch (record.buildings) {
              | Some(buildings) => facetLink("building", buildings)
              | None => ReasonReact.null
              }
            }
          </div>
          <div className=Style.recordImages>
            {
              ReasonReact.array(
                Array.map(
                  img =>
                    <div>
                      <img
                        key=img
                        className={Style.recordImage ++ " record-image"}
                        href=img
                      />
                    </div>,
                  imgs,
                ),
              )
            }
          </div>
          <ul className=Style.recordLinks>
            {
              let formatUrls = urls =>
                switch (urls) {
                | Some(links) =>
                  ReasonReact.array(
                    Array.map(
                      (link: Finna.onlineUrl) => {
                        let (label, url) =
                          switch (link.url, link.label) {
                          | (Some(url), Some(text)) => (text, url)
                          | (Some(url), None) => (url, url)
                          | (None, Some(text)) => (text, text)
                          | _ => ("", "")
                          };
                        <li className=Style.recordLink>
                          <a key=url href=url> {str(label)} </a>
                        </li>;
                      },
                      links,
                    ),
                  )
                | None => ReasonReact.null
                };

              ReasonReact.array([|
                formatUrls(record.onlineUrls),
                formatUrls(record.urls),
              |]);
            }
          </ul>
          <p>
            <a href={Finna.recordBaseUrl ++ record.id}> {str("Finna")} </a>
          </p>
        </div>
      | _ => ReasonReact.null
      };
    },
};
