open Css;

let basePadding = em(1.0);
let textColor = darkgrey;

let greyLight = hex("dae1e7");
let greyLighter = hex("f1f5f8");

let init = () => {
  global("body", [margin(px(0))]);
  global("html", [fontFamily("sans-serif"), color(black)]);
  global("h1, h2, h3", [color(rgb(0, 0, 0))]);
  global(
    "h1, h2, h3, p",
    [margin2(~h=px(0), ~v=em(0.2)), lineHeight(em(1.2))],
  );

  global("h1", [fontWeight(400), fontSize(em(1.3))]);
  global("h2", [fontWeight(400), fontSize(em(1.0))]);
  global("p", [fontWeight(300), fontSize(em(0.9))]);
  global(
    "ul, li",
    [listStyle(none, inside, none), padding(px(0)), margin(px(0))],
  );
  global("img", [maxWidth(pct(100.0)), height(auto)]);
  global(
    "input",
    [
      boxSizing(borderBox),
      maxWidth(pct(100.0)),
      minWidth(pct(100.0)),
      width(pct(100.0)),
      padding(rem(0.7)),
      fontSize(pct(100.0)),
      borderRadius(em(0.2)),
      borderWidth(px(1)),
      borderColor(hex("d8d8d8")),
    ],
  );
};

let buttonStyles = (~r, ~m, ~p) => [
  padding(p),
  margin(m),
  backgroundColor(greyLight),
  borderRadius(rem(r)),
  cursor(`pointer),
  hover([backgroundColor(greyLighter)]),
];

let loader = style([padding(basePadding)]);
let nextPage = style(buttonStyles(~r=0.5, ~p=basePadding, ~m=basePadding));

let searchBox = style([backgroundColor(greyLight), padding(basePadding)]);
let facets =
  style([
    backgroundColor(greyLighter),
    padding(basePadding),
    borderBottom(px(1), solid, greyLight),
  ]);
let facetMenu = style([padding2(~h=px(0), ~v=em(0.2))]);
let facetLink = style(buttonStyles(~r=0.2, ~p=em(0.2), ~m=em(0.2)));

let searchResults = style([]);
let searchResultsInfo = style([padding(basePadding), fontWeight(800)]);

let container = style([]);
let pad = style([padding(basePadding)]);
let recordList = (~visited) =>
  style([
    borderBottom(px(1), solid, greyLight),
    padding2(~v=em(0.5), ~h=em(1.0)),
    backgroundColor(visited ? hex("eff8ff") : white),
    hover([backgroundColor(greyLighter)]),
    cursor(`pointer),
  ]);

let recordFull = style([padding(basePadding)]);
