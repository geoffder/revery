module ContainerComponent = Container;
open Revery_UI;
open Revery_Core;
open Revery_UI_Primitives;
open Revery_Font;

module Hooks = Revery_UI_Hooks;

let floatClampExn = (~min, ~max, a) => {
  if (min > max) {
    failwith("floatClampExn requires [min <= max]");
  };
  if (Float.compare(min, a) > 0) {
    min;
  } else if (Float.compare(max, a) < 0) {
    max;
  } else {
    a;
  };
};

module CharSet = Set.Make(Char);

let endChars =
  CharSet.of_list(['\n', ' ', '/', '_', '-', ',', '.', ';', '"']);

let charsToNextWordEnd = str => {
  let len = String.length(str);
  if (len > 0) {
    let rec loop = i =>
      if (i == len || CharSet.mem(str.[i], endChars)) {
        i;
      } else {
        loop(i + 1);
      };
    loop(1);
  } else {
    0;
  };
};

let charsToPreviousWordEnd = str => {
  let len = String.length(str);
  if (len > 0) {
    let sub_len = len - 1;
    let rec loop = i =>
      if (i == len || CharSet.mem(str.[sub_len - i], endChars)) {
        i;
      } else {
        loop(i + 1);
      };
    loop(1);
  } else {
    0;
  };
};

let removeWordBefore = (text, cursor_position) => {
  open Input;
  let (before, after) = getStringParts(cursor_position, text);
  if (String.length(before) > 0) {
    let next_position = cursor_position - charsToPreviousWordEnd(before);
    let new_text = Str.string_before(before, next_position) ++ after;
    (new_text, next_position);
  } else {
    (after, cursor_position);
  };
};

let removeWordAfter = (text, cursor_position) => {
  open Input;
  let (before, after) = getStringParts(cursor_position, text);
  let new_text =
    if (String.length(after) > 0) {
      Str.string_after(after, charsToNextWordEnd(after));
    } else {
      before;
    };
  (new_text, cursor_position);
};

let removeBetween = (text, p1, p2) => {
  let (first, last) =
    if (p1 > p2) {
      (p2, p1);
    } else {
      (p1, p2);
    };
  (Str.string_before(text, first) ++ Str.string_after(text, last), first);
};

let selectParts = (text, p1, p2) => {
  let (first, last) =
    if (p1 > p2) {
      (p2, p1);
    } else {
      (p1, p2);
    };
  let before = Str.string_before(text, first);
  /* let selected = String.slice(text, first, last); */
  let selected = String.sub(text, first, last - first);
  let after = Str.string_after(text, last);
  (before, selected, after);
};

let copySelected = (text, p1, p2) => {
  let (first, last) =
    if (p1 > p2) {
      (p2, p1);
    } else {
      (p1, p2);
    };
  /* Sdl2.Clipboard.setText(String.slice(text, first, last)); */
  Sdl2.Clipboard.setText(String.sub(text, first, last - first));
};

module Cursor = {
  type state = {
    time: Time.t,
    isOn: bool,
  };

  type action =
    | Reset
    | Tick(Time.t);

  let use = (~interval, ~isFocused) => {
    let%hook (state, dispatch) =
      Hooks.reducer(
        ~initialState={time: Time.zero, isOn: false}, (action, state) => {
        switch (action) {
        | Reset => {isOn: true, time: Time.zero}
        | Tick(increasedTime) =>
          let newTime = Time.(state.time + increasedTime);

          /* if newTime is above the interval a `Tick` has passed */
          newTime >= interval
            ? {isOn: !state.isOn, time: Time.zero}
            : {...state, time: newTime};
        }
      });

    let%hook () =
      Hooks.effect(
        OnMount,
        () => {
          let clear =
            Tick.interval(
              ~name="Revery:Input:Cursor Blink Interval",
              time => dispatch(Tick(time)),
              Time.ms(16),
            );
          Some(clear);
        },
      );

    let cursorOpacity = isFocused && state.isOn ? 1.0 : 0.0;

    (cursorOpacity, () => dispatch(Reset));
  };
};

type state = {
  value: string,
  cursorPosition: int,
};

type action =
  | TextInput(string, int);

let reducer = (action, _state) =>
  switch (action) {
  | TextInput(value, cursorPosition) => {value, cursorPosition}
  };

module Constants = {
  let defaultHeight = 50;
  let defaultWidth = 200;
  let textMargin = 10;
  let cursorWidth = 2;
};

module Styles = {
  open Style;

  let defaultPlaceholderColor = Colors.grey;
  let defaultCursorColor = Colors.black;

  let default = [
    color(Colors.black),
    width(Constants.defaultWidth),
    height(Constants.defaultHeight),
    border(
      // The default border width should be 5% of the full input height
      ~width=float_of_int(Constants.defaultHeight) *. 0.05 |> int_of_float,
      ~color=Colors.black,
    ),
    backgroundColor(Colors.transparentWhite),
  ];

  let _all = (~style) =>
    merge(
      ~source=[
        flexDirection(`Row),
        alignItems(`Center),
        justifyContent(`FlexStart),
        overflow(`Hidden),
        cursor(MouseCursors.text),
        ...default,
      ],
      ~target=style,
    );

  let box = (~style) => extractViewStyles(_all(~style));

  let marginContainer = [
    flexDirection(`Row),
    alignItems(`Center),
    justifyContent(`FlexStart),
    marginLeft(Constants.textMargin),
    marginRight(Constants.textMargin),
    flexGrow(1),
  ];

  let cursor = (~offset) => [
    position(`Absolute),
    marginTop(2),
    transform(Transform.[TranslateX(float(offset))]),
  ];

  let textContainer = [flexGrow(1), overflow(`Hidden)];

  let text =
      (~showPlaceholder, ~scrollOffset, ~placeholderColor, ~color: Color.t) => [
    Style.color(showPlaceholder ? placeholderColor : color),
    alignItems(`Center),
    justifyContent(`FlexStart),
    textWrap(TextWrapping.NoWrap),
    transform(Transform.[TranslateX(float(- scrollOffset^))]),
  ];
};

let%component make =
              (
                ~style=Styles.default,
                ~fontFamily=Family.default,
                ~fontWeight=Weight.Normal,
                ~italic=false,
                ~smoothing=Revery_Font.Smoothing.default,
                ~fontSize=14.0,
                ~underlined=false,
                ~placeholderColor=Styles.defaultPlaceholderColor,
                ~cursorColor=Styles.defaultCursorColor,
                ~autofocus=false,
                ~placeholder="",
                ~onFocus=() => (),
                ~onBlur=() => (),
                ~onKeyDown=_ => (),
                ~onChange=(_, _) => (),
                ~value=?,
                ~cursorPosition=?,
                ~isPassword=false,
                ~maxHeight=Int.max_int,
                ~forceWrap=true,
                (),
              ) => {
  let measureTextWidth = text => {
    let dimensions =
      Revery_Draw.Text.dimensions(
        ~smoothing,
        ~italic,
        ~fontWeight,
        ~fontFamily,
        ~fontSize,
        text,
      );

    dimensions.width;
  };

  // NOTE: in the bonsai code, line height is multiplied by lineHeight from the node style,
  // retrieved by `let style : UI.Style.t = node#getStyle ()`. May have to do something
  // similar? Can I really supply this as a configuration value then?
  let lineHeight = {
    Revery_Draw.Text.lineHeight(~italic, fontFamily, fontSize, fontWeight);
  };

  module Offsets =
    OffsetMap.Make({
      let measure = measureTextWidth;
      let lineHeight = lineHeight;
      let forceWrap = forceWrap;
    });

  let verticalScroll =
      (containerHeight, textHeight, lineHeight, yOffset, yScroll) =>
    if (Float.compare(textHeight, containerHeight) > 0) {
      let offset =
        if (Float.compare(yOffset, yScroll) < 0) {
          yOffset;
        } else if (Float.compare(
                     yOffset +. lineHeight -. yScroll,
                     containerHeight,
                   )
                   > 0) {
          yOffset +. lineHeight -. containerHeight;
        } else {
          yScroll;
        };
      /* Make sure all the space is used to show text (no overscroll). */
      floatClampExn(~min=0., ~max=textHeight -. containerHeight, offset);
    } else {
      0.;
    };

  let horizontalScroll = (margin, textWidth, xOffset, xScroll) =>
    if (Float.compare(textWidth, margin) > 0) {
      let offset =
        if (Float.compare(xOffset, xScroll) < 0) {
          xOffset;
          /* } else if (xOffset - xScroll > margin) { */
        } else if (Float.compare(xOffset -. xScroll, margin) > 0) {
          xOffset -. margin;
        } else {
          xScroll;
        };
      /* Make sure all the space is used to show text (no overscroll). */
      floatClampExn(~min=0., ~max=textWidth -. margin, offset);
    } else {
      0.;
    };

  let verticalNav = (~up, m: Offsets.t, startPosition) => {
    let (row, target_x, _) = Offsets.findPosition(m, startPosition);
    let target_row =
      if (up) {
        row - 1;
      } else {
        row + 1;
      };

    switch (Offsets.IntMap.find_opt(target_row, m)) {
    | Some(row) => Offsets.Row.nearestPosition(row, target_x)
    | None => startPosition
    };
    /* Option.value_map(Map.find(m, target_row), ~default=startPosition, ~f=row => */
    /*   Offsets.Row.nearestPosition(row, target_x) */
    /* ); */
  };

  /* Workaround for Revery text wrapping not working exactly as I'd like.
   * - spaces inserted following newlines on empty rows to prevent collapse
   * - zero width unicodes inserted before leading spaces
   * - spaces which triggered a wrap are replaced by newlines
   * - row starts preceded by non-whitespace (forced break) get a newline */
  let newlineHack = (offsets, text) => {
    let len = String.length(text);
    let f = (acc, (_, {start, xOffsets, _}: Offsets.Row.t)) => {
      let before = Str.string_before(acc, start);
      let after = Str.string_after(acc, start);
      let spacer =
        if (List.length(xOffsets) == 0) {
          " ";
        } else if (start < len && OffsetMap.isSpace(text.[start])) {
          OffsetMap.zeroSpace;
        } else {
          "";
        };
      if (start > 1) {
        switch (text.[start - 1]) {
        | ' ' =>
          String.sub(before, 0, String.length(before) - 1)
          ++ "\n"
          ++ spacer
          ++ after
        | '\n' => before ++ spacer ++ after
        | _ => before ++ "\n" ++ spacer ++ after
        };
      } else {
        before ++ spacer ++ after;
      };
    };
    List.fold_left(f, text, List.rev(Offsets.IntMap.bindings(offsets)));
  };

  let%hook (state, dispatch) =
    Hooks.reducer(
      ~initialState={
        value: Option.value(value, ~default=""),
        cursorPosition: Option.value(cursorPosition, ~default=0),
      },
      reducer,
    );
  <View style={Styles.box(~style)} />;
};
