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

let removeWordBefore = (text, cursorPosition) => {
  open Input;
  let (before, after) = getStringParts(cursorPosition, text);
  if (String.length(before) > 0) {
    let next_position = cursorPosition - charsToPreviousWordEnd(before);
    let new_text = Str.string_before(before, next_position) ++ after;
    (new_text, next_position);
  } else {
    (after, cursorPosition);
  };
};

let removeWordAfter = (text, cursorPosition) => {
  open Input;
  let (before, after) = getStringParts(cursorPosition, text);
  let new_text =
    if (String.length(after) > 0) {
      Str.string_after(after, charsToNextWordEnd(after));
    } else {
      before;
    };
  (new_text, cursorPosition);
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
  selectStart: option(int),
  offsets: OffsetMap.t,
};

type action =
  | TextInput(string, int)
  | Move(int)
  | MoveAndSelect(int, int)
  | MoveAndUnselect(int)
  | Select(int)
  | Unselect;

let reducer = (action, state) =>
  switch (action) {
  | TextInput(value, cursorPosition) => {
      value,
      cursorPosition,
      selectStart: None,
      offsets: state.offsets // TODO: must refresh this. Give as param to update?
    }
  | Move(cursorPosition) => {...state, cursorPosition}
  | MoveAndSelect(cursorPosition, pos) => {
      ...state,
      cursorPosition,
      selectStart: Some(pos),
    }
  | MoveAndUnselect(cursorPosition) => {
      ...state,
      cursorPosition,
      selectStart: None,
    }
  | Select(pos) => {...state, selectStart: Some(pos)}
  | Unselect => {...state, selectStart: None}
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

  let verticalNav = (~up, m: OffsetMap.t, startPosition) => {
    let (row, target_x, _) = Offsets.findPosition(m, startPosition);
    let target_row =
      if (up) {
        row - 1;
      } else {
        row + 1;
      };

    switch (OffsetMap.IntMap.find_opt(target_row, m)) {
    | Some(row) => OffsetMap.Row.nearestPosition(row, target_x)
    | None => startPosition
    };
  };

  /* Workaround for Revery text wrapping not working exactly as I'd like.
   * - spaces inserted following newlines on empty rows to prevent collapse
   * - zero width unicodes inserted before leading spaces
   * - spaces which triggered a wrap are replaced by newlines
   * - row starts preceded by non-whitespace (forced break) get a newline */
  let newlineHack = (offsets, text) => {
    let len = String.length(text);
    let f = (acc, (_, {start, xOffsets, _}: OffsetMap.Row.t)) => {
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
    List.fold_left(f, text, List.rev(OffsetMap.IntMap.bindings(offsets)));
  };

  // TODO: This needs to be obtained from the textNodes parent container width.
  // So the textRef hook will have to go above this, and this will be calculated
  // from it.
  let margin = 100.;

  let%hook (state, dispatch) = {
    let value = Option.value(value, ~default="");
    Hooks.reducer(
      ~initialState={
        value,
        cursorPosition: Option.value(cursorPosition, ~default=0),
        selectStart: None,
        offsets: Offsets.build(margin, value),
      },
      reducer,
    );
  };

  let%hook textRef = Hooks.ref(None);
  let%hook xScrollOffset = Hooks.ref(0);
  let%hook yScrollOffset = Hooks.ref(0);

  let color =
    Selector.select(style, Color, Some(Colors.black)) |> Option.get;

  let value = Option.value(value, ~default=state.value);
  let showPlaceholder = value == "";
  let cursorPosition =
    Option.value(cursorPosition, ~default=state.cursorPosition)
    |> min(String.length(value));

  let%hook clickableRef = Hooks.ref(None);
  let isFocused = () => {
    switch (clickableRef^) {
    | Some(node) => Focus.isFocused(node)
    | None => false
    };
  };

  let%hook (cursorOpacity, resetCursor) =
    Cursor.use(~interval=Time.ms(500), ~isFocused=isFocused());

  let handleFocus = () => {
    resetCursor();
    onFocus();
    Sdl2.TextInput.start();
  };

  let handleBlur = () => {
    resetCursor();
    onBlur();
    Sdl2.TextInput.stop();
  };

  // TODO:This ought to be in the reducer, but since reducer calls are deferred
  // the ordering of side-effects can't be guaranteed.
  //
  // Refactor when https://github.com/briskml/brisk-reconciler/issues/54 has been fixed
  let update = (value, cursorPosition) => {
    onChange(value, cursorPosition);
    dispatch(TextInput(value, cursorPosition));
  };

  let paste = (currentValue, currentCursorPosition) => {
    switch (Sdl2.Clipboard.getText()) {
    | None => ()
    | Some(data) =>
      let (newValue, newCursorPosition) =
        Input.insertString(currentValue, data, currentCursorPosition);
      update(newValue, newCursorPosition);
    };
  };

  let handleTextInput = (event: NodeEvents.textInputEventParams) => {
    resetCursor();
    let (value, cursorPosition) =
      switch (state.selectStart) {
      | Some(pos) => removeBetween(value, pos, cursorPosition)
      | None => (value, cursorPosition)
      };
    let (value, cursorPosition) =
      Input.insertString(value, event.text, cursorPosition);
    update(value, cursorPosition);
  };

  let navigate = (shift, newPosition) =>
    if (shift) {
      if (Option.is_none(state.selectStart) && newPosition != cursorPosition) {
        dispatch(MoveAndSelect(newPosition, cursorPosition));
      } else {
        dispatch(Move(newPosition));
      };
    } else {
      dispatch(MoveAndUnselect(newPosition));
    };

  let handleKeyDown = (event: NodeEvents.keyEventParams) => {
    open Key;

    resetCursor();
    onKeyDown(event);

    let code = event.keycode;
    let super = Sdl2.Keymod.isGuiDown(event.keymod);
    let ctrl = Sdl2.Keymod.isControlDown(event.keymod);
    let shift = Sdl2.Keymod.isShiftDown(event.keymod);

    if (code == Keycode.left) {
      let cursorPosition =
        if (ctrl) {
          let (before, _) = Input.getStringParts(cursorPosition, value);
          cursorPosition - charsToPreviousWordEnd(before);
        } else {
          Input.getSafeStringBounds(value, cursorPosition, -1);
        };
      navigate(shift, cursorPosition);
    } else if (code == Keycode.right) {
      let cursorPosition =
        if (ctrl) {
          let (_, after) = Input.getStringParts(cursorPosition, value);
          cursorPosition + charsToNextWordEnd(after);
        } else {
          Input.getSafeStringBounds(value, cursorPosition, 1);
        };
      navigate(shift, cursorPosition);
    } else if (code == 1073741906) {
      // Up
      let cursorPosition =
        verticalNav(~up=true, state.offsets, cursorPosition);
      navigate(shift, cursorPosition);
    } else if (code == 1073741905) {
      // Down
      let cursorPosition =
        verticalNav(~up=false, state.offsets, cursorPosition);
      navigate(shift, cursorPosition);
    } else if (code == Keycode.delete) {
      let (value, cursorPosition) =
        switch (state.selectStart) {
        | Some(pos) => removeBetween(value, pos, cursorPosition)
        | None =>
          if (ctrl) {
            removeWordAfter(value, cursorPosition);
          } else {
            Input.removeCharacterAfter(value, cursorPosition);
          }
        };
      update(value, cursorPosition);
    } else if (code == Keycode.backspace) {
      let (value, cursorPosition) =
        switch (state.selectStart) {
        | Some(pos) => removeBetween(value, pos, cursorPosition)
        | None =>
          if (ctrl) {
            removeWordBefore(value, cursorPosition);
          } else {
            Input.removeCharacterBefore(value, cursorPosition);
          }
        };
      update(value, cursorPosition);
    } else if (code == Keycode.escape) {
      Focus.loseFocus();
    } else if (code == Keycode.v) {
      if (Environment.isMac && super || !Environment.isMac && ctrl) {
        let (value, cursorPosition) =
          switch (state.selectStart) {
          | Some(pos) => removeBetween(value, pos, cursorPosition)
          | None => (value, cursorPosition)
          };
        paste(value, cursorPosition);
      };
    } else if (code == Keycode.c) {
      if (Environment.isMac && super || !Environment.isMac && ctrl) {
        Option.iter(
          pos => copySelected(value, pos, cursorPosition),
          state.selectStart,
        );
      };
    } else if (code == Keycode.return && shift) {
      let (value, cursorPosition) =
        switch (state.selectStart) {
        | Some(pos) => removeBetween(value, pos, cursorPosition)
        | None => (value, cursorPosition)
        };
      let (value, cursorPosition) =
        Input.insertString(value, "\n", cursorPosition);
      update(value, cursorPosition);
    };
  };

  <View style={Styles.box(~style)} />;
};
