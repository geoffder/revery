module ContainerComponent = Container;
open Revery_UI;
open Revery_Core;
open Revery_UI_Primitives;
open Revery_Font;

module Hooks = Revery_UI_Hooks;

module Log = (val Timber.Log.withNamespace("TextArea"));

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

type state = {
  value: string,
  cursorPosition: int,
  selectStart: option(int),
  offsets: OffsetMap.t,
};

type dragState = {
  pos: int,
  xScroll: float,
  yScroll: float,
  lastTime: Time.t,
  shiftKey: bool,
  startPos: int,
  sceneOffsets: Offset.t,
  maxXOffset: float,
  maxYOffset: float,
  textHeight: float,
};

type action =
  | TextInput(string, int, string => OffsetMap.t)
  | RebuiltOffsets(OffsetMap.t)
  | Move(int)
  | MoveAndSelect(int, int)
  | MoveAndUnselect(int)
  | Select(int)
  | Unselect;

let reducer = (action, state) =>
  switch (action) {
  | TextInput(value, cursorPosition, offsetRefresher) => {
      value,
      cursorPosition,
      selectStart: None,
      offsets: offsetRefresher(value),
    }
  | RebuiltOffsets(offsets) => {...state, offsets}
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
  let defaultHighlightColor = Colors.blueViolet;

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
    /* marginRight(Constants.textMargin), */
    flexGrow(1),
  ];

  let cursor = (~x, ~y) => [
    position(`Absolute),
    marginTop(2),
    transform(Transform.[TranslateX(x), TranslateY(y)]),
  ];

  let textContainer = [flexGrow(1), overflow(`Hidden)];

  let text =
      (
        ~showPlaceholder,
        ~xScroll,
        ~yScroll,
        ~placeholderColor,
        ~color: Color.t,
      ) => [
    Style.color(showPlaceholder ? placeholderColor : color),
    alignItems(`Center),
    justifyContent(`FlexStart),
    textWrap(TextWrapping.WrapIgnoreWhitespace),
    transform(Transform.[TranslateX(-. xScroll), TranslateY(-. yScroll)]),
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
                ~highlightColor=Styles.defaultHighlightColor,
                ~highlightOpacity=0.5,
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

  let maxCharWidth = measureTextWidth("_");

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
    let (row, target_x, _) = OffsetMap.findPosition(m, startPosition);
    let target_row =
      if (up) {
        row - 1;
      } else {
        row + 1;
      };

    switch (OffsetMap.find_opt(target_row, m)) {
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
        if (OffsetMap.cardinal(xOffsets) == 0) {
          " ";
        } else if (start < len && OffsetMap.Utils.isSpace(text.[start])) {
          OffsetMap.Utils.zeroSpace;
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
    List.fold_left(f, text, List.rev(OffsetMap.bindings(offsets)));
  };

  let%hook textRef = Hooks.ref(None);
  let%hook xScrollOffset = Hooks.ref(0.);
  let%hook yScrollOffset = Hooks.ref(0.);
  let%hook xCursorOffset = Hooks.ref(0.);
  let%hook yCursorOffset = Hooks.ref(0.);

  let lineHeight = {
    let h =
      Revery_Draw.Text.lineHeight(~italic, fontFamily, fontSize, fontWeight);
    switch (textRef^) {
    | Some(node) =>
      let style: Style.t = node#getStyle();
      h *. style.lineHeight;
    | None => h
    };
  };

  let (containerWidth, containerHeight) =
    switch (Option.bind(textRef^, n => n#getParent())) {
    | Some(node) =>
      let container: Dimensions.t = (node#measurements(): Dimensions.t);
      Float.(of_int(container.width), of_int(container.height));
    | None =>
      Float.(
        of_int(Constants.defaultWidth),
        of_int(Constants.defaultHeight),
      )
    };

  let%hook (state, dispatch) = {
    let value = Option.value(value, ~default="");
    Hooks.reducer(
      ~initialState={
        value,
        cursorPosition: Option.value(cursorPosition, ~default=0),
        selectStart: None,
        offsets:
          OffsetMap.build(
            ~forceWrap,
            ~lineHeight,
            ~margin=containerWidth,
            ~measure=measureTextWidth,
            value,
          ),
      },
      reducer,
    );
  };

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
    Input.Cursor.use(~interval=Time.ms(500), ~isFocused=isFocused());

  let () = {
    let (_, xOffset, yOffset) =
      OffsetMap.findPosition(state.offsets, cursorPosition);
    xScrollOffset :=
      horizontalScroll(
        containerWidth -. maxCharWidth,
        OffsetMap.maxXOffset(state.offsets),
        xOffset,
        xScrollOffset^,
      );
    yScrollOffset :=
      verticalScroll(
        containerHeight,
        OffsetMap.maxYOffset(state.offsets) +. lineHeight,
        lineHeight,
        yOffset,
        yScrollOffset^,
      );
    xCursorOffset := xOffset;
    yCursorOffset := yOffset;
  };

  let offsetRefresher = newValue =>
    switch (OffsetMap.Utils.firstDiff(value, newValue)) {
    | Some(from) =>
      OffsetMap.refresh(
        ~forceWrap,
        ~lineHeight,
        ~margin=containerWidth,
        ~measure=measureTextWidth,
        ~old=state.offsets,
        ~from,
        newValue,
      )
    | None => state.offsets
    };

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
    dispatch(TextInput(value, cursorPosition, offsetRefresher));
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
    let super = Keymod.isGuiDown(event.keymod);
    let ctrl = Keymod.isControlDown(event.keymod);
    let shift = Keymod.isShiftDown(event.keymod);

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

  let handleRef = node => {
    clickableRef := Some(node);
    if (autofocus) {
      Focus.focus(node);
    };
  };

  let handleMouseMove =
      (dragState, {mouseX, mouseY, _}: NodeEvents.mouseMoveEventParams) => {
    let thisTime = Time.now();
    if ((Float.compare(Time.(toFloatSeconds(thisTime - dragState.lastTime))))(.
          0.016,
        )
        == 1) {
      let (_, xOffset, yOffset) =
        OffsetMap.findPosition(state.offsets, dragState.pos);
      let xScroll =
        horizontalScroll(
          containerWidth -. maxCharWidth,
          dragState.maxXOffset,
          xOffset,
          dragState.xScroll,
        );
      let yScroll =
        verticalScroll(
          containerHeight,
          dragState.textHeight,
          lineHeight,
          yOffset,
          dragState.yScroll,
        );
      let xTextOffset =
        mouseX -. Float.of_int(dragState.sceneOffsets.left) +. xScroll;
      let yTextOffset =
        mouseY -. Float.of_int(dragState.sceneOffsets.top) +. yScroll;
      let pos =
        OffsetMap.nearestPosition(state.offsets, xTextOffset, yTextOffset)
        |> Option.value(~default=String.length(value));
      xScrollOffset := xScroll;
      yScrollOffset := yScroll;
      dispatch(Move(pos));
      Some({...dragState, pos, xScroll, yScroll, lastTime: thisTime});
    } else {
      Some(dragState);
    };
  };

  let handleMouseUp = ({pos, shiftKey, startPos, _}, _) => {
    if (pos == startPos && !shiftKey) {
      dispatch(Unselect);
    };
    None;
  };

  let%hook (captureMouse, captureState) =
    Hooks.mouseCapture(
      ~onMouseMove=handleMouseMove,
      ~onMouseUp=handleMouseUp,
      (),
    );

  let handleMouseDown =
      ({shiftKey, mouseX, mouseY, _}: NodeEvents.mouseButtonEventParams) =>
    switch (textRef^) {
    | Some(node) =>
      let sceneOffsets: Offset.t = node#getSceneOffsets();
      let xTextOffset =
        mouseX -. Float.of_int(sceneOffsets.left) +. xScrollOffset^;
      let yTextOffset =
        mouseY -. Float.of_int(sceneOffsets.top) +. yScrollOffset^;
      let startPos =
        OffsetMap.nearestPosition(state.offsets, xTextOffset, yTextOffset)
        |> Option.value(~default=String.length(value));
      let maxXOffset = OffsetMap.maxXOffset(state.offsets);
      let maxYOffset = OffsetMap.maxYOffset(state.offsets);
      let textHeight = maxYOffset +. lineHeight;

      /* Log.debug( */
      /*   Printf.sprintf( */
      /*     "pos %i; mX %.2f; mY %.2f, xTex %.2f, yTex %.2f", */
      /*     startPos, */
      /*     mouseX, */
      /*     mouseY, */
      /*     xTextOffset, */
      /*     yTextOffset, */
      /*   ), */
      /* ); */

      Option.iter(Focus.focus, clickableRef^);
      resetCursor();
      captureMouse({
        pos: startPos,
        xScroll: xScrollOffset^,
        yScroll: yScrollOffset^,
        lastTime: Time.now(),
        shiftKey,
        startPos,
        sceneOffsets,
        maxXOffset,
        maxYOffset,
        textHeight,
      });
      if (shiftKey) {
        dispatch(MoveAndSelect(startPos, cursorPosition));
      } else {
        dispatch(MoveAndSelect(startPos, startPos));
      };
    | _ => ()
    };

  let handleDimensionsChanged =
      (dims: NodeEvents.DimensionsChangedEventParams.t) =>
    dispatch(
      RebuiltOffsets(
        OffsetMap.build(
          ~forceWrap,
          ~lineHeight,
          ~margin=Float.of_int(dims.width),
          ~measure=measureTextWidth,
          value,
        ),
      ),
    );

  let cursor = () => {
    let x = xCursorOffset^ -. xScrollOffset^;
    let y = yCursorOffset^ -. yScrollOffset^;
    <View style={Styles.cursor(~x, ~y)}>
      <Opacity opacity=cursorOpacity>
        <ContainerComponent
          width=Constants.cursorWidth
          height={fontSize |> int_of_float}
          color=cursorColor
        />
      </Opacity>
    </View>;
  };

  let text = () =>
    <Text
      ref={node => textRef := Some(node)}
      text={newlineHack(state.offsets, value)}
      fontFamily
      fontWeight
      italic
      fontSize
      underlined
      smoothing
      style={Styles.text(
        ~showPlaceholder,
        ~xScroll=xScrollOffset^,
        ~yScroll=yScrollOffset^,
        ~placeholderColor,
        ~color,
      )}
    />;

  let stripe = (~len, ~h, ~x, ~y) => {
    <View
      style=Style.[
        opacity(highlightOpacity),
        width(Int.of_float(len)),
        height(Int.of_float(h)),
        backgroundColor(highlightColor),
        position(`Absolute),
        transform(Transform.[TranslateX(x), TranslateY(y)]),
      ]
    />;
  };

  let highlights = () => {
    (
      switch (state.selectStart, Option.bind(textRef^, n => n#getParent())) {
      | (Some(start), Some(node)) =>
        let first = cursorPosition < start ? cursorPosition : start;
        let last = cursorPosition > start ? cursorPosition : start;
        let (startLine, firstX, firstY) =
          OffsetMap.findPosition(state.offsets, first);
        let (_, lastX, lastY) = OffsetMap.findPosition(state.offsets, last);
        let widths = OffsetMap.rowWidths(state.offsets);
        let striper = (from, n) => {
          let rec loop = (i, acc) =>
            if (i < n) {
              [
                stripe(
                  ~len=OffsetMap.find(from + i, widths),
                  ~h=lineHeight,
                  ~x=0.,
                  ~y=
                    firstY
                    -. yScrollOffset^
                    +. Float.of_int(i + 1)
                    *. lineHeight,
                ),
                ...acc,
              ];
            } else {
              acc;
            };
          loop(0, []);
        };
        switch (Int.of_float((lastY -. firstY) /. lineHeight)) {
        | 0 => [
            stripe(
              ~len=lastX -. firstX,
              ~h=lineHeight,
              ~x=firstX -. xScrollOffset^,
              ~y=firstY -. yScrollOffset^,
            ),
          ]
        | n => [
            stripe(
              ~len=OffsetMap.find(startLine, widths) -. firstX,
              ~h=lineHeight,
              ~x=firstX -. xScrollOffset^,
              ~y=firstY -. yScrollOffset^,
            ),
            ...List.rev([
                 stripe(
                   ~len=lastX,
                   ~h=lineHeight,
                   ~x=0.,
                   ~y=lastY -. yScrollOffset^,
                 ),
                 ...striper(startLine + 1, n - 1),
               ]),
          ]
        };
      | _ => []
      }
    )
    |> React.listToElement;
  };

  <Clickable
    onFocus=handleFocus
    onBlur=handleBlur
    componentRef=handleRef
    onKeyDown=handleKeyDown
    onTextInput=handleTextInput>
    <View style={Styles.box(~style)}>
      <View
        onMouseDown=handleMouseDown
        style=[
          Style.marginRight(
            Int.of_float(maxCharWidth) + Constants.textMargin,
          ),
          ...Styles.marginContainer,
        ]>
        <View
          style=Styles.textContainer
          onDimensionsChanged=handleDimensionsChanged>
          <text />
        </View>
        <highlights />
        <cursor />
      </View>
    </View>
  </Clickable>;
};
