module ContainerComponent = Container;
open Revery_UI;
open Revery_Core;
open Revery_UI_Primitives;
open Revery_Font;

module Hooks = Revery_UI_Hooks;

let is_space = Char.equal(' ');
let is_newline = Char.equal('\n');
let zero_space = "​";

let stringFindIndex = (~pos=0, t, ~f) => {
  let n = String.length(t);
  let rec loop = i =>
    if (i == n) {
      None;
    } else if (f(i, t.[i])) {
      Some(i);
    } else {
      loop(i + 1);
    };
  loop(pos);
};

let listFindMapi = (~f, l) => {
  let rec loop = (i, rem) => {
    switch (rem) {
    | [] => None
    | [h, ...t] =>
      switch (f(i, h)) {
      | Some(r) as res => res
      | None => loop(i + 1, t)
      }
    };
  };
  loop(0, l);
};

let next_non_whitespace = (str, from) =>
  stringFindIndex(
    ~pos=from,
    ~f=(_, c) => !(is_space(c) || is_newline(c)),
    str,
  );

let first_diff = (old, fresh) => {
  let old_len = String.length(old);
  let fresh_len = String.length(fresh);
  let min_len =
    if (fresh_len <= old_len) {
      fresh_len;
    } else {
      old_len;
    };
  let index = ref(None);
  let i = ref(0);
  while (i^ < min_len) {
    if (!Char.equal(old.[i^], fresh.[i^])) {
      index := Some(i^);
      i := min_len;
    } else {
      incr(i);
    };
  };
  switch (index^) {
  | None =>
    if (old_len == fresh_len) {
      None;
    } else {
      Some(min_len);
    }
  | Some(_) as idx => idx
  };
};

module type OffsetMapConfig = {
  let measure: string => float;
  let lineHeight: float;
  let forceWrap: bool;
};

module OffsetMap = (C: OffsetMapConfig) => {
  let measure = C.measure;
  let lineHeight = C.lineHeight;
  let forceWrap = C.forceWrap;

  module Row = {
    type t = {
      start: int,
      y_offset: float,
      x_offsets: list(float),
    };

    let equal = (a, b) =>
      a.start == b.start
      && Float.equal(a.y_offset, b.y_offset)
      && List.for_all2(Float.equal, a.x_offsets, b.x_offsets);

    let to_string = ({start, y_offset, x_offsets}) => {
      let xs = List.fold_left(Printf.sprintf("%s%.2f "), "", x_offsets);
      Printf.sprintf(
        "start = %i; y_offset = %.2f; x_offsets = %s",
        start,
        y_offset,
        xs,
      );
    };

    let nearest_position = (t, x_offset) => {
      let rec loop = (i, last_offset) =>
        fun
        | [] => i
        | [h, ...t] =>
          /* if (Float.(h > x_offset)) { */
          /* if (Float.(h - x_offset < x_offset - last_offset)) { */
          if (Float.compare(h, x_offset) == 1) {
            if (Float.compare(h -. x_offset, x_offset -. last_offset) == (-1)) {
              i + 1;
            } else {
              i;
            };
          } else {
            loop(i + 1, h, t);
          };
      loop(0, 0., t.x_offsets) + t.start;
    };
  };

  module IntMap = Map.Make(Int);
  type t = IntMap.t(Row.t);

  let to_string = m =>
    IntMap.fold(m, ~init="\n", ~f=(~key, ~data, acc) =>
      Printf.sprintf("%srow %i -> %s\n", acc, key, Row.to_string(data))
    );

  let build = (~from_row=0, ~from_pos=1, margin, text) => {
    let len = String.length(text);
    let add_row = (t: t, row, start, offsets) => {
      let x_offsets = List.rev(offsets);
      IntMap.add(
        row,
        Row.{start, y_offset: Float.of_int(row) *. lineHeight, x_offsets},
        t,
      );
    };

    let rec loop = (pos, row, row_start, offsets, t) =>
      /* zero_width character used in newline_hack to prevent leading space collapse */
      if (pos <= len) {
        let pad =
          if (is_space(text.[row_start])) {
            zero_space;
          } else {
            "";
          };
        let width =
          /* try(measure_width(pad ++ String.slice(text, row_start, pos))) { */
          try(measure(pad ++ String.sub(text, row_start, pos - row_start))) {
          | _ =>
            failwith(
              Printf.sprintf("pos = %i; row_start = %i", pos, row_start),
            )
          };
        let next = pos + 1;
        switch (text.[pos - 1]) {
        | ' '
            /* Lookahead to next whitespace to see if upcoming word overflows. */
            when
              next < len
              && {
                let word_start = next_non_whitespace(text, next);
                let lookahead =
                  switch (
                    Option.bind(word_start, n =>
                      String.index_from_opt(text, n, ' ')
                    ),
                    String.index_from_opt(text, next, '\n'),
                  ) {
                  | (Some(next_space), Some(next_break)) =>
                    if (next_space <= next_break) {
                      next_space;
                    } else {
                      next_break;
                    }
                  | (Some(next_space), None) => next_space
                  | (None, Some(next_break)) => next_break
                  | _ => len
                  };
                Float.(
                  measure(
                    /* pad ++ String.slice(text, row_start, lookahead), */
                    pad ++ String.sub(text, row_start, lookahead - row_start),
                  )
                  > margin
                );
              } =>
          loop(next, row + 1, pos, [], add_row(t, row, row_start, offsets))
        | '\n' =>
          loop(next, row + 1, pos, [], add_row(t, row, row_start, offsets))
        | c when forceWrap && Float.(width > margin) =>
          loop(
            pos,
            row + 1,
            pos - 1,
            [],
            add_row(t, row, row_start, offsets),
          )
        | c => loop(next, row, row_start, [width, ...offsets], t)
        };
      } else {
        add_row(t, row, row_start, offsets);
      };
    loop(from_pos, from_row, from_pos - 1, [], IntMap.empty);
  };

  let row_start_of_position = (t, position) => {
    let index = position - 1;
    let n_rows = IntMap.cardinal(t);
    let f = (i, (_, {start, x_offsets, _}: Row.t)) =>
      if (i < n_rows - 1) {
        if (index < start + List.length(x_offsets)) {
          Some((i, start));
        } else {
          None;
        };
      } else {
        Some((i, start));
      };
    listFindMapi(~f, IntMap.bindings(t));
  };

  let update = (~force_wrap, ~old, ~from, font_info, lineHeight, margin, text) => {
    let (i, start) =
      Option.value(~default=(0, 0), row_start_of_position(old, from));
    let fresh = build(~from_row=i, ~from_pos=start + 1, margin, text);
    // NOTE: this combine case shouldn't happen. Could throw exception instead...
    IntMap.union(
      (_, _, fresher) => Some(fresher),
      IntMap.filter((k, _) => k < i, old),
      fresh,
    );
  };

  let nearest_position = (t: t, x_offset, y_offset) => {
    let x_offset = Float.max(0., x_offset);
    let rec find_row = (last_row, i) =>
      switch (IntMap.find_opt(i, t), last_row) {
      | (Some(row) as current, Some(last: Row.t)) =>
        if (Float.(row.y_offset > y_offset)) {
          if (Float.(row.y_offset -. y_offset < y_offset -. last.y_offset)) {
            current;
          } else {
            last_row;
          };
        } else {
          find_row(current, i + 1);
        }
      | (Some(row) as current, None) =>
        if (Float.(row.y_offset > y_offset)) {
          current;
        } else {
          find_row(current, i + 1);
        }
      | (None, Some(_)) => last_row
      | (None, None) => None
      };
    /* Option.map(find_row(None, 0), ~f=row => */
    /*   Row.nearest_position(row, x_offset) */
    /* ); */
    Option.map(
      row => Row.nearest_position(row, x_offset),
      find_row(None, 0),
    );
  };

  let find_position = (t, position) => {
    let index = position - 1;
    let f = (i, (_, r: Row.t)) =>
      if (index < r.start + List.length(r.x_offsets)) {
        let x =
          // TODO: Consider refactoring to use a data type other than list to
          // avoid using nth.
          Option.value(
            ~default=0.,
            List.nth_opt(r.x_offsets, index - r.start),
          );
        Some((i, x, r.y_offset));
      } else {
        None;
      };
    // TODO: Should make a find_seqi equivalent and use Map to_seq
    Option.value(
      ~default=(0, 0., 0.),
      listFindMapi(~f, IntMap.bindings(t)),
    );
  };

  let max_x_offset = t => {
    let f = (_, {x_offsets, _}: Row.t, m) =>
      Float.max(m, List.fold_left(Float.max, 0., x_offsets));
    /* IntMap.fold(t, ~init=0., ~f); */
    IntMap.fold(f, t, 0.);
  };

  let max_y_offset = (t: t) => {
    /* Option.value_map(IntMap.max_binding(t), ~default=0., ~f=((_, row)) => */
    /*   row.y_offset */
    switch (IntMap.max_binding_opt(t)) {
    | Some((_, row)) => row.y_offset
    | None => 0.
    };
  };

  /* let row_widths = (t: t) => */
  /*   List.map(Map.data(t), ~f=({x_offsets, _}) => */
  /*     List.fold(~init=0., ~f=Float.max, x_offsets) */
  /*   ); */

  let row_widths = (t: t) =>
    Seq.map(
      (_, Row.{x_offsets, _}) => List.fold_left(Float.max, 0., x_offsets),
      IntMap.to_seq(t),
    )
    |> List.of_seq;
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
    OffsetMap({
      let measure = measureTextWidth;
      let lineHeight = lineHeight;
      let forceWrap = forceWrap;
    });

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
