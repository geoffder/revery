let isSpace = Char.equal(' ');
let isNewline = Char.equal('\n');
let zeroSpace = "​";

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

let nextNonWhitespace = (str, from) =>
  stringFindIndex(
    ~pos=from,
    ~f=(_, c) => !(isSpace(c) || isNewline(c)),
    str,
  );

module IntMap = Map.Make(Int);

module type Config = {
  let measure: string => float;
  let lineHeight: float;
  let forceWrap: bool;
};

module Row = {
  type t = {
    start: int,
    yOffset: float,
    xOffsets: list(float),
  };

  let equal = (a, b) =>
    a.start == b.start
    && Float.equal(a.yOffset, b.yOffset)
    && List.for_all2(Float.equal, a.xOffsets, b.xOffsets);

  let toString = ({start, yOffset, xOffsets}) => {
    let xs = List.fold_left(Printf.sprintf("%s%.2f "), "", xOffsets);
    Printf.sprintf(
      "start = %i; yOffset = %.2f; xOffsets = %s",
      start,
      yOffset,
      xs,
    );
  };

  let nearestPosition = (t, xOffset) => {
    let rec loop = (i, last_offset) =>
      fun
      | [] => i
      | [h, ...t] =>
        /* if (Float.(h > xOffset)) { */
        /* if (Float.(h - xOffset < xOffset - last_offset)) { */
        if (Float.compare(h, xOffset) == 1) {
          if (Float.compare(h -. xOffset, xOffset -. last_offset) == (-1)) {
            i + 1;
          } else {
            i;
          };
        } else {
          loop(i + 1, h, t);
        };
    loop(0, 0., t.xOffsets) + t.start;
  };
};

type t = IntMap.t(Row.t);

module type S = {
  let measure: string => float;
  let lineHeight: float;
  let forceWrap: bool;

  let toString: t => string;
  let build: (~fromRow: IntMap.key=?, ~fromPos: int=?, float, string) => t;
  let rowStartOfPosition: (t, int) => option((int, int));
  let refresh: (~old: t, ~from: int, float, string) => t;
  let nearestPosition: (t, float, float) => option(int);
  let findPosition: (t, int) => (int, float, float);
  let maxXOffset: t => float;
  let maxYOffset: t => float;
  let rowWidths: t => list(Row.t => float);
};

module Make = (C: Config) : S => {
  let measure = C.measure;
  let lineHeight = C.lineHeight;
  let forceWrap = C.forceWrap;

  module IntMap = Map.Make(Int);

  let toString = m =>
    IntMap.fold(
      (key, data, acc) =>
        Printf.sprintf("%srow %i -> %s\n", acc, key, Row.toString(data)),
      m,
      "\n",
    );

  let build = (~fromRow=0, ~fromPos=1, margin, text) => {
    let len = String.length(text);
    let addRow = (t: t, row, start, offsets) => {
      let xOffsets = List.rev(offsets);
      IntMap.add(
        row,
        Row.{start, yOffset: Float.of_int(row) *. lineHeight, xOffsets},
        t,
      );
    };

    let rec loop = (pos, row, rowStart, offsets, t) =>
      /* zero_width character used in newline_hack to prevent leading space collapse */
      if (pos <= len) {
        let pad =
          if (isSpace(text.[rowStart])) {
            zeroSpace;
          } else {
            "";
          };
        let width =
          /* try(measure_width(pad ++ String.slice(text, rowStart, pos))) { */
          try(measure(pad ++ String.sub(text, rowStart, pos - rowStart))) {
          | _ =>
            failwith(
              Printf.sprintf("pos = %i; rowStart = %i", pos, rowStart),
            )
          };
        let next = pos + 1;
        switch (text.[pos - 1]) {
        | ' '
            /* Lookahead to next whitespace to see if upcoming word overflows. */
            when
              next < len
              && {
                let wordStart = nextNonWhitespace(text, next);
                let lookahead =
                  switch (
                    Option.bind(wordStart, n =>
                      String.index_from_opt(text, n, ' ')
                    ),
                    String.index_from_opt(text, next, '\n'),
                  ) {
                  | (Some(nextSpace), Some(next_break)) =>
                    if (nextSpace <= next_break) {
                      nextSpace;
                    } else {
                      next_break;
                    }
                  | (Some(nextSpace), None) => nextSpace
                  | (None, Some(next_break)) => next_break
                  | _ => len
                  };
                Float.(
                  measure(
                    pad ++ String.sub(text, rowStart, lookahead - rowStart),
                  )
                  > margin
                );
              } =>
          loop(next, row + 1, pos, [], addRow(t, row, rowStart, offsets))
        | '\n' =>
          loop(next, row + 1, pos, [], addRow(t, row, rowStart, offsets))
        | c when forceWrap && Float.(width > margin) =>
          loop(pos, row + 1, pos - 1, [], addRow(t, row, rowStart, offsets))
        | c => loop(next, row, rowStart, [width, ...offsets], t)
        };
      } else {
        addRow(t, row, rowStart, offsets);
      };
    loop(fromPos, fromRow, fromPos - 1, [], IntMap.empty);
  };

  let rowStartOfPosition = (t, position) => {
    let index = position - 1;
    let nRows = IntMap.cardinal(t);
    let f = (i, (_, {start, xOffsets, _}: Row.t)) =>
      if (i < nRows - 1) {
        if (index < start + List.length(xOffsets)) {
          Some((i, start));
        } else {
          None;
        };
      } else {
        Some((i, start));
      };
    listFindMapi(~f, IntMap.bindings(t));
  };

  let refresh = (~old, ~from, margin, text) => {
    let (i, start) =
      Option.value(~default=(0, 0), rowStartOfPosition(old, from));
    let fresh = build(~fromRow=i, ~fromPos=start + 1, margin, text);
    // NOTE: this combine case shouldn't happen. Could throw exception instead...
    IntMap.union(
      (_, _, fresher) => Some(fresher),
      IntMap.filter((k, _) => k < i, old),
      fresh,
    );
  };

  let nearestPosition = (t: t, xOffset, yOffset) => {
    let xOffset = Float.max(0., xOffset);
    let rec findRow = (last_row, i) =>
      switch (IntMap.find_opt(i, t), last_row) {
      | (Some(row) as current, Some(last: Row.t)) =>
        if (Float.(row.yOffset > yOffset)) {
          if (Float.(row.yOffset -. yOffset < yOffset -. last.yOffset)) {
            current;
          } else {
            last_row;
          };
        } else {
          findRow(current, i + 1);
        }
      | (Some(row) as current, None) =>
        if (Float.(row.yOffset > yOffset)) {
          current;
        } else {
          findRow(current, i + 1);
        }
      | (None, Some(_)) => last_row
      | (None, None) => None
      };
    Option.map(row => Row.nearestPosition(row, xOffset), findRow(None, 0));
  };

  let findPosition = (t, position) => {
    let index = position - 1;
    let f = (i, (_, r: Row.t)) =>
      if (index < r.start + List.length(r.xOffsets)) {
        let x =
          // TODO: Consider refactoring to use a data type other than list to
          // avoid using nth.
          Option.value(
            ~default=0.,
            List.nth_opt(r.xOffsets, index - r.start),
          );
        Some((i, x, r.yOffset));
      } else {
        None;
      };
    // TODO: Should make a find_seqi equivalent and use Map to_seq
    Option.value(
      ~default=(0, 0., 0.),
      listFindMapi(~f, IntMap.bindings(t)),
    );
  };

  let maxXOffset = t => {
    let f = (_, {xOffsets, _}: Row.t, m) =>
      Float.max(m, List.fold_left(Float.max, 0., xOffsets));
    IntMap.fold(f, t, 0.);
  };

  let maxYOffset = (t: t) => {
    switch (IntMap.max_binding_opt(t)) {
    | Some((_, row)) => row.yOffset
    | None => 0.
    };
  };

  let rowWidths = (t: t) =>
    Seq.map(
      (_, Row.{xOffsets, _}) => List.fold_left(Float.max, 0., xOffsets),
      IntMap.to_seq(t),
    )
    |> List.of_seq;
};