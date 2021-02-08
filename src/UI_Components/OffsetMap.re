module Utils = {
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

  let firstDiff = (old, fresh) => {
    let oldLen = String.length(old);
    let freshLen = String.length(fresh);
    let minLen = oldLen < freshLen ? oldLen : freshLen;
    let index = ref(None);
    let i = ref(0);
    while (i^ < minLen) {
      if (!Char.equal(old.[i^], fresh.[i^])) {
        index := Some(i^);
        i := minLen;
      } else {
        incr(i);
      };
    };
    switch (index^) {
    | None =>
      if (oldLen == freshLen) {
        None;
      } else {
        Some(minLen);
      }
    | Some(_) as idx => idx
    };
  };
};

include Map.Make(Int);

module Row = {
  type t = {
    start: int,
    yOffset: float,
    xOffsets: Map.Make(Int).t(float),
  };

  let equal = (a, b) =>
    a.start == b.start
    && Float.equal(a.yOffset, b.yOffset)
    && equal(Float.equal, a.xOffsets, b.xOffsets);

  let toString = ({start, yOffset, xOffsets}) => {
    let xs =
      fold((_, x, acc) => Printf.sprintf("%s%.2f ", acc, x), xOffsets, "");
    Printf.sprintf(
      "start = %i; yOffset = %.2f; xOffsets = %s",
      start,
      yOffset,
      xs,
    );
  };

  let nearestPosition = ({start, xOffsets}, target) => {
    let rec loop = (i, lastX) =>
      switch (find_opt(i, xOffsets)) {
      | None => i
      | Some(x) =>
        if (Float.compare(x, target) == 1) {
          if (Float.compare(x -. target, target -. lastX) == (-1)) {
            i + 1;
          } else {
            i;
          };
        } else {
          loop(i + 1, x);
        }
      };
    loop(0, 0.) + start;
  };
};

type t = Map.Make(Int).t(Row.t);

let toString = m =>
  fold(
    (key, data, acc) =>
      Printf.sprintf("%srow %i -> %s\n", acc, key, Row.toString(data)),
    m,
    "\n",
  );

let build =
    (~forceWrap, ~fromRow=0, ~fromPos=1, ~measure, ~lineHeight, ~margin, text) => {
  let len = String.length(text);
  let addRow = (t: t, row, start, xOffsets) => {
    add(
      row,
      Row.{start, yOffset: Float.of_int(row) *. lineHeight, xOffsets},
      t,
    );
  };

  let rec loop = (pos, row, rowStart, offsets, t) =>
    /* zeroWidth character used in newlineHack to prevent leading space collapse */
    if (pos <= len) {
      let pad =
        if (Utils.isSpace(text.[rowStart])) {
          Utils.zeroSpace;
        } else {
          "";
        };
      let width =
        try(measure(pad ++ String.sub(text, rowStart, pos - rowStart))) {
        | _ =>
          failwith(Printf.sprintf("pos = %i; rowStart = %i", pos, rowStart))
        };
      let next = pos + 1;
      switch (text.[pos - 1]) {
      | ' '
          /* Lookahead to next whitespace to see if upcoming word overflows. */
          when
            next < len
            && {
              let wordStart = Utils.nextNonWhitespace(text, next);
              let lookahead =
                switch (
                  Option.bind(wordStart, n =>
                    String.index_from_opt(text, n, ' ')
                  ),
                  String.index_from_opt(text, next, '\n'),
                ) {
                | (Some(nextSpace), Some(nextBreak)) =>
                  if (nextSpace <= nextBreak) {
                    nextSpace;
                  } else {
                    nextBreak;
                  }
                | (Some(nextSpace), None) => nextSpace
                | (None, Some(nextBreak)) => nextBreak
                | _ => len
                };
              Float.(
                measure(
                  pad ++ String.sub(text, rowStart, lookahead - rowStart),
                )
                > margin
              );
            } =>
        loop(next, row + 1, pos, empty, addRow(t, row, rowStart, offsets))
      | '\n' =>
        loop(next, row + 1, pos, empty, addRow(t, row, rowStart, offsets))
      | c when forceWrap && Float.(width > margin) =>
        loop(pos, row + 1, pos - 1, empty, addRow(t, row, rowStart, offsets))
      // NOTE: untested. pos - rowStart could be one off correct index.
      // xOffsets used to be a float list, hence the start field in Row.t.
      | c =>
        loop(next, row, rowStart, add(pos - rowStart, width, offsets), t)
      };
    } else {
      addRow(t, row, rowStart, offsets);
    };
  loop(fromPos, fromRow, fromPos - 1, empty, empty);
};

let rowStartOfPosition = (t, position) => {
  let index = position - 1;
  let nRows = cardinal(t);
  let f = (i, (_, {start, xOffsets, _}: Row.t)) =>
    if (i < nRows - 1) {
      if (index < start + cardinal(xOffsets)) {
        Some((i, start));
      } else {
        None;
      };
    } else {
      Some((i, start));
    };
  Utils.listFindMapi(~f, bindings(t));
};

let refresh = (~forceWrap, ~measure, ~lineHeight, ~margin, ~old, ~from, text) => {
  let (i, start) =
    Option.value(~default=(0, 0), rowStartOfPosition(old, from));
  let fresh =
    build(
      ~forceWrap,
      ~measure,
      ~lineHeight,
      ~margin,
      ~fromRow=i,
      ~fromPos=start + 1,
      text,
    );
  // NOTE: this combine case shouldn't happen. Could throw exception instead...
  union(
    (_, _, fresher) => Some(fresher),
    filter((k, _) => k < i, old),
    fresh,
  );
};

let nearestPosition = (t: t, xOffset, yOffset) => {
  let xOffset = Float.max(0., xOffset);
  let rec findRow = (last_row, i) =>
    switch (find_opt(i, t), last_row) {
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
    if (index < r.start + cardinal(r.xOffsets)) {
      let x =
        Option.value(~default=0., find_opt(index - r.start, r.xOffsets));
      Some((i, x, r.yOffset));
    } else {
      None;
    };
  Option.value(~default=(0, 0., 0.), Utils.listFindMapi(~f, bindings(t)));
};

let maxXOffset = t => {
  let f = (_, {xOffsets, _}: Row.t, m) =>
    Float.max(
      switch (max_binding_opt(xOffsets)) {
      | None => 0.
      | Some((_, width)) => width
      },
      m,
    );
  fold(f, t, 0.);
};

let maxYOffset = (t: t) => {
  switch (max_binding_opt(t)) {
  | Some((_, row)) => row.yOffset
  | None => 0.
  };
};

let rowWidths = (t: t) =>
  List.map(
    (_, Row.{xOffsets, _}) => {
      switch (max_binding_opt(xOffsets)) {
      | None => 0.
      | Some((_, width)) => width
      }
    },
    bindings(t),
  );
