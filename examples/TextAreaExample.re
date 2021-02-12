open Revery;
open Revery.UI;
open Revery.UI.Components;

let containerStyle =
  Style.[
    position(`Absolute),
    top(0),
    bottom(0),
    left(0),
    right(0),
    alignItems(`Center),
    justifyContent(`Center),
    flexDirection(`Column),
  ];

let textStyle =
  Style.[
    color(Colors.white),
    width(100),
    margin(14),
    textWrap(TextWrapping.NoWrap),
  ];

module Example = {
  type state = {text: string};

  let%component make = () => {
    let%hook ({text}, setValue) = Hooks.state({text: ""});

    <View style=containerStyle>
      <View
        style=Style.[
          flexDirection(`Column),
          flexGrow(1),
          alignItems(`Center),
          justifyContent(`Center),
        ]>
        <TextArea
          style={List.rev(
            Style.[
              backgroundColor(Colors.white),
              marginBottom(10),
              ...TextArea.Styles.default,
            ],
          )}
          placeholder="Insert text here"
          onChange={(value, _) => setValue(state => {text: value})}
          value=text
        />
        <Button
          height=50
          width=100
          fontSize=15.
          title="Reset"
          onClick={() => setValue(state => {text: ""})}
        />
      </View>
    </View>;
  };
};

let render = () => <Example />;
