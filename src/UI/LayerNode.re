open Revery_Core;
open Revery_Draw;

module Layout = Layout;
module LayoutTypes = Layout.LayoutTypes;

open ViewNode;
open Style;
open Style.Border;
open Style.BoxShadow;

class layerNode (condition: RenderCondition.t) = {
  as _this;
  inherit (class viewNode)() as _super;
  val mutable _lastCondition: RenderCondition.t = condition;
  val mutable _maybeCanvas: option(CanvasContext.t) = None;
  pub! draw = (parentContext: NodeDrawContext.t) => {
    _super#draw(parentContext);
  };
  pub setCondition = (condition: RenderCondition.t) => {
    _lastCondition = condition;
  };
};
