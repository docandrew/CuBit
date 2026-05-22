------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Immediate-mode UI state
--
--  CuBit.UI.State grew out of the earlier DAGBuild GUI experiment. The model
--  keeps the useful immediate-mode ideas from that prototype: scoped widget
--  IDs, hot/active items, keyboard focus, and hidden-widget focus cleanup. It
--  replaces SDL-specific input/rendering with CuBit surface primitives.
------------------------------------------------------------------------------
package CuBit.UI.State is
   type Widget_ID is new Integer;
   NO_ITEM      : constant Widget_ID := 0;
   INVALID_ITEM : constant Widget_ID := -1;

   MAX_SCOPES : constant Natural := 32;
   type Scope_ID is range 0 .. MAX_SCOPES;
   NO_SCOPE : constant Scope_ID := 0;

   subtype Scope_Index is Scope_ID range 1 .. Scope_ID'Last;
   type Last_ID_List is array (Scope_Index) of Widget_ID;

   type UI_State is record
      pointer : CuBit.UI.Pointer_State := (others => <>);
      frameCounter : Natural := 0;
      lastClickFrame : Natural := 0;
      lastClickX : Natural := 0;
      lastClickY : Natural := 0;

      hotItem    : Widget_ID := NO_ITEM;
      hotScope   : Scope_ID := NO_SCOPE;
      activeItem : Widget_ID := NO_ITEM;
      activeScope : Scope_ID := NO_SCOPE;

      keyboardItem : Widget_ID := NO_ITEM;
      keyboardScope : Scope_ID := NO_SCOPE;
      keyboardHeartbeat : Boolean := False;
      textCursor : Natural := 0;
      textSelectionStart : Natural := 0;
      textSelectionEnd : Natural := 0;
      textSelectionAnchor : Natural := 0;
      textWordSelect : Boolean := False;
      textDoubleClick : Boolean := False;

      currentScope : Scope_ID := NO_SCOPE;
      scopeXOffset : Natural := 0;
      scopeYOffset : Natural := 0;
      rootLastID : Widget_ID := NO_ITEM;
      lastIDs : Last_ID_List := (others => NO_ITEM);

      lastWidget : Widget_ID := NO_ITEM;
      lastScope  : Scope_ID := NO_SCOPE;
      scopeError : Boolean := False;
   end record;

   procedure Begin_Frame (st : in out UI_State);
   procedure Finish_Frame (st : in out UI_State);

   procedure Set_Pointer
      (st : in out UI_State;
      x, y : Natural;
      down : Boolean;
      pressed : Boolean := False;
      released : Boolean := False;
      enabled : Boolean := True);

   procedure Enter_Scope
      (st : in out UI_State;
       xOffset : Natural := 0;
       yOffset : Natural := 0);

   procedure Exit_Scope
      (st : in out UI_State;
       xOffset : Natural := 0;
       yOffset : Natural := 0);

   function Next_ID (st : in out UI_State) return Widget_ID;

   function Offset_Rect
      (st : UI_State; bounds : CuBit.UI.Rect) return CuBit.UI.Rect;

   function Region_Hit
      (st : UI_State; bounds : CuBit.UI.Rect) return Boolean;

   function Button
      (st : in out UI_State; bounds : CuBit.UI.Rect)
      return CuBit.UI.Widget_Result;

   function Is_Last_Widget_Focused (st : UI_State) return Boolean;

   procedure Clear_Keyboard_Focus (st : in out UI_State);

   function Text_Field
      (st : in out UI_State; bounds : CuBit.UI.Rect; text : String)
      return CuBit.UI.Widget_Result;

   function Text_Field_Key
      (st : in out UI_State;
       text : in out String;
       textLen : in out Natural;
       keyCode : Natural;
       modifiers : Natural := 0) return Boolean;

   function Text_Field_Text
      (st : in out UI_State;
       text : in out String;
       textLen : in out Natural;
       codepoint : Natural) return Boolean;

   function Checkbox
      (st : in out UI_State;
       bounds : CuBit.UI.Rect;
       checked : in out Boolean) return CuBit.UI.Widget_Result;

   function Horizontal_Slider
      (st : in out UI_State;
       bounds : CuBit.UI.Rect;
       value : in out Natural;
       minValue, maxValue : Natural) return CuBit.UI.Widget_Result;

   function Vertical_Scrollbar
      (st : in out UI_State;
       bounds : CuBit.UI.Rect;
       value : in out Natural;
       minValue, maxValue : Natural) return CuBit.UI.Widget_Result;
end CuBit.UI.State;
