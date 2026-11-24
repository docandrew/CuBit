------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Stateful widgets that draw themselves and register hit/damage metadata
------------------------------------------------------------------------------
with CuBit.UI.Controls;
with CuBit.UI.Keys;
with CuBit.UI.Keycaps;
with CuBit.UI.State;
with CuBit.UI_Metrics;

package CuBit.UI.Widgets is
   DEFAULT_PADDING : constant Natural := 8;
   DENSE_PADDING   : constant Natural := 6;
   --  Rows or lines one wheel notch moves a list, table or text.
   WHEEL_STEP : constant := 3;

   type Badge_Style is (Badge_Neutral, Badge_Good, Badge_Danger);
   type Toolbar_Icon is
     (Open_Document, Save_Document, Interpret_Source, Compile_Program,
      Run_Program, Pause_Program, Stop_Program, Step_Into, Step_Over);

   --  Shared Bluecurve artwork without button chrome (for trees, lists, etc.).
   STOCK_ICON_SIZE : constant Positive := 16;
   procedure Draw_Stock_Icon
     (c : CuBit.UI.Canvas; x, y : Natural; icon : Toolbar_Icon;
      enabled : Boolean := True);

   subtype Tab_Title is String;
   type Tab_Title_Access is access constant Tab_Title;
   type Tab_Title_List is array (Positive range <>) of Tab_Title_Access;

   procedure Label
      (c : CuBit.UI.Canvas;
       bounds : CuBit.UI.Rect;
       colors : CuBit.UI.Theme;
       text : String;
       muted : Boolean := False);

   procedure Panel
      (c : CuBit.UI.Canvas;
       bounds : CuBit.UI.Rect;
       colors : CuBit.UI.Theme;
       content : out CuBit.UI.Rect;
       padding : Natural := 8);

   procedure Group_Box
      (c : CuBit.UI.Canvas;
       bounds : CuBit.UI.Rect;
       colors : CuBit.UI.Theme;
       title : String;
       content : out CuBit.UI.Rect;
       padding : Natural := 8);

   procedure Badge
      (c : CuBit.UI.Canvas;
       bounds : CuBit.UI.Rect;
       colors : CuBit.UI.Theme;
       label : String;
       style : Badge_Style := Badge_Neutral);

   procedure Key_Value
      (c : CuBit.UI.Canvas;
       bounds : CuBit.UI.Rect;
       colors : CuBit.UI.Theme;
       key : String;
       value : String;
       mutedValue : Boolean := False);

   procedure Toolbar
      (c : CuBit.UI.Canvas;
       bounds : CuBit.UI.Rect;
       colors : CuBit.UI.Theme);

   procedure Toolbar_Button
      (c : CuBit.UI.Canvas;
       bounds : CuBit.UI.Rect;
       colors : CuBit.UI.Theme;
       icon : Toolbar_Icon;
       enabled : Boolean := True;
       pressed : Boolean := False);

   procedure Toolbar_Separator
      (c : CuBit.UI.Canvas;
       bounds : CuBit.UI.Rect;
       colors : CuBit.UI.Theme);

   procedure Metric_Card
      (c : CuBit.UI.Canvas;
       bounds : CuBit.UI.Rect;
       colors : CuBit.UI.Theme;
       title : String;
       value : Natural);

   procedure Split_Pane
      (c : CuBit.UI.Canvas;
       st : in out CuBit.UI.State.UI_State;
       controls : in out CuBit.UI.Controls.Control_Map;
       id : CuBit.UI.Controls.Control_ID;
       bounds : CuBit.UI.Rect;
       damage : CuBit.UI.Rect;
       colors : CuBit.UI.Theme;
       vertical : Boolean;
       position : in out Natural;
       first : out CuBit.UI.Rect;
       second : out CuBit.UI.Rect;
       splitterSize : Natural := 6;
       minFirst : Natural := 96;
       minSecond : Natural := 96;
       retainedInput : Boolean := False);

   --  Read-only, monospace output with shared scrollbars and a Clear button.
   --  The caller owns the text. No keyboard events mutate it. Set firstLine
   --  to Natural'Last to reveal newly appended output; drawing clamps it.
   procedure Output_Box
     (c : CuBit.UI.Canvas;
      st : in out CuBit.UI.State.UI_State;
      controls : in out CuBit.UI.Controls.Control_Map;
      clearId, verticalId, horizontalId : CuBit.UI.Controls.Control_ID;
      bounds : CuBit.UI.Rect; colors : CuBit.UI.Theme; text : String;
      firstLine, firstColumn : in out Natural;
      maxFirstLine : out Natural; cleared : out Boolean);

   procedure Scroll_Area
      (c : CuBit.UI.Canvas;
       st : in out CuBit.UI.State.UI_State;
       controls : in out CuBit.UI.Controls.Control_Map;
       id : CuBit.UI.Controls.Control_ID;
       bounds : CuBit.UI.Rect;
       damage : CuBit.UI.Rect;
       colors : CuBit.UI.Theme;
       contentHeight : Natural;
       scrollY : in out Natural;
       viewport : out CuBit.UI.Rect;
       contentOriginY : out Natural;
       padding : Natural := 4);

   procedure Button
      (c : CuBit.UI.Canvas;
       st : in out CuBit.UI.State.UI_State;
       controls : in out CuBit.UI.Controls.Control_Map;
       id : CuBit.UI.Controls.Control_ID;
       bounds : CuBit.UI.Rect;
       damage : CuBit.UI.Rect;
       colors : CuBit.UI.Theme;
       label : String;
       result : out CuBit.UI.Widget_Result;
       retainedInput : Boolean := False;
       quiet : Boolean := False;
       hint : CuBit.UI.Keys.Chord := CuBit.UI.Keys.No_Chord;
       shade : CuBit.UI.Keycaps.Tone := CuBit.UI.Keycaps.Neutral);
   --  A button's label with its shortcut keycaps after it ("Cancel [Esc]",
   --  CuBit.UI.Keycaps: the chord comes from the keymap or the toolkit's
   --  standard keys, never text), and the width that shows both whole.
   HINT_GAP : constant := CuBit.UI_Metrics.Space_S;
   function Button_Width (label : String; hint : CuBit.UI.Keys.Chord := CuBit.UI.Keys.No_Chord) return Natural;

   type Navigation_Icon is (Navigate_Back, Navigate_Forward, Navigate_Reload);
   procedure Navigation_Button
      (c : CuBit.UI.Canvas;
       st : in out CuBit.UI.State.UI_State;
       controls : in out CuBit.UI.Controls.Control_Map;
       id : CuBit.UI.Controls.Control_ID;
       bounds, damage : CuBit.UI.Rect;
       colors : CuBit.UI.Theme;
       icon : Navigation_Icon;
       caption : String;
       enabled : Boolean;
       result : out CuBit.UI.Widget_Result);

   procedure Disabled_Button
      (c : CuBit.UI.Canvas;
       bounds : CuBit.UI.Rect;
       colors : CuBit.UI.Theme;
       label : String);

   procedure Checkbox
      (c : CuBit.UI.Canvas;
       st : in out CuBit.UI.State.UI_State;
       controls : in out CuBit.UI.Controls.Control_Map;
       id : CuBit.UI.Controls.Control_ID;
       bounds : CuBit.UI.Rect;
       damage : CuBit.UI.Rect;
       colors : CuBit.UI.Theme;
       checked : in out Boolean;
       result : out CuBit.UI.Widget_Result;
       enabled : Boolean := True);
   --  A disabled checkbox is drawn greyed with its state, and is not a
   --  control: it takes no pointer, focus or toggle (checked is unchanged).

   procedure Radio_Button
      (c : CuBit.UI.Canvas;
       st : in out CuBit.UI.State.UI_State;
       controls : in out CuBit.UI.Controls.Control_Map;
       id : CuBit.UI.Controls.Control_ID;
       bounds : CuBit.UI.Rect;
       damage : CuBit.UI.Rect;
       colors : CuBit.UI.Theme;
       label : String;
       selectedValue : Natural;
       value : in out Natural;
       result : out CuBit.UI.Widget_Result);

   procedure List_Item
      (c : CuBit.UI.Canvas;
       st : in out CuBit.UI.State.UI_State;
       controls : in out CuBit.UI.Controls.Control_Map;
       id : CuBit.UI.Controls.Control_ID;
       bounds : CuBit.UI.Rect;
       damage : CuBit.UI.Rect;
       colors : CuBit.UI.Theme;
       label : String;
       itemIndex : Natural;
       selectedIndex : in out Natural;
       result : out CuBit.UI.Widget_Result);

   procedure Menu_Item
      (c : CuBit.UI.Canvas;
       st : in out CuBit.UI.State.UI_State;
       controls : in out CuBit.UI.Controls.Control_Map;
       id : CuBit.UI.Controls.Control_ID;
       bounds : CuBit.UI.Rect;
       damage : CuBit.UI.Rect;
       colors : CuBit.UI.Theme;
       label : String;
       enabled : Boolean;
       result : out CuBit.UI.Widget_Result);

   procedure Menu_Title
      (c : CuBit.UI.Canvas;
       st : in out CuBit.UI.State.UI_State;
       controls : in out CuBit.UI.Controls.Control_Map;
       id : CuBit.UI.Controls.Control_ID;
       bounds : CuBit.UI.Rect;
       damage : CuBit.UI.Rect;
       colors : CuBit.UI.Theme;
       label : String;
       open : Boolean;
       result : out CuBit.UI.Widget_Result);

   procedure Table_Row
      (c : CuBit.UI.Canvas;
       st : in out CuBit.UI.State.UI_State;
       controls : in out CuBit.UI.Controls.Control_Map;
       id : CuBit.UI.Controls.Control_ID;
       bounds : CuBit.UI.Rect;
       damage : CuBit.UI.Rect;
       colors : CuBit.UI.Theme;
       c1, c2, c3 : String;
       rowIndex : Natural;
       selectedIndex : in out Natural;
       result : out CuBit.UI.Widget_Result);

   procedure Tab
      (c : CuBit.UI.Canvas;
       st : in out CuBit.UI.State.UI_State;
       controls : in out CuBit.UI.Controls.Control_Map;
       id : CuBit.UI.Controls.Control_ID;
       bounds : CuBit.UI.Rect;
       damage : CuBit.UI.Rect;
       colors : CuBit.UI.Theme;
       label : String;
       tabIndex : Natural;
       selectedIndex : in out Natural;
       result : out CuBit.UI.Widget_Result);

   --  A retained tab header containing arbitrary native child widgets.
   --  Register children after this call, using content.clip as their damage
   --  and interaction boundary. Later child IDs win hits over the parent.
   --  Consume parent/child actions with Controls.Take_Activated before paint.
   --  contentColors matches the tab background for labels and icons.
   procedure Tab
      (c : CuBit.UI.Canvas;
       st : in out CuBit.UI.State.UI_State;
       controls : in out CuBit.UI.Controls.Control_Map;
       id : CuBit.UI.Controls.Control_ID;
       bounds, damage : CuBit.UI.Rect;
       colors : CuBit.UI.Theme;
       selected : Boolean;
       content : out CuBit.UI.Canvas;
       contentColors : out CuBit.UI.Theme;
       result : out CuBit.UI.Widget_Result;
       orientation : CuBit.UI.Tab_Orientation := CuBit.UI.Horizontal;
       padding : Natural := 2);

   procedure Tab_Panel
      (c : CuBit.UI.Canvas;
       st : in out CuBit.UI.State.UI_State;
       controls : in out CuBit.UI.Controls.Control_Map;
       firstID : CuBit.UI.Controls.Control_ID;
       bounds : CuBit.UI.Rect;
       damage : CuBit.UI.Rect;
       colors : CuBit.UI.Theme;
       labels : Tab_Title_List;
       selectedIndex : in out Natural;
       page : out CuBit.UI.Rect;
       changed : out Boolean;
       stripHeight : Natural := CuBit.UI_Metrics.Control_Height + 1);

   procedure Horizontal_Slider
      (c : CuBit.UI.Canvas;
       st : in out CuBit.UI.State.UI_State;
       controls : in out CuBit.UI.Controls.Control_Map;
       id : CuBit.UI.Controls.Control_ID;
       bounds : CuBit.UI.Rect;
       damage : CuBit.UI.Rect;
       colors : CuBit.UI.Theme;
       minValue, maxValue : Natural;
       value : in out Natural;
       result : out CuBit.UI.Widget_Result);

   procedure Vertical_Scrollbar
      (c : CuBit.UI.Canvas;
       st : in out CuBit.UI.State.UI_State;
       controls : in out CuBit.UI.Controls.Control_Map;
       id : CuBit.UI.Controls.Control_ID;
       bounds : CuBit.UI.Rect;
       damage : CuBit.UI.Rect;
       colors : CuBit.UI.Theme;
       minValue, maxValue : Natural;
       value : in out Natural;
       result : out CuBit.UI.Widget_Result;
       pageSize : Positive := 1;
       retainedInput : Boolean := False;
       wheelArea : CuBit.UI.Rect := (others => 0);
       wheelStep : Positive := WHEEL_STEP);
   --  The wheel (UI-035): a scrollbar given the area it scrolls (its list,
   --  table, tree or text) takes the wheel over that area, in the toolkit
   --  (CuBit.UI.Controls.Route_Wheel); wheelStep is one notch in the
   --  scrollbar's units (rows, lines or pixels).

   procedure Horizontal_Scrollbar
      (c : CuBit.UI.Canvas;
       st : in out CuBit.UI.State.UI_State;
       controls : in out CuBit.UI.Controls.Control_Map;
       id : CuBit.UI.Controls.Control_ID;
       bounds : CuBit.UI.Rect;
       damage : CuBit.UI.Rect;
       colors : CuBit.UI.Theme;
       minValue, maxValue : Natural;
       value : in out Natural;
       result : out CuBit.UI.Widget_Result;
       pageSize : Positive := 1);

   procedure Text_Field
      (c : CuBit.UI.Canvas;
       st : in out CuBit.UI.State.UI_State;
       controls : in out CuBit.UI.Controls.Control_Map;
       id : CuBit.UI.Controls.Control_ID;
       bounds : CuBit.UI.Rect;
       damage : CuBit.UI.Rect;
       colors : CuBit.UI.Theme;
       text : String;
       result : out CuBit.UI.Widget_Result);
end CuBit.UI.Widgets;
