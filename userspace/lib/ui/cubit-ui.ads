------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Small immediate-mode UI drawing primitives for user surfaces
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System;
with CuBit.Theme;

package CuBit.UI is
   subtype Color is Unsigned_32;

   type Rect is record
      x : Natural := 0;
      y : Natural := 0;
      w : Natural := 0;
      h : Natural := 0;
   end record;

   type Canvas is record
      addr : System.Address := System.Null_Address;
      width : Natural := 0;
      height : Natural := 0;
      pitch : Natural := 0;
      clipEnabled : Boolean := False;
      clip : Rect := (others => 0);
   end record;

   type Theme is record
      desktop : Color := 16#2D3343#;
      panel   : Color := 16#343B4D#;
      face    : Color := 16#3D465C#;
      edge    : Color := 16#596172#;
      shadow  : Color := 16#1F2430#;
      text    : Color := 16#D9DEE8#;
      muted   : Color := 16#9AA5B5#;
      accent  : Color := 16#FFCC66#;
      good    : Color := 16#95E6CB#;
      danger  : Color := 16#F28779#;
      field   : Color := 16#FFFFFF#;
      selection : Color := 16#000080#;
      selectionText : Color := 16#FFFFFF#;
      highlight : Color := 16#FFFFFF#;
      darkShadow : Color := 16#404040#;
   end record;

   Mirage : constant Theme :=
     (desktop => 16#2D3343#,
      panel   => 16#343B4D#,
      face    => 16#3D465C#,
      edge    => 16#596172#,
      shadow  => 16#1F2430#,
      text    => 16#D9DEE8#,
      muted   => 16#9AA5B5#,
      accent  => 16#FFCC66#,
      good    => 16#95E6CB#,
      danger  => 16#F28779#,
      field => 16#343B4D#,
      selection => 16#4D668F#,
      selectionText => 16#FFFFFF#,
      highlight => 16#596172#,
      darkShadow => 16#151923#);

   Classic : constant Theme :=
     (desktop => CuBit.Theme.Desktop,
      panel   => CuBit.Theme.Panel,
      face    => CuBit.Theme.Face,
      edge    => CuBit.Theme.Edge,
      shadow  => CuBit.Theme.Shadow,
      text    => CuBit.Theme.Text,
      muted   => CuBit.Theme.Muted,
      accent  => CuBit.Theme.Accent,
      good    => CuBit.Theme.Good,
      danger  => CuBit.Theme.Danger,
      field => CuBit.Theme.Face,
      selection => CuBit.Theme.Accent,
      selectionText => CuBit.Theme.White,
      highlight => CuBit.Theme.White,
      darkShadow => CuBit.Theme.Desktop);

   CuBit_Classic : constant Theme :=
     (desktop => 16#808080#,
      panel => 16#C0C0C0#,
      face => 16#C0C0C0#,
      edge => 16#DFDFDF#,
      shadow => 16#808080#,
      text => 16#000000#,
      muted => 16#404040#,
      accent => 16#000080#,
      good => 16#006000#,
      danger => 16#A00000#,
      field => 16#FFFFFF#,
      selection => 16#000080#,
      selectionText => 16#FFFFFF#,
      highlight => 16#FFFFFF#,
      darkShadow => 16#404040#);

   type Button_Style is (Button_Normal, Button_Hot, Button_Pressed,
                         Button_Disabled, Button_Active);

   type Pointer_State is record
      x : Natural := 0;
      y : Natural := 0;
      down : Boolean := False;
      pressed : Boolean := False;
      released : Boolean := False;
      enabled : Boolean := True;
   end record;

   type Widget_Result is record
      hot : Boolean := False;
      active : Boolean := False;
      activated : Boolean := False;
   end record;

   function Is_Empty (r : Rect) return Boolean;
   function Point_In_Rect (x, y : Natural; r : Rect) return Boolean;
   function Union_Rect (a, b : Rect) return Rect;
   function Inflate_Rect (r : Rect; amount : Natural) return Rect;
   function Clamp_Rect (c : Canvas; r : Rect) return Rect;
   function With_Clip (c : Canvas; clip : Rect) return Canvas;

   procedure Set_Pixel (c : Canvas; x, y : Natural; fill : Color);
   procedure Fill_Rect (c : Canvas; r : Rect; fill : Color);
   procedure Stroke_Rect
      (c : Canvas; r : Rect; light : Color; dark : Color);
   procedure Draw_Glyph
      (c : Canvas; x, y : Natural; ch : Character; fg, bg : Color);
   procedure Draw_Text
      (c : Canvas; x, y : Natural; text : String; fg, bg : Color);
   function UI_Text_Width (text : String) return Natural;
   function UI_Text_Height return Natural;
   procedure Draw_UI_Text
      (c : Canvas; x, y : Natural; text : String; fg, bg : Color);
   procedure Draw_Button_Frame
      (c : Canvas; r : Rect; colors : Theme; style : Button_Style);
   procedure Draw_Button
      (c : Canvas; r : Rect; colors : Theme; style : Button_Style;
       label : String);
   procedure Draw_Menu_Bar
      (c : Canvas; r : Rect; colors : Theme);
   procedure Draw_Menu_Title
      (c : Canvas; r : Rect; colors : Theme;
       hot : Boolean; active : Boolean; label : String);
   procedure Draw_Status_Bar
      (c : Canvas; r : Rect; colors : Theme; left, right : String);
   procedure Draw_Pane
      (c : Canvas; r : Rect; colors : Theme; title : String);
   procedure Draw_Table_Header
      (c : Canvas; r : Rect; colors : Theme; c1, c2, c3 : String);
   procedure Draw_Table_Row
      (c : Canvas; r : Rect; colors : Theme;
       selected : Boolean; hot : Boolean;
       c1, c2, c3 : String);
   procedure Draw_Tab_Strip
      (c : Canvas; r : Rect; colors : Theme);
   procedure Draw_Tab
      (c : Canvas; r : Rect; colors : Theme;
       selected : Boolean; hot : Boolean; active : Boolean;
       label : String);
   procedure Draw_Natural_Value
      (c : Canvas; r : Rect; colors : Theme; value : Natural);
   procedure Draw_Progress_Bar
      (c : Canvas; r : Rect; colors : Theme;
       minValue, maxValue, value : Natural);
   procedure Draw_Swatch
      (c : Canvas; r : Rect; colors : Theme;
       fill : Color; label : String);
   procedure Draw_Text_Field
      (c : Canvas; r : Rect; colors : Theme; text : String;
       focused : Boolean; hot : Boolean);
   procedure Draw_Text_Edit_Field
      (c : Canvas; r : Rect; colors : Theme; text : String;
       cursor, selectionStart, selectionEnd : Natural;
       focused : Boolean; hot : Boolean);
   procedure Draw_Multiline_Text_Edit
      (c : Canvas; r : Rect; colors : Theme; text : String;
       firstLine, visibleLines, cursor, selectionStart, selectionEnd : Positive;
       focused : Boolean; hot : Boolean);
   procedure Draw_Checkbox
      (c : Canvas; r : Rect; colors : Theme;
       checked : Boolean; hot : Boolean; active : Boolean);
   procedure Draw_Radio_Button
      (c : Canvas; r : Rect; colors : Theme;
       selected : Boolean; hot : Boolean; active : Boolean;
       label : String);
   procedure Draw_List_Item
      (c : Canvas; r : Rect; colors : Theme;
       selected : Boolean; hot : Boolean; label : String);
   procedure Draw_Menu_Item
      (c : Canvas; r : Rect; colors : Theme;
       hot : Boolean; active : Boolean; enabled : Boolean;
       label : String);
   procedure Draw_Horizontal_Slider
      (c : Canvas; r : Rect; colors : Theme;
       minValue, maxValue, value : Natural;
       hot : Boolean; active : Boolean);
   procedure Draw_Vertical_Scrollbar
      (c : Canvas; r : Rect; colors : Theme;
       minValue, maxValue, value : Natural;
       hot : Boolean; active : Boolean; pageSize : Positive := 1);

   function Button
      (bounds : Rect; pointer : Pointer_State) return Widget_Result;
end CuBit.UI;
