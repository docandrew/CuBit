------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Small immediate-mode UI drawing primitives for user surfaces
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System;

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
      danger  => 16#F28779#);

   type Button_Style is (Button_Normal, Button_Hot, Button_Pressed,
                         Button_Disabled, Button_Active);

   type Pointer_State is record
      x : Natural := 0;
      y : Natural := 0;
      down : Boolean := False;
      pressed : Boolean := False;
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

   procedure Set_Pixel (c : Canvas; x, y : Natural; fill : Color);
   procedure Fill_Rect (c : Canvas; r : Rect; fill : Color);
   procedure Stroke_Rect
      (c : Canvas; r : Rect; light : Color; dark : Color);
   procedure Draw_Glyph
      (c : Canvas; x, y : Natural; ch : Character; fg, bg : Color);
   procedure Draw_Text
      (c : Canvas; x, y : Natural; text : String; fg, bg : Color);
   procedure Draw_Button_Frame
      (c : Canvas; r : Rect; colors : Theme; style : Button_Style);

   function Button
      (bounds : Rect; pointer : Pointer_State) return Widget_Result;
end CuBit.UI;
