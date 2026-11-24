------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
with CuBit.UI.Keycap_Renderer;

package body CuBit.UI.Keycaps is
   --  The caps' labels: the emphasis face at the small size.
   LABEL_FACE : constant Text_Face := Emphasis_Face;
   LABEL_SIZE : constant Text_Size := Superscript_Size;

   procedure Fill (C : Canvas; R : Rect; Ink : Color) is
   begin
      Fill_Rect (C, R, Ink);
   end Fill;
   function Label_Text_Width (Text : String) return Natural is (Styled_Text_Width (Text, LABEL_FACE, LABEL_SIZE));
   function Label_Text_Height return Natural is (Styled_Line_Height (LABEL_FACE, LABEL_SIZE));
   procedure Draw_Label_Text (C : Canvas; X, Y : Natural; Text : String; Ink : Color) is
   begin
      Draw_Styled_Text_Transparent (C, X, Y, Text, Ink, LABEL_FACE, LABEL_SIZE);
   end Draw_Label_Text;
   function Caption_Width (Text : String) return Natural is (UI_Text_Width (Text));
   function Caption_Height return Natural is (UI_Text_Height);
   procedure Draw_Caption (C : Canvas; X, Y : Natural; Text : String; Ink : Color) is
   begin
      Draw_UI_Text_Transparent (C, X, Y, Text, Ink);
   end Draw_Caption;

   package R is new CuBit.UI.Keycap_Renderer
     (Canvas, Fill, Label_Text_Width, Label_Text_Height, Draw_Label_Text, Caption_Width, Caption_Height,
      Draw_Caption);

   function Cap_Height return Natural renames R.Cap_Height;
   function Cap_Width (Label : String) return Natural renames R.Cap_Width;
   function Width (Item : Keys.Chord) return Natural renames R.Width;
   function Key_Width (Item : Keys.Key) return Natural renames R.Key_Width;

   procedure Draw
     (C : Canvas; X, Y : Natural; Item : Keys.Chord; Colors : Theme;
      Shade : Tone := Neutral; Style : Look := Normal) is
   begin
      R.Draw (C, X, Y, Item, Colors, Shade, Style);
   end Draw;
   procedure Draw_Key
     (C : Canvas; X, Y : Natural; Item : Keys.Key; Colors : Theme;
      Shade : Tone := Neutral; Style : Look := Normal) is
   begin
      R.Draw_Key (C, X, Y, Item, Colors, Shade, Style);
   end Draw_Key;

   function Label_Width (Caption : String; Item : Keys.Chord) return Natural renames R.Label_Width;
   function Label_Height return Natural renames R.Label_Height;
   procedure Draw_Label
     (C : Canvas; X, Y : Natural; Caption : String; Item : Keys.Chord; Colors : Theme;
      Shade : Tone := Neutral) is
   begin
      R.Draw_Label (C, X, Y, Caption, Item, Colors, Shade);
   end Draw_Label;
   function Plain_Label_Width (Caption : String; Item : Keys.Chord) return Natural renames R.Plain_Label_Width;
   procedure Draw_Plain_Label
     (C : Canvas; X, Y : Natural; Caption : String; Item : Keys.Chord; Colors : Theme;
      Shade : Tone := Neutral) is
   begin
      R.Draw_Plain_Label (C, X, Y, Caption, Item, Colors, Shade);
   end Draw_Plain_Label;
   function Modifiers_Width (Set : Keys.Modifier_Set) return Natural renames R.Modifiers_Width;
   procedure Draw_Modifiers (C : Canvas; X, Y : Natural; Set : Keys.Modifier_Set; Colors : Theme) is
   begin
      R.Draw_Modifiers (C, X, Y, Set, Colors);
   end Draw_Modifiers;

   function Ink (Colors : Theme; Shade : Tone; Style : Look) return Color is
     (case Style is
         when On_Selection => Colors.selectionText,
         when Disabled | Unbound => Colors.muted,
         when Normal =>
           (case Shade is
               when Neutral => Colors.muted,
               when Accent => Colors.accent,
               when Danger => Colors.danger));
end CuBit.UI.Keycaps;
