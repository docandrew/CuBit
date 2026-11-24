------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  The keycap hint's drawing over any surface (CuBit.UI.Keycaps): the
--  toolkit's canvas, or the Desktop's own output (its hold-Super hints),
--  so both draw the one cap. The caller supplies the fill and the text.
------------------------------------------------------------------------------
with CuBit.UI.Keys;
with CuBit.UI.Keycaps;

generic
   type Surface (<>) is private;
   with procedure Fill (S : Surface; R : Rect; Ink : Color);
   --  The caps' labels (a short bold face) and the captions beside them.
   with function Label_Text_Width (Text : String) return Natural;
   with function Label_Text_Height return Natural;
   with procedure Draw_Label_Text (S : Surface; X, Y : Natural; Text : String; Ink : Color);
   with function Caption_Width (Text : String) return Natural;
   with function Caption_Height return Natural;
   with procedure Draw_Caption (S : Surface; X, Y : Natural; Text : String; Ink : Color);
package CuBit.UI.Keycap_Renderer is
   package Keys renames CuBit.UI.Keys;
   package KC renames CuBit.UI.Keycaps;

   function Cap_Height return Natural;
   function Cap_Width (Label : String) return Natural;
   function Key_Width (Item : Keys.Key) return Natural;
   function Width (Item : Keys.Chord) return Natural;
   procedure Draw
     (S : Surface; X, Y : Natural; Item : Keys.Chord; Colors : Theme;
      Shade : KC.Tone := KC.Neutral; Style : KC.Look := KC.Normal);
   procedure Draw_Key
     (S : Surface; X, Y : Natural; Item : Keys.Key; Colors : Theme;
      Shade : KC.Tone := KC.Neutral; Style : KC.Look := KC.Normal);
   function Label_Width (Caption : String; Item : Keys.Chord) return Natural;
   function Label_Height return Natural;
   procedure Draw_Label
     (S : Surface; X, Y : Natural; Caption : String; Item : Keys.Chord; Colors : Theme;
      Shade : KC.Tone := KC.Neutral);
   function Plain_Label_Width (Caption : String; Item : Keys.Chord) return Natural;
   procedure Draw_Plain_Label
     (S : Surface; X, Y : Natural; Caption : String; Item : Keys.Chord; Colors : Theme;
      Shade : KC.Tone := KC.Neutral);
   function Modifiers_Width (Set : Keys.Modifier_Set) return Natural;
   procedure Draw_Modifiers (S : Surface; X, Y : Natural; Set : Keys.Modifier_Set; Colors : Theme);
end CuBit.UI.Keycap_Renderer;
