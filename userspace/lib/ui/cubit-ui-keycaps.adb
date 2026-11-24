------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
package body CuBit.UI.Keycaps is
   use type Keys.Key;

   LABEL_FACE : constant Text_Face := Emphasis_Face;
   LABEL_SIZE : constant Text_Size := Superscript_Size;
   --  The dashed border of an unbound cap: this many pixels on, as many off.
   DASH : constant := 2;

   function Cap_Label_Height return Natural is (Styled_Line_Height (LABEL_FACE, LABEL_SIZE));
   function Cap_Height return Natural is (BORDER + TOP_PADDING + Cap_Label_Height + BOTTOM_BORDER);
   function Cap_Width (Label : String) return Natural is
     (Natural'Max (Cap_Height - BOTTOM_BORDER,
                   2 * (BORDER + SIDE_PADDING) + Styled_Text_Width (Label, LABEL_FACE, LABEL_SIZE)));

   --  The arrow keys show an arrow, not a word: short enough that a
   --  chord's caps sit under one toolbar button.
   type Arrow is (No_Arrow, Arrow_Up, Arrow_Down, Arrow_Left, Arrow_Right);
   function Arrow_Of (Item : Keys.Key) return Arrow is
     (case Item is
         when Keys.Up => Arrow_Up, when Keys.Down => Arrow_Down, when Keys.Left => Arrow_Left,
         when Keys.Right => Arrow_Right, when others => No_Arrow);
   --  The arrow: a head HEAD pixels deep and a shaft, ARROW_LENGTH long.
   ARROW_LENGTH : constant := 9;
   HEAD : constant := 4;
   SHAFT : constant := 2;
   function Arrow_Width return Natural is (Natural'Max (Cap_Height - BOTTOM_BORDER, 2 * (BORDER + SIDE_PADDING) + ARROW_LENGTH));

   function Key_Width (Item : Keys.Key) return Natural is
     (if Arrow_Of (Item) /= No_Arrow then Arrow_Width else Cap_Width (Keys.Label (Item)));

   procedure Draw_Arrow (C : Canvas; Box : Rect; Direction : Arrow; Ink : Color) is
      CX : constant Natural := Box.x + Box.w / 2;
      CY : constant Natural := Box.y + Box.h / 2;
      X0 : constant Natural := (if CX > ARROW_LENGTH / 2 then CX - ARROW_LENGTH / 2 else 0);
      Y0 : constant Natural := (if CY > ARROW_LENGTH / 2 then CY - ARROW_LENGTH / 2 else 0);
   begin
      for I in 0 .. HEAD loop
         case Direction is
            when Arrow_Left =>
               Fill_Rect (C, (X0 + I, (if CY > I then CY - I else 0), 1, 2 * I + 1), Ink);
            when Arrow_Right =>
               Fill_Rect (C, (X0 + ARROW_LENGTH - 1 - I, (if CY > I then CY - I else 0), 1, 2 * I + 1), Ink);
            when Arrow_Up =>
               Fill_Rect (C, ((if CX > I then CX - I else 0), Y0 + I, 2 * I + 1, 1), Ink);
            when Arrow_Down =>
               Fill_Rect (C, ((if CX > I then CX - I else 0), Y0 + ARROW_LENGTH - 1 - I, 2 * I + 1, 1), Ink);
            when No_Arrow => null;
         end case;
      end loop;
      case Direction is
         when Arrow_Left => Fill_Rect (C, (X0 + HEAD, CY - SHAFT / 2, ARROW_LENGTH - HEAD, SHAFT), Ink);
         when Arrow_Right => Fill_Rect (C, (X0, CY - SHAFT / 2, ARROW_LENGTH - HEAD, SHAFT), Ink);
         when Arrow_Up => Fill_Rect (C, (CX - SHAFT / 2, Y0 + HEAD, SHAFT, ARROW_LENGTH - HEAD), Ink);
         when Arrow_Down => Fill_Rect (C, (CX - SHAFT / 2, Y0, SHAFT, ARROW_LENGTH - HEAD), Ink);
         when No_Arrow => null;
      end case;
   end Draw_Arrow;

   function Width (Item : Keys.Chord) return Natural is
      Total : Natural := 0;
   begin
      if not Keys.Is_Chord (Item) then
         return 0;
      end if;
      for M of Keys.DISPLAY_ORDER loop
         if Item.Modifiers (M) then
            Total := Total + Cap_Width (Keys.Label (M)) + CAP_GAP;
         end if;
      end loop;
      return Total + Key_Width (Item.Pressed);
   end Width;

   function Ink (Colors : Theme; Shade : Tone; Style : Look) return Color is
     (case Style is
         when On_Selection => Colors.selectionText,
         when Disabled | Unbound => Colors.muted,
         when Normal =>
           (case Shade is
               when Neutral => Colors.muted,
               when Accent => Colors.accent,
               when Danger => Colors.danger));

   procedure Draw_Cap
     (C : Canvas; X, Y : Natural; Label : String; Colors : Theme; Shade : Tone; Style : Look;
      Glyph : Arrow := No_Arrow)
   is
      W : constant Natural := (if Glyph /= No_Arrow then Arrow_Width else Cap_Width (Label));
      H : constant Natural := Cap_Height;
      Line : constant Color := Ink (Colors, Shade, Style);
      Text_W : constant Natural := Styled_Text_Width (Label, LABEL_FACE, LABEL_SIZE);
      Text_X : constant Natural := X + (if W > Text_W then (W - Text_W) / 2 else 0);
   begin
      --  The face: lifted off the surface (the field colour) unless on a
      --  selected row, where the selection shows through.
      if Style in Normal | Disabled then
         Fill_Rect (C, (X + 1, Y + 1, W - 2, H - 2), (if Style = Normal then Colors.field else Colors.face));
      end if;
      if Style = Unbound then
         --  Dashed: every other DASH pixels of the outline.
         declare
            Pos : Natural := 1;
         begin
            while Pos + 1 < W loop
               Fill_Rect (C, (X + Pos, Y, Natural'Min (DASH, W - 1 - Pos), BORDER), Line);
               Fill_Rect (C, (X + Pos, Y + H - BORDER, Natural'Min (DASH, W - 1 - Pos), BORDER), Line);
               Pos := Pos + 2 * DASH;
            end loop;
            Pos := 1;
            while Pos + 1 < H loop
               Fill_Rect (C, (X, Y + Pos, BORDER, Natural'Min (DASH, H - 1 - Pos)), Line);
               Fill_Rect (C, (X + W - BORDER, Y + Pos, BORDER, Natural'Min (DASH, H - 1 - Pos)), Line);
               Pos := Pos + 2 * DASH;
            end loop;
         end;
      else
         --  Rounded: the corner pixels stay off. The bottom edge is the
         --  key's thickness.
         Fill_Rect (C, (X + 1, Y, W - 2, BORDER), Line);
         Fill_Rect (C, (X + 1, Y + H - BOTTOM_BORDER, W - 2, BOTTOM_BORDER), Line);
         Fill_Rect (C, (X, Y + 1, BORDER, H - 1 - BOTTOM_BORDER), Line);
         Fill_Rect (C, (X + W - BORDER, Y + 1, BORDER, H - 1 - BOTTOM_BORDER), Line);
      end if;
      if Glyph /= No_Arrow then
         Draw_Arrow (C, (X, Y + BORDER, W, H - BORDER - BOTTOM_BORDER), Glyph,
                     (if Style = Normal and then Shade = Neutral then Colors.text else Line));
      else
         Draw_Styled_Text_Transparent
           (C, Text_X, Y + BORDER + TOP_PADDING, Label,
            (if Style = Normal and then Shade = Neutral then Colors.text else Line), LABEL_FACE, LABEL_SIZE);
      end if;
   end Draw_Cap;

   procedure Draw_Key
     (C : Canvas; X, Y : Natural; Item : Keys.Key; Colors : Theme;
      Shade : Tone := Neutral; Style : Look := Normal) is
   begin
      if Item /= Keys.Unknown then
         Draw_Cap (C, X, Y, Keys.Label (Item), Colors, Shade, Style, Arrow_Of (Item));
      end if;
   end Draw_Key;

   procedure Draw
     (C : Canvas; X, Y : Natural; Item : Keys.Chord; Colors : Theme;
      Shade : Tone := Neutral; Style : Look := Normal)
   is
      Pen : Natural := X;
   begin
      if not Keys.Is_Chord (Item) then
         return;
      end if;
      for M of Keys.DISPLAY_ORDER loop
         if Item.Modifiers (M) then
            Draw_Cap (C, Pen, Y, Keys.Label (M), Colors, Shade, Style);
            Pen := Pen + Cap_Width (Keys.Label (M)) + CAP_GAP;
         end if;
      end loop;
      Draw_Cap (C, Pen, Y, Keys.Label (Item.Pressed), Colors, Shade, Style, Arrow_Of (Item.Pressed));
   end Draw;
   function Label_Width (Caption : String; Item : Keys.Chord) return Natural is
     (2 * LABEL_PADDING + (if Caption'Length > 0 then UI_Text_Width (Caption) + LABEL_GAP else 0) + Width (Item));
   function Label_Height return Natural is (2 * LABEL_PADDING + Natural'Max (UI_Text_Height, Cap_Height));

   procedure Draw_Label
     (C : Canvas; X, Y : Natural; Caption : String; Item : Keys.Chord; Colors : Theme;
      Shade : Tone := Neutral)
   is
      Box : constant Rect := (X, Y, Label_Width (Caption, Item), Label_Height);
      Pen : Natural := X + LABEL_PADDING;
   begin
      Fill_Rect (C, Box, Colors.panel);
      Stroke_Rect (C, Box, Colors.edge, Colors.shadow);
      if Caption'Length > 0 then
         Draw_UI_Text_Transparent (C, Pen, Y + (Box.h - UI_Text_Height) / 2, Caption, Colors.text);
         Pen := Pen + UI_Text_Width (Caption) + LABEL_GAP;
      end if;
      Draw (C, Pen, Y + (Box.h - Cap_Height) / 2, Item, Colors, Shade);
   end Draw_Label;
   function Plain_Label_Width (Caption : String; Item : Keys.Chord) return Natural is
     ((if Caption'Length > 0 then UI_Text_Width (Caption) + LABEL_GAP else 0) + Width (Item));

   procedure Draw_Plain_Label
     (C : Canvas; X, Y : Natural; Caption : String; Item : Keys.Chord; Colors : Theme;
      Shade : Tone := Neutral)
   is
      Pen : Natural := X;
      H : constant Natural := Natural'Max (UI_Text_Height, Cap_Height);
   begin
      if Caption'Length > 0 then
         Draw_UI_Text_Transparent (C, Pen, Y + (H - UI_Text_Height) / 2, Caption, Colors.text);
         Pen := Pen + UI_Text_Width (Caption) + LABEL_GAP;
      end if;
      Draw (C, Pen, Y + (H - Cap_Height) / 2, Item, Colors, Shade);
   end Draw_Plain_Label;

   function Modifiers_Width (Set : Keys.Modifier_Set) return Natural is
      Total : Natural := 0;
   begin
      for M of Keys.DISPLAY_ORDER loop
         if Set (M) then
            Total := Total + (if Total > 0 then CAP_GAP else 0) + Cap_Width (Keys.Label (M));
         end if;
      end loop;
      return Total;
   end Modifiers_Width;

   procedure Draw_Modifiers (C : Canvas; X, Y : Natural; Set : Keys.Modifier_Set; Colors : Theme) is
      Pen : Natural := X;
   begin
      for M of Keys.DISPLAY_ORDER loop
         if Set (M) then
            Draw_Cap (C, Pen, Y, Keys.Label (M), Colors, Neutral, Normal);
            Pen := Pen + Cap_Width (Keys.Label (M)) + CAP_GAP;
         end if;
      end loop;
   end Draw_Modifiers;
end CuBit.UI.Keycaps;
