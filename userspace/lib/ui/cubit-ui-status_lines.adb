------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
with CuBit.UI.Keycaps;

package body CuBit.UI.Status_Lines is
   package Caps renames CuBit.UI.Keycaps;
   --  The status bar's text area inside its recessed frame.
   INSET_X : constant := 8;
   INSET_Y : constant := 3;
   RIGHT_GAP : constant := 16;

   procedure Clear (L : out Line) is
   begin
      L := (others => <>);
   end Clear;

   procedure Append (L : in out Line; Item : CuBit.UI.Keys.Chord; Text : String) is
   begin
      if L.Count < MAXIMUM_HINTS then
         L.Count := L.Count + 1;
         L.Hints (L.Count).Item := Item;
         L.Hints (L.Count).Length := Natural'Min (Text'Length, MAXIMUM_HINT_TEXT);
         L.Hints (L.Count).Text (1 .. L.Hints (L.Count).Length) :=
           Text (Text'First .. Text'First + L.Hints (L.Count).Length - 1);
      end if;
   end Append;

   procedure Add_Hint (L : in out Line; Item : CuBit.UI.Keys.Chord; Text : String) is
   begin
      --  With always-visible keycaps hidden, key hints leave the line.
      if CuBit.UI.Keys.Is_Chord (Caps.Persistent (Item)) then
         Append (L, Item, Text);
      end if;
   end Add_Hint;

   procedure Add_Text (L : in out Line; Text : String) is
   begin
      Append (L, CuBit.UI.Keys.No_Chord, Text);
   end Add_Text;

   function Count (L : Line) return Hint_Count is (L.Count);

   function Hint_Width (H : Hint) return Natural is
     ((if CuBit.UI.Keys.Is_Chord (H.Item) then Caps.Width (H.Item) + CAP_GAP else 0)
      + UI_Text_Width (H.Text (1 .. H.Length)));

   --  The right text as it shows, and the room left for the rest.
   procedure Split (R : Rect; Right : String; Shown_Right : out Natural; Room : out Natural; Right_W : out Natural)
   is
      Inner : constant Natural := (if R.w > 2 * INSET_X then R.w - 2 * INSET_X else 0);
   begin
      Right_W := UI_Text_Width (Fit_UI_Parts (Right, Inner / 2));
      Shown_Right := Right_W;
      Room := (if Inner > Right_W + (if Right_W > 0 then RIGHT_GAP else 0)
               then Inner - Right_W - (if Right_W > 0 then RIGHT_GAP else 0) else 0);
   end Split;

   function Shown (R : Rect; Lead : String; L : Line; Right : String) return Hint_Count is
      Shown_Right, Room, Right_W : Natural;
      Used : Natural;
      Result : Hint_Count := 0;
   begin
      Split (R, Right, Shown_Right, Room, Right_W);
      Used := UI_Text_Width (Fit_UI_Parts (Lead, Room));
      for K in 1 .. L.Count loop
         declare
            Need : constant Natural := (if Used > 0 then HINT_GAP else 0) + Hint_Width (L.Hints (K));
         begin
            exit when Used + Need > Room;
            Used := Used + Need;
            Result := K;
         end;
      end loop;
      return Result;
   end Shown;

   procedure Draw (C : Canvas; R : Rect; Colors : Theme; Lead : String; L : Line; Right : String) is
      Shown_Right, Room, Right_W : Natural;
      Text_Box : Rect;
      Clipped : Canvas;
      Lead_Text : String := Lead;
      Pen : Natural;
      Last : Hint_Count;
      Text_Y : constant Natural := R.y + (if R.h > UI_Text_Height then (R.h - UI_Text_Height) / 2 else 0);
      Cap_Y : constant Natural := R.y + (if R.h > Caps.Height then (R.h - Caps.Height) / 2 else 0);
   begin
      if Is_Empty (R) then
         return;
      end if;
      Text_Box := (R.x + INSET_X, R.y + INSET_Y, (if R.w > 2 * INSET_X then R.w - 2 * INSET_X else 0),
                   (if R.h > 2 * INSET_Y then R.h - 2 * INSET_Y else 0));
      Clipped := With_Clip (C, Text_Box);
      Split (R, Right, Shown_Right, Room, Right_W);
      --  The frame: the same recessed well as Draw_Status_Bar.
      Draw_Status_Bar (C, R, Colors, "", "");
      declare
         Lead_Shown : constant String := Fit_UI_Parts (Lead_Text, Room);
      begin
         Draw_UI_Text_Transparent (Clipped, Text_Box.x, Text_Y, Lead_Shown, Colors.text);
         Pen := Text_Box.x + UI_Text_Width (Lead_Shown);
      end;
      Last := Shown (R, Lead, L, Right);
      for K in 1 .. Last loop
         declare
            H : Hint renames L.Hints (K);
         begin
            if Pen > Text_Box.x then
               Pen := Pen + HINT_GAP;
            end if;
            --  "other pane [Tab]": the one order everywhere, text then cap.
            Draw_UI_Text_Transparent (Clipped, Pen, Text_Y, H.Text (1 .. H.Length), Colors.text);
            Pen := Pen + UI_Text_Width (H.Text (1 .. H.Length));
            if CuBit.UI.Keys.Is_Chord (H.Item) then
               Pen := Pen + CAP_GAP;
               Caps.Draw (Clipped, Pen, Cap_Y, H.Item, Colors);
               Pen := Pen + Caps.Width (H.Item);
            end if;
         end;
      end loop;
      if Right_W > 0 then
         Draw_UI_Text_Transparent
           (Clipped, Text_Box.x + Text_Box.w - Right_W, Text_Y, Fit_UI_Parts (Right, Text_Box.w / 2), Colors.muted);
      end if;
   end Draw;
end CuBit.UI.Status_Lines;
