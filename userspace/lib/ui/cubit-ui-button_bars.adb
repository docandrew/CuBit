------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
with CuBit.UI.Widgets;

package body CuBit.UI.Button_Bars is
   use type Keys.Modifier_Set, Keys.Key;
   use type Command;
   package Caps renames CuBit.UI.Keycaps;

   --  The least gap between two reveal labels.
   LABEL_GAP : constant := CuBit.UI_Metrics.Space_XS;

   procedure Clear
     (B : out Bar; First_ID : Controls.Control_ID; Style : Bar_Style := Key_Bar_Style;
      Align : Alignment := Align_Left) is
   begin
      B := (First_ID => First_ID, Style => Style, Align => Align, others => <>);
   end Clear;

   procedure Append (B : in out Bar; Item : Button) is
   begin
      --  One place stays for a key bar's layer indicator.
      if B.Count < MAXIMUM_BUTTONS - 1 then
         B.Count := B.Count + 1;
         B.Buttons (B.Count) := Item;
      end if;
   end Append;

   procedure Add_Command
     (B : in out Bar; Item : Command; Look : Face := Caption_With_Keycap; Starts_Cluster : Boolean := False) is
   begin
      Append (B, (From => Command_Source, Item => Item, Key => Keys.Unknown, Look => Look,
                  Starts_Cluster => Starts_Cluster));
   end Add_Command;

   procedure Add_Key (B : in out Bar; Item : Keys.Key; Starts_Cluster : Boolean := False) is
   begin
      Append (B, (From => Key_Source, Item => Command'First, Key => Item, Look => Caption_With_Keycap,
                  Starts_Cluster => Starts_Cluster));
   end Add_Key;

   function Count (B : Bar) return Button_Count is (B.Count);
   function Button_ID (B : Bar; K : Button_Index) return Controls.Control_ID is (B.First_ID + K - 1);

   procedure Command_Of
     (B : Bar; Map : Commands.Keymap; K : Button_Index; Layer : Keys.Modifier_Set;
      Found : out Boolean; Item : out Command) is
   begin
      if K > B.Count then
         Found := False;
         Item := Command'First;
      elsif B.Buttons (K).From = Command_Source then
         Found := True;
         Item := B.Buttons (K).Item;
      else
         Commands.Find (Map, (Modifiers => Layer, Pressed => B.Buttons (K).Key), Found, Item);
      end if;
   end Command_Of;

   function Layer_Shown (B : Bar; Map : Commands.Keymap; Revealed : Keys.Modifier_Set) return Keys.Modifier_Set is
   begin
      if Keys.Is_Empty (Revealed) then
         return Keys.No_Modifiers;
      end if;
      for K in 1 .. B.Count loop
         if B.Buttons (K).From = Key_Source
           and then Commands.Is_Bound (Map, (Modifiers => Revealed, Pressed => B.Buttons (K).Key))
         then
            return Revealed;
         end if;
      end loop;
      return Keys.No_Modifiers;
   end Layer_Shown;

   function Shown_Chord (B : Bar; Map : Commands.Keymap; K : Button_Index; Revealed : Keys.Modifier_Set)
     return Keys.Chord is
   begin
      if K > B.Count then
         return Keys.No_Chord;
      elsif B.Buttons (K).From = Key_Source then
         return (Modifiers => Layer_Shown (B, Map, Revealed), Pressed => B.Buttons (K).Key);
      else
         return Commands.Hint (Map, B.Buttons (K).Item);
      end if;
   end Shown_Chord;

   --  The button runs something now: a command (bound, for a key) that is
   --  enabled.
   function Live (B : Bar; Map : Commands.Keymap; K : Button_Index; Layer : Keys.Modifier_Set) return Boolean is
      Found : Boolean;
      Item : Command;
   begin
      Command_Of (B, Map, K, Layer, Found, Item);
      return Found and then Enabled (Item);
   end Live;

   function Shown_Caption (B : Bar; Map : Commands.Keymap; K : Button_Index; Revealed : Keys.Modifier_Set)
     return String
   is
      Found : Boolean;
      Item : Command;
   begin
      Command_Of (B, Map, K, Layer_Shown (B, Map, Revealed), Found, Item);
      return (if Found then Caption (Item) else "");
   end Shown_Caption;

   function Shows_Keycap (B : Bar; Map : Commands.Keymap; K : Button_Index; Revealed : Keys.Modifier_Set)
     return Boolean is
     (K <= B.Count and then B.Buttons (K).Look = Caption_With_Keycap
      and then Keys.Is_Chord (Caps.Persistent (Shown_Chord (B, Map, K, Revealed))));

   --  A caption button's width for a caption and a cap of Cap pixels.
   function Caption_Width (Text : String; Cap : Natural) return Natural is
     (2 * BUTTON_INSET + Cap
      + (if Text'Length > 0 then (if Cap > 0 then CAPTION_GAP else 0) + UI_Text_Width (Text) else 0));

   --  A key's cap in its cell, unless always-visible keycaps are hidden.
   function Key_Cap_Width (Item : Keys.Key) return Natural is
     (if Caps.Persistent_Shown then Caps.Key_Width (Item) else 0);

   function Needed_Width (B : Bar; Map : Commands.Keymap; K : Button_Index; Revealed : Keys.Modifier_Set)
     return Natural
   is
   begin
      if K > B.Count then
         return 0;
      elsif B.Buttons (K).Look = Icon_Only then
         return ICON_BUTTON_SIZE;
      elsif B.Buttons (K).From = Key_Source then
         --  A key's cell shows the key's cap alone (the layer shows once,
         --  at the start of the bar) and keeps its plain width where the
         --  layer's caption fits in it.
         return Natural'Max
           (Caption_Width (Shown_Caption (B, Map, K, Keys.No_Modifiers), Key_Cap_Width (B.Buttons (K).Key)),
            Caption_Width (Shown_Caption (B, Map, K, Revealed), Key_Cap_Width (B.Buttons (K).Key)));
      else
         return Caption_Width (Shown_Caption (B, Map, K, Revealed),
                               Caps.Width (Caps.Persistent (Shown_Chord (B, Map, K, Revealed))));
      end if;
   end Needed_Width;

   --  The layer indicator's width ([Ctrl] at the start), 0 for none.
   function Indicator_Width (B : Bar; Map : Commands.Keymap; Revealed : Keys.Modifier_Set) return Natural is
     (if Keys.Is_Empty (Layer_Shown (B, Map, Revealed)) then 0
      else Caps.Modifiers_Width (Layer_Shown (B, Map, Revealed)) + 2 * BUTTON_INSET);

   function Row_Height (B : Bar) return Natural is
     ((if (for some K in 1 .. B.Count => B.Buttons (K).Look = Icon_Only) then ICON_BUTTON_SIZE
       else CAPTION_BUTTON_HEIGHT));

   --  Where the buttons go: the layout's first cell is the layer
   --  indicator when one shows (Offset 1), then the buttons.
   procedure Arrange
     (B : Bar; Map : Commands.Keymap; Width : Natural; Revealed : Keys.Modifier_Set;
      Widths : out Layout.Width_Table; Places : out Layout.Place_Table; Rows : out Layout.Row_Number;
      Offset : out Natural)
   is
      Starts : Layout.Start_Table := [others => False];
      Inner : constant Natural := (if Width > 2 * BAR_PADDING then Width - 2 * BAR_PADDING else 0);
      Indicator : constant Natural := Indicator_Width (B, Map, Revealed);
   begin
      Widths := [others => 0];
      Offset := (if Indicator > 0 then 1 else 0);
      if Offset = 1 then
         Widths (1) := Natural'Min (Layout.MAXIMUM_COORDINATE, Indicator);
      end if;
      for K in 1 .. B.Count loop
         Widths (K + Offset) := Natural'Min (Layout.MAXIMUM_COORDINATE, Needed_Width (B, Map, K, Revealed));
         Starts (K + Offset) := B.Buttons (K).Starts_Cluster;
      end loop;
      Layout.Arrange (Widths, Starts, B.Count + Offset, Natural'Min (Inner, Layout.MAXIMUM_COORDINATE),
                      BUTTON_GAP, CLUSTER_GAP, Places, Rows);
   end Arrange;

   function Height (B : Bar; Map : Commands.Keymap; Width : Natural; Revealed : Keys.Modifier_Set) return Natural is
      Widths : Layout.Width_Table;
      Places : Layout.Place_Table;
      Rows : Layout.Row_Number;
      Offset : Natural;
   begin
      if B.Count = 0 then
         return 0;
      end if;
      Arrange (B, Map, Width, Revealed, Widths, Places, Rows, Offset);
      return 2 * BAR_PADDING + Rows * Row_Height (B) + (Rows - 1) * ROW_GAP;
   end Height;

   function Flashing (B : Bar; K : Button_Index; Now_Ms : Unsigned_64) return Boolean is
     (B.Flash_Button = K and then Now_Ms < B.Flash_Until);

   procedure Draw
     (C : Canvas; Map : in out Controls.Control_Map; Tips : in out Tooltips.Tip_Table; B : in out Bar;
      Keymap : Commands.Keymap; Area : Rect; Colors : Theme; Pointer : Pointer_State;
      Revealed : Keys.Modifier_Set; Now_Ms : Unsigned_64)
   is
      Inner : constant Canvas := With_Clip (C, Area);
      Layer : constant Keys.Modifier_Set := Layer_Shown (B, Keymap, Revealed);
      Height_Of_Row : constant Natural := Row_Height (B);
      Widths : Layout.Width_Table;
      Places : Layout.Place_Table;
      Rows : Layout.Row_Number;
      Offset : Natural;
      Avail : constant Natural := (if Area.w > 2 * BAR_PADDING then Area.w - 2 * BAR_PADDING else 0);
      function Row_End (Row : Natural) return Natural is
         Last : Natural := 0;
      begin
         for K in 1 .. B.Count + Offset loop
            if Places (K).Row = Row then
               Last := Natural'Max (Last, Places (K).X + Widths (K));
            end if;
         end loop;
         return Last;
      end Row_End;
   begin
      B.Layer := Layer;
      B.Area := Area;
      B.Areas := [others => (others => 0)];
      case B.Style is
         when Key_Bar_Style =>
            Fill_Rect (C, Area, Colors.panel);
            Fill_Rect (C, (Area.x, Area.y, Area.w, (if Area.h > 0 then 1 else 0)), Colors.edge);
         when Toolbar_Style =>
            CuBit.UI.Widgets.Toolbar (C, Area, Colors);
         when Plain_Style =>
            null;
      end case;
      Arrange (B, Keymap, Area.w, Revealed, Widths, Places, Rows, Offset);
      B.Row_Count := Rows;
      if Offset = 1 then
         --  The layer, once: [Ctrl] before the keys it changes.
         declare
            Row_Shift : constant Natural :=
              (if B.Align = Align_Right and then Avail > Row_End (0) then Avail - Row_End (0) else 0);
         begin
            Caps.Draw_Modifiers
              (Inner, Area.x + BAR_PADDING + Row_Shift + BUTTON_INSET,
               Area.y + BAR_PADDING + (if Height_Of_Row > Caps.Height then (Height_Of_Row - Caps.Height) / 2 else 0),
               Layer, Colors);
         end;
      end if;
      for K in 1 .. B.Count loop
         declare
            Row : constant Natural := Places (K + Offset).Row;
            Shift : constant Natural :=
              (if B.Align = Align_Right and then Avail > Row_End (Row) then Avail - Row_End (Row) else 0);
            Button_H : constant Natural :=
              (if B.Buttons (K).Look = Icon_Only then ICON_BUTTON_SIZE else CAPTION_BUTTON_HEIGHT);
            Box : constant Rect :=
              (Area.x + BAR_PADDING + Shift + Places (K + Offset).X,
               Area.y + BAR_PADDING + Row * (Height_Of_Row + ROW_GAP) + (Height_Of_Row - Button_H) / 2,
               Widths (K + Offset), Button_H);
            Found : Boolean;
            Item : Command;
            Chord : constant Keys.Chord := Shown_Chord (B, Keymap, K, Revealed);
            Is_Live : constant Boolean := Live (B, Keymap, K, Layer);
            Hot : constant Boolean := Is_Live and then Pointer.enabled and then Point_In_Rect (Pointer.x, Pointer.y, Box);
            Pressed : constant Boolean :=
              Is_Live and then (Controls.Is_Active (Map, Button_ID (B, K)) or else Flashing (B, K, Now_Ms));
         begin
            B.Areas (K) := Box;
            Command_Of (B, Keymap, K, Layer, Found, Item);
            if Is_Live then
               Controls.Add_Button (Map, Button_ID (B, K), Box, Box);
               Controls.Set_Shortcut (Map, Button_ID (B, K), Chord);
               Tooltips.Set (Tips, Button_ID (B, K), Description (Item), Chord, Shade (Item));
            end if;
            if B.Style = Toolbar_Style and then K > 1 and then B.Buttons (K).Starts_Cluster
              and then Places (K - 1 + Offset).Row = Row and then Box.x >= CLUSTER_GAP
            then
               CuBit.UI.Widgets.Toolbar_Separator
                 (Inner, (Box.x - CLUSTER_GAP, Box.y, CLUSTER_GAP, Box.h), Colors);
            end if;
            if B.Buttons (K).Look = Icon_Only then
               CuBit.UI.Icons.Draw_Tool_Button
                 (Inner, Box, Colors, Picture (Item), Is_Live, Hot, Pressed, Found and then Checked (Item));
            else
               declare
                  Text : constant String := (if Found then Caption (Item) else "");
                  Key_Only : constant Boolean := B.Buttons (K).From = Key_Source;
                  Cap : constant Natural :=
                    (if Key_Only then Key_Cap_Width (B.Buttons (K).Key) else Caps.Width (Caps.Persistent (Chord)));
                  Cap_Y : constant Natural := Box.y + (if Box.h > Caps.Height then (Box.h - Caps.Height) / 2 else 0);
                  Text_Y : constant Natural := Box.y + (if Box.h > UI_Text_Height then (Box.h - UI_Text_Height) / 2 else 0);
                  Text_X, Cap_X : Natural;
                  On : constant Boolean := Is_Live and then Checked (Item);
                  Look : constant Caps.Look :=
                    (if not Is_Live then Caps.Unbound elsif On then Caps.On_Selection else Caps.Normal);
               begin
                  Draw_Button_Frame
                    (Inner, Box, Colors,
                     (if not Is_Live then Button_Disabled elsif Pressed then Button_Pressed
                      elsif On then Button_Active elsif Hot then Button_Hot else Button_Normal));
                  --  "Copy [F5]": the caption, then the keycap at the right
                  --  end (a cell wider than its caption keeps the caps
                  --  lined up at its right edge).
                  Text_X := Box.x + BUTTON_INSET;
                  Cap_X := (if Box.w > BUTTON_INSET + Cap then Box.x + Box.w - BUTTON_INSET - Cap else Box.x);
                  if Cap = 0 then
                     null;
                  elsif Key_Only then
                     Caps.Draw_Key (Inner, Cap_X, Cap_Y, B.Buttons (K).Key, Colors,
                                    (if Found then Shade (Item) else Caps.Neutral), Look);
                  elsif Cap > 0 then
                     Caps.Draw (Inner, Cap_X, Cap_Y, Chord, Colors, (if Found then Shade (Item) else Caps.Neutral),
                                Look);
                  end if;
                  if Text'Length > 0 then
                     Draw_UI_Text_Transparent
                       (Inner, Text_X, Text_Y, Text,
                        (if not Is_Live then Colors.muted elsif On then Colors.selectionText else Colors.text));
                  end if;
               end;
            end if;
         end;
      end loop;
   end Draw;

   function Revealed_At (B : Bar; Keymap : Commands.Keymap; K : Button_Index; Revealed : Keys.Modifier_Set)
     return Boolean
   is
   begin
      if K > B.Count or else B.Buttons (K).Look /= Icon_Only or else Keys.Is_Empty (Revealed) then
         return False;
      end if;
      declare
         Chord : constant Keys.Chord := Commands.Hint (Keymap, B.Buttons (K).Item);
      begin
         --  One model for every modifier: the buttons whose chord is
         --  exactly what is held (Alt: Back and Forward; Ctrl: Places ...).
         return Keys.Is_Chord (Chord) and then Chord.Modifiers = Revealed;
      end;
   end Revealed_At;

   --  Reveal labels sit over the bar's own row, each centred on its
   --  button: never over the content below, never moving the layout (the
   --  bar's icons are covered only while the modifier is held). A label is
   --  "Places [Ctrl][B]"; when neighbours would collide, every label goes
   --  compact, the key's cap alone ([B]: the held modifier is implied),
   --  and a label that still collides is nudged right past its neighbour.
   type Label_Form is (Full_Label, Compact_Label);

   function Label_Chord (Item : Keys.Chord; Revealed : Keys.Modifier_Set; Form : Label_Form) return Keys.Chord is
     (if Form = Full_Label then Item else (Modifiers => Keys.No_Modifiers, Pressed => Item.Pressed));
   function Label_Caption (B : Bar; K : Button_Index; Form : Label_Form) return String is
     (if Form = Full_Label then Caption (B.Buttons (K).Item) else "");

   procedure Place_Labels
     (B : Bar; Keymap : Commands.Keymap; Revealed : Keys.Modifier_Set; Window : Rect;
      Boxes : out Rect_Table; Form : out Label_Form)
   is
      Height : constant Natural := Caps.Label_Height;
      function Width_Of (K : Button_Index; F : Label_Form) return Natural is
        (Caps.Label_Width (Label_Caption (B, K, F),
                           Label_Chord (Commands.Hint (Keymap, B.Buttons (K).Item), Revealed, F)));
      function Left_Of (K : Button_Index; W : Natural) return Natural is
        (if B.Areas (K).x + B.Areas (K).w / 2 > W / 2 then B.Areas (K).x + B.Areas (K).w / 2 - W / 2 else 0);
      --  Whether the labels of form F fit centred, none overlapping.
      function Fits (F : Label_Form) return Boolean is
         Pen : Natural := 0;
         First : Boolean := True;
      begin
         for K in 1 .. B.Count loop
            if Revealed_At (B, Keymap, K, Revealed) and then not Is_Empty (B.Areas (K)) then
               if not First and then Left_Of (K, Width_Of (K, F)) < Pen + LABEL_GAP then
                  return False;
               end if;
               First := False;
               Pen := Left_Of (K, Width_Of (K, F)) + Width_Of (K, F);
            end if;
         end loop;
         return True;
      end Fits;
      Pen : Natural := 0;
      First : Boolean := True;
      Right_Edge : constant Natural := Natural'Min (Window.x + Window.w, B.Area.x + B.Area.w);
   begin
      Boxes := [others => (others => 0)];
      Form := (if Fits (Full_Label) then Full_Label else Compact_Label);
      for K in 1 .. B.Count loop
         if Revealed_At (B, Keymap, K, Revealed) and then not Is_Empty (B.Areas (K)) then
            declare
               W : constant Natural := Width_Of (K, Form);
               X : Natural := Natural'Max (Left_Of (K, W), B.Area.x);
               Box : constant Rect := B.Areas (K);
            begin
               if not First and then X < Pen + LABEL_GAP then
                  X := Pen + LABEL_GAP;
               end if;
               if X + W > Right_Edge then
                  X := (if Right_Edge > W then Right_Edge - W else 0);
               end if;
               Boxes (K) := (X, Box.y + (if Box.h > Height then (Box.h - Height) / 2 else 0), W, Height);
               Pen := X + W;
               First := False;
            end;
         end if;
      end loop;
   end Place_Labels;

   procedure Draw_Reveal
     (C : Canvas; B : Bar; Keymap : Commands.Keymap; Colors : Theme; Revealed : Keys.Modifier_Set;
      Window : Rect)
   is
      Boxes : Rect_Table;
      Form : Label_Form;
   begin
      Place_Labels (B, Keymap, Revealed, Window, Boxes, Form);
      for K in 1 .. B.Count loop
         if not Is_Empty (Boxes (K)) then
            Caps.Draw_Label
              (C, Boxes (K).x, Boxes (K).y, Label_Caption (B, K, Form),
               Label_Chord (Commands.Hint (Keymap, B.Buttons (K).Item), Revealed, Form), Colors,
               Shade (B.Buttons (K).Item));
         end if;
      end loop;
   end Draw_Reveal;

   function Reveal_Area (B : Bar; Keymap : Commands.Keymap; Revealed : Keys.Modifier_Set; Window : Rect)
     return Rect
   is
      Boxes : Rect_Table;
      Form : Label_Form;
      Result : Rect := (others => 0);
   begin
      Place_Labels (B, Keymap, Revealed, Window, Boxes, Form);
      for K in 1 .. B.Count loop
         if not Is_Empty (Boxes (K)) then
            Result := (if Is_Empty (Result) then Boxes (K) else Union_Rect (Result, Boxes (K)));
         end if;
      end loop;
      return Result;
   end Reveal_Area;

   function Reveal_Label (B : Bar; Keymap : Commands.Keymap; K : Button_Index; Revealed : Keys.Modifier_Set;
                          Window : Rect) return Rect
   is
      Boxes : Rect_Table;
      Form : Label_Form;
   begin
      Place_Labels (B, Keymap, Revealed, Window, Boxes, Form);
      return Boxes (K);
   end Reveal_Label;

   procedure Take_Chosen
     (B : Bar; Map : in out Controls.Control_Map; Keymap : Commands.Keymap; Found : out Boolean;
      Item : out Command)
   is
   begin
      Found := False;
      Item := Command'First;
      for K in 1 .. B.Count loop
         if Controls.Take_Activated (Map, Button_ID (B, K)) and then not Found then
            Command_Of (B, Keymap, K, B.Layer, Found, Item);
         end if;
      end loop;
   end Take_Chosen;

   procedure Flash (B : in out Bar; Keymap : Commands.Keymap; Pressed : Keys.Chord; Now_Ms : Unsigned_64;
                    Damage : out Rect)
   is
      Found : Boolean;
      Item, Target : Command;
      Bound : Boolean;
   begin
      Damage := (if B.Flash_Button > 0 then B.Areas (B.Flash_Button) else (others => 0));
      Commands.Find (Keymap, Pressed, Bound, Target);
      for K in 1 .. B.Count loop
         if B.Buttons (K).From = Key_Source then
            Found := B.Buttons (K).Key = Pressed.Pressed and then Bound;
         else
            Command_Of (B, Keymap, K, Pressed.Modifiers, Found, Item);
            Found := Found and then Bound and then Item = Target;
         end if;
         if Found then
            B.Flash_Button := K;
            B.Flash_Until := (if Now_Ms > NEVER - FLASH_MS then NEVER else Now_Ms + FLASH_MS);
            Damage := (if Is_Empty (Damage) then B.Areas (K) else Union_Rect (Damage, B.Areas (K)));
            return;
         end if;
      end loop;
   end Flash;

   procedure Tick (B : in out Bar; Now_Ms : Unsigned_64; Damage : out Rect) is
   begin
      Damage := (others => 0);
      if B.Flash_Button > 0 and then Now_Ms >= B.Flash_Until then
         Damage := B.Areas (B.Flash_Button);
         B.Flash_Button := 0;
      end if;
   end Tick;

   function Next_Deadline (B : Bar) return Unsigned_64 is
     (if B.Flash_Button > 0 then B.Flash_Until else NEVER);

   function Button_Area (B : Bar; K : Button_Index) return Rect is (B.Areas (K));
   function Area_Of (B : Bar) return Rect is (B.Area);
   function Rows (B : Bar) return Natural is (B.Row_Count);
end CuBit.UI.Button_Bars;
