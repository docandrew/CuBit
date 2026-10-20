with Ada.Unchecked_Deallocation;
with System;
with Client_Canvas_Geometry;
with CuBit.Fonts;
with CuBit.UI.Controls;
with CuBit.UI.Editor;
with CuBit.UI.Surfaces;
with Calc_History;
with Calc_Numbers;
with Calc_Syntax;

package body Calc_Shells is
   use CuBit.UI;
   package Menus renames CuBit.UI.Menus;
   package Controls renames CuBit.UI.Controls;
   package Editor renames CuBit.UI.Editor;
   use type Menus.Text;
   use type Calc_Engine.Failure_Kind;
   use type Calc_Format.Decimal_Form;
   use type Calc_Numbers.Exactness;
   use type System.Address;

   ---------------------------------------------------------------------------
   --  Geometry (logical pixels).

   MENU_BAR_HEIGHT : constant := 24;
   MENU_ROW_HEIGHT : constant := 24;
   MENU_MINIMUM_WIDTH : constant := 220;
   MENU_BASE : constant := 1;
   STATUS_HEIGHT : constant := 24;
   MARGIN : constant := 12;
   GAP : constant := 6;
   PAD_ROWS : constant := 5;
   PAD_COLUMNS : constant := 5;
   BUTTON_HEIGHT : constant := 44;
   PAD_HEIGHT : constant := PAD_ROWS * BUTTON_HEIGHT + (PAD_ROWS - 1) * GAP + 2 * MARGIN;
   ENTRY_HEIGHT : constant := 30;
   ENTRY_INSET : constant := 8;
   --  The big result: the UI face at twice its size.
   BIG_NUMERATOR : constant := 2;
   BIG_DENOMINATOR : constant := 1;
   BIG_HEIGHT : constant := 40;
   --  Digits on the pad: one and a half times the UI face.
   KEY_NUMERATOR : constant := 3;
   KEY_DENOMINATOR : constant := 2;
   SECONDARY_HEIGHT : constant := 20;
   DISPLAY_HEIGHT : constant := MARGIN + ENTRY_HEIGHT + GAP + BIG_HEIGHT + SECONDARY_HEIGHT + GAP;
   ROW_HEIGHT : constant := 46;
   ROW_INSET : constant := 10;
   DOUBLE_CLICK_MS : constant := 400;
   HELP_WIDTH : constant := 380;
   HELP_LINE : constant := 20;
   --  Symbol strokes: arm length and thickness, per button height.
   SYMBOL_ARM : constant := 6;
   STROKE : constant := 2;
   SUBTLE_SHARE : constant := 10;   --  percent of the app colour in a tint
   TINT_SHARE : constant := 22;
   MAXIMUM_STATUS : constant := 160;

   --  The Calculator's identity colour (system.ccl's launcher entry): teal.
   APP_COLOR : constant Color := 16#12A58F#;
   WHITE : constant Color := 16#FFFFFF#;

   ---------------------------------------------------------------------------
   --  Commands and menus.

   type Command is
     (No_Command, Clear_History_Command, Quit_Command,
      Copy_Result_Command, Copy_Decimal_Command, Copy_Expression_Command, Paste_Command, Select_All_Command,
      Clear_Entry_Command, Fraction_First_Command, Places_10_Command, Places_20_Command, Places_50_Command,
      Places_100_Command, Help_Command);
   type Menu_Name is (Calculator_Menu, Edit_Menu, View_Menu, Help_Menu);

   function Menu_Of (Item : Command) return Menu_Name is
     (case Item is
         when Clear_History_Command | Quit_Command | No_Command => Calculator_Menu,
         when Copy_Result_Command .. Clear_Entry_Command => Edit_Menu,
         when Fraction_First_Command .. Places_100_Command => View_Menu,
         when Help_Command => Help_Menu);
   function Caption (Item : Command) return String is
     (case Item is
         when No_Command => "",
         when Clear_History_Command => "Clear History",
         when Quit_Command => "Quit",
         when Copy_Result_Command => "Copy Result",
         when Copy_Decimal_Command => "Copy as Decimal",
         when Copy_Expression_Command => "Copy Expression",
         when Paste_Command => "Paste",
         when Select_All_Command => "Select All",
         when Clear_Entry_Command => "Clear Entry",
         when Fraction_First_Command => "Fractions First",
         when Places_10_Command => "10 Decimal Places",
         when Places_20_Command => "20 Decimal Places",
         when Places_50_Command => "50 Decimal Places",
         when Places_100_Command => "100 Decimal Places",
         when Help_Command => "Keys and Functions");
   function Shortcut (Item : Command) return String is
     (case Item is
         when Clear_History_Command => "Ctrl+L",
         when Quit_Command => "Ctrl+Q",
         when Copy_Result_Command => "Ctrl+C",
         when Copy_Decimal_Command => "Ctrl+Shift+C",
         when Paste_Command => "Ctrl+V",
         when Select_All_Command => "Ctrl+A",
         when Clear_Entry_Command => "Esc",
         when Fraction_First_Command => "Ctrl+D",
         when Help_Command => "F1",
         when others => "");
   function Starts_Group (Item : Command) return Boolean is
     (Item in Quit_Command | Paste_Command | Clear_Entry_Command | Places_10_Command);
   function Title (Item : Menu_Name) return String is
     (case Item is
         when Calculator_Menu => "Calculator", when Edit_Menu => "Edit", when View_Menu => "View",
         when Help_Menu => "Help");
   function Places_Of (Item : Command) return Calc_Format.Places_Count is
     (case Item is
         when Places_10_Command => 10, when Places_20_Command => 20, when Places_50_Command => 50,
         when others => 100);

   type Command_Texts is array (Command) of Menus.Text;
   type Title_Texts is array (Menu_Name) of Menus.Text;
   Captions, Shortcuts : Command_Texts := [others => null];
   Titles : Title_Texts := [others => null];
   Menu_Popup_Width : Positive := MENU_MINIMUM_WIDTH;

   function Lower (C : Character) return Character is
     (if C in 'A' .. 'Z' then Character'Val (Character'Pos (C) - Character'Pos ('A') + Character'Pos ('a')) else C);

   procedure Prepare_Menu_Texts is
   begin
      if Titles (Menu_Name'First) /= null then
         return;
      end if;
      for M in Menu_Name loop
         Titles (M) := new String'(Title (M));
      end loop;
      for C in Command range Clear_History_Command .. Command'Last loop
         Captions (C) := new String'(Caption (C));
         Shortcuts (C) := (if Shortcut (C)'Length = 0 then null else new String'(Shortcut (C)));
      end loop;
   end Prepare_Menu_Texts;

   ---------------------------------------------------------------------------
   --  The pad.

   type Pad_Key is
     (Clear_Pad, Open_Pad, Close_Pad, Power_Pad, Divide_Pad,
      Seven_Pad, Eight_Pad, Nine_Pad, Times_Pad, Root_Pad,
      Four_Pad, Five_Pad, Six_Pad, Minus_Pad, Factorial_Pad,
      One_Pad, Two_Pad, Three_Pad, Plus_Pad, Pi_Pad,
      Zero_Pad, Point_Pad, Erase_Pad, Ans_Pad, Equals_Pad);
   subtype Digit_Pad is Pad_Key with Static_Predicate =>
     Digit_Pad in Seven_Pad .. Nine_Pad | Four_Pad .. Six_Pad | One_Pad .. Three_Pad | Zero_Pad | Point_Pad;
   subtype Operator_Pad is Pad_Key with Static_Predicate =>
     Operator_Pad in Divide_Pad | Times_Pad | Minus_Pad | Plus_Pad | Power_Pad;
   type Symbol is (Text_Symbol, Divide_Symbol, Times_Symbol, Minus_Symbol, Plus_Symbol, Root_Symbol,
                   Pi_Symbol, Erase_Symbol, Equals_Symbol);

   function Label (K : Pad_Key) return String is
     (case K is
         when Clear_Pad => "C", when Open_Pad => "(", when Close_Pad => ")", when Power_Pad => "^",
         when Divide_Pad => "/", when Seven_Pad => "7", when Eight_Pad => "8", when Nine_Pad => "9",
         when Times_Pad => "*", when Root_Pad => "sqrt", when Four_Pad => "4", when Five_Pad => "5",
         when Six_Pad => "6", when Minus_Pad => "-", when Factorial_Pad => "!", when One_Pad => "1",
         when Two_Pad => "2", when Three_Pad => "3", when Plus_Pad => "+", when Pi_Pad => "pi",
         when Zero_Pad => "0", when Point_Pad => ".", when Erase_Pad => "del", when Ans_Pad => "ans",
         when Equals_Pad => "=");
   --  What a press types (the clear, erase and equals keys act instead).
   function Inserts (K : Pad_Key) return String is
     (case K is
         when Root_Pad => "sqrt(", when Clear_Pad | Erase_Pad | Equals_Pad => "",
         when others => Label (K));
   function Shape (K : Pad_Key) return Symbol is
     (case K is
         when Divide_Pad => Divide_Symbol, when Times_Pad => Times_Symbol, when Minus_Pad => Minus_Symbol,
         when Plus_Pad => Plus_Symbol, when Root_Pad => Root_Symbol, when Pi_Pad => Pi_Symbol,
         when Erase_Pad => Erase_Symbol, when Equals_Pad => Equals_Symbol, when others => Text_Symbol);

   ---------------------------------------------------------------------------
   --  State.

   type Text_Access is access String;
   procedure Release is new Ada.Unchecked_Deallocation (String, Text_Access);
   procedure Set (Into : in out Text_Access; Value : String);
   --  Display text is cut to this many characters before fitting: a result
   --  can be far longer than a secondary stack, and no row shows more.
   SHOWN_CHARACTERS : constant := 256;
   function Head (Text : String) return String is
     (Text (Text'First .. Text'First + Natural'Min (Text'Length, SHOWN_CHARACTERS) - 1));

   type Shell_State is record
      Host : access Host_Type;
      Config : Settings;
      Field : Editor.Edit_State;
      History : Calc_History.History;
      Places : Calc_Format.Places_Count := 20;
      Fraction_First : Boolean := True;
      --  The live result of the entry, both texts (Has_Preview False: none).
      Preview_Fraction, Preview_Decimal : Text_Access;
      Preview_Form : Calc_Format.Decimal_Form := Calc_Format.Integer_Form;
      Has_Preview, Preview_Approximate, Preview_Integer : Boolean := False;
      Failure : Calc_Engine.Failure_Kind := Calc_Engine.No_Failure;
      Failure_Column : Natural := 0;
      --  Up/Down recall: 0 is the entry being typed (Draft holds it).
      Recall : Natural := 0;
      Draft : Text_Access;
      Selected : Natural := 0;
      Scroll : Natural := 0;
      Entry_Scroll : Natural := 0;
      Bounds : Rect := (others => 0);
      Menu_Area, History_Area, Display_Area, Entry_Area, Big_Area, Secondary_Area, Pad_Area,
        Status_Area : Rect := (others => 0);
      Hot_Key, Pressed_Key : Pad_Key := Clear_Pad;
      Hot, Pressed : Boolean := False;
      Map : Controls.Control_Map;
      Menu_Model : Menus.Model;
      Menu : Menus.Menu_State;
      Menu_Block_Release : Boolean := False;
      Popup_Area : Rect := (others => 0);
      Status : String (1 .. MAXIMUM_STATUS) := [others => ' '];
      Status_Length : Natural := 0;
      --  The status names a failure (it goes once the entry changes).
      Status_Failure : Boolean := False;
      Help_On : Boolean := False;
      Quit : Boolean := False;
      Dirty : Rect := (others => 0);
      Last_Click_Ms : Unsigned_64 := 0;
      Last_Click_Row : Natural := 0;
      Paste_Pending : Boolean := False;
      Paste_Buffer : Text_Access;
      --  The theme last painted: a scheme switch (Settings) repaints all.
      Theme_Seen : Theme;
   end record;

   procedure Set (Into : in out Text_Access; Value : String) is
   begin
      Release (Into);
      Into := new String'(Value);
   end Set;


   procedure Damage (S : in out Shell_State; Area : Rect) is
   begin
      if not Is_Empty (Area) then
         S.Dirty := (if Is_Empty (S.Dirty) then Area else Union_Rect (S.Dirty, Area));
      end if;
   end Damage;

   procedure Damage_All (S : in out Shell_State) is
   begin
      Damage (S, S.Bounds);
   end Damage_All;

   procedure Say (S : in out Shell_State; Message : String; Failure : Boolean := False) is
      Length : constant Natural := Natural'Min (Message'Length, MAXIMUM_STATUS);
   begin
      S.Status (1 .. Length) := Message (Message'First .. Message'First + Length - 1);
      S.Status_Length := Length;
      S.Status_Failure := Failure;
      Damage (S, S.Status_Area);
   end Say;

   READY_MESSAGE : constant String := "Type an expression; Enter evaluates it exactly";

   function Image (Value : Natural) return String is
      Text : constant String := Natural'Image (Value);
   begin
      return Text (Text'First + 1 .. Text'Last);
   end Image;

   ---------------------------------------------------------------------------
   --  Colours.

   function Channel (C : Color; Shift : Natural) return Natural is
     (Natural (Shift_Right (C, Shift) and 16#FF#));
   --  A + (B - A) * Share / 100, per channel.
   function Mix (A, B : Color; Share : Natural) return Color is
      function Part (Shift : Natural) return Color is
        (Color ((Channel (A, Shift) * (100 - Share) + Channel (B, Shift) * Share) / 100));
   begin
      return Shift_Left (Part (16), 16) or Shift_Left (Part (8), 8) or Part (0);
   end Mix;
   function Luma (C : Color) return Natural is
     (2126 * Channel (C, 16) + 7152 * Channel (C, 8) + 722 * Channel (C, 0));
   function Dark (Colors : Theme) return Boolean is (Luma (Colors.field) < Luma (Colors.text));

   ---------------------------------------------------------------------------
   --  Layout.

   procedure Layout (S : in out Shell_State) is
      B : constant Rect := S.Bounds;
      Bottom : constant Natural := B.y + B.h;
      Status_Top : constant Natural := (if B.h > STATUS_HEIGHT then Bottom - STATUS_HEIGHT else B.y);
      Pad_Top : constant Natural := (if Status_Top > B.y + PAD_HEIGHT then Status_Top - PAD_HEIGHT else B.y);
      Display_Top : constant Natural :=
        (if Pad_Top > B.y + MENU_BAR_HEIGHT + DISPLAY_HEIGHT then Pad_Top - DISPLAY_HEIGHT
         else B.y + MENU_BAR_HEIGHT);
      Inner : constant Natural := (if B.w > 2 * MARGIN then B.w - 2 * MARGIN else 0);
   begin
      S.Menu_Area := (B.x, B.y, B.w, MENU_BAR_HEIGHT);
      S.Status_Area := (B.x, Status_Top, B.w, Bottom - Status_Top);
      S.Pad_Area := (B.x, Pad_Top, B.w, Status_Top - Pad_Top);
      S.Display_Area := (B.x, Display_Top, B.w, Pad_Top - Display_Top);
      S.History_Area := (B.x, B.y + MENU_BAR_HEIGHT, B.w,
                         (if Display_Top > B.y + MENU_BAR_HEIGHT then Display_Top - B.y - MENU_BAR_HEIGHT else 0));
      S.Entry_Area := (B.x + MARGIN, Display_Top + MARGIN, Inner, ENTRY_HEIGHT);
      S.Big_Area := (B.x + MARGIN, S.Entry_Area.y + ENTRY_HEIGHT + GAP, Inner, BIG_HEIGHT);
      S.Secondary_Area := (B.x + MARGIN, S.Big_Area.y + BIG_HEIGHT, Inner, SECONDARY_HEIGHT);
   end Layout;

   function Key_Area (S : Shell_State; K : Pad_Key) return Rect is
      Row : constant Natural := Pad_Key'Pos (K) / PAD_COLUMNS;
      Column : constant Natural := Pad_Key'Pos (K) mod PAD_COLUMNS;
      Inner : constant Natural := (if S.Pad_Area.w > 2 * MARGIN + (PAD_COLUMNS - 1) * GAP
                                   then S.Pad_Area.w - 2 * MARGIN - (PAD_COLUMNS - 1) * GAP else 0);
      Width : constant Natural := Inner / PAD_COLUMNS;
      --  The last column takes what division leaves, so the pad is flush.
      Extra : constant Natural := (if Column = PAD_COLUMNS - 1 then Inner - Width * PAD_COLUMNS else 0);
   begin
      return (S.Pad_Area.x + MARGIN + Column * (Width + GAP), S.Pad_Area.y + MARGIN + Row * (BUTTON_HEIGHT + GAP),
              Width + Extra, BUTTON_HEIGHT);
   end Key_Area;

   function Visible_Rows (S : Shell_State) return Natural is (S.History_Area.h / ROW_HEIGHT);

   function Maximum_Scroll (S : Shell_State) return Natural is
     (if Calc_History.Count (S.History) > Visible_Rows (S)
      then Calc_History.Count (S.History) - Visible_Rows (S) else 0);

   --  The history row at Y (1 = newest), 0 for none. Rows stack upward
   --  from the display: the newest sits just above the entry.
   function Row_At (S : Shell_State; Y : Natural) return Natural is
      Bottom : constant Natural := S.History_Area.y + S.History_Area.h;
   begin
      if Y < S.History_Area.y or else Y >= Bottom then
         return 0;
      end if;
      declare
         From_Bottom : constant Natural := (Bottom - 1 - Y) / ROW_HEIGHT;
         Index : constant Natural := From_Bottom + 1 + S.Scroll;
      begin
         return (if Index <= Calc_History.Count (S.History) then Index else 0);
      end;
   end Row_At;

   function Row_Area (S : Shell_State; Index : Positive) return Rect is
      Bottom : constant Integer := S.History_Area.y + S.History_Area.h;
      Top : constant Integer := Bottom - (Index - S.Scroll) * ROW_HEIGHT;
   begin
      if Index <= S.Scroll or else Top < S.History_Area.y then
         return (others => 0);
      end if;
      return (S.History_Area.x, Top, S.History_Area.w, ROW_HEIGHT);
   end Row_Area;

   ---------------------------------------------------------------------------
   --  Results and the preview.

   --  The two texts a result shows: the primary (big, or right in a
   --  history row) and the other form, each cut for display.
   function Primary_Of
     (S : Shell_State; Fraction, Decimal : String; Form : Calc_Format.Decimal_Form;
      Approximate, Is_Integer : Boolean) return String
   is
      Rounded : constant Boolean := Form not in Calc_Format.Exact_Form;
      Cut : constant Boolean := Decimal'Length > SHOWN_CHARACTERS;
   begin
      if Approximate or else Is_Integer or else not S.Fraction_First then
         return Head (Decimal) & (if Rounded or else Cut then "..." else "");
      end if;
      return Head (Fraction) & (if Fraction'Length > SHOWN_CHARACTERS then "..." else "");
   end Primary_Of;

   function Secondary_Of
     (S : Shell_State; Fraction, Decimal : String; Form : Calc_Format.Decimal_Form;
      Approximate, Is_Integer : Boolean) return String
   is
      Rounded : constant Boolean := Form not in Calc_Format.Exact_Form;
   begin
      if Approximate then
         return "approximate, within 1e-" & Image (S.Config.Result_Budget.Precision);
      elsif Is_Integer then
         return "exact integer" & (if Decimal'Length > SHOWN_CHARACTERS / 8
                                   then ", " & Image (Decimal'Length - (if Decimal (Decimal'First) = '-' then 1 else 0))
                                        & " digits"
                                   else "");
      elsif S.Fraction_First then
         return Head (Decimal) & (if Rounded or else Decimal'Length > SHOWN_CHARACTERS then "..." else "");
      end if;
      return Head (Fraction) & (if Fraction'Length > SHOWN_CHARACTERS then "..." else "");
   end Secondary_Of;

   function Preview_Primary (S : Shell_State) return String is
     (if S.Has_Preview and then S.Preview_Fraction /= null and then S.Preview_Decimal /= null
      then Primary_Of (S, S.Preview_Fraction.all, S.Preview_Decimal.all, S.Preview_Form, S.Preview_Approximate,
                       S.Preview_Integer)
      elsif S.Has_Preview then "0" else "");
   function Preview_Secondary (S : Shell_State) return String is
     (if S.Has_Preview and then S.Preview_Fraction /= null and then S.Preview_Decimal /= null
      then Secondary_Of (S, S.Preview_Fraction.all, S.Preview_Decimal.all, S.Preview_Form, S.Preview_Approximate,
                         S.Preview_Integer)
      else "");

   procedure Show_Number (S : in out Shell_State; X : Calc_Numbers.Number) is
      Fraction : String (1 .. Calc_Format.Fraction_Bound (X));
      Decimal : String (1 .. Calc_Format.Decimal_Bound (X, S.Places));
      Fraction_Last, Decimal_Last : Natural;
   begin
      Calc_Format.Fraction_Text (X, Fraction, Fraction_Last);
      Calc_Format.Decimal_Text (X, S.Places, Decimal, Decimal_Last, S.Preview_Form);
      Set (S.Preview_Fraction, Fraction (1 .. Fraction_Last));
      Set (S.Preview_Decimal, Decimal (1 .. Decimal_Last));
      S.Preview_Approximate := Calc_Numbers.Kind (X) = Calc_Numbers.Approximate;
      S.Preview_Integer := Calc_Numbers.Is_Integer (X);
      S.Has_Preview := True;
   end Show_Number;

   --  The display shows the entry's value as it is typed; for an empty
   --  entry, the latest result.
   procedure Refresh_Preview (S : in out Shell_State) is
      Text : constant String := Editor.Content (S.Field);
      Result : Calc_Numbers.Number;
      Position : Calc_Syntax.Column;
   begin
      S.Failure := Calc_Engine.No_Failure;
      S.Failure_Column := 0;
      S.Has_Preview := False;
      if (for all C of Text => C = ' ') then
         if Calc_History.Count (S.History) > 0 then
            Set (S.Preview_Fraction, Calc_History.Fraction (S.History, 1).all);
            Set (S.Preview_Decimal, Calc_History.Decimal (S.History, 1).all);
            S.Preview_Form := Calc_History.Form (S.History, 1);
            S.Preview_Approximate := Calc_History.Kind (S.History, 1) = Calc_Numbers.Approximate;
            S.Preview_Integer := Calc_History.Is_Integer (S.History, 1);
            S.Has_Preview := True;
         else
            Release (S.Preview_Fraction);
            Release (S.Preview_Decimal);
            S.Preview_Approximate := False;
            S.Has_Preview := True;  --  shows 0
         end if;
      else
         Calc_History.Evaluate (S.History, Text, S.Config.Preview_Budget, Result, S.Failure, Position);
         if S.Failure = Calc_Engine.No_Failure then
            Show_Number (S, Result);
            Calc_Numbers.Free (Result);
         else
            S.Failure_Column := Position;
         end if;
      end if;
      Damage (S, S.Display_Area);
   end Refresh_Preview;

   --  An incomplete entry (still being typed) is not an error to shout:
   --  the display waits quietly.
   function Incomplete (S : Shell_State) return Boolean is
     (S.Failure in Calc_Engine.Expected_Operand | Calc_Engine.Unclosed_Parenthesis | Calc_Engine.Empty_Expression);

   procedure Edited (S : in out Shell_State) is
   begin
      S.Recall := 0;
      if S.Status_Failure then
         Say (S, READY_MESSAGE);
      end if;
      Refresh_Preview (S);
   end Edited;

   procedure Replace_Entry (S : in out Shell_State; Text : String) is
      Accepted : Boolean;
   begin
      Editor.Initialize (S.Field, Text (Text'First .. Text'First + Natural'Min (Text'Length, Editor.MAX_TEXT_LENGTH) - 1),
                         Accepted);
      Editor.Move (S.Field, Editor.Move_End);
      S.Entry_Scroll := 0;
      if S.Status_Failure then
         Say (S, READY_MESSAGE);
      end if;
      Refresh_Preview (S);
   end Replace_Entry;

   procedure Insert (S : in out Shell_State; Text : String) is
      Changed : Boolean;
   begin
      Editor.Insert (S.Field, Text, Changed);
      if Changed then
         Edited (S);
      end if;
   end Insert;

   procedure Evaluate_Entry (S : in out Shell_State) is
      Text : constant String := Editor.Content (S.Field);
      Based : constant String (1 .. Text'Length) := Text;
      Terms : Calc_Syntax.Term_Array (1 .. Text'Length);
      Count : Natural;
      Error : Calc_Syntax.Syntax_Error;
      Position : Calc_Syntax.Column;
      Previous, Result : Calc_Numbers.Number;
      Failure : Calc_Engine.Failure_Kind;
      use type Calc_Syntax.Syntax_Error;
   begin
      if (for all C of Text => C = ' ') then
         return;
      end if;
      Calc_Syntax.Parse (Based, Terms, Count, Error, Position);
      if Error /= Calc_Syntax.No_Error then
         S.Failure := Calc_Engine.Failure_Of (Error);
         S.Failure_Column := Position;
         Say (S, Calc_Engine.Message (S.Failure) & " (column " & Image (Position) & ")", Failure => True);
         Damage (S, S.Display_Area);
         return;
      end if;
      if Calc_History.Has_Previous (S.History) then
         Calc_History.Copy_Latest (S.History, Previous);
      end if;
      Calc_Engine.Evaluate_Terms
        (Based, Terms (1 .. Count), Previous, Calc_History.Has_Previous (S.History), S.Config.Result_Budget,
         Result, Failure, Position);
      Calc_Numbers.Free (Previous);
      if Failure /= Calc_Engine.No_Failure then
         S.Failure := Failure;
         S.Failure_Column := Position;
         Say (S, Calc_Engine.Message (Failure) & " (column " & Image (Position) & ")", Failure => True);
         Damage (S, S.Display_Area);
         return;
      end if;
      Calc_History.Add (S.History, Based, Terms (1 .. Count), Result, S.Places);
      S.Scroll := 0;
      S.Selected := 0;
      Say (S, (if Calc_History.Kind (S.History, 1) = Calc_Numbers.Approximate then "Approximate: "
               else "Exact: ") & Based);
      Replace_Entry (S, "");
      Damage (S, S.History_Area);
   end Evaluate_Entry;

   --  Up (Older) and Down through earlier expressions; past the newest the
   --  entry being typed comes back.
   procedure Recall (S : in out Shell_State; Older : Boolean) is
      Count : constant Natural := Calc_History.Count (S.History);
   begin
      if Older and then S.Recall < Count then
         if S.Recall = 0 then
            Set (S.Draft, Editor.Content (S.Field));
         end if;
         S.Recall := S.Recall + 1;
      elsif not Older and then S.Recall > 0 then
         S.Recall := S.Recall - 1;
      else
         return;
      end if;
      declare
         Kept : constant Natural := S.Recall;
      begin
         if Kept = 0 then
            Replace_Entry (S, (if S.Draft = null then "" else S.Draft.all));
         else
            Replace_Entry (S, Calc_History.Source (S.History, Kept).all);
         end if;
         S.Recall := Kept;
         S.Selected := Kept;
         if Kept > 0 then
            --  Keep the recalled row in view.
            if Kept <= S.Scroll then
               S.Scroll := Kept - 1;
            elsif Kept > S.Scroll + Visible_Rows (S) then
               S.Scroll := Kept - Visible_Rows (S);
            end if;
         end if;
         Damage (S, S.History_Area);
      end;
   end Recall;

   --  The text Ctrl+C copies: the selection in the entry; else the selected
   --  row's (or the latest) result, exact (Decimal: as the decimal shown).
   procedure Copy_Result (S : in out Shell_State; Decimal : Boolean) is
      Selection : constant String := Editor.Selected_Text (S.Field);
      Index : constant Natural := (if S.Selected > 0 then S.Selected else 1);
   begin
      if Selection'Length > 0 and then not Decimal then
         Copy (S.Host.all, Selection);
         Say (S, "Copied the selection");
      elsif Calc_History.Count (S.History) >= Index then
         declare
            Text : constant Calc_History.Text_View :=
              (if Decimal then Calc_History.Decimal (S.History, Index) else Calc_History.Fraction (S.History, Index));
            Shown : constant Natural := Natural'Min (Text'Length, 40);
         begin
            Copy (S.Host.all, Text.all);
            Say (S, "Copied " & Text (Text'First .. Text'First + Shown - 1)
                    & (if Text'Length > Shown then "... (" & Image (Text'Length) & " characters)" else ""));
         end;
      else
         Say (S, "Nothing to copy yet");
      end if;
   end Copy_Result;

   procedure Set_Places (S : in out Shell_State; Places : Calc_Format.Places_Count) is
   begin
      S.Places := Places;
      Calc_History.Reformat (S.History, Places);
      Refresh_Preview (S);
      Say (S, Image (Places) & " decimal places");
      Damage_All (S);
   end Set_Places;

   procedure Run_Command (S : in out Shell_State; Item : Command) is
   begin
      case Item is
         when No_Command => null;
         when Clear_History_Command =>
            Calc_History.Clear (S.History);
            S.Scroll := 0;
            S.Selected := 0;
            S.Recall := 0;
            Refresh_Preview (S);
            Say (S, "History cleared");
            Damage_All (S);
         when Quit_Command => S.Quit := True;
         when Copy_Result_Command => Copy_Result (S, Decimal => False);
         when Copy_Decimal_Command => Copy_Result (S, Decimal => True);
         when Copy_Expression_Command =>
            if Calc_History.Count (S.History) >= Natural'Max (S.Selected, 1) then
               Copy (S.Host.all, Calc_History.Source (S.History, Natural'Max (S.Selected, 1)).all);
               Say (S, "Copied the expression");
            end if;
         when Paste_Command =>
            Request_Paste (S.Host.all);
            S.Paste_Pending := True;
         when Select_All_Command =>
            Editor.Select_All (S.Field);
            Damage (S, S.Display_Area);
         when Clear_Entry_Command =>
            Replace_Entry (S, "");
            S.Recall := 0;
         when Fraction_First_Command =>
            S.Fraction_First := not S.Fraction_First;
            Refresh_Preview (S);
            Say (S, (if S.Fraction_First then "Fractions first" else "Decimals first"));
            Damage_All (S);
         when Places_10_Command .. Places_100_Command => Set_Places (S, Places_Of (Item));
         when Help_Command =>
            S.Help_On := not S.Help_On;
            Damage_All (S);
      end case;
   end Run_Command;

   procedure Press_Key (S : in out Shell_State; K : Pad_Key) is
      Changed : Boolean;
   begin
      case K is
         when Clear_Pad => Run_Command (S, Clear_Entry_Command);
         when Erase_Pad =>
            Editor.Backspace (S.Field, Changed);
            if Changed then
               Edited (S);
            end if;
         when Equals_Pad => Evaluate_Entry (S);
         when others => Insert (S, Inserts (K));
      end case;
   end Press_Key;

   ---------------------------------------------------------------------------
   --  Menus.

   function Build_Menus (S : Shell_State) return Menus.Model is
      Result : Menus.Model;
      Taken : array (Character range 'a' .. 'z') of Boolean := [others => False];
      function Mnemonic_Of (Text : String) return Character is
      begin
         for C of Text loop
            if Lower (C) in Taken'Range and then not Taken (Lower (C)) then
               Taken (Lower (C)) := True;
               return Lower (C);
            end if;
         end loop;
         return ' ';
      end Mnemonic_Of;
   begin
      for M in Menu_Name loop
         Taken := [others => False];
         Result.Menu_Count := Result.Menu_Count + 1;
         Result.Menus (Result.Menu_Count) := (Caption => Titles (M), Mnemonic => Lower (Title (M) (1)));
         for C in Command range Clear_History_Command .. Command'Last loop
            if Menu_Of (C) = M and then Result.Item_Count + 2 <= Menus.Max_Items then
               if Starts_Group (C) then
                  Result.Item_Count := Result.Item_Count + 1;
                  Result.Items (Result.Item_Count) := (Parent => Result.Menu_Count, Separator => True, others => <>);
               end if;
               Result.Item_Count := Result.Item_Count + 1;
               Result.Items (Result.Item_Count) :=
                 (Parent => Result.Menu_Count, Caption => Captions (C), Shortcut => Shortcuts (C),
                  Mnemonic => Mnemonic_Of (Caption (C)), Command => Command'Pos (C),
                  Enabled => (case C is
                                 when Copy_Result_Command | Copy_Decimal_Command | Copy_Expression_Command
                                    | Clear_History_Command => Calc_History.Count (S.History) > 0,
                                 when others => True),
                  Checked => (case C is
                                 when Fraction_First_Command => S.Fraction_First,
                                 when Places_10_Command .. Places_100_Command => S.Places = Places_Of (C),
                                 when Help_Command => S.Help_On,
                                 when others => False),
                  Separator => False);
            end if;
         end loop;
      end loop;
      return Result;
   end Build_Menus;

   function Popup_Of (S : Shell_State) return Rect is
     (Menus.Popup_Bounds (S.Menu, S.Menu_Model, S.Menu_Area, S.Bounds, Menu_Popup_Width, MENU_ROW_HEIGHT));

   procedure Menu_Changed (S : in out Shell_State) is
      Popup : constant Rect := Popup_Of (S);
   begin
      Damage (S, S.Menu_Area);
      Damage (S, S.Popup_Area);
      Damage (S, Popup);
      S.Popup_Area := Popup;
   end Menu_Changed;

   procedure Menu_Key (S : in out Shell_State; Item : Event; Handled : out Boolean) is
      Was_Open : constant Boolean := Menus.Is_Open (S.Menu);
      Event_Key : Menus.Key := Menus.Mnemonic;
      Letter : Character := ' ';
      Chosen : Natural := 0;
   begin
      Handled := False;
      if Was_Open then
         case Item.Pressed is
            when F10 => Event_Key := Menus.Activate;
            when Left => Event_Key := Menus.Left;
            when Right => Event_Key := Menus.Right;
            when Up => Event_Key := Menus.Up;
            when Down => Event_Key := Menus.Down;
            when Home => Event_Key := Menus.Home;
            when End_Key => Event_Key := Menus.End_Key;
            when Enter => Event_Key := Menus.Enter;
            when Escape => Event_Key := Menus.Escape;
            when Tab => Event_Key := Menus.Tab_Key;
            when others =>
               if Item.Typed in 'a' .. 'z' then
                  Letter := Item.Typed;
               else
                  Handled := True;  --  nothing else reaches the window while a menu is open
                  return;
               end if;
         end case;
      elsif Item.Pressed = F10 then
         Event_Key := Menus.Activate;
      elsif Item.Alt and then Item.Typed in 'a' .. 'z' then
         Letter := Item.Typed;
      else
         return;
      end if;
      S.Menu_Model := Build_Menus (S);
      Menus.Handle_Key (S.Menu, S.Menu_Model, Event_Key, Chosen, Handled, Letter);
      if Handled then
         Menu_Changed (S);
      end if;
      if Chosen /= 0 then
         Run_Command (S, Command'Val (Chosen));
      end if;
   end Menu_Key;

   procedure Menu_Pointer (S : in out Shell_State; Item : Event; Handled : out Boolean) is
      Target : constant Controls.Control_ID := Controls.Hit (S.Map, Item.X, Item.Y);
      Was_Open : constant Boolean := Menus.Is_Open (S.Menu);
      Ours : constant Boolean := Menus.Is_Menu_Control (MENU_BASE, Target);
      Action : constant Controls.Pointer_Action :=
        (case Item.Action is
            when Press => Controls.Pointer_Press,
            when Release => Controls.Pointer_Release,
            when Move => Controls.Pointer_Move);
      Chosen : Natural := 0;
      Changed, Dispatched : Boolean;
      Old_Hot : constant Natural := Menus.Hot_Title (S.Menu);
   begin
      Handled := False;
      if not Was_Open and then not Ours and then not S.Menu_Block_Release
        and then (Item.Action /= Move or else Menus.Hot_Title (S.Menu) = 0)
      then
         return;
      end if;
      if Item.Action = Release and then S.Menu_Block_Release then
         S.Menu_Block_Release := False;
         Handled := True;
         return;
      end if;
      S.Menu_Model := Build_Menus (S);
      if Item.Action = Press and then Was_Open and then not Ours then
         S.Menu_Block_Release := True;
      else
         Controls.Dispatch_Pointer (S.Map, Target, Action, Item.X, Item.Y, Changed, Dispatched);
      end if;
      Menus.Handle_Pointer (S.Menu, S.Menu_Model, S.Map, MENU_BASE, Target, Action, Chosen, Handled);
      Handled := Handled or else Was_Open or else (Ours and then Item.Action /= Move);
      if Was_Open /= Menus.Is_Open (S.Menu) or else Old_Hot /= Menus.Hot_Title (S.Menu) or else Handled then
         Menu_Changed (S);
      end if;
      if Chosen /= 0 then
         Run_Command (S, Command'Val (Chosen));
      end if;
   end Menu_Pointer;

   ---------------------------------------------------------------------------
   --  Input.

   procedure Handle_Key (S : in out Shell_State; Item : Event) is
      Changed : Boolean;
      Menu_Handled : Boolean;
   begin
      Menu_Key (S, Item, Menu_Handled);
      if Menu_Handled then
         return;
      end if;
      if S.Help_On and then Item.Pressed in Escape | F1 then
         S.Help_On := False;
         Damage_All (S);
         return;
      end if;
      if Item.Control then
         case Item.Typed is
            when 'a' => Run_Command (S, Select_All_Command);
            when 'c' => Run_Command (S, (if Item.Shift then Copy_Decimal_Command else Copy_Result_Command));
            when 'x' =>
               declare
                  Selection : constant String := Editor.Selected_Text (S.Field);
               begin
                  if Selection'Length > 0 then
                     Copy (S.Host.all, Selection);
                     Editor.Backspace (S.Field, Changed);
                     Edited (S);
                  end if;
               end;
            when 'v' => Run_Command (S, Paste_Command);
            when 'l' => Run_Command (S, Clear_History_Command);
            when 'd' => Run_Command (S, Fraction_First_Command);
            when 'q' => Run_Command (S, Quit_Command);
            when others => null;
         end case;
         if Item.Pressed in Left | Right then
            Editor.Move (S.Field, (if Item.Pressed = Left then Editor.Move_Word_Left else Editor.Move_Word_Right),
                         Item.Shift);
            Damage (S, S.Display_Area);
         end if;
         return;
      end if;
      case Item.Pressed is
         when Enter => Evaluate_Entry (S);
         when Escape =>
            if Editor.Length (S.Field) > 0 then
               Run_Command (S, Clear_Entry_Command);
            elsif S.Selected > 0 then
               S.Selected := 0;
               Damage (S, S.History_Area);
            end if;
         when Backspace =>
            Editor.Backspace (S.Field, Changed);
            if Changed then
               Edited (S);
            end if;
         when Delete =>
            Editor.Delete_Forward (S.Field, Changed);
            if Changed then
               Edited (S);
            end if;
         when Left | Right | Home | End_Key =>
            Editor.Move (S.Field,
                         (case Item.Pressed is
                             when Left => Editor.Move_Left, when Right => Editor.Move_Right,
                             when Home => Editor.Move_Start, when others => Editor.Move_End),
                         Item.Shift);
            Damage (S, S.Display_Area);
         when Up => Recall (S, Older => True);
         when Down => Recall (S, Older => False);
         when Page_Up | Page_Down =>
            declare
               Page : constant Natural := Natural'Max (Visible_Rows (S), 1);
            begin
               S.Scroll := (if Item.Pressed = Page_Up then Natural'Min (S.Scroll + Page, Maximum_Scroll (S))
                            elsif S.Scroll > Page then S.Scroll - Page else 0);
               Damage (S, S.History_Area);
            end;
         when F1 => Run_Command (S, Help_Command);
         when others => null;
      end case;
   end Handle_Key;

   function Key_At (S : Shell_State; X, Y : Natural; Found : out Boolean) return Pad_Key is
   begin
      Found := False;
      if Point_In_Rect (X, Y, S.Pad_Area) then
         for K in Pad_Key loop
            if Point_In_Rect (X, Y, Key_Area (S, K)) then
               Found := True;
               return K;
            end if;
         end loop;
      end if;
      return Clear_Pad;
   end Key_At;

   procedure Handle_Pointer (S : in out Shell_State; Item : Event) is
      Menu_Handled, Found : Boolean;
      K : Pad_Key;
   begin
      Menu_Pointer (S, Item, Menu_Handled);
      if Menu_Handled then
         return;
      end if;
      if S.Help_On and then Item.Action = Press then
         S.Help_On := False;
         Damage_All (S);
         return;
      end if;
      K := Key_At (S, Item.X, Item.Y, Found);
      case Item.Action is
         when Move =>
            if Found /= S.Hot or else (Found and then K /= S.Hot_Key) then
               if S.Hot then
                  Damage (S, Key_Area (S, S.Hot_Key));
               end if;
               S.Hot := Found;
               S.Hot_Key := K;
               if Found then
                  Damage (S, Key_Area (S, K));
               end if;
            end if;
         when Press =>
            if Found then
               S.Pressed := True;
               S.Pressed_Key := K;
               Damage (S, Key_Area (S, K));
            elsif Point_In_Rect (Item.X, Item.Y, S.Entry_Area) then
               declare
                  Text : constant String := Editor.Content (S.Field);
                  Offset : constant Integer := Integer (Item.X) - Integer (S.Entry_Area.x + ENTRY_INSET)
                    + Integer (S.Entry_Scroll);
                  Boundary : constant Natural :=
                    (if Offset <= 0 then 0 else CuBit.Fonts.Boundary_At (CuBit.Fonts.Sans, Text, Offset));
               begin
                  Editor.Place_Cursor (S.Field, Boundary + 1, Item.Shift);
                  Damage (S, S.Display_Area);
               end;
            else
               declare
                  Row : constant Natural := Row_At (S, Item.Y);
               begin
                  if Row > 0 then
                     if Row = S.Last_Click_Row and then Item.Time_Ms >= S.Last_Click_Ms
                       and then Item.Time_Ms - S.Last_Click_Ms <= DOUBLE_CLICK_MS
                     then
                        --  Double click: the expression comes back to edit.
                        Replace_Entry (S, Calc_History.Source (S.History, Row).all);
                        Say (S, "Recalled; Enter evaluates it again");
                     end if;
                     S.Selected := Row;
                     S.Last_Click_Row := Row;
                     S.Last_Click_Ms := Item.Time_Ms;
                     Damage (S, S.History_Area);
                  end if;
               end;
            end if;
         when Release =>
            if S.Pressed then
               S.Pressed := False;
               Damage (S, Key_Area (S, S.Pressed_Key));
               if Found and then K = S.Pressed_Key then
                  Press_Key (S, K);
               end if;
            end if;
      end case;
   end Handle_Pointer;

   ---------------------------------------------------------------------------
   --  Public operations.

   procedure Initialize (S : in out Shell; Host : not null access Host_Type; Config : Settings) is
      Accepted : Boolean;
   begin
      S.State := new Shell_State;
      S.State.Host := Host;
      S.State.Config := Config;
      S.State.Places := Config.Places;
      Calc_History.Create (S.State.History, Config.History_Capacity);
      Editor.Initialize (S.State.Field, "", Accepted);
      S.State.Paste_Buffer := new String (1 .. Editor.MAX_TEXT_LENGTH);
      Prepare_Menu_Texts;
      Menu_Popup_Width := Positive'Max (MENU_MINIMUM_WIDTH, Menus.Required_Width (Build_Menus (S.State.all)));
      S.State.Menu_Model := Build_Menus (S.State.all);
      Say (S.State.all, READY_MESSAGE);
      Refresh_Preview (S.State.all);
   end Initialize;

   procedure Handle (S : in out Shell; Item : Event) is
      St : Shell_State renames S.State.all;
   begin
      case Item.Kind is
         when Key_Event => Handle_Key (St, Item);
         when Text_Event =>
            if Menus.Is_Open (St.Menu) then
               declare
                  Handled : Boolean;
               begin
                  Menu_Key (St, (Kind => Key_Event, Typed => Lower (Item.Typed), others => <>), Handled);
               end;
            elsif Item.Typed in ' ' .. '~' then
               if St.Help_On then
                  St.Help_On := False;
                  Damage_All (St);
               end if;
               Insert (St, [1 => Item.Typed]);
            end if;
         when Pointer_Event => Handle_Pointer (St, Item);
         when Wheel_Event =>
            if Point_In_Rect (Item.X, Item.Y, St.History_Area) then
               St.Scroll := Natural'Max (0, Natural'Min (Integer (St.Scroll) + Item.Steps, Maximum_Scroll (St)));
               Damage (St, St.History_Area);
            end if;
         when Resize_Event => Damage_All (St);
         when Modifier_Event =>
            declare
               Old_Cues : constant Boolean := Menus.Cues_Shown (St.Menu);
            begin
               Menus.Set_Alt_Held (St.Menu, Item.Alt);
               if Menus.Cues_Shown (St.Menu) /= Old_Cues then
                  Menu_Changed (St);
               end if;
            end;
      end case;
   end Handle;

   procedure Pump (S : in out Shell) is
      St : Shell_State renames S.State.all;
      Length : Natural;
      Ready : Boolean;
   begin
      if not St.Paste_Pending then
         return;
      end if;
      Poll_Paste (St.Host.all, St.Paste_Buffer.all, Length, Ready);
      if Ready then
         St.Paste_Pending := False;
         declare
            Pasted : String := St.Paste_Buffer (1 .. Length);
         begin
            --  One line of printable text: line breaks become spaces.
            for C of Pasted loop
               if C not in ' ' .. '~' then
                  C := ' ';
               end if;
            end loop;
            Insert (St, Pasted);
         end;
      end if;
   end Pump;

   procedure Set_Bounds (S : in out Shell; Bounds : Rect) is
   begin
      if Bounds /= S.State.Bounds then
         S.State.Bounds := Bounds;
         Layout (S.State.all);
         S.State.Scroll := Natural'Min (S.State.Scroll, Maximum_Scroll (S.State.all));
         Damage_All (S.State.all);
      end if;
   end Set_Bounds;

   procedure Take_Damage (S : in out Shell; Area : out Rect) is
   begin
      if Current_Theme /= S.State.Theme_Seen then
         S.State.Theme_Seen := Current_Theme;
         Damage_All (S.State.all);
      end if;
      Area := S.State.Dirty;
      S.State.Dirty := (others => 0);
   end Take_Damage;

   function Quit_Requested (S : Shell) return Boolean is (S.State.Quit);
   function Entry_Text (S : Shell) return String is (Editor.Content (S.State.Field));
   function Preview_Text (S : Shell) return String is (Preview_Primary (S.State.all));
   function Preview_Failure (S : Shell) return Calc_Engine.Failure_Kind is (S.State.Failure);
   function History_Count (S : Shell) return Natural is (Calc_History.Count (S.State.History));
   function History_Source (S : Shell; Index : Positive) return String is
     (Calc_History.Source (S.State.History, Index).all);
   function History_Result (S : Shell; Index : Positive) return String is
     (Head (Calc_History.Fraction (S.State.History, Index).all));
   function Status_Message (S : Shell) return String is (S.State.Status (1 .. S.State.Status_Length));
   function Places (S : Shell) return Calc_Format.Places_Count is (S.State.Places);
   function Menu_Bar (S : Shell) return Menus.Model is (Build_Menus (S.State.all));
   function Open_Menu (S : Shell) return Natural is (Menus.Open_Menu (S.State.Menu));
   function Help_Shown (S : Shell) return Boolean is (S.State.Help_On);

   function Button_Area (S : Shell; Label : String) return Rect is
   begin
      for K in Pad_Key loop
         if Calc_Shells.Label (K) = Label then
            return Key_Area (S.State.all, K);
         end if;
      end loop;
      return (others => 0);
   end Button_Area;

   ---------------------------------------------------------------------------
   --  Drawing.

   --  A canvas over Area whose logical pixels are Numerator/Denominator of
   --  the parent's: the same fonts and fills, larger and still sharp.
   function Scaled (C : Canvas; Area : Rect; Numerator, Denominator : Positive) return Canvas is
      V : Canvas := CuBit.UI.Surfaces.View (C, Area);
      N : constant Natural := V.densityNumerator * Numerator;
      D : constant Natural := V.densityDenominator * Denominator;
      function Common (A, B : Natural) return Natural is
         X : Natural := A;
         Y : Natural := B;
         T : Natural;
      begin
         while Y /= 0 loop
            T := X mod Y;
            X := Y;
            Y := T;
         end loop;
         return Natural'Max (X, 1);
      end Common;
      G : constant Natural := Common (N, D);
   begin
      if V.addr = System.Null_Address or else N / G > Client_Canvas_Geometry.Component'Last
        or else D / G > Client_Canvas_Geometry.Component'Last
      then
         return V;
      end if;
      V.densityNumerator := N / G;
      V.densityDenominator := D / G;
      V.originX := 0;
      V.originY := 0;
      V.width := V.width * Denominator / Numerator;
      V.height := V.height * Denominator / Numerator;
      if V.clipEnabled then
         V.clip := (V.clip.x * Denominator / Numerator, V.clip.y * Denominator / Numerator,
                    (V.clip.w * Denominator + Numerator - 1) / Numerator,
                    (V.clip.h * Denominator + Numerator - 1) / Numerator);
      end if;
      V.inputClipEnabled := False;
      return V;
   end Scaled;

   --  A stroke from (X0, Y0) to (X1, Y1), Width thick.
   procedure Line (C : Canvas; X0, Y0, X1, Y1 : Integer; Width : Positive; Ink : Color) is
      DX : constant Integer := abs (X1 - X0);
      DY : constant Integer := -abs (Y1 - Y0);
      SX : constant Integer := (if X0 < X1 then 1 else -1);
      SY : constant Integer := (if Y0 < Y1 then 1 else -1);
      Error : Integer := DX + DY;
      X : Integer := X0;
      Y : Integer := Y0;
      E2 : Integer;
      Half : constant Natural := Width / 2;
   begin
      loop
         if X - Half >= 0 and then Y - Half >= 0 then
            Fill_Rect (C, (X - Half, Y - Half, Width, Width), Ink);
         end if;
         exit when X = X1 and then Y = Y1;
         E2 := 2 * Error;
         if E2 >= DY then
            Error := Error + DY;
            X := X + SX;
         end if;
         if E2 <= DX then
            Error := Error + DX;
            Y := Y + SY;
         end if;
      end loop;
   end Line;

   procedure Draw_Symbol (C : Canvas; Box : Rect; Item : Symbol; Ink : Color) is
      CX : constant Integer := Box.x + Box.w / 2;
      CY : constant Integer := Box.y + Box.h / 2;
      A : constant Integer := SYMBOL_ARM;
   begin
      case Item is
         when Text_Symbol => null;
         when Plus_Symbol =>
            Fill_Rect (C, (CX - A, CY - 1, 2 * A + 1, STROKE), Ink);
            Fill_Rect (C, (CX - 1, CY - A, STROKE, 2 * A + 1), Ink);
         when Minus_Symbol =>
            Fill_Rect (C, (CX - A, CY - 1, 2 * A + 1, STROKE), Ink);
         when Equals_Symbol =>
            Fill_Rect (C, (CX - A, CY - 4, 2 * A + 1, STROKE), Ink);
            Fill_Rect (C, (CX - A, CY + 2, 2 * A + 1, STROKE), Ink);
         when Divide_Symbol =>
            Fill_Rect (C, (CX - A, CY - 1, 2 * A + 1, STROKE), Ink);
            Fill_Rect (C, (CX - 1, CY - A, 3, 3), Ink);
            Fill_Rect (C, (CX - 1, CY + A - 2, 3, 3), Ink);
         when Times_Symbol =>
            Line (C, CX - A + 1, CY - A + 1, CX + A - 1, CY + A - 1, STROKE, Ink);
            Line (C, CX - A + 1, CY + A - 1, CX + A - 1, CY - A + 1, STROKE, Ink);
         when Root_Symbol =>
            --  The small rising tick, the fall, the long rise, the bar
            --  over the radicand.
            Line (C, CX - A - 4, CY + 1, CX - A - 2, CY, STROKE, Ink);
            Line (C, CX - A - 2, CY, CX - A + 1, CY + A, STROKE, Ink);
            Line (C, CX - A + 1, CY + A, CX - A + 6, CY - A - 1, STROKE, Ink);
            Fill_Rect (C, (CX - A + 6, CY - A - 2, 2 * A + 2, STROKE), Ink);
         when Pi_Symbol =>
            Fill_Rect (C, (CX - A, CY - A + 1, 2 * A + 1, STROKE), Ink);
            Line (C, CX - A / 2, CY - A + 2, CX - A / 2 - 2, CY + A, STROKE, Ink);
            Fill_Rect (C, (CX + A / 2 - 1, CY - A + 2, STROKE, 2 * A - 2), Ink);
            Fill_Rect (C, (CX + A / 2, CY + A - 1, 3, STROKE), Ink);
         when Erase_Symbol =>
            --  A key with a point to the left, and a cross in it.
            Line (C, CX - A - 4, CY, CX - A + 1, CY - A + 1, STROKE, Ink);
            Line (C, CX - A - 4, CY, CX - A + 1, CY + A - 1, STROKE, Ink);
            Fill_Rect (C, (CX - A + 1, CY - A, 2 * A + 4, STROKE), Ink);
            Fill_Rect (C, (CX - A + 1, CY + A - 1, 2 * A + 4, STROKE), Ink);
            Fill_Rect (C, (CX + A + 3, CY - A, STROKE, 2 * A + 1), Ink);
            Line (C, CX - 1, CY - 3, CX + 4, CY + 2, STROKE, Ink);
            Line (C, CX - 1, CY + 2, CX + 4, CY - 3, STROKE, Ink);
      end case;
   end Draw_Symbol;

   --  Approximately equal: two tildes, one above the other.
   procedure Draw_Approximately (C : Canvas; X, Y : Natural; Ink : Color) is
   begin
      Draw_UI_Text_Transparent (C, X, (if Y > 3 then Y - 3 else 0), "~", Ink);
      Draw_UI_Text_Transparent (C, X, Y + 3, "~", Ink);
   end Draw_Approximately;
   APPROXIMATELY_WIDTH : constant := 14;

   procedure Draw_Key (S : Shell_State; C : Canvas; K : Pad_Key; Colors : Theme) is
      Box : constant Rect := Key_Area (S, K);
      Down : constant Boolean := S.Pressed and then S.Pressed_Key = K;
      Hot : constant Boolean := S.Hot and then S.Hot_Key = K;
      Night : constant Boolean := Dark (Colors);
      Base : constant Color :=
        (if K = Equals_Pad then APP_COLOR
         elsif K in Operator_Pad then Mix (Colors.face, APP_COLOR, TINT_SHARE)
         elsif K in Digit_Pad | Clear_Pad | Open_Pad | Close_Pad | Power_Pad | Factorial_Pad then Mix (Colors.field, Colors.face, (if Night then 30 else 35))
         else Mix (Colors.face, Colors.panel, 50));
      Lit : constant Color := (if Hot then Mix (Base, WHITE, (if Night then 12 else 30)) else Base);
      Top : constant Color := (if Down then Mix (Lit, Colors.shadow, 18) else Mix (Lit, WHITE, (if Night then 6 else 28)));
      Bottom : constant Color := (if Down then Mix (Lit, WHITE, 4) else Mix (Lit, Colors.shadow, 10));
      Ink : constant Color :=
        (if K = Equals_Pad then WHITE
         elsif K = Clear_Pad then Colors.danger
         elsif K in Operator_Pad then Mix (Colors.text, APP_COLOR, (if Night then 35 else 55))
         else Colors.text);
      Edge : constant Color := Mix (Colors.edge, Colors.shadow, (if Night then 40 else 25));
   begin
      if Box.w < 4 or else Box.h < 4 then
         return;
      end if;
      Fill_Vertical_Gradient (C, Box, Top, Bottom);
      --  A soft frame: the corners cut, a highlight inside the top edge.
      Fill_Rect (C, (Box.x + 1, Box.y, Box.w - 2, 1), Edge);
      Fill_Rect (C, (Box.x + 1, Box.y + Box.h - 1, Box.w - 2, 1), Mix (Edge, Colors.shadow, 30));
      Fill_Rect (C, (Box.x, Box.y + 1, 1, Box.h - 2), Edge);
      Fill_Rect (C, (Box.x + Box.w - 1, Box.y + 1, 1, Box.h - 2), Edge);
      Fill_Rect (C, (Box.x, Box.y, 1, 1), Colors.panel);
      Fill_Rect (C, (Box.x + Box.w - 1, Box.y, 1, 1), Colors.panel);
      Fill_Rect (C, (Box.x, Box.y + Box.h - 1, 1, 1), Colors.panel);
      Fill_Rect (C, (Box.x + Box.w - 1, Box.y + Box.h - 1, 1, 1), Colors.panel);
      if not Down then
         Fill_Rect (C, (Box.x + 1, Box.y + 1, Box.w - 2, 1), Mix (Top, WHITE, (if Night then 10 else 45)));
      end if;
      declare
         Face : constant Rect := (Box.x, Box.y + (if Down then 1 else 0), Box.w, Box.h);
      begin
         if Shape (K) /= Text_Symbol then
            Draw_Symbol (C, Face, Shape (K), Ink);
         elsif K in Digit_Pad | Clear_Pad | Open_Pad | Close_Pad | Power_Pad | Factorial_Pad then
            declare
               Big : constant Canvas := Scaled (C, Face, KEY_NUMERATOR, KEY_DENOMINATOR);
               W : constant Natural := UI_Text_Width (Label (K));
               Inner_W : constant Natural := Face.w * KEY_DENOMINATOR / KEY_NUMERATOR;
               Inner_H : constant Natural := Face.h * KEY_DENOMINATOR / KEY_NUMERATOR;
            begin
               Draw_UI_Text_Transparent
                 (Big, (if Inner_W > W then (Inner_W - W) / 2 else 0),
                  (if Inner_H > UI_Text_Height then (Inner_H - UI_Text_Height) / 2 else 0), Label (K), Ink);
            end;
         else
            declare
               W : constant Natural := UI_Text_Width (Label (K));
            begin
               Draw_UI_Text_Transparent
                 (C, Face.x + (if Face.w > W then (Face.w - W) / 2 else 0),
                  Face.y + (if Face.h > UI_Text_Height then (Face.h - UI_Text_Height) / 2 else 0), Label (K), Ink);
            end;
         end if;
      end;
   end Draw_Key;

   procedure Draw_History (S : Shell_State; C : Canvas; Colors : Theme) is
      Area : constant Rect := S.History_Area;
      Night : constant Boolean := Dark (Colors);
      Field : constant Color := Colors.field;
      Rule : constant Color := Mix (Colors.field, Colors.edge, 45);
   begin
      Fill_Rect (C, Area, Field);
      if Calc_History.Count (S.History) = 0 then
         declare
            Hint1 : constant String := "Exact arithmetic: 1/3 + 1/6 is 1/2, never 0.4999...";
            Hint2 : constant String := "Try  2^100,  100!,  sqrt(2),  pi,  gcd(12, 18)";
            Hint3 : constant String := "Up recalls, F1 lists every key and function.";
            Mid : constant Natural := Area.y + Area.h / 2;
            procedure Centered (Text : String; Y : Natural; Ink : Color) is
               W : constant Natural := UI_Text_Width (Text);
            begin
               Draw_UI_Text_Transparent (With_Clip (C, Area), Area.x + (if Area.w > W then (Area.w - W) / 2 else 0), Y,
                                         Text, Ink);
            end Centered;
         begin
            if Area.h > 3 * HELP_LINE then
               Centered (Hint1, Mid - 3 * HELP_LINE / 2, Colors.muted);
               Centered (Hint2, Mid - HELP_LINE / 2, Mix (Colors.muted, APP_COLOR, 45));
               Centered (Hint3, Mid + HELP_LINE / 2, Colors.muted);
            end if;
         end;
         return;
      end if;
      for Index in S.Scroll + 1 .. Natural'Min (Calc_History.Count (S.History), S.Scroll + Visible_Rows (S) + 1) loop
         declare
            Row : constant Rect := Row_Area (S, Index);
         begin
            if not Is_Empty (Row) then
               declare
                  Selected : constant Boolean := Index = S.Selected;
                  Back : constant Color := (if Selected then Mix (Field, APP_COLOR, (if Night then 28 else 16))
                                            elsif Index mod 2 = 0 then Mix (Field, Colors.panel, 35) else Field);
                  Clip : constant Canvas := With_Clip (C, Row);
                  Fraction : constant Calc_History.Text_View := Calc_History.Fraction (S.History, Index);
                  Decimal : constant Calc_History.Text_View := Calc_History.Decimal (S.History, Index);
                  Form : constant Calc_Format.Decimal_Form := Calc_History.Form (S.History, Index);
                  Approximate : constant Boolean := Calc_History.Kind (S.History, Index) = Calc_Numbers.Approximate;
                  Whole : constant Boolean := Calc_History.Is_Integer (S.History, Index);
                  Width : constant Natural := (if Row.w > 2 * ROW_INSET then Row.w - 2 * ROW_INSET else 0);
               begin
                  Fill_Rect (C, Row, Back);
                  Fill_Rect (C, (Row.x, Row.y + Row.h - 1, Row.w, 1), Rule);
                  if Selected then
                     Fill_Rect (C, (Row.x, Row.y, 3, Row.h - 1), APP_COLOR);
                  end if;
                  --  The expression, as typed, then the result right-aligned.
                  Draw_Code_Text_Transparent
                    (Clip, Row.x + ROW_INSET, Row.y + 5, Fit_Code_Text (Calc_History.Source (S.History, Index).all, Width),
                     Colors.muted);
                  declare
                     Shown : constant String :=
                       Fit_UI_Text (Primary_Of (S, Fraction.all, Decimal.all, Form, Approximate, Whole),
                                    (if Width > APPROXIMATELY_WIDTH then Width - APPROXIMATELY_WIDTH else 0));
                     W : constant Natural := UI_Text_Width (Shown);
                     X : constant Natural := Row.x + Row.w - ROW_INSET - W;
                     Y : constant Natural := Row.y + Row.h - UI_Text_Height - 6;
                     --  The other form; for a long integer, how long it is.
                     Other : constant String :=
                       (if Approximate then ""
                        elsif Whole then (if UI_Text_Width (Head (Decimal.all)) > Width or else
                                             Decimal'Length > SHOWN_CHARACTERS
                                          then Image (Decimal'Length - (if Decimal (Decimal'First) = '-' then 1 else 0))
                                               & " digits"
                                          else "")
                        else Secondary_Of (S, Fraction.all, Decimal.all, Form, Approximate, Whole));
                     Room : constant Natural := (if X > Row.x + 2 * ROW_INSET + APPROXIMATELY_WIDTH
                                                 then X - Row.x - 2 * ROW_INSET - APPROXIMATELY_WIDTH else 0);
                  begin
                     Draw_UI_Text_Transparent (Clip, X, Y, Shown, Colors.text);
                     if Approximate and then X > APPROXIMATELY_WIDTH then
                        Draw_Approximately (Clip, X - APPROXIMATELY_WIDTH, Y, Mix (Colors.text, APP_COLOR, 60));
                     elsif Whole and then Other'Length > 0 then
                        --  A long integer's length, on the expression's line.
                        Draw_UI_Text_Transparent
                          (Clip, Row.x + Row.w - ROW_INSET - UI_Text_Width (Other), Row.y + 5, Other,
                           Mix (Colors.muted, APP_COLOR, 35));
                     elsif Other'Length > 0 and then Room > 40 then
                        declare
                           Fitted : constant String := Fit_UI_Text (Other, Room);
                           OW : constant Natural := UI_Text_Width (Fitted);
                        begin
                           Draw_UI_Text_Transparent (Clip, X - OW - 2 * ROW_INSET, Y, Fitted,
                                                     Mix (Colors.muted, Back, 15));
                        end;
                     end if;
                  end;
               end;
            end if;
         end;
      end loop;
      --  A scroll hint: entries above the top.
      if S.Scroll + Visible_Rows (S) < Calc_History.Count (S.History) then
         declare
            Hint : constant String := Image (Calc_History.Count (S.History) - S.Scroll - Visible_Rows (S)) & " more";
            W : constant Natural := UI_Text_Width (Hint);
            Left : constant Natural := Area.x + (if Area.w > W then (Area.w - W) / 2 else 0);
         begin
            --  Centred on the top edge, clear of each row's texts.
            Fill_Rect (C, (Left - ROW_INSET, Area.y, W + 2 * ROW_INSET, UI_Text_Height + 2),
                       Mix (Field, Colors.panel, 60));
            Draw_UI_Text_Transparent (C, Left, Area.y + 1, Hint, Colors.muted);
         end;
      end if;
   end Draw_History;

   procedure Draw_Entry (S : in out Shell_State; C : Canvas; Colors : Theme) is
      Box : constant Rect := S.Entry_Area;
      Text : constant String := Editor.Content (S.Field);
      Inner : constant Natural := (if Box.w > 2 * ENTRY_INSET then Box.w - 2 * ENTRY_INSET else 0);
      Caret : constant Natural := CuBit.Fonts.Boundary (CuBit.Fonts.Sans, Text, Editor.Cursor (S.Field) - 1);
      From : constant Natural := CuBit.Fonts.Boundary (CuBit.Fonts.Sans, Text, Editor.Selection_First (S.Field) - 1);
      To : constant Natural := CuBit.Fonts.Boundary (CuBit.Fonts.Sans, Text, Editor.Selection_Last (S.Field) - 1);
      Text_Y : constant Natural := Box.y + (if Box.h > UI_Text_Height then (Box.h - UI_Text_Height) / 2 else 0);
   begin
      --  Keep the caret in view.
      if Caret < S.Entry_Scroll then
         S.Entry_Scroll := Caret;
      elsif Caret > S.Entry_Scroll + Inner then
         S.Entry_Scroll := Caret - Inner;
      end if;
      Fill_Rect (C, Box, Colors.field);
      Stroke_Rect (C, Box, APP_COLOR, Mix (APP_COLOR, Colors.field, 40));
      Stroke_Rect (C, Inflate_Rect (Box, 0), APP_COLOR, APP_COLOR);
      declare
         Area : constant Rect := (Box.x + ENTRY_INSET, Box.y + 2, Inner, (if Box.h > 4 then Box.h - 4 else 0));
         Clip : constant Canvas := With_Clip (C, Area);
         X0 : constant Integer := Integer (Area.x) - Integer (S.Entry_Scroll);
         function At_X (Offset : Natural) return Natural is
           (Natural (Integer'Max (0, X0 + Integer (Offset))));
      begin
         if From < To then
            Fill_Rect (Clip, (At_X (From), Text_Y, To - From, UI_Text_Height), Mix (Colors.field, APP_COLOR, 30));
         end if;
         if Text'Length = 0 then
            Draw_UI_Text_Transparent (Clip, Area.x, Text_Y, "Type an expression, such as 2/3 + 1/6", Colors.muted);
         elsif X0 >= 0 then
            Draw_UI_Text_Transparent (Clip, Natural (X0), Text_Y, Text, Colors.text);
         else
            --  Scrolled: draw from the first character that shows.
            declare
               First : constant Natural := CuBit.Fonts.Boundary_At (CuBit.Fonts.Sans, Text, S.Entry_Scroll);
               Skip : constant Natural := CuBit.Fonts.Boundary (CuBit.Fonts.Sans, Text, First);
            begin
               Draw_UI_Text_Transparent
                 (Clip, At_X (Skip), Text_Y, Text (Text'First + First .. Text'Last), Colors.text);
            end;
         end if;
         --  The failing column, underlined (not while still typing).
         if S.Failure /= Calc_Engine.No_Failure and then not Incomplete (S) and then S.Failure_Column in 1 .. Text'Length
         then
            declare
               Left : constant Natural := CuBit.Fonts.Boundary (CuBit.Fonts.Sans, Text, S.Failure_Column - 1);
               Right : constant Natural := CuBit.Fonts.Boundary (CuBit.Fonts.Sans, Text, S.Failure_Column);
            begin
               Fill_Rect (Clip, (At_X (Left), Text_Y + UI_Text_Height, Natural'Max (Right - Left, 4), 2), Colors.danger);
            end;
         end if;
         Fill_Rect (Clip, (At_X (Caret), Text_Y - 1, 2, UI_Text_Height + 2), APP_COLOR);
      end;
   end Draw_Entry;

   procedure Draw_Display (S : in out Shell_State; C : Canvas; Colors : Theme) is
      Area : constant Rect := S.Display_Area;
      Back : constant Color := Mix (Colors.panel, APP_COLOR, SUBTLE_SHARE / 2);
   begin
      Fill_Rect (C, Area, Back);
      Fill_Rect (C, (Area.x, Area.y, Area.w, 1), Mix (Colors.edge, APP_COLOR, 30));
      Draw_Entry (S, C, Colors);
      declare
         Big : constant Rect := S.Big_Area;
         View : constant Canvas := Scaled (C, Big, BIG_NUMERATOR, BIG_DENOMINATOR);
         Inner_W : constant Natural := Big.w * BIG_DENOMINATOR / BIG_NUMERATOR;
         Text : constant String := Preview_Primary (S);
         Room : constant Natural := (if Inner_W > APPROXIMATELY_WIDTH then Inner_W - APPROXIMATELY_WIDTH else 0);
         Shown : constant String := Fit_UI_Text (Text, Room);
         W : constant Natural := UI_Text_Width (Shown);
         Waiting : constant Boolean := S.Failure /= Calc_Engine.No_Failure;
         Ink : constant Color := (if Editor.Length (S.Field) = 0 then Mix (Colors.text, Back, 35) else Colors.text);
      begin
         if not Waiting then
            Draw_UI_Text_Transparent (View, (if Inner_W > W then Inner_W - W else 0), 0, Shown, Ink);
            if S.Preview_Approximate and then Inner_W > W + APPROXIMATELY_WIDTH then
               Draw_Approximately (View, Inner_W - W - APPROXIMATELY_WIDTH, 0, Mix (Ink, APP_COLOR, 60));
            end if;
         end if;
      end;
      declare
         Line : constant Rect := S.Secondary_Area;
         Message : constant String :=
           (if S.Failure = Calc_Engine.No_Failure then Preview_Secondary (S)
            elsif Incomplete (S) then "..."
            else Calc_Engine.Message (S.Failure));
         Shown : constant String := Fit_UI_Text (Message, Line.w);
         W : constant Natural := UI_Text_Width (Shown);
         Ink : constant Color :=
           (if S.Failure /= Calc_Engine.No_Failure and then not Incomplete (S) then Colors.danger else Colors.muted);
      begin
         Draw_UI_Text_Transparent (C, Line.x + (if Line.w > W then Line.w - W else 0), Line.y, Shown, Ink);
      end;
   end Draw_Display;

   procedure Draw_Help (S : Shell_State; C : Canvas; Colors : Theme) is
      LINES : constant := 17;
      function Line_Text (K : Positive) return String is
        (case K is
            when 1 => "Keys",
            when 2 => "Enter  evaluate        Esc  clear the entry",
            when 3 => "Up / Down  earlier expressions",
            when 4 => "Ctrl+C  copy the result (exact)",
            when 5 => "Ctrl+Shift+C  copy it as a decimal",
            when 6 => "Ctrl+D  fractions or decimals first",
            when 7 => "Ctrl+L  clear the history    F10  menus",
            when 9 => "Expressions",
            when 10 => "+  -  *  /  ^ (power)  mod  ! (factorial)  ( )",
            when 11 => "abs floor ceil round trunc  round(x, places)",
            when 12 => "sqrt  gcd(a, b)  lcm(a, b)  num  den",
            when 13 => "pi  e  ans (the previous result)",
            when 14 => "1.25   3e-8   2/3   -2^2 = -4   2^3^2 = 512",
            when 16 => "Results are exact rationals; sqrt, pi and e",
            when 17 => "are marked approximate. CCL: (calc.evaluate ""1/3"")",
            when others => "");
      HEADINGS : constant array (1 .. LINES) of Boolean := [1 | 9 => True, others => False];
      W : constant Natural := Natural'Min (HELP_WIDTH, (if S.Bounds.w > 2 * MARGIN then S.Bounds.w - 2 * MARGIN else 0));
      H : constant Natural := LINES * HELP_LINE + 2 * MARGIN;
      Box : constant Rect := (S.Bounds.x + (if S.Bounds.w > W then (S.Bounds.w - W) / 2 else 0),
                              S.Bounds.y + MENU_BAR_HEIGHT + MARGIN, W, H);
      Clip : constant Canvas := With_Clip (C, Box);
   begin
      Fill_Rect (C, (Box.x + 3, Box.y + 3, Box.w, Box.h), Mix (Colors.shadow, Colors.panel, 40));
      Fill_Rect (C, Box, Colors.field);
      Stroke_Rect (C, Box, APP_COLOR, APP_COLOR);
      Fill_Rect (C, (Box.x, Box.y, Box.w, 3), APP_COLOR);
      for K in 1 .. LINES loop
         Draw_UI_Text_Transparent
           (Clip, Box.x + MARGIN, Box.y + MARGIN + (K - 1) * HELP_LINE, Line_Text (K),
            (if HEADINGS (K) then APP_COLOR else Colors.text));
      end loop;
   end Draw_Help;

   procedure Render (S : in out Shell; C : Canvas) is
      St : Shell_State renames S.State.all;
      Colors : constant Theme := Current_Theme;
      Clip : constant Rect := (if C.clipEnabled then C.clip else St.Bounds);
      function Touches (Area : Rect) return Boolean is
        (not Is_Empty (Area) and then not Is_Empty (Clip)
         and then Area.x < Clip.x + Clip.w and then Clip.x < Area.x + Area.w
         and then Area.y < Clip.y + Clip.h and then Clip.y < Area.y + Area.h);
   begin
      Controls.Clear (St.Map);
      if Touches (St.History_Area) then
         Draw_History (St, C, Colors);
      end if;
      if Touches (St.Display_Area) then
         Draw_Display (St, C, Colors);
      end if;
      if Touches (St.Pad_Area) then
         Fill_Rect (C, St.Pad_Area, Colors.panel);
         for K in Pad_Key loop
            if Touches (Key_Area (St, K)) then
               Draw_Key (St, C, K, Colors);
            end if;
         end loop;
      end if;
      if Touches (St.Status_Area) then
         declare
            Right : constant String := Image (St.Places) & " places";
         begin
            Draw_Status_Bar (C, St.Status_Area, Colors, St.Status (1 .. St.Status_Length), Right);
         end;
      end if;
      if St.Help_On then
         Draw_Help (St, C, Colors);
      end if;
      --  The menu bar and its popup, over everything; its controls cover
      --  the whole window whatever this frame's clip.
      St.Menu_Model := Build_Menus (St);
      declare
         Whole : Canvas := C;
      begin
         Whole.inputClipEnabled := True;
         Whole.inputClip := St.Bounds;
         Menus.Draw (Whole, St.Map, St.Menu, St.Menu_Model, MENU_BASE, St.Menu_Area, Colors, Menu_Popup_Width,
                     MENU_ROW_HEIGHT);
      end;
      St.Popup_Area := Popup_Of (St);
   end Render;
end Calc_Shells;
