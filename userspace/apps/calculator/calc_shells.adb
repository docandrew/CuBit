with Ada.Unchecked_Deallocation;
with System;
with Client_Canvas_Geometry;
with CuBit.Fonts;
with CuBit.UI.Controls;
with CuBit.UI.Editor;
with CuBit.UI.Surfaces;
with Calc_Balls;
with Calc_History;
with Calc_Symbols;
with Calc_Syntax;
with Calc_Words;

package body Calc_Shells is
   use CuBit.UI;
   package Menus renames CuBit.UI.Menus;
   package Controls renames CuBit.UI.Controls;
   package Editor renames CuBit.UI.Editor;
   use type Menus.Text;
   use type Calc_Engine.Failure_Kind;
   use type Calc_Format.Decimal_Form;
   use type Calc_History.Text_Access;
   use type Calc.Mode_Member;
   use type Calc.Angle_Member;
   use type Calc.Word_Member;
   use type Calc.Base_Member;
   use type System.Address;

   ---------------------------------------------------------------------------
   --  Geometry (logical pixels; the desktop scales them to the display).

   MENU_BAR_HEIGHT : constant := 24;
   MENU_ROW_HEIGHT : constant := 24;
   MENU_MINIMUM_WIDTH : constant := 200;
   MENU_BASE : constant := 1;
   STATUS_HEIGHT : constant := 24;
   MARGIN : constant := 8;
   GAP : constant := 4;
   --  A pad button as designed (the pad stretches with the window).
   BUTTON_WIDTH : constant := 52;
   BUTTON_HEIGHT : constant := 32;
   ENTRY_HEIGHT : constant := 26;
   ENTRY_INSET : constant := 6;
   --  The big result: the UI face at twice its size.
   BIG_NUMERATOR : constant := 2;
   BIG_DENOMINATOR : constant := 1;
   BIG_HEIGHT : constant := 34;
   --  Digits on the pad: one and a quarter times the UI face.
   KEY_NUMERATOR : constant := 5;
   KEY_DENOMINATOR : constant := 4;
   SECONDARY_HEIGHT : constant := 18;
   --  The chips above the entry (angle unit; word size and sign).
   STRIP_HEIGHT : constant := 20;
   CHIP_PADDING : constant := 7;
   CHIP_GAP : constant := 3;
   --  Programmer mode: the four bases, then the bit field, 16 bits a row.
   BASE_ROW_HEIGHT : constant := 17;
   BASE_LABEL_WIDTH : constant := 34;
   BIT_ROW_HEIGHT : constant := 17;
   BITS_PER_ROW : constant := 16;
   BITS_PER_NIBBLE : constant := 4;
   NIBBLE_GAP : constant := 5;
   BIT_LABEL_WIDTH : constant := 22;
   --  The bit field of an unbounded word shows its low 64 bits.
   UNBOUNDED_BITS : constant := 64;
   BIT_ROWS : constant := UNBOUNDED_BITS / BITS_PER_ROW;
   ROW_HEIGHT : constant := 38;
   ROW_INSET : constant := 8;
   DOUBLE_CLICK_MS : constant := 400;
   HELP_WIDTH : constant := 300;
   HELP_LINE : constant := 18;
   --  Key symbols (Calc_Symbols): their box and stroke, the stroke one and
   --  a half logical pixels rounded to whole physical pixels.
   SYMBOL_BOX : constant := 14;
   WIDE_SYMBOL_BOX : constant := 18;
   STROKE_NUMERATOR : constant := 3;
   STROKE_DENOMINATOR : constant := 2;
   SUBTLE_SHARE : constant := 10;   --  percent of the app colour in a tint
   TINT_SHARE : constant := 22;
   MAXIMUM_STATUS : constant := 160;

   --  The Calculator's identity colour (system.ccl's launcher entry): teal.
   APP_COLOR : constant Color := 16#12A58F#;
   WHITE : constant Color := 16#FFFFFF#;
   ALPHA_SHIFT : constant := 24;
   RGB_MASK : constant Color := 16#FFFFFF#;

   ---------------------------------------------------------------------------
   --  Modes and their options.

   function Base_Of (Item : Calc.Base_Member) return Calc_Format.Display_Base is
     (case Item is
         when Calc.Decimal => 10, when Calc.Hexadecimal => 16, when Calc.Octal => 8, when Calc.Binary => 2);
   function Prefix (Item : Calc.Base_Member) return String is
     (case Item is
         when Calc.Decimal => "", when Calc.Hexadecimal => "0x", when Calc.Octal => "0o",
         when Calc.Binary => "0b");
   function Base_Caption (Item : Calc.Base_Member) return String is
     (case Item is
         when Calc.Decimal => "DEC", when Calc.Hexadecimal => "HEX", when Calc.Octal => "OCT",
         when Calc.Binary => "BIN");
   function Word_Caption (Item : Calc.Word_Member) return String is
     (case Item is
         when Calc.Unbounded => "unbounded", when Calc.Bits_8 => "8-bit", when Calc.Bits_16 => "16-bit",
         when Calc.Bits_32 => "32-bit", when Calc.Bits_64 => "64-bit");
   function Angle_Caption (Item : Calc.Angle_Member) return String is
     (case Item is
         when Calc.Radians => "RAD", when Calc.Degrees => "DEG", when Calc.Gradians => "GRAD");
   --  The base rows, top to bottom.
   type Base_Order is array (Positive range <>) of Calc.Base_Member;
   BASE_ROWS : constant Base_Order := [Calc.Hexadecimal, Calc.Decimal, Calc.Octal, Calc.Binary];

   function Next (Item : Calc.Angle_Member) return Calc.Angle_Member is
     (if Item = Calc.Angle_Member'Last then Calc.Angle_Member'First else Calc.Angle_Member'Succ (Item));
   function Next (Item : Calc.Word_Member) return Calc.Word_Member is
     (if Item = Calc.Word_Member'Last then Calc.Word_Member'First else Calc.Word_Member'Succ (Item));
   function Next (Item : Calc.Base_Member) return Calc.Base_Member is
     (if Item = Calc.Base_Member'Last then Calc.Base_Member'First else Calc.Base_Member'Succ (Item));

   ---------------------------------------------------------------------------
   --  Commands and menus.

   type Command is
     (No_Command, Clear_History_Command, Quit_Command,
      Copy_Result_Command, Copy_Decimal_Command, Copy_Expression_Command, Paste_Command, Select_All_Command,
      Clear_Entry_Command,
      Standard_Command, Scientific_Command, Programmer_Command, Fraction_First_Command,
      Places_10_Command, Places_20_Command, Places_50_Command, Places_100_Command,
      Radians_Command, Degrees_Command, Gradians_Command,
      Bits_8_Command, Bits_16_Command, Bits_32_Command, Bits_64_Command, Unbounded_Command, Signed_Command,
      Hexadecimal_Command, Decimal_Command, Octal_Command, Binary_Command,
      Help_Command);
   subtype Mode_Command is Command range Standard_Command .. Programmer_Command;
   subtype Places_Command is Command range Places_10_Command .. Places_100_Command;
   subtype Angle_Command is Command range Radians_Command .. Gradians_Command;
   subtype Word_Command is Command range Bits_8_Command .. Unbounded_Command;
   subtype Base_Command is Command range Hexadecimal_Command .. Binary_Command;
   type Menu_Name is (Calculator_Menu, Edit_Menu, View_Menu, Options_Menu, Help_Menu);

   function Menu_Of (Item : Command) return Menu_Name is
     (case Item is
         when No_Command | Clear_History_Command | Quit_Command => Calculator_Menu,
         when Copy_Result_Command .. Clear_Entry_Command => Edit_Menu,
         when Standard_Command .. Places_100_Command => View_Menu,
         when Radians_Command .. Binary_Command => Options_Menu,
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
         when Standard_Command => "Standard",
         when Scientific_Command => "Scientific",
         when Programmer_Command => "Programmer",
         when Fraction_First_Command => "Fractions First",
         when Places_10_Command => "10 Decimal Places",
         when Places_20_Command => "20 Decimal Places",
         when Places_50_Command => "50 Decimal Places",
         when Places_100_Command => "100 Decimal Places",
         when Radians_Command => "Radians",
         when Degrees_Command => "Degrees",
         when Gradians_Command => "Gradians",
         when Bits_8_Command => "8-bit Word",
         when Bits_16_Command => "16-bit Word",
         when Bits_32_Command => "32-bit Word",
         when Bits_64_Command => "64-bit Word",
         when Unbounded_Command => "Unbounded",
         when Signed_Command => "Signed",
         when Hexadecimal_Command => "Hexadecimal",
         when Decimal_Command => "Decimal",
         when Octal_Command => "Octal",
         when Binary_Command => "Binary",
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
         when Standard_Command => "Ctrl+1",
         when Scientific_Command => "Ctrl+2",
         when Programmer_Command => "Ctrl+3",
         when Fraction_First_Command => "Ctrl+D",
         when Signed_Command => "Ctrl+S",
         when Help_Command => "F1",
         when others => "");
   function Starts_Group (Item : Command) return Boolean is
     (Item in Quit_Command | Paste_Command | Clear_Entry_Command | Fraction_First_Command | Places_10_Command
            | Bits_8_Command | Signed_Command | Hexadecimal_Command);
   function Title (Item : Menu_Name) return String is
     (case Item is
         when Calculator_Menu => "Calculator", when Edit_Menu => "Edit", when View_Menu => "View",
         when Options_Menu => "Options", when Help_Menu => "Help");
   function Places_Of (Item : Places_Command) return Calc_Format.Places_Count is
     (case Item is
         when Places_10_Command => 10, when Places_20_Command => 20, when Places_50_Command => 50,
         when Places_100_Command => 100);
   function Mode_Of (Item : Mode_Command) return Calc.Mode_Member is
     (case Item is
         when Standard_Command => Calc.Standard, when Scientific_Command => Calc.Scientific,
         when Programmer_Command => Calc.Programmer);
   function Angle_Of (Item : Angle_Command) return Calc.Angle_Member is
     (case Item is
         when Radians_Command => Calc.Radians, when Degrees_Command => Calc.Degrees,
         when Gradians_Command => Calc.Gradians);
   function Word_Of (Item : Word_Command) return Calc.Word_Member is
     (case Item is
         when Bits_8_Command => Calc.Bits_8, when Bits_16_Command => Calc.Bits_16,
         when Bits_32_Command => Calc.Bits_32, when Bits_64_Command => Calc.Bits_64,
         when Unbounded_Command => Calc.Unbounded);
   function Base_Of (Item : Base_Command) return Calc.Base_Member is
     (case Item is
         when Hexadecimal_Command => Calc.Hexadecimal, when Decimal_Command => Calc.Decimal,
         when Octal_Command => Calc.Octal, when Binary_Command => Calc.Binary);

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
   --  The pads.

   type Pad_Key is
     (Clear_Pad, Erase_Pad, Open_Pad, Close_Pad, Comma_Pad, Power_Pad, Divide_Pad, Times_Pad, Minus_Pad, Plus_Pad,
      Equals_Pad, Root_Pad, Factorial_Pad, Pi_Pad, Euler_Pad, Ans_Pad, Point_Pad, Modulo_Pad,
      Zero_Pad, One_Pad, Two_Pad, Three_Pad, Four_Pad, Five_Pad, Six_Pad, Seven_Pad, Eight_Pad, Nine_Pad,
      A_Pad, B_Pad, C_Pad, D_Pad, E_Pad, F_Pad,
      Sine_Pad, Cosine_Pad, Tangent_Pad, Arcsine_Pad, Arccosine_Pad, Arctangent_Pad, Ln_Pad, Log_Pad, Exp_Pad,
      And_Pad, Or_Pad, Xor_Pad, Not_Pad, Shift_Left_Pad, Shift_Right_Pad, Rotate_Left_Pad, Rotate_Right_Pad);
   subtype Digit_Pad is Pad_Key range Zero_Pad .. F_Pad;
   subtype Function_Pad is Pad_Key range Sine_Pad .. Exp_Pad;
   subtype Bitwise_Pad is Pad_Key range And_Pad .. Rotate_Right_Pad;
   subtype Operator_Pad is Pad_Key with Static_Predicate =>
     Operator_Pad in Divide_Pad | Times_Pad | Minus_Pad | Plus_Pad | Power_Pad | Modulo_Pad;
   --  Keys whose caption is set in the pad's larger digits.
   subtype Large_Pad is Pad_Key with Static_Predicate =>
     Large_Pad in Digit_Pad | Point_Pad | Open_Pad | Close_Pad | Comma_Pad | Clear_Pad | Power_Pad
                | Factorial_Pad | Euler_Pad;

   function Label (K : Pad_Key) return String is
     (case K is
         when Clear_Pad => "C", when Erase_Pad => "del", when Open_Pad => "(", when Close_Pad => ")",
         when Comma_Pad => ",", when Power_Pad => "^", when Divide_Pad => "/", when Times_Pad => "*",
         when Minus_Pad => "-", when Plus_Pad => "+", when Equals_Pad => "=", when Root_Pad => "sqrt",
         when Factorial_Pad => "!", when Pi_Pad => "pi", when Euler_Pad => "e", when Ans_Pad => "ans",
         when Point_Pad => ".", when Modulo_Pad => "mod",
         when Zero_Pad => "0", when One_Pad => "1", when Two_Pad => "2", when Three_Pad => "3",
         when Four_Pad => "4", when Five_Pad => "5", when Six_Pad => "6", when Seven_Pad => "7",
         when Eight_Pad => "8", when Nine_Pad => "9",
         when A_Pad => "A", when B_Pad => "B", when C_Pad => "C", when D_Pad => "D", when E_Pad => "E",
         when F_Pad => "F",
         when Sine_Pad => "sin", when Cosine_Pad => "cos", when Tangent_Pad => "tan", when Arcsine_Pad => "asin",
         when Arccosine_Pad => "acos", when Arctangent_Pad => "atan", when Ln_Pad => "ln", when Log_Pad => "log",
         when Exp_Pad => "exp",
         when And_Pad => "&", when Or_Pad => "|", when Xor_Pad => "xor", when Not_Pad => "~",
         when Shift_Left_Pad => "<<", when Shift_Right_Pad => ">>", when Rotate_Left_Pad => "rotl",
         when Rotate_Right_Pad => "rotr");
   --  What the key shows when that differs from what it types.
   function Caption (K : Pad_Key; Mode : Calc.Mode_Member) return String is
     (case K is
         when Clear_Pad => (if Mode = Calc.Programmer then "AC" else "C"),
         when Factorial_Pad => "n!", when Power_Pad => "x^y",
         when And_Pad => "AND", when Or_Pad => "OR", when Xor_Pad => "XOR", when Not_Pad => "NOT",
         when Rotate_Left_Pad => "ROL", when Rotate_Right_Pad => "ROR",
         when others => Label (K));
   --  What a press types (the clear, erase and equals keys act instead).
   function Inserts (K : Pad_Key) return String is
     (case K is
         when Root_Pad | Function_Pad | Rotate_Left_Pad | Rotate_Right_Pad => Label (K) & "(",
         when And_Pad | Or_Pad | Xor_Pad | Shift_Left_Pad | Shift_Right_Pad | Modulo_Pad => " " & Label (K) & " ",
         when Clear_Pad | Erase_Pad | Equals_Pad => "",
         when others => Label (K));
   function Has_Symbol (K : Pad_Key) return Boolean is
     (K in Divide_Pad | Times_Pad | Minus_Pad | Plus_Pad | Root_Pad | Pi_Pad | Erase_Pad | Equals_Pad);
   function Symbol_Of (K : Pad_Key) return Calc_Symbols.Symbol is
     (case K is
         when Divide_Pad => Calc_Symbols.Divide_Symbol, when Times_Pad => Calc_Symbols.Times_Symbol,
         when Minus_Pad => Calc_Symbols.Minus_Symbol, when Plus_Pad => Calc_Symbols.Plus_Symbol,
         when Root_Pad => Calc_Symbols.Root_Symbol, when Pi_Pad => Calc_Symbols.Pi_Symbol,
         when Erase_Pad => Calc_Symbols.Erase_Symbol, when others => Calc_Symbols.Equals_Symbol);
   function Digit_Value (K : Digit_Pad) return Natural is (Pad_Key'Pos (K) - Pad_Key'Pos (Zero_Pad));

   --  Each mode's pad, rows top to bottom.
   type Key_Grid is array (Natural range <>, Natural range <>) of Pad_Key;
   STANDARD_PAD : constant Key_Grid :=
     [[Clear_Pad, Open_Pad, Close_Pad, Power_Pad, Divide_Pad],
      [Seven_Pad, Eight_Pad, Nine_Pad, Times_Pad, Root_Pad],
      [Four_Pad, Five_Pad, Six_Pad, Minus_Pad, Factorial_Pad],
      [One_Pad, Two_Pad, Three_Pad, Plus_Pad, Pi_Pad],
      [Zero_Pad, Point_Pad, Erase_Pad, Ans_Pad, Equals_Pad]];
   SCIENTIFIC_PAD : constant Key_Grid :=
     [[Sine_Pad, Arcsine_Pad, Clear_Pad, Open_Pad, Close_Pad, Power_Pad, Divide_Pad],
      [Cosine_Pad, Arccosine_Pad, Seven_Pad, Eight_Pad, Nine_Pad, Times_Pad, Root_Pad],
      [Tangent_Pad, Arctangent_Pad, Four_Pad, Five_Pad, Six_Pad, Minus_Pad, Factorial_Pad],
      [Ln_Pad, Exp_Pad, One_Pad, Two_Pad, Three_Pad, Plus_Pad, Pi_Pad],
      [Log_Pad, Euler_Pad, Zero_Pad, Point_Pad, Erase_Pad, Ans_Pad, Equals_Pad]];
   PROGRAMMER_PAD : constant Key_Grid :=
     [[And_Pad, Or_Pad, Xor_Pad, Not_Pad, Clear_Pad, Erase_Pad],
      [Shift_Left_Pad, Shift_Right_Pad, Rotate_Left_Pad, Rotate_Right_Pad, Open_Pad, Close_Pad],
      [A_Pad, B_Pad, Seven_Pad, Eight_Pad, Nine_Pad, Divide_Pad],
      [C_Pad, D_Pad, Four_Pad, Five_Pad, Six_Pad, Times_Pad],
      [E_Pad, F_Pad, One_Pad, Two_Pad, Three_Pad, Minus_Pad],
      [Modulo_Pad, Comma_Pad, Zero_Pad, Ans_Pad, Equals_Pad, Plus_Pad]];

   function Grid (Mode : Calc.Mode_Member) return Key_Grid is
     (case Mode is
         when Calc.Standard => STANDARD_PAD, when Calc.Scientific => SCIENTIFIC_PAD,
         when Calc.Programmer => PROGRAMMER_PAD);
   function Pad_Rows (Mode : Calc.Mode_Member) return Positive is (Grid (Mode)'Length (1));
   function Pad_Columns (Mode : Calc.Mode_Member) return Positive is (Grid (Mode)'Length (2));

   --  Heights of each mode's parts.
   function Pad_Height (Mode : Calc.Mode_Member) return Positive is
     (Pad_Rows (Mode) * BUTTON_HEIGHT + (Pad_Rows (Mode) - 1) * GAP + 2 * MARGIN);
   function Pad_Width (Mode : Calc.Mode_Member) return Positive is
     (Pad_Columns (Mode) * BUTTON_WIDTH + (Pad_Columns (Mode) - 1) * GAP + 2 * MARGIN);
   function Display_Height (Mode : Calc.Mode_Member) return Positive is
     (MARGIN + ENTRY_HEIGHT + GAP + BIG_HEIGHT + SECONDARY_HEIGHT + GAP
      + (if Mode = Calc.Standard then 0 else STRIP_HEIGHT + GAP)
      + (if Mode = Calc.Programmer
         then BASE_ROWS'Length * BASE_ROW_HEIGHT + GAP + BIT_ROWS * BIT_ROW_HEIGHT + MARGIN else 0));
   --  History rows a mode opens with.
   function History_Rows (Mode : Calc.Mode_Member) return Positive is (if Mode = Calc.Programmer then 2 else 3);

   function Size_Of (Mode : Calc.Mode_Member) return Window_Size is
      Fixed : constant Positive := MENU_BAR_HEIGHT + Display_Height (Mode) + Pad_Height (Mode) + STATUS_HEIGHT;
   begin
      return (Width => Pad_Width (Mode), Height => Fixed + History_Rows (Mode) * ROW_HEIGHT,
              Minimum_Width => Pad_Width (Mode), Minimum_Height => Fixed + ROW_HEIGHT);
   end Size_Of;

   ---------------------------------------------------------------------------
   --  The chips above the entry.

   type Chip is
     (Radians_Chip, Degrees_Chip, Gradians_Chip, Bits_8_Chip, Bits_16_Chip, Bits_32_Chip, Bits_64_Chip,
      Unbounded_Chip, Signed_Chip);
   subtype Angle_Chip is Chip range Radians_Chip .. Gradians_Chip;
   subtype Word_Chip is Chip range Bits_8_Chip .. Unbounded_Chip;
   function Angle_Of (Item : Angle_Chip) return Calc.Angle_Member is
     (case Item is
         when Radians_Chip => Calc.Radians, when Degrees_Chip => Calc.Degrees, when Gradians_Chip => Calc.Gradians);
   function Word_Of (Item : Word_Chip) return Calc.Word_Member is
     (case Item is
         when Bits_8_Chip => Calc.Bits_8, when Bits_16_Chip => Calc.Bits_16, when Bits_32_Chip => Calc.Bits_32,
         when Bits_64_Chip => Calc.Bits_64, when Unbounded_Chip => Calc.Unbounded);
   SIGNED_TEXT : constant String := "Signed";
   UNSIGNED_TEXT : constant String := "Unsigned";
   function Strip_Label (Mode : Calc.Mode_Member) return String is
     (case Mode is
         when Calc.Scientific => "Angle", when Calc.Programmer => "Bits", when Calc.Standard => "");

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
   --  The end of a text instead (numbers: their low digits).
   function Tail (Text : String) return String is
     (Text (Text'Last - Natural'Min (Text'Length, SHOWN_CHARACTERS) + 1 .. Text'Last));
   function Head (Text : Calc_History.Text_Access) return String is (if Text = null then "" else Head (Text.all));
   function Tail (Text : Calc_History.Text_Access) return String is (if Text = null then "" else Tail (Text.all));
   function Length (Text : Calc_History.Text_Access) return Natural is (if Text = null then 0 else Text'Length);

   type Shell_State is record
      Host : access Host_Type;
      Config : Settings;
      Choice : CCL_Calc_Options.Choice;
      Field : Editor.Edit_State;
      History : Calc_History.History;
      Places : Calc_Format.Places_Count := 20;
      Fraction_First : Boolean := True;
      --  The live result of the entry (Own: Preview holds it; else the
      --  newest entry's, for an empty entry).
      Preview : Calc_History.Texts;
      Has_Preview, Preview_Own : Boolean := False;
      Failure : Calc_Engine.Failure_Kind := Calc_Engine.No_Failure;
      Failure_Column : Natural := 0;
      --  Up/Down recall: 0 is the entry being typed (Draft holds it).
      Recall : Natural := 0;
      Draft : Text_Access;
      Selected : Natural := 0;
      Scroll : Natural := 0;
      Entry_Scroll : Natural := 0;
      Bounds : Rect := (others => 0);
      Menu_Area, History_Area, Display_Area, Strip_Area, Entry_Area, Big_Area, Secondary_Area, Bases_Area,
        Bits_Area, Pad_Area, Status_Area : Rect := (others => 0);
      Hot_Key, Pressed_Key : Pad_Key := Clear_Pad;
      Hot, Pressed : Boolean := False;
      --  Programmer mode: the bit field has the keyboard; its cursor bit.
      Bit_Focus : Boolean := False;
      Bit_Cursor : Natural := 0;
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
      --  The mode changed: the window should take its size.
      Size_Wanted : Boolean := False;
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

   function Mode (S : Shell_State) return Calc.Mode_Member is (S.Choice.Mode);
   function Programmer (S : Shell_State) return Boolean is (S.Choice.Mode = Calc.Programmer);
   function Engine_Options (S : Shell_State) return Calc_Engine.Options is
     (CCL_Calc_Options.Engine_Options (S.Choice));
   --  Bits the bit field shows.
   function View_Bits (S : Shell_State) return Positive is
     (if Calc_Words.Width (Engine_Options (S).Word) = 0 then UNBOUNDED_BITS
      else Calc_Words.Width (Engine_Options (S).Word));

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

   function Ready_Message (S : Shell_State) return String is
     (case Mode (S) is
         when Calc.Standard => "Enter evaluates, exactly",
         when Calc.Scientific => "Every digit shown is guaranteed",
         when Calc.Programmer => "Tab: the bits by keyboard");

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
      M : constant Calc.Mode_Member := Mode (S);
      Bottom : constant Natural := B.y + B.h;
      Status_Top : constant Natural := (if B.h > STATUS_HEIGHT then Bottom - STATUS_HEIGHT else B.y);
      Pad_Top : constant Natural :=
        (if Status_Top > B.y + Pad_Height (M) then Status_Top - Pad_Height (M) else B.y);
      Display_Top : constant Natural :=
        (if Pad_Top > B.y + MENU_BAR_HEIGHT + Display_Height (M) then Pad_Top - Display_Height (M)
         else B.y + MENU_BAR_HEIGHT);
      Inner : constant Natural := (if B.w > 2 * MARGIN then B.w - 2 * MARGIN else 0);
      Left : constant Natural := B.x + MARGIN;
      Y : Natural := Display_Top + MARGIN;
   begin
      S.Menu_Area := (B.x, B.y, B.w, MENU_BAR_HEIGHT);
      S.Status_Area := (B.x, Status_Top, B.w, Bottom - Status_Top);
      S.Pad_Area := (B.x, Pad_Top, B.w, Status_Top - Pad_Top);
      S.Display_Area := (B.x, Display_Top, B.w, (if Pad_Top > Display_Top then Pad_Top - Display_Top else 0));
      S.History_Area := (B.x, B.y + MENU_BAR_HEIGHT, B.w,
                         (if Display_Top > B.y + MENU_BAR_HEIGHT then Display_Top - B.y - MENU_BAR_HEIGHT else 0));
      if M = Calc.Standard then
         S.Strip_Area := (others => 0);
      else
         S.Strip_Area := (Left, Y, Inner, STRIP_HEIGHT);
         Y := Y + STRIP_HEIGHT + GAP;
      end if;
      S.Entry_Area := (Left, Y, Inner, ENTRY_HEIGHT);
      Y := Y + ENTRY_HEIGHT + GAP;
      S.Big_Area := (Left, Y, Inner, BIG_HEIGHT);
      Y := Y + BIG_HEIGHT;
      S.Secondary_Area := (Left, Y, Inner, SECONDARY_HEIGHT);
      Y := Y + SECONDARY_HEIGHT + GAP;
      if M = Calc.Programmer then
         S.Bases_Area := (Left, Y, Inner, BASE_ROWS'Length * BASE_ROW_HEIGHT);
         Y := Y + S.Bases_Area.h + GAP;
         S.Bits_Area := (Left, Y, Inner, BIT_ROWS * BIT_ROW_HEIGHT);
      else
         S.Bases_Area := (others => 0);
         S.Bits_Area := (others => 0);
      end if;
   end Layout;

   function Key_Area (S : Shell_State; Row, Column : Natural) return Rect is
      Columns : constant Positive := Pad_Columns (Mode (S));
      Inner : constant Natural := (if S.Pad_Area.w > 2 * MARGIN + (Columns - 1) * GAP
                                   then S.Pad_Area.w - 2 * MARGIN - (Columns - 1) * GAP else 0);
      Width : constant Natural := Inner / Columns;
      --  The last column takes what division leaves, so the pad is flush.
      Extra : constant Natural := (if Column = Columns - 1 then Inner - Width * Columns else 0);
   begin
      return (S.Pad_Area.x + MARGIN + Column * (Width + GAP), S.Pad_Area.y + MARGIN + Row * (BUTTON_HEIGHT + GAP),
              Width + Extra, BUTTON_HEIGHT);
   end Key_Area;

   --  Where K is on this mode's pad (empty when it is not there).
   function Key_Area (S : Shell_State; K : Pad_Key) return Rect is
      G : constant Key_Grid := Grid (Mode (S));
   begin
      for Row in G'Range (1) loop
         for Column in G'Range (2) loop
            if G (Row, Column) = K then
               return Key_Area (S, Row, Column);
            end if;
         end loop;
      end loop;
      return (others => 0);
   end Key_Area;

   function Key_Enabled (S : Shell_State; K : Pad_Key) return Boolean is
     (if not Programmer (S) then True
      elsif K in Digit_Pad then Digit_Value (K) < Natural (Base_Of (S.Choice.Base))
      else True);

   function Chip_Visible (S : Shell_State; Item : Chip) return Boolean is
     (case Mode (S) is
         when Calc.Standard => False,
         when Calc.Scientific => Item in Angle_Chip,
         when Calc.Programmer => Item not in Angle_Chip);
   function Chip_Caption (S : Shell_State; Item : Chip) return String is
     (case Item is
         when Radians_Chip => "RAD", when Degrees_Chip => "DEG", when Gradians_Chip => "GRAD",
         when Bits_8_Chip => "8", when Bits_16_Chip => "16", when Bits_32_Chip => "32", when Bits_64_Chip => "64",
         when Unbounded_Chip => "Unbounded",
         when Signed_Chip => (if S.Choice.Signed then SIGNED_TEXT else UNSIGNED_TEXT));
   function Chip_On (S : Shell_State; Item : Chip) return Boolean is
     (case Item is
         when Angle_Chip => S.Choice.Angle = Angle_Of (Item),
         when Word_Chip => S.Choice.Word = Word_Of (Item),
         when Signed_Chip => S.Choice.Signed);

   function Chip_Area (S : Shell_State; Item : Chip) return Rect is
      Strip : constant Rect := S.Strip_Area;
      X : Natural := Strip.x + UI_Text_Width (Strip_Label (Mode (S))) + 2 * CHIP_GAP;
   begin
      if Is_Empty (Strip) or else not Chip_Visible (S, Item) then
         return (others => 0);
      end if;
      if Item = Signed_Chip then
         --  At the strip's right end, as wide as its longer caption.
         declare
            W : constant Natural := Natural'Max (UI_Text_Width (SIGNED_TEXT), UI_Text_Width (UNSIGNED_TEXT))
                                    + 2 * CHIP_PADDING;
         begin
            return (Strip.x + (if Strip.w > W then Strip.w - W else 0), Strip.y, Natural'Min (W, Strip.w), Strip.h);
         end;
      end if;
      for Each in Chip loop
         exit when Each = Item;
         if Chip_Visible (S, Each) and then Each /= Signed_Chip then
            X := X + UI_Text_Width (Chip_Caption (S, Each)) + 2 * CHIP_PADDING + CHIP_GAP;
         end if;
      end loop;
      return (X, Strip.y, UI_Text_Width (Chip_Caption (S, Item)) + 2 * CHIP_PADDING, Strip.h);
   end Chip_Area;

   function Base_Row_Area (S : Shell_State; Base : Calc.Base_Member) return Rect is
   begin
      if Is_Empty (S.Bases_Area) then
         return (others => 0);
      end if;
      for Index in BASE_ROWS'Range loop
         if BASE_ROWS (Index) = Base then
            return (S.Bases_Area.x, S.Bases_Area.y + (Index - BASE_ROWS'First) * BASE_ROW_HEIGHT, S.Bases_Area.w,
                    BASE_ROW_HEIGHT);
         end if;
      end loop;
      return (others => 0);
   end Base_Row_Area;

   function Bit_Cell_Width (S : Shell_State) return Natural is
     (if S.Bits_Area.w > BIT_LABEL_WIDTH + (BITS_PER_ROW / BITS_PER_NIBBLE - 1) * NIBBLE_GAP
      then (S.Bits_Area.w - BIT_LABEL_WIDTH - (BITS_PER_ROW / BITS_PER_NIBBLE - 1) * NIBBLE_GAP) / BITS_PER_ROW
      else 0);

   --  Bit Bit (0 the least significant) of the bit field, highest first.
   function Bit_Area (S : Shell_State; Bit : Natural) return Rect is
      Width : constant Positive := View_Bits (S);
   begin
      if Is_Empty (S.Bits_Area) or else Bit >= Width then
         return (others => 0);
      end if;
      declare
         From_Top : constant Natural := Width - 1 - Bit;
         Row : constant Natural := From_Top / BITS_PER_ROW;
         Column : constant Natural := From_Top mod BITS_PER_ROW;
         Cell : constant Natural := Bit_Cell_Width (S);
      begin
         return (S.Bits_Area.x + BIT_LABEL_WIDTH + Column * Cell + (Column / BITS_PER_NIBBLE) * NIBBLE_GAP,
                 S.Bits_Area.y + Row * BIT_ROW_HEIGHT, Cell, BIT_ROW_HEIGHT);
      end;
   end Bit_Area;

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

   NO_TEXTS : constant Calc_History.Texts := (others => <>);

   --  What the display shows: the entry's live result, or for an empty
   --  entry the newest result.
   function Current (S : Shell_State) return Calc_History.Texts is
     (if S.Has_Preview and then S.Preview_Own then S.Preview
      elsif S.Has_Preview and then Calc_History.Count (S.History) > 0 then Calc_History.Shown (S.History, 1)
      else NO_TEXTS);

   --  An integer's text in Base, as typed back (0xFF, -0b101), no prefix
   --  when Bare; empty when T is no programmer result.
   function Based_Text (T : Calc_History.Texts; Base : Calc.Base_Member; Bare : Boolean := False) return String is
      Digits_Text : constant Calc_History.Text_Access :=
        (case Base is
            when Calc.Decimal => T.Fraction, when Calc.Hexadecimal => T.Hexadecimal,
            when Calc.Octal => T.Octal, when Calc.Binary => T.Binary);
      Shown : constant String := Tail (Digits_Text);
   begin
      if not T.Programmer or else Shown'Length = 0 then
         return "";
      elsif Bare or else Base = Calc.Decimal then
         return Shown;
      elsif Shown (Shown'First) = '-' then
         return "-" & Prefix (Base) & Shown (Shown'First + 1 .. Shown'Last);
      end if;
      return Prefix (Base) & Shown;
   end Based_Text;

   --  The two texts a result shows: the primary (big, or right in a
   --  history row) and the other form, each cut for display.
   function Primary_Of (S : Shell_State; T : Calc_History.Texts) return String is
      Rounded : constant Boolean := T.Form in Calc_Format.Rounded | Calc_Format.Scientific_Rounded;
   begin
      if T.Decimal = null then
         return "";
      elsif T.Programmer and then Programmer (S) then
         return Based_Text (T, S.Choice.Base, Bare => True);
      elsif not T.Exact or else T.Whole or else not S.Fraction_First then
         return Head (T.Decimal) & (if Rounded or else Length (T.Decimal) > SHOWN_CHARACTERS then "..." else "");
      end if;
      return Head (T.Fraction) & (if Length (T.Fraction) > SHOWN_CHARACTERS then "..." else "");
   end Primary_Of;

   function Digit_Count (Text : Calc_History.Text_Access) return Natural is
     (if Text = null or else Text'Length = 0 then 0
      else Text'Length - (if Text (Text'First) = '-' then 1 else 0));

   function Secondary_Of (S : Shell_State; T : Calc_History.Texts) return String is
      Rounded : constant Boolean := T.Form in Calc_Format.Rounded | Calc_Format.Scientific_Rounded;
      Word : constant String := Word_Caption (S.Choice.Word) & (if S.Choice.Signed then " signed" else " unsigned");
   begin
      if T.Decimal = null then
         return "";
      elsif not T.Exact then
         return "approximate: error at most " & Head (T.Bound);
      elsif T.Programmer and then Programmer (S) then
         return (if T.Wrapped then "wrapped into the " & Word & " word" else "exact, " & Word);
      elsif T.Whole then
         return "exact integer" & (if Digit_Count (T.Decimal) > SHOWN_CHARACTERS / 8
                                   then ", " & Image (Digit_Count (T.Decimal)) & " digits" else "");
      elsif S.Fraction_First then
         return Head (T.Decimal) & (if Rounded or else Length (T.Decimal) > SHOWN_CHARACTERS then "..." else "");
      end if;
      return Head (T.Fraction) & (if Length (T.Fraction) > SHOWN_CHARACTERS then "..." else "");
   end Secondary_Of;

   function Preview_Primary (S : Shell_State) return String is
     (if not S.Has_Preview then ""
      elsif Current (S).Decimal = null then "0"
      else Primary_Of (S, Current (S)));

   --  The display shows the entry's value as it is typed; for an empty
   --  entry, the latest result.
   procedure Refresh_Preview (S : in out Shell_State) is
      Text : constant String := Editor.Content (S.Field);
      Result : Calc_Balls.Ball;
      Wrapped : Boolean;
      Position : Calc_Syntax.Column;
   begin
      S.Failure := Calc_Engine.No_Failure;
      S.Failure_Column := 0;
      S.Has_Preview := False;
      S.Preview_Own := False;
      if (for all C of Text => C = ' ') then
         S.Has_Preview := True;
         if Programmer (S) and then Calc_History.Count (S.History) > 0 then
            --  The newest result in this word and sign (they may have
            --  changed since): ans, evaluated now. A result that is no
            --  integer shows as it was.
            Calc_History.Evaluate
              (S.History, Calc_Syntax.PREVIOUS_RESULT_NAME, S.Config.Preview_Budget, Engine_Options (S), Result,
               Wrapped, S.Failure, Position);
            if S.Failure = Calc_Engine.No_Failure then
               Calc_History.Describe (Result, Engine_Options (S), Wrapped, S.Places, S.Preview);
               Calc_Balls.Free (Result);
               S.Preview_Own := True;
            end if;
            S.Failure := Calc_Engine.No_Failure;
         end if;
      else
         Calc_History.Evaluate
           (S.History, Text, S.Config.Preview_Budget, Engine_Options (S), Result, Wrapped, S.Failure, Position);
         if S.Failure = Calc_Engine.No_Failure then
            Calc_History.Describe (Result, Engine_Options (S), Wrapped, S.Places, S.Preview);
            Calc_Balls.Free (Result);
            S.Has_Preview := True;
            S.Preview_Own := True;
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
         Say (S, Ready_Message (S));
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
         Say (S, Ready_Message (S));
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

   --  A digit from the pad. In programmer mode with another base than
   --  decimal, a digit that starts a number brings the base's prefix: the
   --  expression stays the one language (0xFF), the pad types it.
   procedure Insert_Digit (S : in out Shell_State; Digit : String) is
      Text : constant String := Editor.Content (S.Field);
      Before : constant Natural := Natural'Min (Editor.Cursor (S.Field) - 1, Text'Length);
      Previous : constant Character := (if Before > 0 then Text (Text'First + Before - 1) else ' ');
      Starts : constant Boolean :=
        Previous not in '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' | '_' | '.';
   begin
      if Programmer (S) and then S.Choice.Base /= Calc.Decimal and then Starts then
         Insert (S, Prefix (S.Choice.Base) & Digit);
      else
         Insert (S, Digit);
      end if;
   end Insert_Digit;

   procedure Evaluate_Entry (S : in out Shell_State) is
      Text : constant String := Editor.Content (S.Field);
      Based : constant String (1 .. Text'Length) := Text;
      Terms : Calc_Syntax.Term_Array (1 .. Text'Length);
      Count : Natural;
      Error : Calc_Syntax.Syntax_Error;
      Position : Calc_Syntax.Column;
      Previous, Result : Calc_Balls.Ball;
      Wrapped : Boolean;
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
         Engine_Options (S), Result, Wrapped, Failure, Position);
      Calc_Balls.Free (Previous);
      if Failure /= Calc_Engine.No_Failure then
         S.Failure := Failure;
         S.Failure_Column := Position;
         Say (S, Calc_Engine.Message (Failure) & " (column " & Image (Position) & ")", Failure => True);
         Damage (S, S.Display_Area);
         return;
      end if;
      Calc_History.Add (S.History, Based, Terms (1 .. Count), Result, Engine_Options (S), Wrapped, S.Places);
      S.Scroll := 0;
      S.Selected := 0;
      declare
         Added : constant Calc_History.Texts := Calc_History.Shown (S.History, 1);
      begin
         Say (S, (if not Added.Exact then "Approximate: " elsif Added.Wrapped then "Wrapped: " else "Exact: ")
                 & Based);
      end;
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
   --  row's (or the latest) result, exact (Decimal: as the decimal shown;
   --  a programmer result: as typed in the base shown).
   procedure Copy_Result (S : in out Shell_State; Decimal : Boolean) is
      Selection : constant String := Editor.Selected_Text (S.Field);
      Index : constant Natural := (if S.Selected > 0 then S.Selected else 1);
   begin
      if Selection'Length > 0 and then not Decimal then
         Copy (S.Host.all, Selection);
         Say (S, "Copied the selection");
      elsif Calc_History.Count (S.History) >= Index then
         declare
            T : constant Calc_History.Texts := Calc_History.Shown (S.History, Index);
            Text : constant Calc_History.Text_Access :=
              (if Decimal or else not T.Exact then T.Decimal else T.Fraction);
            Shown : constant Natural := Natural'Min (Text'Length, 40);
         begin
            if T.Programmer and then Programmer (S) and then not Decimal then
               Copy (S.Host.all, Based_Text (T, S.Choice.Base));
               Say (S, "Copied " & Based_Text (T, S.Choice.Base));
            else
               Copy (S.Host.all, Text.all);
               Say (S, "Copied " & Text (Text'First .. Text'First + Shown - 1)
                       & (if Text'Length > Shown then "... (" & Image (Text'Length) & " characters)" else ""));
            end if;
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

   --  The options changed: remember them, re-evaluate what is shown.
   procedure Options_Changed (S : in out Shell_State; Message : String) is
   begin
      Save_Options (S.Host.all, S.Choice);
      S.Bit_Cursor := Natural'Min (S.Bit_Cursor, View_Bits (S) - 1);
      Refresh_Preview (S);
      Say (S, Message);
      Damage_All (S);
   end Options_Changed;

   procedure Set_Mode (S : in out Shell_State; Item : Calc.Mode_Member) is
   begin
      if Item = S.Choice.Mode then
         return;
      end if;
      S.Choice.Mode := Item;
      S.Bit_Focus := False;
      S.Hot := False;
      S.Pressed := False;
      Layout (S);
      S.Size_Wanted := True;
      Options_Changed (S, (case Item is
                              when Calc.Standard => "Standard mode",
                              when Calc.Scientific => "Scientific mode",
                              when Calc.Programmer => "Programmer mode"));
   end Set_Mode;

   procedure Set_Angle (S : in out Shell_State; Item : Calc.Angle_Member) is
   begin
      S.Choice.Angle := Item;
      Options_Changed (S, (case Item is
                              when Calc.Radians => "Angles in radians", when Calc.Degrees => "Angles in degrees",
                              when Calc.Gradians => "Angles in gradians"));
   end Set_Angle;

   procedure Set_Word (S : in out Shell_State; Item : Calc.Word_Member) is
   begin
      S.Choice.Word := Item;
      Options_Changed (S, (if Item = Calc.Unbounded then "Unbounded integers" else Word_Caption (Item) & " word"));
   end Set_Word;

   procedure Set_Signed (S : in out Shell_State; Signed : Boolean) is
   begin
      S.Choice.Signed := Signed;
      Options_Changed (S, (if Signed then "Signed (two's complement)" else "Unsigned"));
   end Set_Signed;

   procedure Set_Base (S : in out Shell_State; Item : Calc.Base_Member) is
   begin
      S.Choice.Base := Item;
      Options_Changed (S, "Showing " & Base_Caption (Item)
                          & (if Item = Calc.Decimal then "" else "; the pad types " & Prefix (Item)));
   end Set_Base;

   --  Bit Bit of the shown value flipped: the entry becomes the new value,
   --  as typed in the base shown.
   procedure Toggle_Bit (S : in out Shell_State; Bit : Natural) is
      Value : constant String := Based_Text (Current (S), Calc.Hexadecimal);
      Text : constant String :=
        (if Value'Length = 0 then "0" else "(" & Value & ")") & " xor (1 << " & Image (Bit) & ")";
      Result : Calc_Balls.Ball;
      Wrapped : Boolean;
      Failure : Calc_Engine.Failure_Kind;
      Position : Calc_Syntax.Column;
      Shown : Calc_History.Texts;
   begin
      Calc_History.Evaluate
        (S.History, Text, S.Config.Preview_Budget, Engine_Options (S), Result, Wrapped, Failure, Position);
      if Failure /= Calc_Engine.No_Failure then
         Say (S, Calc_Engine.Message (Failure), Failure => True);
         return;
      end if;
      Calc_History.Describe (Result, Engine_Options (S), Wrapped, S.Places, Shown);
      Calc_Balls.Free (Result);
      Replace_Entry (S, Based_Text (Shown, S.Choice.Base));
      Calc_History.Release (Shown);
      S.Bit_Cursor := Bit;
      Say (S, "Bit " & Image (Bit) & " flipped");
   end Toggle_Bit;

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
         when Mode_Command => Set_Mode (S, Mode_Of (Item));
         when Fraction_First_Command =>
            S.Fraction_First := not S.Fraction_First;
            Refresh_Preview (S);
            Say (S, (if S.Fraction_First then "Fractions first" else "Decimals first"));
            Damage_All (S);
         when Places_Command => Set_Places (S, Places_Of (Item));
         when Angle_Command => Set_Angle (S, Angle_Of (Item));
         when Word_Command => Set_Word (S, Word_Of (Item));
         when Signed_Command => Set_Signed (S, not S.Choice.Signed);
         when Base_Command => Set_Base (S, Base_Of (Item));
         when Help_Command =>
            S.Help_On := not S.Help_On;
            Damage_All (S);
      end case;
   end Run_Command;

   procedure Press_Key (S : in out Shell_State; K : Pad_Key) is
      Changed : Boolean;
   begin
      if not Key_Enabled (S, K) then
         return;
      end if;
      case K is
         when Clear_Pad => Run_Command (S, Clear_Entry_Command);
         when Erase_Pad =>
            Editor.Backspace (S.Field, Changed);
            if Changed then
               Edited (S);
            end if;
         when Equals_Pad => Evaluate_Entry (S);
         when Digit_Pad => Insert_Digit (S, Label (K));
         when others => Insert (S, Inserts (K));
      end case;
   end Press_Key;

   procedure Press_Chip (S : in out Shell_State; Item : Chip) is
   begin
      case Item is
         when Angle_Chip => Set_Angle (S, Angle_Of (Item));
         when Word_Chip => Set_Word (S, Word_Of (Item));
         when Signed_Chip => Set_Signed (S, not S.Choice.Signed);
      end case;
   end Press_Chip;

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
                                 when Word_Command | Signed_Command | Base_Command => Programmer (S),
                                 when Places_Command | Fraction_First_Command => not Programmer (S),
                                 when others => True),
                  Checked => (case C is
                                 when Mode_Command => Mode (S) = Mode_Of (C),
                                 when Fraction_First_Command => S.Fraction_First,
                                 when Places_Command => S.Places = Places_Of (C),
                                 when Angle_Command => S.Choice.Angle = Angle_Of (C),
                                 when Word_Command => S.Choice.Word = Word_Of (C),
                                 when Signed_Command => S.Choice.Signed,
                                 when Base_Command => S.Choice.Base = Base_Of (C),
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

   --  The bit field's keys (programmer mode, after Tab): arrows move, Space
   --  or Enter flips, Esc or Tab gives the keyboard back to the entry.
   procedure Bit_Key (S : in out Shell_State; Item : Event; Handled : out Boolean) is
      Width : constant Positive := View_Bits (S);
   begin
      Handled := True;
      case Item.Pressed is
         when Left => S.Bit_Cursor := Natural'Min (S.Bit_Cursor + 1, Width - 1);
         when Right => S.Bit_Cursor := (if S.Bit_Cursor > 0 then S.Bit_Cursor - 1 else 0);
         when Up => S.Bit_Cursor := Natural'Min (S.Bit_Cursor + BITS_PER_ROW, Width - 1);
         when Down => S.Bit_Cursor := (if S.Bit_Cursor >= BITS_PER_ROW then S.Bit_Cursor - BITS_PER_ROW else 0);
         when Home => S.Bit_Cursor := Width - 1;
         when End_Key => S.Bit_Cursor := 0;
         when Enter => Toggle_Bit (S, S.Bit_Cursor);
         when Escape | Tab =>
            S.Bit_Focus := False;
            Say (S, Ready_Message (S));
         when others => Handled := False;
      end case;
      Damage (S, S.Bits_Area);
      Damage (S, S.Entry_Area);
   end Bit_Key;

   procedure Handle_Key (S : in out Shell_State; Item : Event) is
      Changed : Boolean;
      Handled : Boolean;
   begin
      Menu_Key (S, Item, Handled);
      if Handled then
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
            when '1' => Set_Mode (S, Calc.Standard);
            when '2' => Set_Mode (S, Calc.Scientific);
            when '3' => Set_Mode (S, Calc.Programmer);
            when 'u' => Set_Angle (S, Next (S.Choice.Angle));
            when 'w' => if Programmer (S) then Set_Word (S, Next (S.Choice.Word)); end if;
            when 'b' => if Programmer (S) then Set_Base (S, Next (S.Choice.Base)); end if;
            when 's' => if Programmer (S) then Set_Signed (S, not S.Choice.Signed); end if;
            when others => null;
         end case;
         if Item.Pressed in Left | Right then
            Editor.Move (S.Field, (if Item.Pressed = Left then Editor.Move_Word_Left else Editor.Move_Word_Right),
                         Item.Shift);
            Damage (S, S.Display_Area);
         end if;
         return;
      end if;
      if S.Bit_Focus then
         Bit_Key (S, Item, Handled);
         if Handled then
            return;
         end if;
         S.Bit_Focus := False;
         Damage (S, S.Bits_Area);
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
         when Tab =>
            if Programmer (S) then
               S.Bit_Focus := True;
               Say (S, "Bits: arrows move, Space flips, Esc returns");
               Damage (S, S.Bits_Area);
               Damage (S, S.Entry_Area);
            end if;
         when F1 => Run_Command (S, Help_Command);
         when others => null;
      end case;
   end Handle_Key;

   function Key_At (S : Shell_State; X, Y : Natural; Found : out Boolean) return Pad_Key is
      G : constant Key_Grid := Grid (Mode (S));
   begin
      Found := False;
      if Point_In_Rect (X, Y, S.Pad_Area) then
         for Row in G'Range (1) loop
            for Column in G'Range (2) loop
               if Point_In_Rect (X, Y, Key_Area (S, Row, Column)) then
                  Found := True;
                  return G (Row, Column);
               end if;
            end loop;
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
      Found := Found and then Key_Enabled (S, K);
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
               return;
            end if;
            for C in Chip loop
               if Point_In_Rect (Item.X, Item.Y, Chip_Area (S, C)) then
                  Press_Chip (S, C);
                  return;
               end if;
            end loop;
            if Programmer (S) then
               for B of BASE_ROWS loop
                  if Point_In_Rect (Item.X, Item.Y, Base_Row_Area (S, B)) then
                     Set_Base (S, B);
                     return;
                  end if;
               end loop;
               for Bit in 0 .. View_Bits (S) - 1 loop
                  if Point_In_Rect (Item.X, Item.Y, Bit_Area (S, Bit)) then
                     Toggle_Bit (S, Bit);
                     Damage (S, S.Bits_Area);
                     return;
                  end if;
               end loop;
            end if;
            if Point_In_Rect (Item.X, Item.Y, S.Entry_Area) then
               declare
                  Text : constant String := Editor.Content (S.Field);
                  Offset : constant Integer := Integer (Item.X) - Integer (S.Entry_Area.x + ENTRY_INSET)
                    + Integer (S.Entry_Scroll);
                  Boundary : constant Natural :=
                    (if Offset <= 0 then 0 else CuBit.Fonts.Boundary_At (CuBit.Fonts.Sans, Text, Offset));
               begin
                  S.Bit_Focus := False;
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
      Accepted, Found : Boolean;
      Remembered : CCL_Calc_Options.Choice;
   begin
      S.State := new Shell_State;
      S.State.Host := Host;
      S.State.Config := Config;
      S.State.Places := Config.Places;
      Load_Options (Host.all, Remembered, Found);
      if Found then
         S.State.Choice := Remembered;
      end if;
      Calc_History.Create (S.State.History, Config.History_Capacity);
      Editor.Initialize (S.State.Field, "", Accepted);
      S.State.Paste_Buffer := new String (1 .. Editor.MAX_TEXT_LENGTH);
      Prepare_Menu_Texts;
      Menu_Popup_Width := Positive'Max (MENU_MINIMUM_WIDTH, Menus.Required_Width (Build_Menus (S.State.all)));
      S.State.Menu_Model := Build_Menus (S.State.all);
      Say (S.State.all, Ready_Message (S.State.all));
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
            elsif St.Bit_Focus and then Item.Typed = ' ' then
               Toggle_Bit (St, St.Bit_Cursor);
               Damage (St, St.Bits_Area);
            elsif Item.Typed in ' ' .. '~' then
               if St.Help_On then
                  St.Help_On := False;
                  Damage_All (St);
               end if;
               if St.Bit_Focus then
                  St.Bit_Focus := False;
                  Damage (St, St.Bits_Area);
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

   procedure Take_Size_Request (S : in out Shell; Size : out Window_Size; Wanted : out Boolean) is
   begin
      Size := Size_Of (S.State.Choice.Mode);
      Wanted := S.State.Size_Wanted;
      S.State.Size_Wanted := False;
   end Take_Size_Request;

   function Options (S : Shell) return CCL_Calc_Options.Choice is (S.State.Choice);
   function Quit_Requested (S : Shell) return Boolean is (S.State.Quit);
   function Entry_Text (S : Shell) return String is (Editor.Content (S.State.Field));
   function Preview_Text (S : Shell) return String is (Preview_Primary (S.State.all));
   function Preview_Bound (S : Shell) return String is (Head (Current (S.State.all).Bound));
   function Preview_In (S : Shell; Base : Calc.Base_Member) return String is
     (Based_Text (Current (S.State.all), Base, Bare => True));
   function Preview_Wrapped (S : Shell) return Boolean is (Current (S.State.all).Wrapped);
   function Preview_Failure (S : Shell) return Calc_Engine.Failure_Kind is (S.State.Failure);
   function History_Count (S : Shell) return Natural is (Calc_History.Count (S.State.History));
   function History_Source (S : Shell; Index : Positive) return String is
     (Calc_History.Source (S.State.History, Index).all);
   function History_Result (S : Shell; Index : Positive) return String is
     (if Calc_History.Shown (S.State.History, Index).Exact then Head (Calc_History.Shown (S.State.History, Index).Fraction)
      else Head (Calc_History.Shown (S.State.History, Index).Decimal));
   function Status_Message (S : Shell) return String is (S.State.Status (1 .. S.State.Status_Length));
   function Places (S : Shell) return Calc_Format.Places_Count is (S.State.Places);
   function Menu_Bar (S : Shell) return Menus.Model is (Build_Menus (S.State.all));
   function Open_Menu (S : Shell) return Natural is (Menus.Open_Menu (S.State.Menu));
   function Help_Shown (S : Shell) return Boolean is (S.State.Help_On);
   function Bit_Focus (S : Shell) return Boolean is (S.State.Bit_Focus);

   function Button_Area (S : Shell; Label : String) return Rect is
   begin
      for K in Pad_Key loop
         if Calc_Shells.Label (K) = Label and then not Is_Empty (Key_Area (S.State.all, K)) then
            return Key_Area (S.State.all, K);
         end if;
      end loop;
      return (others => 0);
   end Button_Area;

   function Bit_Area (S : Shell; Bit : Natural) return Rect is (Bit_Area (S.State.all, Bit));

   function Chip_Area (S : Shell; Caption : String) return Rect is
   begin
      for C in Chip loop
         if Chip_Caption (S.State.all, C) = Caption then
            return Chip_Area (S.State.all, C);
         end if;
      end loop;
      return (others => 0);
   end Chip_Area;

   function Base_Row_Area (S : Shell; Base : Calc.Base_Member) return Rect is (Base_Row_Area (S.State.all, Base));

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

   --  A key symbol (Calc_Symbols) centred in Face, rasterized at the
   --  canvas's physical resolution: its stroke whole physical pixels, its
   --  box's parity the stroke's, so straight strokes land on pixels.
   procedure Draw_Symbol (C : Canvas; Face : Rect; Item : Calc_Symbols.Symbol; Ink : Color) is
      N : constant Positive := C.densityNumerator;
      D : constant Positive := C.densityDenominator;
      Box_Size : constant Positive :=
        (if Item in Calc_Symbols.Erase_Symbol | Calc_Symbols.Root_Symbol then WIDE_SYMBOL_BOX else SYMBOL_BOX);
      Stroke : constant Positive :=
        Positive'Max (1, (STROKE_NUMERATOR * N * 2 + STROKE_DENOMINATOR * D) / (2 * STROKE_DENOMINATOR * D));
      Rounded : constant Positive := Positive'Max (1, (Box_Size * N + D / 2) / D);
      Edge : constant Calc_Symbols.Edge :=
        Natural'Min (Calc_Symbols.MAXIMUM_EDGE, (if (Rounded - Stroke) mod 2 = 0 then Rounded else Rounded + 1));
      Map : Calc_Symbols.Coverage_Map (0 .. Edge - 1, 0 .. Edge - 1);
      Pixels : ARGB_Bitmap (0 .. Edge - 1, 0 .. Edge - 1);
      Box : constant Rect :=
        (Face.x + (if Face.w > Box_Size then (Face.w - Box_Size) / 2 else 0),
         Face.y + (if Face.h > Box_Size then (Face.h - Box_Size) / 2 else 0), Box_Size, Box_Size);
   begin
      Calc_Symbols.Rasterize
        (Item, Natural'Min (Calc_Symbols.Stroke_Width'Last, Stroke * Calc_Symbols.SUBPIXEL), Map);
      for Row in Map'Range (1) loop
         for Column in Map'Range (2) loop
            Pixels (Row, Column) := Shift_Left (Color (Map (Row, Column)), ALPHA_SHIFT) or (Ink and RGB_MASK);
         end loop;
      end loop;
      Draw_Bitmap_Native (With_Clip (C, Face), Box, Pixels);
   end Draw_Symbol;

   --  Approximately equal: two tildes, one above the other.
   procedure Draw_Approximately (C : Canvas; X, Y : Natural; Ink : Color) is
   begin
      Draw_UI_Text_Transparent (C, X, (if Y > 3 then Y - 3 else 0), "~", Ink);
      Draw_UI_Text_Transparent (C, X, Y + 3, "~", Ink);
   end Draw_Approximately;
   APPROXIMATELY_WIDTH : constant := 14;

   procedure Draw_Key (S : Shell_State; C : Canvas; K : Pad_Key; Box : Rect; Colors : Theme) is
      Enabled : constant Boolean := Key_Enabled (S, K);
      Down : constant Boolean := S.Pressed and then S.Pressed_Key = K;
      Hot : constant Boolean := Enabled and then S.Hot and then S.Hot_Key = K;
      Night : constant Boolean := Dark (Colors);
      Base : constant Color :=
        (if K = Equals_Pad then APP_COLOR
         elsif K in Operator_Pad then Mix (Colors.face, APP_COLOR, TINT_SHARE)
         elsif K in Large_Pad then Mix (Colors.field, Colors.face, (if Night then 30 else 35))
         else Mix (Colors.face, Colors.panel, 50));
      Lit : constant Color := (if Hot then Mix (Base, WHITE, (if Night then 12 else 30)) else Base);
      Top : constant Color := (if Down then Mix (Lit, Colors.shadow, 18) else Mix (Lit, WHITE, (if Night then 6 else 28)));
      Bottom : constant Color := (if Down then Mix (Lit, WHITE, 4) else Mix (Lit, Colors.shadow, 10));
      Strong : constant Color :=
        (if K = Equals_Pad then WHITE
         elsif K = Clear_Pad then Colors.danger
         elsif K in Operator_Pad then Mix (Colors.text, APP_COLOR, (if Night then 35 else 55))
         elsif K in Bitwise_Pad | Function_Pad then Mix (Colors.text, APP_COLOR, (if Night then 20 else 30))
         else Colors.text);
      Ink : constant Color := (if Enabled then Strong else Mix (Colors.muted, Base, 45));
      Edge : constant Color := Mix (Colors.edge, Colors.shadow, (if Night then 40 else 25));
      Text : constant String := Caption (K, Mode (S));
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
         if Has_Symbol (K) then
            Draw_Symbol (C, Face, Symbol_Of (K), Ink);
         elsif K in Large_Pad then
            declare
               Big : constant Canvas := Scaled (C, Face, KEY_NUMERATOR, KEY_DENOMINATOR);
               W : constant Natural := UI_Text_Width (Text);
               Inner_W : constant Natural := Face.w * KEY_DENOMINATOR / KEY_NUMERATOR;
               Inner_H : constant Natural := Face.h * KEY_DENOMINATOR / KEY_NUMERATOR;
            begin
               Draw_UI_Text_Transparent
                 (Big, (if Inner_W > W then (Inner_W - W) / 2 else 0),
                  (if Inner_H > UI_Text_Height then (Inner_H - UI_Text_Height) / 2 else 0), Text, Ink);
            end;
         else
            declare
               W : constant Natural := UI_Text_Width (Text);
            begin
               Draw_UI_Text_Transparent
                 (C, Face.x + (if Face.w > W then (Face.w - W) / 2 else 0),
                  Face.y + (if Face.h > UI_Text_Height then (Face.h - UI_Text_Height) / 2 else 0), Text, Ink);
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
            Hint1 : constant String :=
              (case Mode (S) is
                  when Calc.Standard => "Exact: 1/3 + 1/6 is 1/2, not 0.4999",
                  when Calc.Scientific => "Every digit shown is guaranteed",
                  when Calc.Programmer => "Integers in a machine word");
            Hint2 : constant String :=
              (case Mode (S) is
                  when Calc.Standard => "Try  2^100,  100!,  gcd(12, 18)",
                  when Calc.Scientific => "Try  sin(30) in DEG,  ln(2),  e^pi",
                  when Calc.Programmer => "Try  0xFF + 1,  ~0,  1 << 63");
            Hint3 : constant String :=
              (if Mode (S) = Calc.Programmer then "Tab: flip bits by keyboard" else "F1 lists every key");
            Mid : constant Natural := Area.y + Area.h / 2;
            procedure Centered (Text : String; Y : Natural; Ink : Color) is
               Shown : constant String := Fit_UI_Text (Text, Area.w);
               W : constant Natural := UI_Text_Width (Shown);
            begin
               Draw_UI_Text_Transparent
                 (With_Clip (C, Area), Area.x + (if Area.w > W then (Area.w - W) / 2 else 0), Y, Shown, Ink);
            end Centered;
         begin
            if Area.h > 3 * HELP_LINE then
               Centered (Hint1, Mid - 3 * HELP_LINE / 2, Colors.muted);
               Centered (Hint2, Mid - HELP_LINE / 2, Mix (Colors.muted, APP_COLOR, 45));
               Centered (Hint3, Mid + HELP_LINE / 2, Colors.muted);
            elsif Area.h > HELP_LINE then
               Centered (Hint2, Mid - HELP_LINE / 2, Mix (Colors.muted, APP_COLOR, 45));
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
                  T : constant Calc_History.Texts := Calc_History.Shown (S.History, Index);
                  Approximate : constant Boolean := not T.Exact;
                  Based : constant Boolean := T.Programmer and then Programmer (S);
                  Width : constant Natural := (if Row.w > 2 * ROW_INSET then Row.w - 2 * ROW_INSET else 0);
               begin
                  Fill_Rect (C, Row, Back);
                  Fill_Rect (C, (Row.x, Row.y + Row.h - 1, Row.w, 1), Rule);
                  if Selected then
                     Fill_Rect (C, (Row.x, Row.y, 3, Row.h - 1), APP_COLOR);
                  end if;
                  --  The expression, as typed, then the result right-aligned.
                  Draw_Code_Text_Transparent
                    (Clip, Row.x + ROW_INSET, Row.y + 4, Fit_Code_Text (Calc_History.Source (S.History, Index).all, Width),
                     Colors.muted);
                  declare
                     Room : constant Natural :=
                       (if Width > APPROXIMATELY_WIDTH then Width - APPROXIMATELY_WIDTH else 0);
                     Shown : constant String :=
                       (if Based then Fit_UI_Text_Tail (Based_Text (T, S.Choice.Base), Room)
                        else Fit_UI_Text (Primary_Of (S, T), Room));
                     W : constant Natural := UI_Text_Width (Shown);
                     X : constant Natural := Row.x + Row.w - ROW_INSET - W;
                     Y : constant Natural := Row.y + Row.h - UI_Text_Height - 5;
                     --  The other form; for a long integer, how long it is;
                     --  for an approximation, its bound.
                     Other : constant String :=
                       (if Approximate then "within " & Head (T.Bound)
                        elsif Based then (if T.Wrapped then "wrapped" else "")
                        elsif T.Whole then (if UI_Text_Width (Head (T.Decimal)) > Width or else
                                               Length (T.Decimal) > SHOWN_CHARACTERS
                                            then Image (Digit_Count (T.Decimal)) & " digits"
                                            else "")
                        else Secondary_Of (S, T));
                     Left_Room : constant Natural := (if X > Row.x + 2 * ROW_INSET + APPROXIMATELY_WIDTH
                                                      then X - Row.x - 2 * ROW_INSET - APPROXIMATELY_WIDTH else 0);
                  begin
                     Draw_UI_Text_Transparent (Clip, X, Y, Shown, Colors.text);
                     if Approximate and then X > APPROXIMATELY_WIDTH then
                        Draw_Approximately (Clip, X - APPROXIMATELY_WIDTH, Y, Mix (Colors.text, APP_COLOR, 60));
                     end if;
                     if (T.Whole and then not Approximate) and then Other'Length > 0 then
                        --  A long integer's length or a wrap, on the
                        --  expression's line.
                        Draw_UI_Text_Transparent
                          (Clip, Row.x + Row.w - ROW_INSET - UI_Text_Width (Other), Row.y + 4, Other,
                           Mix (Colors.muted, APP_COLOR, 35));
                     elsif Other'Length > 0 and then Left_Room > 40 then
                        declare
                           Fitted : constant String := Fit_UI_Text (Other, Left_Room);
                           OW : constant Natural := UI_Text_Width (Fitted);
                           Left : constant Natural :=
                             X - OW - ROW_INSET - (if Approximate then APPROXIMATELY_WIDTH else 0);
                        begin
                           Draw_UI_Text_Transparent (Clip, Left, Y, Fitted, Mix (Colors.muted, Back, 15));
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
      Frame : constant Color := (if S.Bit_Focus then Mix (Colors.edge, Colors.field, 30) else APP_COLOR);
   begin
      --  Keep the caret in view.
      if Caret < S.Entry_Scroll then
         S.Entry_Scroll := Caret;
      elsif Caret > S.Entry_Scroll + Inner then
         S.Entry_Scroll := Caret - Inner;
      end if;
      Fill_Rect (C, Box, Colors.field);
      Stroke_Rect (C, Box, Frame, Frame);
      declare
         Area : constant Rect := (Box.x + ENTRY_INSET, Box.y + 2, Inner, (if Box.h > 4 then Box.h - 4 else 0));
         Clip : constant Canvas := With_Clip (C, Area);
         X0 : constant Integer := Integer (Area.x) - Integer (S.Entry_Scroll);
         function At_X (Offset : Natural) return Natural is
           (Natural (Integer'Max (0, X0 + Integer (Offset))));
         Placeholder : constant String :=
           (case Mode (S) is
               when Calc.Standard => "Type an expression, such as 2/3 + 1/6",
               when Calc.Scientific => "Type an expression, such as sin(pi/6)",
               when Calc.Programmer => "Type an expression, such as 0xF0 | 0b1010");
      begin
         if From < To then
            Fill_Rect (Clip, (At_X (From), Text_Y, To - From, UI_Text_Height), Mix (Colors.field, APP_COLOR, 30));
         end if;
         if Text'Length = 0 then
            Draw_UI_Text_Transparent (Clip, Area.x, Text_Y, Fit_UI_Text (Placeholder, Area.w), Colors.muted);
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
         if not S.Bit_Focus then
            Fill_Rect (Clip, (At_X (Caret), Text_Y - 1, 2, UI_Text_Height + 2), APP_COLOR);
         end if;
      end;
   end Draw_Entry;

   procedure Draw_Strip (S : Shell_State; C : Canvas; Colors : Theme; Back : Color) is
      Strip : constant Rect := S.Strip_Area;
      Label_Text : constant String := Strip_Label (Mode (S));
      Text_Y : constant Natural := Strip.y + (if Strip.h > UI_Text_Height then (Strip.h - UI_Text_Height) / 2 else 0);
   begin
      if Is_Empty (Strip) then
         return;
      end if;
      Draw_UI_Text_Transparent (C, Strip.x, Text_Y, Label_Text, Colors.muted);
      for Item in Chip loop
         if Chip_Visible (S, Item) then
            declare
               Box : constant Rect := Chip_Area (S, Item);
               On : constant Boolean := Chip_On (S, Item);
               Text : constant String := Chip_Caption (S, Item);
               Fill : constant Color := (if On then APP_COLOR else Mix (Back, Colors.field, 50));
               Ink : constant Color := (if On then WHITE else Colors.text);
               W : constant Natural := UI_Text_Width (Text);
            begin
               Fill_Rect (C, Box, Fill);
               Stroke_Rect (C, Box, Mix (Fill, Colors.edge, 40), Mix (Fill, Colors.edge, 40));
               Draw_UI_Text_Transparent
                 (C, Box.x + (if Box.w > W then (Box.w - W) / 2 else 0), Text_Y, Text, Ink);
            end;
         end if;
      end loop;
   end Draw_Strip;

   procedure Draw_Bases (S : Shell_State; C : Canvas; Colors : Theme; Back : Color) is
      T : constant Calc_History.Texts := Current (S);
   begin
      for B of BASE_ROWS loop
         declare
            Row : constant Rect := Base_Row_Area (S, B);
            Chosen : constant Boolean := B = S.Choice.Base;
            Value : constant String := Based_Text (T, B, Bare => True);
            Room : constant Natural := (if Row.w > BASE_LABEL_WIDTH + ROW_INSET then Row.w - BASE_LABEL_WIDTH - ROW_INSET
                                        else 0);
            Shown : constant String := Fit_UI_Text_Tail (Value, Room);
            W : constant Natural := UI_Text_Width (Shown);
            Text_Y : constant Natural := Row.y + (if Row.h > UI_Text_Height then (Row.h - UI_Text_Height) / 2 else 0);
         begin
            if Chosen then
               Fill_Rect (C, Row, Mix (Back, APP_COLOR, 18));
               Fill_Rect (C, (Row.x, Row.y, 2, Row.h), APP_COLOR);
            end if;
            Draw_Code_Text_Transparent
              (C, Row.x + ROW_INSET / 2, Text_Y, Base_Caption (B), (if Chosen then APP_COLOR else Colors.muted));
            Draw_UI_Text_Transparent
              (With_Clip (C, Row), Row.x + Row.w - W - ROW_INSET / 2, Text_Y, Shown,
               (if Chosen then Colors.text else Mix (Colors.text, Back, 25)));
         end;
      end loop;
   end Draw_Bases;

   procedure Draw_Bits (S : Shell_State; C : Canvas; Colors : Theme; Back : Color) is
      T : constant Calc_History.Texts := Current (S);
      Width : constant Positive := View_Bits (S);
      --  The word's bits, the most significant first (zeros without a
      --  programmer result). An unbounded value shows its low 64 bits.
      Bits : constant String := Tail (T.Bits);
      Live : constant Boolean := T.Programmer and then Bits'Length = Width;
      One : constant Color := Mix (Back, APP_COLOR, 28);
   begin
      for Row in 0 .. (Width + BITS_PER_ROW - 1) / BITS_PER_ROW - 1 loop
         declare
            High : constant Natural := Width - 1 - Row * BITS_PER_ROW;
            Label_Text : constant String := Image (High);
            First : constant Rect := Bit_Area (S, High);
            LW : constant Natural := Code_Text_Width (Label_Text);
         begin
            Draw_Code_Text_Transparent
              (C, S.Bits_Area.x + (if BIT_LABEL_WIDTH > LW + 4 then BIT_LABEL_WIDTH - LW - 4 else 0),
               First.y + (if First.h > Code_Text_Height then (First.h - Code_Text_Height) / 2 else 0), Label_Text,
               Colors.muted);
         end;
      end loop;
      for Bit in 0 .. Width - 1 loop
         declare
            Cell : constant Rect := Bit_Area (S, Bit);
            Set_Bit : constant Boolean := Live and then Bits (Bits'Last - Bit) = '1';
            Digit : constant String := (if Set_Bit then "1" else "0");
            W : constant Natural := Code_Text_Width (Digit);
         begin
            if Set_Bit then
               Fill_Rect (C, (Cell.x + 1, Cell.y + 1, Natural'Max (Cell.w, 2) - 2, Natural'Max (Cell.h, 2) - 2), One);
            end if;
            if S.Bit_Focus and then Bit = S.Bit_Cursor then
               Stroke_Rect (C, Cell, APP_COLOR, APP_COLOR);
            end if;
            Draw_Code_Text_Transparent
              (C, Cell.x + (if Cell.w > W then (Cell.w - W) / 2 else 0),
               Cell.y + (if Cell.h > Code_Text_Height then (Cell.h - Code_Text_Height) / 2 else 0), Digit,
               (if Set_Bit then Colors.text elsif Live then Mix (Colors.muted, Back, 20)
                else Mix (Colors.muted, Back, 50)));
         end;
      end loop;
   end Draw_Bits;

   procedure Draw_Display (S : in out Shell_State; C : Canvas; Colors : Theme) is
      Area : constant Rect := S.Display_Area;
      Back : constant Color := Mix (Colors.panel, APP_COLOR, SUBTLE_SHARE / 2);
      T : constant Calc_History.Texts := Current (S);
   begin
      Fill_Rect (C, Area, Back);
      Fill_Rect (C, (Area.x, Area.y, Area.w, 1), Mix (Colors.edge, APP_COLOR, 30));
      Draw_Strip (S, C, Colors, Back);
      Draw_Entry (S, C, Colors);
      declare
         Big : constant Rect := S.Big_Area;
         View : constant Canvas := Scaled (C, Big, BIG_NUMERATOR, BIG_DENOMINATOR);
         Inner_W : constant Natural := Big.w * BIG_DENOMINATOR / BIG_NUMERATOR;
         Text : constant String := Preview_Primary (S);
         Room : constant Natural := (if Inner_W > APPROXIMATELY_WIDTH then Inner_W - APPROXIMATELY_WIDTH else 0);
         Based : constant Boolean := T.Programmer and then Programmer (S);
         Shown : constant String := (if Based then Fit_UI_Text_Tail (Text, Room) else Fit_UI_Text (Text, Room));
         W : constant Natural := UI_Text_Width (Shown);
         Waiting : constant Boolean := S.Failure /= Calc_Engine.No_Failure;
         Ink : constant Color := (if Editor.Length (S.Field) = 0 then Mix (Colors.text, Back, 35) else Colors.text);
      begin
         if not Waiting then
            Draw_UI_Text_Transparent (View, (if Inner_W > W then Inner_W - W else 0), 0, Shown, Ink);
            if T.Decimal /= null and then not T.Exact and then Inner_W > W + APPROXIMATELY_WIDTH then
               Draw_Approximately (View, Inner_W - W - APPROXIMATELY_WIDTH, 0, Mix (Ink, APP_COLOR, 60));
            end if;
         end if;
      end;
      declare
         Line : constant Rect := S.Secondary_Area;
         Message : constant String :=
           (if S.Failure = Calc_Engine.No_Failure then Secondary_Of (S, T)
            elsif Incomplete (S) then "..."
            else Calc_Engine.Message (S.Failure));
         Shown : constant String := Fit_UI_Text (Message, Line.w);
         W : constant Natural := UI_Text_Width (Shown);
         Ink : constant Color :=
           (if S.Failure /= Calc_Engine.No_Failure and then not Incomplete (S) then Colors.danger
            elsif T.Wrapped and then S.Failure = Calc_Engine.No_Failure then Mix (Colors.danger, Colors.muted, 40)
            else Colors.muted);
      begin
         Draw_UI_Text_Transparent (C, Line.x + (if Line.w > W then Line.w - W else 0), Line.y, Shown, Ink);
      end;
      if Programmer (S) then
         Draw_Bases (S, C, Colors, Back);
         Draw_Bits (S, C, Colors, Back);
      end if;
   end Draw_Display;

   procedure Draw_Help (S : Shell_State; C : Canvas; Colors : Theme) is
      LINES : constant := 21;
      function Line_Text (K : Positive) return String is
        (case K is
            when 1 => "Keys",
            when 2 => "Enter  evaluate      Esc  clear",
            when 3 => "Up / Down  earlier expressions",
            when 4 => "Ctrl+C  copy    Ctrl+Shift+C  decimal",
            when 5 => "Ctrl+1 2 3  Standard, Scientific,",
            when 6 => "            Programmer",
            when 7 => "Ctrl+U  angle unit   Ctrl+D  fractions",
            when 8 => "Ctrl+W  word   Ctrl+B  base   Ctrl+S  sign",
            when 9 => "Tab  the bit field (Programmer)",
            when 11 => "Expressions",
            when 12 => "+ - * / ^ mod ! ( )  ans pi e",
            when 13 => "sqrt root(x, n) abs floor ceil round",
            when 14 => "sin cos tan asin acos atan ln log exp",
            when 15 => "& | xor ~ << >> rotl(x, n) rotr(x, n)",
            when 16 => "0x1F 0o17 0b101   1.5e-3   -2^2 = -4",
            when 18 => "Exact unless marked approximate (~);",
            when 19 => "then every digit shown is within the",
            when 20 => "stated error bound.",
            when 21 => "CCL: (calc ""1/3""), calc.evaluate-with",
            when others => "");
      HEADINGS : constant array (1 .. LINES) of Boolean := [1 | 11 => True, others => False];
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
           (Clip, Box.x + MARGIN, Box.y + MARGIN + (K - 1) * HELP_LINE,
            Fit_UI_Text (Line_Text (K), (if Box.w > 2 * MARGIN then Box.w - 2 * MARGIN else 0)),
            (if HEADINGS (K) then APP_COLOR else Colors.text));
      end loop;
   end Draw_Help;

   function Status_Right (S : Shell_State) return String is
     (case Mode (S) is
         when Calc.Standard => Image (S.Places) & " places",
         when Calc.Scientific => Angle_Caption (S.Choice.Angle) & "  " & Image (S.Places) & " places",
         when Calc.Programmer =>
            Word_Caption (S.Choice.Word) & (if S.Choice.Signed then " signed  " else " unsigned  ")
            & Base_Caption (S.Choice.Base));

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
         declare
            G : constant Key_Grid := Grid (Mode (St));
         begin
            for Row in G'Range (1) loop
               for Column in G'Range (2) loop
                  if Touches (Key_Area (St, Row, Column)) then
                     Draw_Key (St, C, G (Row, Column), Key_Area (St, Row, Column), Colors);
                  end if;
               end loop;
            end loop;
         end;
      end if;
      if Touches (St.Status_Area) then
         Draw_Status_Bar (C, St.Status_Area, Colors, St.Status (1 .. St.Status_Length), Status_Right (St));
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
