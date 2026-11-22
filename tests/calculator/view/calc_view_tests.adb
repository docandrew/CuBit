------------------------------------------------------------------------------
--  Hosted tests of the Calculator's window (docs/calculator.md, "Tests"):
--  scripted keys, text and clicks into Calc_Shells, frames rendered into
--  memory with the real toolkit and fonts, at 100%, 150% and 200%
--  density. --screens writes PPM frames (run.sh --screens turns them into
--  PNG). Linux-hosted: the clipboard and Config are in memory and there is
--  no desktop (a mode's size request is applied here as the desktop would).
------------------------------------------------------------------------------
with Ada.Command_Line;
with Ada.Environment_Variables;
with Ada.Streams.Stream_IO;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Appearance;
with CuBit.UI;
with CuBit.UI.Theme_CCL;
with Calc_Engine;
with Calc_Host_Memory;
with Calc_Shell; use Calc_Shell;
with Calc_Symbols;

procedure Calc_View_Tests is
   use type Calc_Engine.Failure_Kind;
   use type Calc.Mode_Member;
   use type Calc.Angle_Member;
   use type Calc.Word_Member;
   use type Calc.Base_Member;
   BYTES_PER_PIXEL : constant := 4;
   CHANNEL_BITS : constant := 8;
   CHANNEL_MASK : constant := 16#FF#;
   --  Room for the largest mode at the largest density tested.
   LARGEST_DENSITY : constant := 2;
   BUFFER_WIDTH : constant := 600 * LARGEST_DENSITY;
   BUFFER_HEIGHT : constant := 800 * LARGEST_DENSITY;
   --  The first Calculator's window (APP-002): the compact one is smaller.
   FIRST_WIDTH : constant := 420;
   FIRST_HEIGHT : constant := 660;
   Screens : constant Boolean :=
     Ada.Command_Line.Argument_Count >= 1 and then Ada.Command_Line.Argument (1) = "--screens";

   type Pixels is array (Natural range <>) of Unsigned_32;
   type Pixels_Access is access Pixels;
   Image : constant Pixels_Access := new Pixels'(0 .. BUFFER_WIDTH * BUFFER_HEIGHT - 1 => 0);
   --  The window's logical size and the density (Numerator / Denominator).
   Width, Height : Positive := 1;
   Numerator, Denominator : Positive := 1;

   function Physical (Logical : Natural) return Natural is ((Logical * Numerator + Denominator - 1) / Denominator);

   function Whole return CuBit.UI.Canvas is
     ((addr => Image.all'Address, width => Width, height => Height,
       pitch => Physical (Width) * BYTES_PER_PIXEL, densityNumerator => Numerator,
       densityDenominator => Denominator, others => <>));

   Host : aliased Calc_Host_Memory.Host;
   App : Shell;
   Passed, Failed : Natural := 0;
   Clock_Ms : Unsigned_64 := 1_000;

   procedure Check (Condition : Boolean; What : String) is
   begin
      if Condition then
         Passed := Passed + 1;
      else
         Failed := Failed + 1;
         Put_Line ("FAIL: " & What);
      end if;
   end Check;

   procedure Frame is
      Area : CuBit.UI.Rect;
   begin
      Take_Damage (App, Area);
      if not CuBit.UI.Is_Empty (Area) then
         Render (App, CuBit.UI.With_Clip (Whole, Area));
      end if;
   end Frame;

   procedure Resize (W, H : Positive) is
   begin
      Width := W;
      Height := H;
      Set_Bounds (App, (0, 0, W, H));
      Frame;
   end Resize;

   --  The desktop's part of a mode switch: the window takes the mode's size.
   procedure Follow_Mode is
      Size : Window_Size;
      Wanted : Boolean;
   begin
      Take_Size_Request (App, Size, Wanted);
      if Wanted then
         Resize (Size.Width, Size.Height);
      end if;
   end Follow_Mode;

   procedure Save_PPM (Path : String) is
      use Ada.Streams;
      File : Ada.Streams.Stream_IO.File_Type;
      W : constant Natural := Physical (Width);
      H : constant Natural := Physical (Height);
      Header : constant String :=
        "P6" & ASCII.LF & Natural'Image (W) & Natural'Image (H) & ASCII.LF & "255" & ASCII.LF;
      Row : Stream_Element_Array (1 .. Stream_Element_Offset (W * 3));
   begin
      Ada.Streams.Stream_IO.Create (File, Ada.Streams.Stream_IO.Out_File, Path);
      for C of Header loop
         Stream_Element_Array'Write (Ada.Streams.Stream_IO.Stream (File), [1 => Stream_Element (Character'Pos (C))]);
      end loop;
      for Y in 0 .. H - 1 loop
         for X in 0 .. W - 1 loop
            declare
               P : constant Unsigned_32 := Image (Y * W + X);
               Base : constant Stream_Element_Offset := Stream_Element_Offset (X * 3);
            begin
               Row (Base + 1) := Stream_Element (Shift_Right (P, 2 * CHANNEL_BITS) and CHANNEL_MASK);
               Row (Base + 2) := Stream_Element (Shift_Right (P, CHANNEL_BITS) and CHANNEL_MASK);
               Row (Base + 3) := Stream_Element (P and CHANNEL_MASK);
            end;
         end loop;
         Stream_Element_Array'Write (Ada.Streams.Stream_IO.Stream (File), Row);
      end loop;
      Ada.Streams.Stream_IO.Close (File);
   end Save_PPM;

   --  Everything repainted at a density.
   procedure Repaint_At (N, D : Positive) is
   begin
      Numerator := N;
      Denominator := D;
      Handle (App, (Kind => Resize_Event, others => <>));
      Frame;
   end Repaint_At;

   procedure Shot (Name : String) is
   begin
      Frame;
      if Screens then
         Save_PPM ("build/screen-" & Name & ".ppm");
      end if;
   end Shot;

   --  The frame at 100%, then at 150% and 200% (Name-150, Name-200).
   procedure Shots (Name : String) is
   begin
      Shot (Name);
      if Screens then
         Repaint_At (3, 2);
         Save_PPM ("build/screen-" & Name & "-150.ppm");
         Repaint_At (2, 1);
         Save_PPM ("build/screen-" & Name & "-200.ppm");
         Repaint_At (1, 1);
      end if;
   end Shots;

   --  system.ccl's themes (run.sh exports them from ccl-config's plan).
   procedure Install_Themes is
   begin
      for Each in CuBit.Appearance.Color_Scheme loop
         declare
            use type CuBit.Appearance.Color_Scheme;
            Name : constant String :=
              (if Each = CuBit.Appearance.Alloy_Light then "CALC_THEME_LIGHT" else "CALC_THEME_DARK");
            Fallback : constant CuBit.UI.Theme := CuBit.UI.Default_Palette (Each);
            Loaded : CuBit.UI.Theme_CCL.Result;
         begin
            CuBit.UI.Install_Palette (Each, Fallback);
            if Ada.Environment_Variables.Exists (Name)
              and then Ada.Environment_Variables.Value (Name)'Length in 1 .. CuBit.UI.Theme_CCL.Maximum_Source
            then
               CuBit.UI.Theme_CCL.Load (Ada.Environment_Variables.Value (Name), Fallback, Loaded);
               if Loaded.Success then
                  CuBit.UI.Install_Palette (Each, Loaded.Value);
               end if;
            end if;
         end;
      end loop;
   end Install_Themes;

   procedure Press (K : Key; Control, Shift : Boolean := False) is
   begin
      Handle (App, Key_Press (K, Control, Shift));
      Pump (App);
      Frame;
   end Press;

   procedure Chord (Letter : Character; Shift : Boolean := False) is
   begin
      Handle (App, (Kind => Key_Event, Pressed => No_Key, Typed => Letter, Control => True, Shift => Shift,
                    others => <>));
      Pump (App);
      Follow_Mode;
      Frame;
   end Chord;

   procedure Type_Text (S : String) is
   begin
      for C of S loop
         Handle (App, Calc_Shell.Text (C));
      end loop;
      Frame;
   end Type_Text;

   procedure Evaluate (S : String) is
   begin
      Type_Text (S);
      Press (Enter);
   end Evaluate;

   procedure Click_At (Box : CuBit.UI.Rect) is
      X : constant Natural := Box.x + Box.w / 2;
      Y : constant Natural := Box.y + Box.h / 2;
   begin
      Clock_Ms := Clock_Ms + 1_000;
      Handle (App, (Kind => Pointer_Event, Action => Move, X => X, Y => Y, Time_Ms => Clock_Ms, others => <>));
      Handle (App, (Kind => Pointer_Event, Action => Calc_Shell.Press, X => X, Y => Y, Time_Ms => Clock_Ms,
                    others => <>));
      Frame;
      Handle (App, (Kind => Pointer_Event, Action => Release, X => X, Y => Y, Time_Ms => Clock_Ms, others => <>));
      Follow_Mode;
      Frame;
   end Click_At;

   procedure Click (Label : String) is
      Box : constant CuBit.UI.Rect := Button_Area (App, Label);
   begin
      Check (not CuBit.UI.Is_Empty (Box), "the pad has " & Label);
      Click_At (Box);
   end Click;

   procedure Chip (Caption : String) is
      Box : constant CuBit.UI.Rect := Chip_Area (App, Caption);
   begin
      Check (not CuBit.UI.Is_Empty (Box), "the strip has " & Caption);
      Click_At (Box);
   end Chip;

   function Settings return Calc_Shell.Settings is
     ((Result_Budget => (Budget => 32_768, Precision => 50),
       Preview_Budget => (Budget => 2_048, Precision => 50),
       History_Capacity => 1_000, Places => 20));

   --  The symbols: whole-pixel strokes land on whole pixels (no partial
   --  rows in a horizontal stroke), centred, nothing clipped at the box.
   procedure Symbol_Checks is
      use Calc_Symbols;
   begin
      for Scale in 1 .. 3 loop
         declare
            Stroke : constant Positive := Scale + 1;
            Size : constant Positive := 14 * Scale + (if (14 * Scale - Stroke) mod 2 = 0 then 0 else 1);
            Map : Coverage_Map (0 .. Size - 1, 0 .. Size - 1);
            Full, Partial : Natural := 0;
            Middle : constant Natural := Size / 2;
         begin
            Rasterize (Minus_Symbol, Stroke * SUBPIXEL, Map);
            for Row in Map'Range (1) loop
               if Map (Row, Middle) = OPAQUE then
                  Full := Full + 1;
               elsif Map (Row, Middle) > 0 then
                  Partial := Partial + 1;
               end if;
            end loop;
            Check (Full = Stroke and then Partial = 0,
                   "minus at" & Size'Image & ": a" & Stroke'Image & "-pixel stroke, sharp (" & Full'Image
                   & Partial'Image & ")");
            for Item in Symbol loop
               Rasterize (Item, Stroke * SUBPIXEL, Map);
               declare
                  Edge_Ink, Left_Ink, Right_Ink : Natural := 0;
               begin
                  for K in 0 .. Size - 1 loop
                     Edge_Ink := Edge_Ink + Map (0, K) + Map (Size - 1, K) + Map (K, 0) + Map (K, Size - 1);
                  end loop;
                  for Row in Map'Range (1) loop
                     for Column in 0 .. Size / 2 - 1 loop
                        Left_Ink := Left_Ink + Map (Row, Column);
                        Right_Ink := Right_Ink + Map (Row, Size - 1 - Column);
                     end loop;
                  end loop;
                  Check (Edge_Ink < OPAQUE * Stroke, Item'Image & " at" & Size'Image & " stays inside its box");
                  if Item in Plus_Symbol | Minus_Symbol | Times_Symbol | Divide_Symbol | Equals_Symbol then
                     Check (Left_Ink = Right_Ink, Item'Image & " at" & Size'Image & " is symmetric:" & Left_Ink'Image
                            & Right_Ink'Image);
                  end if;
               end;
            end loop;
         end;
      end loop;
   end Symbol_Checks;
begin
   Install_Themes;
   CuBit.UI.Set_Color_Scheme (CuBit.Appearance.Alloy_Light);
   Symbol_Checks;

   --  The compact window: noticeably smaller than the first Calculator's.
   declare
      Standard_Size : constant Window_Size := Size_Of (Calc.Standard);
      Scientific_Size : constant Window_Size := Size_Of (Calc.Scientific);
      Programmer_Size : constant Window_Size := Size_Of (Calc.Programmer);
   begin
      Check (Standard_Size.Width * Standard_Size.Height * 10 <= FIRST_WIDTH * FIRST_HEIGHT * 6,
             "Standard opens at most 60% of the first window's area:" & Standard_Size.Width'Image
             & Standard_Size.Height'Image);
      Check (Scientific_Size.Width > Standard_Size.Width and then Programmer_Size.Height > Standard_Size.Height,
             "Scientific grows wider, Programmer taller");
      Check (Standard_Size.Minimum_Width <= Standard_Size.Width
             and then Standard_Size.Minimum_Height < Standard_Size.Height, "minima below the designed size");
      Width := Standard_Size.Width;
      Height := Standard_Size.Height;
   end;

   Initialize (App, Host'Unchecked_Access, Settings);
   Check (Options (App).Mode = Calc.Standard, "nothing remembered: Standard");
   Resize (Width, Height);
   Shots ("empty");
   Check (Preview_Text (App) = "0", "an empty calculator shows 0: " & Preview_Text (App));

   --  Live preview as it is typed.
   Type_Text ("2/3 + 1/6");
   Check (Preview_Text (App) = "5/6", "live preview of 2/3 + 1/6: " & Preview_Text (App));
   Shot ("typing");
   Press (Enter);
   Check (History_Count (App) = 1 and then History_Source (App, 1) = "2/3 + 1/6"
          and then History_Result (App, 1) = "5/6", "Enter records 2/3 + 1/6 = 5/6");
   Check (Entry_Text (App) = "", "Enter clears the entry");
   Evaluate ("2^100");
   Evaluate ("sqrt(2)");
   Check (History_Result (App, 1)'Length > 10 and then History_Result (App, 1) (1 .. 10) = "1.41421356",
          "sqrt(2) is approximate, its guaranteed digits: " & History_Result (App, 1));
   Evaluate ("1/7");
   Evaluate ("ans * 7");
   Check (History_Result (App, 1) = "1", "ans is the exact previous result: 1/7 * 7 = " & History_Result (App, 1));
   Evaluate ("100!");
   Check (History_Count (App) = 6, "six entries");

   --  Errors: incomplete input waits quietly, a real one is named.
   Type_Text ("2 +");
   Check (Preview_Failure (App) = Calc_Engine.Expected_Operand, "2 + is incomplete");
   Press (Escape);
   Type_Text ("1/(3-3)");
   Check (Preview_Failure (App) = Calc_Engine.Division_By_Zero, "1/(3-3) is a division by zero");
   Shot ("error");
   Press (Enter);
   Check (History_Count (App) = 6 and then Entry_Text (App) = "1/(3-3)", "a failed Enter keeps the entry");
   Press (Escape);
   Check (Entry_Text (App) = "", "Esc clears the entry");
   Check (Status_Message (App) = "Enter evaluates, exactly",
          "the failure leaves the status line with the entry: " & Status_Message (App));

   --  Recall: Up brings back earlier expressions, Down returns.
   Type_Text ("draft");
   Press (Up);
   Check (Entry_Text (App) = "100!", "Up recalls the newest: " & Entry_Text (App));
   Press (Up);
   Check (Entry_Text (App) = "ans * 7", "Up again: " & Entry_Text (App));
   Press (Down);
   Press (Down);
   Check (Entry_Text (App) = "draft", "Down past the newest returns the draft: " & Entry_Text (App));
   Press (Escape);

   --  The pad: 7 * 6 = 42 by pointer.
   Click ("7");
   Click ("*");
   Click ("6");
   Check (Entry_Text (App) = "7*6", "the pad types: " & Entry_Text (App));
   Shots ("pad");
   Click ("=");
   Check (History_Result (App, 1) = "42", "the pad's = evaluates: " & History_Result (App, 1));
   Click ("sqrt");
   Click ("2");
   Click (")");
   Check (Entry_Text (App) = "sqrt(2)", "sqrt opens a call: " & Entry_Text (App));
   Click ("del");
   Check (Entry_Text (App) = "sqrt(2", "del erases: " & Entry_Text (App));
   Click ("C");
   Check (Entry_Text (App) = "", "C clears");

   --  The clipboard: Ctrl+C copies the exact result, Ctrl+Shift+C the decimal.
   Evaluate ("1/3");
   Chord ('c');
   Check (Calc_Host_Memory.Clipboard (Host) = "1/3", "Ctrl+C copies 1/3: " & Calc_Host_Memory.Clipboard (Host));
   Chord ('c', Shift => True);
   Check (Calc_Host_Memory.Clipboard (Host) = "0.(3)", "Ctrl+Shift+C copies 0.(3): "
          & Calc_Host_Memory.Clipboard (Host));
   Calc_Host_Memory.Set_Clipboard (Host, "22/7");
   Chord ('v');
   Check (Entry_Text (App) = "22/7", "Ctrl+V pastes: " & Entry_Text (App));
   Press (Enter);

   --  View: places and fractions first.
   Chord ('d');
   Shot ("decimals-first");
   Chord ('d');
   Press (F10);
   Check (Open_Menu (App) = 1, "F10 opens the Calculator menu");
   Press (Right);
   Press (Right);
   Check (Open_Menu (App) = 3, "the View menu");
   Shots ("menu");
   Press (Escape);
   Check (Open_Menu (App) = 0, "Esc closes it");
   Press (F1);
   Check (Help_Shown (App), "F1 shows the keys and functions");
   Shots ("help");
   Press (Escape);
   Check (not Help_Shown (App), "Esc hides them");
   Shots ("history");

   --  Long results stay fast to draw and correct to copy.
   Evaluate ("2^100000");
   Chord ('c');
   Check (Calc_Host_Memory.Clipboard (Host)'Length = 30_103, "2^100000 copies all 30,103 digits:"
          & Natural'Image (Calc_Host_Memory.Clipboard (Host)'Length));
   Shot ("big");

   ---------------------------------------------------------------------------
   --  Scientific: Ctrl+2, the angle unit, approximations with their bound.
   Chord ('2');
   Check (Options (App).Mode = Calc.Scientific, "Ctrl+2: Scientific");
   Check (Width = Size_Of (Calc.Scientific).Width, "the window took Scientific's size");
   Check (Calc_Host_Memory.Stored_Options (Host) =
            "(Calc_Options mode => Calc_Mode.Scientific angle => Calc_Angle.Radians word => Calc_Word.Bits_64"
            & " signed => true base => Calc_Base.Decimal)",
          "the mode is remembered as a typed Calc_Options: " & Calc_Host_Memory.Stored_Options (Host));
   Type_Text ("sin(1)");
   Check (Preview_Bound (App)'Length > 0 and then Preview_Text (App)'Length > 10
          and then Preview_Text (App) (1 .. 10) = "0.84147098", "sin(1) radians, approximate: " & Preview_Text (App)
          & " within " & Preview_Bound (App));
   Shots ("scientific");
   Press (Escape);
   Chip ("DEG");
   Check (Options (App).Angle = Calc.Degrees, "the DEG chip");
   Type_Text ("sin(30)");
   Check (Preview_Text (App) = "1/2" and then Preview_Bound (App) = "", "sin 30 degrees is exactly 1/2: "
          & Preview_Text (App));
   Press (Escape);
   Click ("cos");
   Click ("6");
   Click ("0");
   Click (")");
   Check (Entry_Text (App) = "cos(60)" and then Preview_Text (App) = "1/2", "the cos key: " & Entry_Text (App)
          & " = " & Preview_Text (App));
   Press (Enter);
   Chord ('u');
   Check (Options (App).Angle = Calc.Gradians, "Ctrl+U: the next angle unit");
   Click ("ln");
   Type_Text ("2)");
   Check (Preview_Text (App)'Length > 8 and then Preview_Text (App) (1 .. 8) = "0.693147",
          "the ln key: " & Preview_Text (App));
   Press (Enter);
   Click ("pi");
   Shots ("scientific-pi");
   Press (Enter);
   Check (History_Result (App, 1)'Length > 8 and then History_Result (App, 1) (1 .. 8) = "3.141592",
          "pi from the pad: " & History_Result (App, 1));
   Chip ("RAD");

   ---------------------------------------------------------------------------
   --  Programmer: Ctrl+3, words, signs, bases, the bit field.
   Chord ('3');
   Check (Options (App).Mode = Calc.Programmer and then Height = Size_Of (Calc.Programmer).Height,
          "Ctrl+3: Programmer, at its size");
   Chip ("8");
   Check (Options (App).Word = Calc.Bits_8, "the 8 chip");
   Type_Text ("0xFF + 1");
   Check (Preview_In (App, Calc.Decimal) = "0" and then Preview_Wrapped (App),
          "0xFF + 1 wraps to 0 in a signed 8-bit word: " & Preview_In (App, Calc.Decimal));
   Press (Escape);
   Type_Text ("-1");
   Check (Preview_In (App, Calc.Hexadecimal) = "FF" and then Preview_In (App, Calc.Binary) = "11111111"
          and then Preview_In (App, Calc.Octal) = "377", "-1 in 8 bits: FF 377 11111111");
   Chip ("Signed");
   Check (not Options (App).Signed and then Preview_In (App, Calc.Decimal) = "255", "unsigned, -1 is 255: "
          & Preview_In (App, Calc.Decimal));
   Chip ("Unsigned");
   Press (Escape);
   Chip ("16");
   Click_At (Base_Row_Area (App, Calc.Hexadecimal));
   Check (Options (App).Base = Calc.Hexadecimal, "the HEX row shows hexadecimal");
   Click ("F");
   Click ("F");
   Click ("&");
   Click ("A");
   Check (Entry_Text (App) = "0xFF & 0xA", "the pad types prefixed digits: " & Entry_Text (App));
   Check (Preview_In (App, Calc.Hexadecimal) = "000A", "0xFF & 0xA = 000A: " & Preview_In (App, Calc.Hexadecimal));
   Press (Escape);
   Click_At (Base_Row_Area (App, Calc.Decimal));
   Click ("A");
   Check (Entry_Text (App) = "", "A is off in decimal");
   Type_Text ("5");
   Click_At (Bit_Area (App, 3));
   Check (Entry_Text (App) = "13", "clicking bit 3 of 5 gives 13: " & Entry_Text (App));
   Shots ("programmer");
   Press (Tab);
   Check (Bit_Focus (App), "Tab gives the bit field the keyboard");
   Press (Left);
   Handle (App, Calc_Shell.Text (' '));
   Frame;
   Check (Entry_Text (App) = "29", "Left then Space flips bit 4: " & Entry_Text (App));
   Press (Escape);
   Check (not Bit_Focus (App), "Esc gives it back");
   Press (Escape);
   Type_Text ("1 << 15");
   Check (Preview_In (App, Calc.Decimal) = "-32768", "1 << 15 is -32768 in a signed 16-bit word: "
          & Preview_In (App, Calc.Decimal));
   Press (Enter);
   Click ("rotl");
   Type_Text ("ans, 1)");
   Check (Preview_In (App, Calc.Decimal) = "1", "rotl(-32768, 1) = 1 in 16 bits: " & Preview_In (App, Calc.Decimal));
   Press (Enter);
   Chord ('w');
   Check (Options (App).Word = Calc.Bits_32, "Ctrl+W: the next word size");
   Chord ('b');
   Check (Options (App).Base = Calc.Hexadecimal, "Ctrl+B: the next base");
   Chord ('b');
   Chord ('b');
   Check (Options (App).Base = Calc.Binary, "Ctrl+B twice more: binary");
   Shots ("programmer-binary");

   --  The remembered options: a new window starts as this one left off.
   declare
      Stored : constant String := Calc_Host_Memory.Stored_Options (Host);
      Again : Shell;
   begin
      Check (Stored = "(Calc_Options mode => Calc_Mode.Programmer angle => Calc_Angle.Radians word => Calc_Word.Bits_32"
                      & " signed => true base => Calc_Base.Binary)", "remembered: " & Stored);
      Initialize (Again, Host'Unchecked_Access, Settings);
      Check (Options (Again).Mode = Calc.Programmer and then Options (Again).Word = Calc.Bits_32
             and then Options (Again).Base = Calc.Binary, "a new window starts in the remembered mode");
      Calc_Host_Memory.Set_Stored_Options (Host, "(Calc_Options mode => Calc_Mode.Programmer)");
      Initialize (Again, Host'Unchecked_Access, Settings);
      Check (Options (Again).Mode = Calc.Standard, "a non-canonical stored value is refused: Standard");
      Calc_Host_Memory.Set_Stored_Options (Host, Stored);
   end;

   CuBit.UI.Set_Color_Scheme (CuBit.Appearance.Alloy_Dark);
   Handle (App, (Kind => Resize_Event, others => <>));
   Shots ("programmer-dark");
   Chord ('1');
   Check (Options (App).Mode = Calc.Standard and then Width = Size_Of (Calc.Standard).Width,
          "Ctrl+1: back to Standard, at its size");
   Type_Text ("(1 + sqrt(5)) / 2");
   Shots ("dark");
   Chord ('2');
   Shots ("scientific-dark");
   Chord ('1');
   CuBit.UI.Set_Color_Scheme (CuBit.Appearance.Alloy_Light);
   Chord ('l');
   Check (History_Count (App) = 0, "Ctrl+L clears the history");

   Put_Line ("calculator window:" & Passed'Image & " checks passed," & Failed'Image & " failed");
   if Failed > 0 then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Calc_View_Tests;
