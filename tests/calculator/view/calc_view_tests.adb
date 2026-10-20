------------------------------------------------------------------------------
--  Hosted tests of the Calculator's window (docs/calculator.md, "Tests"):
--  scripted keys, text and clicks into Calc_Shells, frames rendered into
--  memory with the real toolkit and fonts. --screens writes PPM frames
--  (run.sh --screens turns them into PNG). Linux-hosted: the clipboard is
--  in memory and there is no desktop.
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

procedure Calc_View_Tests is
   use type Calc_Engine.Failure_Kind;
   WIDTH : constant := 420;
   HEIGHT : constant := 660;
   BYTES_PER_PIXEL : constant := 4;
   CHANNEL_BITS : constant := 8;
   CHANNEL_MASK : constant := 16#FF#;
   Screens : constant Boolean :=
     Ada.Command_Line.Argument_Count >= 1 and then Ada.Command_Line.Argument (1) = "--screens";

   type Pixels is array (Natural range <>) of Unsigned_32;
   type Pixels_Access is access Pixels;
   Image : constant Pixels_Access := new Pixels'(0 .. WIDTH * HEIGHT - 1 => 0);
   Whole : constant CuBit.UI.Canvas :=
     (addr => Image.all'Address, width => WIDTH, height => HEIGHT, pitch => WIDTH * BYTES_PER_PIXEL, others => <>);
   Bounds : constant CuBit.UI.Rect := (0, 0, WIDTH, HEIGHT);

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

   procedure Save_PPM (Path : String) is
      use Ada.Streams;
      File : Ada.Streams.Stream_IO.File_Type;
      Header : constant String :=
        "P6" & ASCII.LF & Natural'Image (WIDTH) & Natural'Image (HEIGHT) & ASCII.LF & "255" & ASCII.LF;
      Row : Stream_Element_Array (1 .. Stream_Element_Offset (WIDTH * 3));
   begin
      Ada.Streams.Stream_IO.Create (File, Ada.Streams.Stream_IO.Out_File, Path);
      for C of Header loop
         Stream_Element_Array'Write (Ada.Streams.Stream_IO.Stream (File), [1 => Stream_Element (Character'Pos (C))]);
      end loop;
      for Y in 0 .. HEIGHT - 1 loop
         for X in 0 .. WIDTH - 1 loop
            declare
               P : constant Unsigned_32 := Image (Y * WIDTH + X);
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

   procedure Shot (Name : String) is
   begin
      Frame;
      if Screens then
         Save_PPM ("build/screen-" & Name & ".ppm");
      end if;
   end Shot;

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

   procedure Click (Label : String) is
      Box : constant CuBit.UI.Rect := Button_Area (App, Label);
      X : constant Natural := Box.x + Box.w / 2;
      Y : constant Natural := Box.y + Box.h / 2;
   begin
      Clock_Ms := Clock_Ms + 1_000;
      Handle (App, (Kind => Pointer_Event, Action => Move, X => X, Y => Y, Time_Ms => Clock_Ms, others => <>));
      Handle (App, (Kind => Pointer_Event, Action => Calc_Shell.Press, X => X, Y => Y, Time_Ms => Clock_Ms,
                    others => <>));
      Frame;
      Handle (App, (Kind => Pointer_Event, Action => Release, X => X, Y => Y, Time_Ms => Clock_Ms, others => <>));
      Frame;
   end Click;
begin
   Install_Themes;
   CuBit.UI.Set_Color_Scheme (CuBit.Appearance.Alloy_Light);
   Initialize
     (App, Host'Unchecked_Access,
      (Result_Budget => (Budget => 32_768, Precision => 50),
       Preview_Budget => (Budget => 2_048, Precision => 50),
       History_Capacity => 1_000, Places => 20));
   Set_Bounds (App, Bounds);
   Shot ("empty");
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
   Check (Status_Message (App) = "Type an expression; Enter evaluates it exactly",
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
   Shot ("pad");
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
   Shot ("menu");
   Press (Escape);
   Check (Open_Menu (App) = 0, "Esc closes it");
   Press (F1);
   Check (Help_Shown (App), "F1 shows the keys and functions");
   Shot ("help");
   Press (Escape);
   Check (not Help_Shown (App), "Esc hides them");
   Shot ("history");

   --  Long results stay fast to draw and correct to copy.
   Evaluate ("2^100000");
   Chord ('c');
   Check (Calc_Host_Memory.Clipboard (Host)'Length = 30_103, "2^100000 copies all 30,103 digits:"
          & Natural'Image (Calc_Host_Memory.Clipboard (Host)'Length));
   Shot ("big");

   CuBit.UI.Set_Color_Scheme (CuBit.Appearance.Alloy_Dark);
   Handle (App, (Kind => Resize_Event, others => <>));
   Type_Text ("(1 + sqrt(5)) / 2");
   Shot ("dark");
   Chord ('l');
   Check (History_Count (App) = 0, "Ctrl+L clears the history");

   Put_Line ("calculator window:" & Passed'Image & " checks passed," & Failed'Image & " failed");
   if Failed > 0 then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Calc_View_Tests;
