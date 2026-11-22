with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_Display_Topology; use Intel_GPU_Display_Topology;
with Intel_GPU_Cursor_Program; use Intel_GPU_Cursor_Program;
with Intel_GPU_Native_Cursor_Program;
--  Hosted: register values against the i915 v6.16 layout, and the native
--  writer against a memory page standing in for the MMIO page.
procedure Cursor_Program_Tests is
   Checks : Natural := 0;
   procedure Check (Condition : Boolean; Name : String) is
   begin
      if not Condition then
         Ada.Text_IO.Put_Line ("FAIL " & Name);
         raise Program_Error with Name;
      end if;
      Checks := Checks + 1;
   end Check;
   V : Register_Values;
begin
   --  Known words: 64x64 ARGB on ADL (version 13) is 0x10000027, as i915
   --  writes MCURSOR_MODE_64_ARGB_AX | MCURSOR_ARB_SLOTS(1).
   V := Encode ((Width_64, 64, 16#0010_0000#), 0, 0, 13, False);
   Check (V.Control = 16#1000_0027# and V.FBC_Control = 0 and
          V.Base = 16#0010_0000# and V.Position_Word = 0, "64 square adl");
   V := Encode ((Width_256, 256, 16#0002_0000#), 0, 0, 12, True);
   Check (V.Control = 16#23#, "256 on tgl has no arbitration workaround");
   V := Encode ((Width_128, 24, 16#1000#), 0, 0, 13, False);
   Check (V.Control = 16#1000_0022# and V.FBC_Control = 16#8000_0017#,
          "non-square uses CUR_FBC_CTL height - 1");
   Check (Encode_Position (-1, -1) = 16#8001_8001#, "sign-magnitude -1,-1");
   Check (Encode_Position (5, -7) = 16#8007_0005#, "mixed signs");
   Check (Encode_Position (Position'Last, 0) = 16#7FFF#, "largest x");
   for X in Position range -300 .. 300 loop
      for Y in Position'(-3) .. 3 loop
         Check (Decode_X (Encode_Position (X, Y)) = X and then
                Decode_Y (Encode_Position (X, Y)) = Y, "position round trip");
      end loop;
   end loop;
   Check (Valid_Image ((Width_64, 64, 4096), False) and then
          not Valid_Image ((Width_64, 64, 4096), True) and then
          Valid_Image ((Width_64, 64, 65536), True), "base alignment");
   Check (not Valid_Image ((Width_64, 65, 4096), False), "taller than wide");
   Check (not Valid_Image ((Width_64, 64, 0), False), "null base");
   Check (Valid_Position ((Width_64, 64, 4096), -63, -63, 1920, 1080) and then
          not Valid_Position ((Width_64, 64, 4096), -64, 0, 1920, 1080) and then
          not Valid_Position ((Width_64, 64, 4096), 1920, 0, 1920, 1080),
          "never entirely off the pipe");
   Check (Offset (A, Control) = 16#70080# and Offset (A, Base) = 16#70084# and
          Offset (A, Position_Field) = 16#70088# and
          Offset (A, FBC_Control) = 16#700A0# and Offset (D, Control) = 16#73080#,
          "register offsets");
   Check (Full_Update (Full_Update'Last) = Base and Move_Update (Move_Update'Last) = Base,
          "base write arms every update");
   declare
      type Page_Words is array (0 .. 1023) of Unsigned_32;
      Page_Memory : Page_Words := [others => 16#DEAD_BEEF#] with Alignment => 4096;
      Held : Boolean := False;
      function Power_Held return Boolean is (Held);
      package Writer is new Intel_GPU_Native_Cursor_Program
        (Page_Memory'Address, Power_Held);
      use type Writer.Outcome;
      Values : constant Register_Values :=
        Encode ((Width_64, 32, 16#0004_0000#), -10, 20, 13, False);
   begin
      Check (Writer.Program (False, Values) = Writer.Rejected and Writer.Writes = 0,
             "not owner");
      Check (Writer.Program (True, Values) = Writer.Power_Unavailable and
             Writer.Writes = 0, "no power");
      Held := True;
      Check (Writer.Program (True, Values) = Writer.Written and Writer.Writes = 4,
             "full update writes four registers");
      Check (Page_Memory (16#80# / 4) = Values.Control and
             Page_Memory (16#84# / 4) = Values.Base and
             Page_Memory (16#88# / 4) = Values.Position_Word and
             Page_Memory (16#A0# / 4) = Values.FBC_Control, "values in place");
      Check (Page_Memory (16#8C# / 4) = 16#DEAD_BEEF#, "no other register");
      Check (Writer.Move (True, Encode ((Width_64, 32, 16#0004_0000#), 1, 2, 13, False))
               = Writer.Written and Writer.Writes = 6, "move writes two");
      Check (Page_Memory (16#88# / 4) = Encode_Position (1, 2), "moved");
      Check (Writer.Program (True, Disabled) = Writer.Written and
             Page_Memory (16#80# / 4) = 0, "disable");
   end;
   Ada.Text_IO.Put_Line ("PASS intel cursor program:" & Checks'Image & " checks");
end Cursor_Program_Tests;
