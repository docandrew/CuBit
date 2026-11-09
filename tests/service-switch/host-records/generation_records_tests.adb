--  Hosted tests of system generation records (System_Generation_Records):
--  the text ccl-remote writes checks against its CCL type with the same
--  checker the device reads it back with (CCL.Typed_Values), and what is
--  not a generation is refused. Run: tests/service-switch/run.sh.
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces;
with CCL.Typed_Values;
with System_Generation_Records;
with System_Generation_Records.Reading;

procedure Generation_Records_Tests is
   package R renames System_Generation_Records;
   use type R.Digest;
   use type Interfaces.Unsigned_32;
   Failures : Natural := 0;

   procedure Check (Condition : Boolean; What : String) is
   begin
      Put_Line ((if Condition then "PASS: " else "FAIL: ") & What);
      if not Condition then
         Failures := Failures + 1;
      end if;
   end Check;

   function Checks (Source : String) return Boolean is
      Result : CCL.Typed_Values.Check_Result;
   begin
      CCL.Typed_Values.Check (R.Schema, R.Root_Name, Source, Result);
      if not Result.Success then
         Put_Line ("  checker: " & Result.Message (1 .. Result.Message_Length));
      end if;
      return Result.Success;
   end Checks;

   G : R.Generation_Value;
   Digest : R.Digest;
   Valid : Boolean;
begin
   G.Number := 2;
   G.Previous := 1;
   G.Count := 2;
   G.Pins (1).Length := 8;
   G.Pins (1).Service (1 .. 8) := "acpi.svc";
   G.Pins (1).Program := [for I in R.Digest_Index => Interfaces.Unsigned_8 (I)];
   G.Pins (2).Length := 9;
   G.Pins (2).Service (1 .. 9) := "clock.svc";
   G.Pins (2).Program := [others => 16#AB#];
   declare
      Text : constant String := R.Record_Text (G);
   begin
      Put_Line ("  " & Text);
      Check (Checks (Text), "a generation's record checks against System_Generation");
      Check (R.Hex (G.Pins (2).Program) (1 .. 4) = "abab", "hex text is lower-case");
   end;
   R.From_Hex (R.Hex (G.Pins (1).Program), Digest, Valid);
   Check (Valid and then Digest = G.Pins (1).Program, "hex round-trips");
   R.From_Hex ("AB" & [1 .. R.Hex_Length - 2 => '0'], Digest, Valid);
   Check (not Valid, "upper-case hex is not a pin");
   Check (R.Pin_Of (G, "clock.svc") = 2 and then R.Pin_Of (G, "logstore.svc") = 0, "pins by service");
   Check (not Checks ("(System_Generation number => 2 pins => [])"), "a record without previous is refused");
   Check (not Checks ("(System_Generation number => ""2"" previous => 1 pins => [])"),
          "a number that is text is refused");
   Check (not Checks ("(Service_Pin service => ""acpi.svc"" executable => ""00"")"),
          "a pin alone is not a generation");

   --  The shared reader (ccl-remote, procmgr at boot, the installer).
   declare
      package Reading renames System_Generation_Records.Reading;
      Back : R.Generation_Value;
      N : Reading.Kept_Generation;
      First : constant R.Generation_Value := (Number => 1, Previous => 0, Count => 0, Pins => <>);
   begin
      Reading.Parse (R.Record_Text (G) & ASCII.LF, 2, Back, Valid);
      Check (Valid and then Back.Number = 2 and then Back.Previous = 1 and then Back.Count = 2
             and then Back.Pins (1).Service (1 .. Back.Pins (1).Length) = "acpi.svc"
             and then Back.Pins (2).Program = G.Pins (2).Program,
             "the reader gives back what was written");
      Reading.Parse (R.Record_Text (G), 3, Back, Valid);
      Check (not Valid, "a record read under another number is refused");
      Put_Line ("  " & R.Record_Text (First));
      Reading.Parse (R.Record_Text (First), 1, Back, Valid);
      Check (Valid and then Back.Count = 0, "generation 1 as the installer writes it reads back (no pins)");
      Reading.Parse ("(System_Generation number => 2 previous => 2 pins => (list-of Service_Pin))", 2, Back, Valid);
      Check (not Valid, "a generation whose previous is not below it is refused");
      Reading.Parse ("(System_Generation number => 2 previous => 1 pins => " &
                     "[(Service_Pin service => ""a.svc"" executable => """ & R.Hex (Digest) & """) " &
                     "(Service_Pin service => ""a.svc"" executable => """ & R.Hex (Digest) & """)])",
                     2, Back, Valid);
      Check (not Valid, "a record pinning one service twice is refused");
      Reading.Parse ("(System_Generation number => 2 previous => 1 pins => " &
                     "[(Service_Pin service => ""../x"" executable => """ & R.Hex (Digest) & """)])",
                     2, Back, Valid);
      Check (not Valid, "a pin naming no service is refused");
      Reading.Parse_Current ("7" & ASCII.LF, N, Valid);
      Check (Valid and then N = 7, "the current pointer reads");
      Reading.Parse_Current (Reading.Current_Text (12), N, Valid);
      Check (Valid and then N = 12, "the current pointer round-trips");
      Reading.Parse_Current ("0", N, Valid);
      Check (Valid and then N = 0, "0 names the image's generation");
      declare
         procedure Refuses (Bad : String) is
         begin
            Reading.Parse_Current (Bad, N, Valid);
            Check (not Valid, "not a current pointer: """ & Bad & """");
         end Refuses;
      begin
         Refuses ("07");
         Refuses ("");
         Refuses ("-1");
         Refuses ("257");
         Refuses ("1 ");
         Refuses ("x");
      end;
   end;

   if Failures = 0 then
      Put_Line ("GENERATION-RECORDS: PASS");
   else
      Put_Line ("GENERATION-RECORDS: FAIL" & Failures'Image);
      raise Program_Error;
   end if;
end Generation_Records_Tests;
