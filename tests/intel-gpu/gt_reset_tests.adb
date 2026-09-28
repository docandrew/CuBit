with Interfaces; use Interfaces;
with Intel_GPU_GT_Reset;
with Ada.Text_IO;
procedure GT_Reset_Tests is
   Clock : Unsigned_64 := 100;
   Step : Unsigned_64 := 50;
   Writes, Reads : Natural := 0;
   Fail_Cycle, Invalid_Cycle : Natural := 0;
   Regress, Slow_Read : Boolean := False;
   Missing : Boolean := False;
   function Read_Reset return Unsigned_32 is
   begin
      Reads := Reads + 1;
      if Slow_Read then Clock := Clock + 2_000; end if;
      return (if Writes = Invalid_Cycle then Unsigned_32'Last
              elsif Writes = Fail_Cycle then 1 else 0);
   end;
   procedure Write_Reset (Value : Unsigned_32) is
   begin pragma Assert (Value = 1); Writes := Writes + 1; end;
   function Now return Unsigned_64 is
     (if Missing then Unsigned_64'Last else Clock);
   procedure Pause is
   begin
      if Regress then Clock := Clock - 1; else Clock := Clock + Step; end if;
   end;
   package Reset is new Intel_GPU_GT_Reset (Read_Reset, Write_Reset, Now, Pause);
   use type Reset.Result;
   use type Reset.State;
   procedure Run (Expected : Reset.Result; Expected_Writes : Natural;
                  Polls : Positive := 3) is
      Object : Reset.Attempt;
      Status : Reset.Result;
   begin
      Writes := 0; Reads := 0; Clock := 100;
      Reset.Execute (Object, Polls, Status);
      pragma Assert (Status = Expected and Writes = Expected_Writes);
      pragma Assert (Reset.Current (Object) =
        (if Status = Reset.Complete then Reset.Reset_Complete else Reset.Quarantined));
      if Status = Reset.Complete then
         pragma Assert (Clock >= 152 and Reads = 2);
      end if;
      Reset.Execute (Object, 3, Status);
      pragma Assert (Status = Reset.Invalid_State and Writes = Expected_Writes);
   end;
begin
   Run (Reset.Complete, 2);
   Run (Reset.Timed_Out, 2, 1); -- exactly50 rounded us is not sufficient
   Step := 51; Run (Reset.Timed_Out, 2, 1);
   Step := 52; Run (Reset.Complete, 2, 1);
   Step := 50;
   Missing := True; Run (Reset.Invalid_Clock, 0); Missing := False;
   for Cycle in 1 .. 2 loop
      Fail_Cycle := Cycle; Run (Reset.Timed_Out, Cycle);
      Fail_Cycle := 0; Invalid_Cycle := Cycle; Run (Reset.Invalid_MMIO, Cycle);
      Invalid_Cycle := 0;
   end loop;
   Step := 0; Run (Reset.Timed_Out, 2);
   Step := 50; Regress := True; Run (Reset.Invalid_Clock, 2);
   Regress := False; Slow_Read := True; Run (Reset.Timed_Out, 1);
   Ada.Text_IO.Put_Line ("PASS: full GT reset repetition, settling and quarantine");
end GT_Reset_Tests;
