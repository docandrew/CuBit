with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_Retirement_Invalidate;
with Intel_GPU_TLB_Registers;
procedure Retirement_Invalidate_Tests is
   package R renames Intel_GPU_TLB_Registers;
   IOs, Gates, Clocks, Engine_Writes, Engine_Reads, GuC_Writes, GuC_Reads : Natural := 0;
   Fail_IO, Lose_Gate, Fail_Clock, Stall : Natural := 0;
   function Gate return Boolean is
   begin
      Gates := Gates + 1;
      return Lose_Gate = 0 or else Gates < Lose_Gate;
   end Gate;
   procedure IO_Result (OK : out Boolean) is
   begin
      IOs := IOs + 1;
      OK := IOs /= Fail_IO;
   end IO_Result;
   procedure Write_Engine (Offset, Value : Unsigned_32; OK : out Boolean) is
   begin
      pragma Assert (GuC_Writes = 0 and Engine_Reads = 0 and Value = 1);
      pragma Assert (Offset = (if Engine_Writes = 0 then R.GFX_Offset else R.OA_Offset));
      Engine_Writes := Engine_Writes + 1;
      pragma Assert (Engine_Writes <= 2);
      IO_Result (OK);
   end Write_Engine;
   procedure Read_Engine (Offset : Unsigned_32; Value : out Unsigned_32; OK : out Boolean) is
   begin
      pragma Assert (Engine_Writes = 2 and GuC_Writes = 0);
      pragma Assert (Offset = (if Engine_Reads mod 2 = 0 then R.GFX_Offset else R.OA_Offset));
      Engine_Reads := Engine_Reads + 1;
      Value := (if Stall = 1 then 1 else 0);
      IO_Result (OK);
   end Read_Engine;
   procedure Write_GuC (Value : Unsigned_32; OK : out Boolean) is
   begin
      pragma Assert (Value = 1 and Engine_Writes = 2 and Engine_Reads = 2 and Stall /= 1);
      GuC_Writes := GuC_Writes + 1;
      pragma Assert (GuC_Writes = 1);
      IO_Result (OK);
   end Write_GuC;
   procedure Read_GuC (Value : out Unsigned_32; OK : out Boolean) is
   begin
      pragma Assert (GuC_Writes = 1);
      GuC_Reads := GuC_Reads + 1;
      Value := (if Stall = 2 then 1 else 0);
      IO_Result (OK);
   end Read_GuC;
   procedure Clock (Value : out Unsigned_64; OK : out Boolean) is
   begin
      Clocks := Clocks + 1;
      Value := 100; -- Frozen clock must still terminate through the poll cap.
      OK := Clocks /= Fail_Clock;
   end Clock;
   package Completion is new Intel_GPU_Retirement_Invalidate
     (Gate, Write_Engine, Read_Engine, Write_GuC, Read_GuC, Clock);
   use type Completion.Result;
   Successful_Gates, Successful_Clocks : Natural;
   Cases : Natural := 0;
   procedure Run (Failure, Loss, Bad_Clock, Stuck : Natural := 0) is
      Attempt : Completion.Attempt;
      Status : Completion.Result;
      Saved_IOs, Saved_Gates, Saved_Clocks : Natural;
   begin
      IOs := 0; Gates := 0; Clocks := 0; Engine_Writes := 0;
      Engine_Reads := 0; GuC_Writes := 0; GuC_Reads := 0;
      Fail_IO := Failure; Lose_Gate := Loss; Fail_Clock := Bad_Clock; Stall := Stuck;
      Completion.Execute (Attempt, Status, Poll_Limit => 3);
      if Failure = 0 and Loss = 0 and Bad_Clock = 0 and Stuck = 0 then
         pragma Assert (Status = Completion.Complete and IOs = 6);
         Successful_Gates := Gates; Successful_Clocks := Clocks;
      else
         pragma Assert (Status /= Completion.Complete);
         if Failure in 1 .. 4 or Stuck = 1 then
            pragma Assert (Status = Completion.Engine_Failed and GuC_Writes = 0);
         elsif Failure in 5 .. 6 or Stuck = 2 then
            pragma Assert (Status = Completion.GuC_Failed);
         end if;
      end if;
      if Stuck = 1 then pragma Assert (Engine_Reads = 6); end if;
      if Stuck = 2 then pragma Assert (GuC_Reads = 3); end if;
      Saved_IOs := IOs; Saved_Gates := Gates; Saved_Clocks := Clocks;
      Fail_IO := 0; Lose_Gate := 0; Fail_Clock := 0; Stall := 0;
      Completion.Execute (Attempt, Status);
      pragma Assert (Status = Completion.Rejected and IOs = Saved_IOs and
        Gates = Saved_Gates and Clocks = Saved_Clocks);
      Cases := Cases + 1;
   end Run;
begin
   Run;
   for I in 1 .. 6 loop Run (Failure => I); end loop;
   for I in 1 .. Successful_Gates loop Run (Loss => I); end loop;
   for I in 1 .. Successful_Clocks loop Run (Bad_Clock => I); end loop;
   Run (Stuck => 1); Run (Stuck => 2);
   Ada.Text_IO.Put_Line ("Retirement completion PASS" & Natural'Image (Cases) &
     " cases: engine-before-GuC, every I/O/owner/clock failure, bounded stalls, no replay (mock MMIO)");
end Retirement_Invalidate_Tests;
