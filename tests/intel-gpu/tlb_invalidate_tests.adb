with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_ADLN_TLB_Invalidate;
with Intel_GPU_TLB_Registers;
procedure TLB_Invalidate_Tests is
   package Regs renames Intel_GPU_TLB_Registers;
   Alive : Boolean := True;
   Calls, Writes, Reads, Fail_At, Lose_At : Natural := 0;
   GFX_Busy, OA_Busy : Boolean := False;
   Noise : Unsigned_32 := 0;
   Time, Step : Unsigned_64 := 0;
   Backwards : Boolean := False;
   function Gate return Boolean is (Alive);
   procedure Event (OK : out Boolean) is
   begin
      Calls := Calls + 1;
      if Calls = Lose_At then Alive := False; end if;
      OK := Calls /= Fail_At;
   end Event;
   procedure Write_Register (Offset, Value : Unsigned_32; OK : out Boolean) is
   begin
      Writes := Writes + 1;
      pragma Assert (Writes <= 2 and Value = 1);
      pragma Assert (Offset = (if Writes = 1 then Regs.GFX_Offset else Regs.OA_Offset));
      Event (OK);
   end Write_Register;
   procedure Read_Register (Offset : Unsigned_32; Value : out Unsigned_32; OK : out Boolean) is
   begin
      Reads := Reads + 1;
      pragma Assert (Writes = 2);
      pragma Assert (Offset = (if Reads mod 2 = 1 then Regs.GFX_Offset else Regs.OA_Offset));
      Value := Noise or
        (if (Offset = Regs.GFX_Offset and GFX_Busy) or
            (Offset = Regs.OA_Offset and OA_Busy) then 1 else 0);
      Event (OK);
   end Read_Register;
   procedure Clock_US (Value : out Unsigned_64; OK : out Boolean) is
   begin
      Event (OK);
      Value := (if Backwards and Calls > 1 then 0 else Time);
      Time := Time + Step;
   end Clock_US;
   package Invalidator is new Intel_GPU_ADLN_TLB_Invalidate
     (Gate, Write_Register, Read_Register, Clock_US);
   use Invalidator;
   Status : Result;
   procedure Reset is
   begin
      Alive := True; Calls := 0; Writes := 0; Reads := 0;
      Fail_At := 0; Lose_At := 0; GFX_Busy := False; OA_Busy := False; Noise := 0;
      Time := 0; Step := 0; Backwards := False;
   end Reset;
   procedure Run (Expected : Result; Limit : Positive := 3) is
      State : Attempt;
      Before : Natural;
   begin
      Execute (State, Status, Limit);
      pragma Assert (Status = Expected);
      Before := Calls;
      Execute (State, Status, Limit);
      pragma Assert (Status = Rejected and Calls = Before);
   end Run;
begin
   Reset; Run (Complete); pragma Assert (Writes = 2 and Reads = 2 and Calls = 10);
   for Position in 1 .. 10 loop
      Reset; Fail_At := Position;
      Run ((case Position is
              when 3 | 5 => Write_Failed,
              when 7 | 9 => Read_Failed,
              when others => Invalid_Clock));
      pragma Assert (Calls = Position);
      Reset; Lose_At := Position; Run (Ownership_Lost);
      pragma Assert (Calls = Position);
   end loop;
   for Bit in 1 .. 31 loop
      Reset; Noise := Shift_Left (Unsigned_32'(1), Bit); Run (Complete);
   end loop;
   Reset; Alive := False; Run (Ownership_Lost); pragma Assert (Calls = 0);
   Reset; GFX_Busy := True; Run (Timed_Out); pragma Assert (Reads = 6);
   Reset; OA_Busy := True; Run (Timed_Out); pragma Assert (Reads = 6);
   Reset; Step := 1000; Run (Timed_Out); pragma Assert (Reads = 1);
   Reset; Time := 1; Backwards := True; Run (Invalid_Clock);
   pragma Assert (Writes = 0);
   Reset; Time := Unsigned_64'Last; Step := 1; Run (Invalid_Clock);
   pragma Assert (Writes = 0);
   Ada.Text_IO.Put_Line ("TLB invalidation PASS: GFX/OA order, field-only polling, every callback failure/owner loss, deadline, stopped/backwards clock, no retry (mock MMIO)");
end TLB_Invalidate_Tests;
