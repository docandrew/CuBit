with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_GuC_MMIO_Invalidate;
with Intel_GPU_TLB_Registers;
procedure GuC_MMIO_Invalidate_Tests is
   Scenario, Reads, Writes, Clocks : Natural := 0;
   Owner : Boolean := True;
   function Gate return Boolean is (Owner);
   procedure Write_Request (Value : Unsigned_32; OK : out Boolean) is
   begin
      pragma Assert (Value = 1 and Reads = 0);
      Writes := Writes + 1;
      OK := Scenario /= 2;
      if Scenario = 3 then Owner := False; end if;
   end Write_Request;
   procedure Read_Status (Value : out Unsigned_32; OK : out Boolean) is
   begin
      pragma Assert (Writes = 1);
      Reads := Reads + 1; OK := Scenario /= 4;
      Value := (if Scenario = 5 then Unsigned_32'Last
                elsif Scenario = 6 or else Reads < 3 then 1
                else 16#FFFF_FFFE#);
      if Scenario = 7 then Owner := False; Value := 0; end if;
   end Read_Status;
   procedure Clock_US (Value : out Unsigned_64; OK : out Boolean) is
   begin
      Clocks := Clocks + 1;
      OK := Scenario /= 8;
      Value := 100;
      if Scenario = 9 and Clocks > 1 then Value := 99;
      elsif Scenario = 10 and Clocks > 1 then Value := 4100;
      elsif Scenario = 11 then Value := Unsigned_64'Last;
      elsif Scenario = 12 and Clocks = 1 then Owner := False;
      end if;
   end Clock_US;
   package Probe is new Intel_GPU_GuC_MMIO_Invalidate
     (Gate, Write_Request, Read_Status, Clock_US);
   use type Probe.Result;
begin
   for Bit in 1 .. 31 loop
      pragma Assert (not Intel_GPU_TLB_Registers.GuC_Pending
        (Shift_Left (Unsigned_32'(1), Bit)));
      pragma Assert (Intel_GPU_TLB_Registers.GuC_Pending
        (Shift_Left (Unsigned_32'(1), Bit) or 1));
   end loop;
   for Case_Number in 0 .. 12 loop
      declare
         Attempt : Probe.Attempt;
         Status : Probe.Result;
         Before_Reads, Before_Writes, Before_Clocks : Natural;
      begin
         Scenario := Case_Number; Reads := 0; Writes := 0; Clocks := 0;
         Owner := Scenario /= 1;
         Probe.Execute (Attempt, Status, Poll_Limit => 8);
         pragma Assert (Status =
           (case Scenario is
              when 0 => Probe.Complete,
              when 1 => Probe.Rejected,
              when 2 => Probe.Write_Failed,
              when 3 | 7 | 12 => Probe.Ownership_Lost,
              when 4 | 5 => Probe.Read_Failed,
              when 6 | 10 => Probe.Timed_Out,
              when others => Probe.Invalid_Clock));
         if Scenario in 1 | 8 | 11 | 12 then
            pragma Assert (Writes = 0 and Reads = 0);
         else pragma Assert (Writes = 1); end if;
         if Scenario = 0 then pragma Assert (Reads = 3); end if;
         if Scenario = 6 then pragma Assert (Reads = 8); end if;
         Before_Reads := Reads; Before_Writes := Writes; Before_Clocks := Clocks;
         Owner := True;
         Probe.Execute (Attempt, Status);
         pragma Assert (Status = Probe.Rejected and Reads = Before_Reads and
           Writes = Before_Writes and Clocks = Before_Clocks);
      end;
   end loop;
   Ada.Text_IO.Put_Line ("GuC MMIO invalidation PASS: typed request, field-only completion, deadline, frozen clock, faults, no replay");
end GuC_MMIO_Invalidate_Tests;
