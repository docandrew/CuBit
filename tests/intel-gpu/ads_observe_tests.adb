with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_ADLN_Inventory;
with Intel_GPU_ADLN_Steering;
with Intel_GPU_ADLN_EU;
with Intel_GPU_ADS_System_Info;
with Intel_GPU_ADS_Observe;
procedure ADS_Observe_Tests is
   use Intel_GPU_ADLN_Steering;
   Values : array (Positive range 1 .. 10) of Unsigned_32;
   Offsets : constant array (Positive range 1 .. 5) of Unsigned_32 :=
     [Slice_Register, DSS_Register, L3_Register,
      Intel_GPU_ADS_System_Info.Doorbell_Register, Intel_GPU_ADLN_EU.EU_Disable_Register];
   Reads : Natural := 0;
   function Read (Offset : Unsigned_32) return Unsigned_32 is
   begin
      pragma Assert (Reads < 10);
      pragma Assert (Offset = Offsets (Reads mod 5 + 1));
      Reads := Reads + 1;
      return Values (Reads);
   end Read;
   package Reader is new Intel_GPU_ADS_Observe (Read);
   Description : constant Intel_GPU_ADLN_Inventory.Inventory :=
     Intel_GPU_ADLN_Inventory.Decode (16#8086#, 16#46D2#, 16#000E00FE#);
   Result : Reader.Observation;
   procedure Baseline is
   begin
      Reads := 0;
      Values := [1, 63, 0, 16#FF0000#, 0, 1, 63, 0, 16#FF0000#, 0];
   end Baseline;
begin
   pragma Assert (Description.Valid);
   Baseline;
   Result := Reader.Capture (Description, False);
   pragma Assert (not Result.Valid and Reads = 0);
   Result := Reader.Capture ((others => <>), True);
   pragma Assert (not Result.Valid and Reads = 0);
   for Count in Unsigned_32 range 0 .. 255 loop
      Baseline;
      Values (4) := Shift_Left (Count, 16);
      Values (9) := Values (4);
      Result := Reader.Capture (Description, True);
      pragma Assert (Result.Valid and Reads = 10 and Result.Topology.Valid);
      pragma Assert (Result.Execution_Units.Valid and Result.Execution_Units.Total_EUs = 96);
      pragma Assert (Result.Doorbell = Values (4));
      pragma Assert (Unsigned_32 (Result.System_Info.Bytes (584)) +
        256 * Unsigned_32 (Result.System_Info.Bytes (585)) = Count + 1);
   end loop;
   for Fault in Values'Range loop
      Baseline;
      Values (Fault) := Unsigned_32'Last;
      Result := Reader.Capture (Description, True);
      pragma Assert (not Result.Valid and Reads = 10);
      pragma Assert (not Result.Topology.Valid and not Result.System_Info.Valid);
      pragma Assert (Result.Doorbell = Unsigned_32'Last);
      Baseline;
      Values (Fault) := Values (Fault) xor 16#10000000#;
      Result := Reader.Capture (Description, True);
      pragma Assert (not Result.Valid and Reads = 10);
   end loop;
   for Fault in 1 .. 3 loop
      Baseline;
      Values (Fault) := (if Fault = 3 then 15 else 0);
      Values (Fault + 5) := Values (Fault);
      Result := Reader.Capture (Description, True);
      pragma Assert (not Result.Valid and Reads = 10);
   end loop;
   Baseline;
   Values (5) := 255; Values (10) := 255;
   Result := Reader.Capture (Description, True);
   pragma Assert (not Result.Valid and not Result.Execution_Units.Valid);
   Ada.Text_IO.Put_Line
     ("ADS observe PASS: admission, ten reads, doorbells, EU topology and unstable/sentinel rejection");
end ADS_Observe_Tests;
