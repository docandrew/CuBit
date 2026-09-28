with Interfaces; use Interfaces;
with Intel_GPU_Reset_Prepare;
with Ada.Text_IO;
procedure Reset_Prepare_Tests is
   Samples : array (1 .. 8) of Unsigned_32 := [others => 0];
   Reads, Writes : Natural := 0;
   Written : Unsigned_32 := 0;
   Clock, Advance : Unsigned_64 := 0;
   Regress : Boolean := False;
   function Read_Control return Unsigned_32 is
   begin Reads := Reads + 1; return Samples (Reads); end;
   procedure Write_Control (Value : Unsigned_32) is
   begin Writes := Writes + 1; Written := Value; end;
   procedure Pause is
   begin
      if Regress then Clock := Clock - 1; else Clock := Clock + Advance; end if;
   end;
   function Now return Unsigned_64 is (Clock);
   package Reset is new Intel_GPU_Reset_Prepare (Read_Control, Write_Control, Pause, Now);
   use type Reset.Result;
   Status : Reset.Result;
   procedure Clear is
   begin Reads := 0; Writes := 0; Clock := 0; Advance := 0;
      Regress := False; Samples := [others => 0]; end;
begin
   Samples (1) := 2;
   Reset.Prepare (3, Status);
   pragma Assert (Status = Reset.Ready and Writes = 0);
   Clear; Samples (2) := 2;
   Reset.Prepare (3, Status);
   pragma Assert (Status = Reset.Ready and Writes = 1 and Written = 16#10001#);
   Clear; Samples (1) := 6;
   Reset.Prepare (3, Status);
   pragma Assert (Status = Reset.Ready and Written = 16#40004#);
   Clear;
   Reset.Prepare (3, Status);
   pragma Assert (Status = Reset.Timed_Out and Reads = 4);
   Reset.Cancel;
   pragma Assert (Writes = 2 and Written = 16#10000#);
   Clear; Samples := [others => 4];
   Reset.Prepare (3, Status);
   pragma Assert (Status = Reset.Timed_Out and Written = 16#40004#);
   Clear; Samples (1) := Unsigned_32'Last;
   Reset.Prepare (3, Status);
   pragma Assert (Status = Reset.Invalid_MMIO and Writes = 0);
   Clear; Samples (2) := Unsigned_32'Last;
   Reset.Prepare (3, Status);
   pragma Assert (Status = Reset.Invalid_MMIO and Writes = 1);
   Clear; Advance := 700;
   Reset.Prepare (3, Status);
   pragma Assert (Status = Reset.Timed_Out and Reads = 2);
   Clear; Clock := 1; Regress := True;
   Reset.Prepare (3, Status);
   pragma Assert (Status = Reset.Invalid_Clock);
   Clear;
   Reset.Prepare (3, Status, 0);
   pragma Assert (Status = Reset.Timed_Out and Reads = 0 and Writes = 0);
   Ada.Text_IO.Put_Line ("PASS: reset preparation branches, bounds and cancellation write");
end Reset_Prepare_Tests;
