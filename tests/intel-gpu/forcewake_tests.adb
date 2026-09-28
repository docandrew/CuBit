with Interfaces; use Interfaces;
with Intel_GPU_Forcewake;
with Ada.Text_IO;
with Intel_GPU_ADLN_Inventory; use Intel_GPU_ADLN_Inventory;
procedure Forcewake_Tests is
   procedure Run_Domain (Item : Domain) is
   Samples : array (Positive range 1 .. 16) of Unsigned_32 := [others => 0];
   Writes : array (Positive range 1 .. 4) of Unsigned_32 := [others => 0];
   Reads, Written, Pauses : Natural := 0;
   Clock, Pause_Advance, Read_Advance : Unsigned_64 := 0;
   Regress_On_Pause : Boolean := False;
   Raise_On_Read : Boolean := False;
   function Now_Milliseconds return Unsigned_64 is (Clock);
   function Read_32 (Offset : Unsigned_32) return Unsigned_32 is
   begin
      pragma Assert (Offset = Ack_Register (Item));
      if Raise_On_Read then
         raise Constraint_Error with "injected callback failure";
      end if;
      Reads := Reads + 1;
      Clock := Clock + Read_Advance;
      return Samples (Reads);
   end Read_32;
   procedure Write_32 (Offset, Value : Unsigned_32) is
   begin
      pragma Assert (Offset = Request_Register (Item));
      Written := Written + 1;
      Writes (Written) := Value;
   end Write_32;
   procedure Pause is
   begin
      Pauses := Pauses + 1;
      if Regress_On_Pause then
         Clock := Clock - 1;
      else
         Clock := Clock + Pause_Advance;
      end if;
   end Pause;
   package FW is new Intel_GPU_Forcewake
     (Read_32, Write_32, Pause, Now_Milliseconds,
      Request_Register (Item), Ack_Register (Item));
   use type FW.Result;
   use type FW.Ownership_State;
   Leases : array (Positive range 1 .. 32) of FW.Lease;
   Case_Index : Positive := 1;
   Status : FW.Result;
   procedure Reset is
   begin
      Case_Index := Case_Index + 1;
      Reads := 0; Written := 0; Pauses := 0;
      Samples := [others => 0]; Writes := [others => 0];
      Clock := 0; Pause_Advance := 0; Read_Advance := 0;
      Regress_On_Pause := False;
      Raise_On_Read := False;
   end Reset;
   procedure Prime_Lease is
   begin
      Samples (2) := 1;
      FW.Acquire (Leases (Case_Index), 1, Status);
      pragma Assert (Status = FW.Ready);
      Reads := 0; Written := 0; Pauses := 0;
      Samples := [others => 0]; Writes := [others => 0];
   end Prime_Lease;
begin
   FW.Release (Leases (Case_Index), 1, Status);
   pragma Assert (Status = FW.Invalid_State and Reads = 0 and Written = 0);
   Samples (2) := 1;
   FW.Acquire (Leases (Case_Index), 1, Status);
   pragma Assert (Status = FW.Ready and Reads = 2 and Written = 1 and Pauses = 0);
   pragma Assert (Writes (1) = 16#10001#);
   pragma Assert (FW.State (Leases (Case_Index)) = FW.Held);
   FW.Acquire (Leases (Case_Index), 1, Status);
   pragma Assert (Status = FW.Invalid_State and Reads = 2 and Written = 1);
   FW.Release (Leases (Case_Index), 1, Status);
   pragma Assert (Status = FW.Ready and Writes (2) = 16#10000#);
   pragma Assert (FW.State (Leases (Case_Index)) = FW.Idle);
   FW.Release (Leases (Case_Index), 1, Status);
   pragma Assert (Status = FW.Invalid_State and Reads = 3 and Written = 2);
   Reset;
   Samples := [others => 1];
   FW.Acquire (Leases (Case_Index), 3, Status);
   pragma Assert (Status = FW.Timed_Out and Reads = 3 and Written = 0 and Pauses = 2);
   pragma Assert (FW.State (Leases (Case_Index)) = FW.Faulted);
   FW.Acquire (Leases (Case_Index), 1, Status);
   pragma Assert (Status = FW.Invalid_State and Reads = 3 and Written = 0);
   Reset;
   FW.Acquire (Leases (Case_Index), 3, Status);
   pragma Assert (Status = FW.Timed_Out and Reads = 4 and Written = 2 and Pauses = 2);
   pragma Assert (Writes (1) = 16#10001# and Writes (2) = 16#10000#);
   Reset;
   Samples (1) := Unsigned_32'Last;
   FW.Acquire (Leases (Case_Index), 3, Status);
   pragma Assert (Status = FW.Invalid_MMIO and Reads = 1 and Written = 0);
   Reset;
   Samples (2) := Unsigned_32'Last;
   FW.Acquire (Leases (Case_Index), 3, Status);
   pragma Assert (Status = FW.Invalid_MMIO and Written = 2);
   pragma Assert (Writes (2) = 16#10000#);
   Reset;
   Samples (1) := 1; Samples (2) := 2; Samples (3) := 2; Samples (4) := 3;
   FW.Acquire (Leases (Case_Index), 3, Status);
   pragma Assert (Status = FW.Ready and Reads = 4 and Pauses = 2);
   Reset;
   Prime_Lease;
   Samples := [others => 1];
   FW.Release (Leases (Case_Index), 3, Status);
   pragma Assert (Status = FW.Timed_Out and Written = 1 and Reads = 3);
   pragma Assert (FW.State (Leases (Case_Index)) = FW.Faulted);
   Reset;
   Prime_Lease;
   Samples (1) := Unsigned_32'Last;
   FW.Release (Leases (Case_Index), 3, Status);
   pragma Assert (Status = FW.Invalid_MMIO and Written = 1 and Reads = 1);
   Reset;
   FW.Acquire (Leases (Case_Index), 3, Status, 0);
   pragma Assert (Status = FW.Timed_Out and Reads = 0 and Written = 0);
   Reset;
   Pause_Advance := 25;
   FW.Acquire (Leases (Case_Index), 10, Status);
   pragma Assert (Status = FW.Timed_Out and Reads = 3 and Pauses = 2);
   pragma Assert (Written = 2 and Writes (2) = 16#10000#);
   Reset;
   --  Shared budget: time spent waiting for old ack clear is not refunded.
   Samples (1) := 1; Samples (2) := 0; Samples (3) := 0; Samples (4) := 1;
   Pause_Advance := 25;
   FW.Acquire (Leases (Case_Index), 10, Status);
   pragma Assert (Status = FW.Timed_Out and Reads = 3 and Written = 2);
   Reset;
   Clock := 100; Regress_On_Pause := True;
   FW.Acquire (Leases (Case_Index), 10, Status);
   pragma Assert (Status = FW.Invalid_Clock and Written = 2);
   Reset;
   Clock := Unsigned_64'Last - 10; Pause_Advance := 20;
   FW.Acquire (Leases (Case_Index), 10, Status);
   pragma Assert (Status = FW.Invalid_Clock and Written = 2);
   Reset;
   Samples (2) := 1; Read_Advance := 25;
   FW.Acquire (Leases (Case_Index), 10, Status);
   pragma Assert (Status = FW.Timed_Out and Reads = 2 and Written = 2);
   Reset;
   Prime_Lease;
   FW.Release (Leases (Case_Index), 3, Status, 0);
   pragma Assert (Status = FW.Timed_Out and Reads = 0 and Written = 1);
   pragma Assert (Writes (1) = 16#10000#);
   Reset;
   Raise_On_Read := True;
   begin
      FW.Acquire (Leases (Case_Index), 1, Status);
      raise Program_Error with "expected callback failure";
   exception
      when Constraint_Error => null;
   end;
   pragma Assert (FW.State (Leases (Case_Index)) = FW.Faulted);
   Raise_On_Read := False;
   FW.Acquire (Leases (Case_Index), 1, Status);
   pragma Assert (Status = FW.Invalid_State and Reads = 0 and Written = 0);
   Ada.Text_IO.Put_Line ("PASS: " & Domain'Image (Item) &
     " forcewake handshake, deadlines and failure cleanup");
   end Run_Domain;
begin
   for Item in Domain loop
      Run_Domain (Item);
   end loop;
end Forcewake_Tests;
