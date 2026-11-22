with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_ADLN_Inventory;
with Intel_GPU_ADLN_Steering;
with Intel_GPU_ADLN_GT_Settings;
with Intel_GPU_GT_Configure;
procedure GT_Configure_Tests is
   Inventory : constant Intel_GPU_ADLN_Inventory.Inventory :=
     Intel_GPU_ADLN_Inventory.Decode (16#8086#,16#46D2#,0);
   Topology : constant Intel_GPU_ADLN_Steering.Topology :=
     Intel_GPU_ADLN_Steering.Decode (1,1,0);
   Plan : constant Intel_GPU_ADLN_GT_Settings.Plan :=
     Intel_GPU_ADLN_GT_Settings.Build (Inventory, Topology);
   Registers : array (1 .. 5) of Unsigned_32;
   Writes, Scenario : Natural := 0;
   Owned : Boolean := True;
   function Owner return Boolean is (Owned);
   function Find (Offset : Unsigned_32; MCR : Boolean) return Positive is
   begin
      for I in 1 .. Plan.Count loop
         if Plan.Items (I).Offset = Offset then
            pragma Assert (Plan.Items (I).MCR = MCR);
            return I;
         end if;
      end loop;
      raise Program_Error;
   end Find;
   function Read32 (Offset : Unsigned_32; MCR : Boolean) return Unsigned_32 is
      I : constant Positive := Find (Offset, MCR);
   begin
      if Scenario = 4 and I = 2 then return Unsigned_32'Last; end if;
      return Registers (I);
   end Read32;
   procedure Write32 (Offset, Value : Unsigned_32; MCR : Boolean; Success : out Boolean) is
      I : constant Positive := Find (Offset, MCR);
   begin
      Writes := Writes + 1;
      if not (Scenario = 1 and I = 5) and not (Scenario = 2 and I = 4) then
         Registers (I) := Value;
      end if;
      Success := Scenario /= 3;
      if Scenario = 5 then Owned := False; end if;
   end Write32;
   package C is new Intel_GPU_GT_Configure (Owner, Read32, Write32);
   use type C.Result;
   Status : C.Result;
begin
   for Case_Number in 0 .. 5 loop
      declare Attempt : C.Attempt; Old_Writes : Natural; begin
         Scenario := Case_Number; Writes := 0; Owned := True;
         Registers := [others => 2];
         C.Configure (Attempt, Inventory, Topology, Status);
         pragma Assert (Status = (case Scenario is
           when 0 => C.Ready, when 1 => C.Ready_With_Firmware_Override,
           when 2 => C.Readback_Failed, when 3 => C.Write_Failed,
           when 4 => C.Read_Failed, when others => C.Ownership_Lost));
         if Scenario = 1 then
            pragma Assert (C.Last_Offset (Attempt) = 16#9424# and C.Last_Readback (Attempt) = 2);
         end if;
         Old_Writes := Writes;
         C.Configure (Attempt, Inventory, Topology, Status);
         pragma Assert (Status = C.Rejected and Writes = Old_Writes);
      end;
   end loop;
   Ada.Text_IO.Put_Line ("GT configure PASS: normal, firmware override, hard failures and no retry (mock MMIO)");
end GT_Configure_Tests;
