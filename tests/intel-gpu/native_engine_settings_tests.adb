with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_ADLN_Inventory;
with Intel_GPU_ADLN_Steering;
with Intel_GPU_ADLN_Engine_Settings;
with Intel_GPU_Native_Engine_Settings;
with Intel_GPU_Reset_Pages;
procedure Native_Engine_Settings_Tests is
   function Owner return Boolean is (False);
   function Inventory return Intel_GPU_ADLN_Inventory.Inventory is
     (True, [others => True], [others => True]);
   function Topology return Intel_GPU_ADLN_Steering.Topology is
     (Intel_GPU_ADLN_Steering.Decode (1, 1, 0));
   procedure Check (Engine : Intel_GPU_ADLN_Inventory.Engine) is
      package IO is new Intel_GPU_Native_Engine_Settings (Engine, Owner, Inventory, Topology);
      Plan : constant Intel_GPU_ADLN_Engine_Settings.Settings_Plan :=
        Intel_GPU_ADLN_Engine_Settings.Build (Inventory, Engine, 3);
      OK : Boolean;
   begin
      for I in 1 .. Plan.Count loop
         pragma Assert (IO.Address_For (Plan.Entries (I).Offset) /= 0);
         pragma Assert (IO.Read32 (Plan.Entries (I).Offset, Plan.Entries (I).CPU_Steered) = Unsigned_32'Last);
         IO.Write32 (Plan.Entries (I).Offset, 0, Plan.Entries (I).CPU_Steered, OK);
         pragma Assert (not OK);
      end loop;
      pragma Assert (IO.Address_For (16#FDC#) = Intel_GPU_Reset_Pages.Virtual_Base + 11 * 4096 + 16#FDC#);
      pragma Assert (IO.Address_For (16#FDD#) = 0 and IO.Address_For (16#70000#) = 0);
      IO.Write32 (16#FDC#, 0, False, OK);
      pragma Assert (not OK);
   end Check;
begin
   for E in Intel_GPU_ADLN_Inventory.Engine loop Check (E); end loop;
   Ada.Text_IO.Put_Line ("Native engine adapter PASS: mapped allowlist, unowned operations do no MMIO");
end Native_Engine_Settings_Tests;
