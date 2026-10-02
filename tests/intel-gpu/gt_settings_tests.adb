with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_ADLN_Inventory;
with Intel_GPU_ADLN_Steering;
with Intel_GPU_ADLN_GT_Settings; use Intel_GPU_ADLN_GT_Settings;
procedure GT_Settings_Tests is
   Inventory : Intel_GPU_ADLN_Inventory.Inventory;
   Topology : Intel_GPU_ADLN_Steering.Topology;
   P : Plan;
begin
   for Mask in Unsigned_32 range 1 .. 63 loop
      for Media in Unsigned_32 range 0 .. 7 loop
         Inventory := Intel_GPU_ADLN_Inventory.Decode (16#8086#,16#46D2#,Media);
         Topology := Intel_GPU_ADLN_Steering.Decode (1, Mask, 0);
         P := Build (Inventory, Topology);
         pragma Assert (P.Count = 3 + Boolean'Pos ((Media and 1) = 0) + Boolean'Pos ((Media and 4) = 0));
         pragma Assert (P.Items (1).Offset = 16#FDC# and
           P.Items (1).Set_Bits = (16#80000000# or Shift_Left (Unsigned_32 (Topology.Default_Instance),24)));
         pragma Assert (P.Items (P.Count - 1) = (16#9550#,0,16#200#,16#200#,True));
         pragma Assert (P.Items (P.Count) = (16#9424#,2,0,0,False));
         for I in 2 .. P.Count - 2 loop
            pragma Assert (P.Items (I).Offset in 16#1C3F10# | 16#1D3F10#);
            pragma Assert (P.Items (I).Set_Bits = 16#400000#);
         end loop;
      end loop;
   end loop;
   Topology := Intel_GPU_ADLN_Steering.Decode (1,3,0);
   Topology.Default_Instance := 1;
   pragma Assert (Build (Inventory,Topology).Count = 0);
   Topology.Valid := False;
   pragma Assert (Build (Inventory,Topology).Count = 0);
   Ada.Text_IO.Put_Line ("ADL-N GT plan PASS: topology, fused video engines, DFR and firmware-lock exception");
end GT_Settings_Tests;
