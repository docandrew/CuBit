with Interfaces; use Interfaces;
with Intel_GPU_ADLN_Inventory;
with Intel_GPU_ADLN_Steering;
package Intel_GPU_ADLN_GT_Settings with SPARK_Mode is
   type Setting is record
      Offset, Clear_Mask, Set_Bits, Verify_Mask : Unsigned_32 := 0;
      MCR : Boolean := False;
   end record;
   type Entries is array (Positive range 1 .. 5) of Setting;
   type Plan is record
      Count : Natural range 0 .. 5 := 0;
      Items : Entries := [others => <>];
   end record;
   -- ADL-N/IP12.0 only. Numeric plan, not hardware authority. All entries
   -- use fresh RMW, not upper16 masked-write semantics.
   function Build
     (Inventory : Intel_GPU_ADLN_Inventory.Inventory;
      Topology : Intel_GPU_ADLN_Steering.Topology) return Plan;
end Intel_GPU_ADLN_GT_Settings;
