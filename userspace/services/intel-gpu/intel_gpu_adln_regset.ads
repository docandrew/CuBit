with Interfaces;
with Intel_GPU_ADLN_Inventory;
with Intel_GPU_ADLN_Steering;
with Intel_GPU_ADS_Regset;
package Intel_GPU_ADLN_Regset with SPARK_Mode is
   use type Intel_GPU_ADLN_Inventory.Engine;
   type Register_Set is record
      Ready : Boolean := False;
      Registers : Intel_GPU_ADS_Regset.Register_List;
   end record;
   function Build_Common
     (Description : Intel_GPU_ADLN_Inventory.Inventory;
      Item : Intel_GPU_ADLN_Inventory.Engine;
      Steering : Intel_GPU_ADLN_Steering.Topology;
      MMIO_Bytes : Interfaces.Unsigned_32) return Register_Set
     with Post => (if Build_Common'Result.Ready then
                      Build_Common'Result.Registers.Count = 54);
   -- Pinned v6.16 gen12 (<12.55) common register entries only. Ready does NOT
   -- mean complete ADS: per-engine workarounds must be added and the underlying
   -- engine/whitelist/MOCS state initialized before firmware use. No MMIO here.
   function Build
     (Description : Intel_GPU_ADLN_Inventory.Inventory;
      Item : Intel_GPU_ADLN_Inventory.Engine;
      Steering : Intel_GPU_ADLN_Steering.Topology;
      MMIO_Bytes : Interfaces.Unsigned_32) return Register_Set
     with Post => (if Build'Result.Ready then
                      Build'Result.Registers.Count =
                        (if Item = Intel_GPU_ADLN_Inventory.Render then 63 else 55));
   -- Common entries plus the pinned ADL-N engine-setting registers. This is
   -- save-list metadata only, not proof that settings have been applied, MOCS
   -- initialized, contexts prepared, ADS pointers published or recovery ready.
end Intel_GPU_ADLN_Regset;
