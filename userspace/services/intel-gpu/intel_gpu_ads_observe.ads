with Interfaces;
with Intel_GPU_ADLN_Inventory;
with Intel_GPU_ADLN_Steering;
with Intel_GPU_ADLN_EU;
with Intel_GPU_ADS_System_Info;
generic
   with function Read_32 (Offset : Interfaces.Unsigned_32)
     return Interfaces.Unsigned_32;
package Intel_GPU_ADS_Observe is
   type Observation is record
      Valid : Boolean := False;
      Topology : Intel_GPU_ADLN_Steering.Topology;
      Execution_Units : Intel_GPU_ADLN_EU.Topology;
      Doorbell : Interfaces.Unsigned_32 := Interfaces.Unsigned_32'Last;
      System_Info : Intel_GPU_ADS_System_Info.System_Info;
   end record;
   -- Bounded ten reads, no writes. Caller retains exclusive device ownership,
   -- stable PCI D0 and all required forcewake across the complete operation.
   -- Matching snapshots detect observed changes, not hardware atomicity.
   function Capture
     (Description : Intel_GPU_ADLN_Inventory.Inventory;
      Power_Held : Boolean) return Observation;
end Intel_GPU_ADS_Observe;
