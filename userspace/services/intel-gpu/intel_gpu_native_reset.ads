with Intel_GPU_ADLN_Devices;
with Interfaces;
with Intel_GPU_ADLN_Steering;
with Intel_GPU_ADLN_EU;
with Intel_GPU_ADLN_Inventory;
with Intel_GPU_GT_Frequency;
package Intel_GPU_Native_Reset is
   -- Caller has mapped the six approved pages at61200000, forcewake page
   -- at60200000, and readonly BAR at60000000. D0 ownership of an ADL-N
   -- Device (Intel_GPU_ADLN_Devices), authenticated fuse and exclusive
   -- submission ownership are prerequisites.
   -- One shot, retaining power on success/failure; no firmware/PTE publication.
   function Execute
     (Device : Intel_GPU_ADLN_Devices.Device_ID; Fuse : Interfaces.Unsigned_32) return String;
   function Last_Succeeded return Boolean;
   -- Zero until a stable CS clock sample is captured under retained forcewake.
   -- Independent of reset success; not a timestamp-counter read capability.
   function Timestamp_Hz return Interfaces.Unsigned_32;
   -- Captured only after successful reset while this adapter retains forcewake.
   -- Invalid observations never make the independent reset result successful.
   function ADS_Observed return Boolean;
   function ADS_Inventory return Intel_GPU_ADLN_Inventory.Inventory;
   function ADS_Topology return Intel_GPU_ADLN_Steering.Topology;
   function ADS_Execution_Units return Intel_GPU_ADLN_EU.Topology;
   function ADS_Doorbell return Interfaces.Unsigned_32;
   -- The render GT's frequency registers (Intel_GPU_GT_Frequency), read
   -- only, under the forcewake this adapter retains after a successful
   -- reset. Valid False otherwise or on an access fault.
   function GT_Frequency return Intel_GPU_GT_Frequency.Snapshot;
   -- GEN12_RPSTAT1 alone (the actual frequency), cheap enough per job.
   -- Valid False under the same conditions.
   procedure GT_Status (Status : out Interfaces.Unsigned_32; Valid : out Boolean);
end Intel_GPU_Native_Reset;
