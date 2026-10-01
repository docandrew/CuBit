with Interfaces;
with Intel_GPU_ADLN_Steering;
with Intel_GPU_ADLN_EU;
with Intel_GPU_ADLN_Inventory;
package Intel_GPU_Native_Reset is
   -- Caller has mapped the six approved pages at61200000, forcewake page
   -- at60200000, and readonly BAR at60000000. Static8086:46D2 D0 ownership,
   -- authenticated fuse and exclusive submission ownership are prerequisites.
   -- One shot, retaining power on success/failure; no firmware/PTE publication.
   function Execute (Fuse : Interfaces.Unsigned_32) return String;
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
end Intel_GPU_Native_Reset;
