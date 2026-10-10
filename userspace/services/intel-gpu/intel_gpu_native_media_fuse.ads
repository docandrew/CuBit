with Interfaces;
with Intel_GPU_Media_Engines;
generic
   -- Caller holds GT forcewake across Sample and owns the device. Owner_Ready
   -- is rechecked after the reads so a lost owner never yields an inventory.
   with function Owner_Ready return Boolean;
   -- CPU virtual address of the mapped register page base that contains
   -- Intel_GPU_Media_Engines.Fuse_Register (page 0x9000), or 0 if unmapped.
   -- Read-only access suffices; nothing is written.
   with function Fuse_Page_Base return Interfaces.Unsigned_64;
package Intel_GPU_Native_Media_Fuse is
   -- Two volatile reads, decoded only if both agree (Decode_Stable).
   -- Not evidence of engine power, reset or forcewake; no retry.
   function Sample (Vendor, Device : Interfaces.Unsigned_16)
     return Intel_GPU_Media_Engines.Engines;
   -- The raw stable value for logs, or Unreadable.
   function Last_Raw return Intel_GPU_Media_Engines.Fuse_Word;
end Intel_GPU_Native_Media_Fuse;
