with Interfaces;
with Intel_GPU_ADLN_Inventory;
-- One package instance per hardware owner. Serialized, non-raising, bounded
-- callbacks and stable PCI power/identity are caller obligations.
generic
   with function Read_32 (Offset : Interfaces.Unsigned_32) return Interfaces.Unsigned_32;
   with procedure Write_32 (Offset, Value : Interfaces.Unsigned_32);
   with procedure Pause;
   with function Now_Milliseconds return Interfaces.Unsigned_64;
   Poll_Limit : Positive := 100_000;
   with procedure Recover_Domain
     (Item : Intel_GPU_ADLN_Inventory.Domain; Expected : Interfaces.Unsigned_32;
      Recovered : in out Boolean) is null;
package Intel_GPU_ADLN_Forcewake is
   type Ownership_State is (Idle, Held, Faulted);
   function State return Ownership_State;
   function Uncertain return Intel_GPU_ADLN_Inventory.Domain_Set;
   -- First failed handshake, preserved across cleanup; no diagnostic MMIO.
   function Failure_Detail return String;
   -- Fuse must be captured under GT forcewake, from this authenticated device.
   procedure Acquire (Vendor, Device : Interfaces.Unsigned_16;
                      Fuse : Interfaces.Unsigned_32; Success : out Boolean);
   procedure Release (Success : out Boolean);
end Intel_GPU_ADLN_Forcewake;
