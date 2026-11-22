with System;
with Interfaces; use Interfaces;
-- Linux-hosted stand-in for Intel_GPU_Slot_Storage regions: zero-filled,
-- aligned heap memory, never freed (test lifetime).
package Test_Storage is
   Alignment : constant := 64;
   -- The driver's G2H answer room: a 16 KiB ring less i915's 4 KiB reserve.
   Reply_Words : constant := 3072;
   function Allocate (Bytes : Unsigned_64) return System.Address;
end Test_Storage;
