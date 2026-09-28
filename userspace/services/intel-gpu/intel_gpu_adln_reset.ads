with Interfaces;
-- One instance per exclusively owned GPU. Not concurrent/reentrant. Hold must
-- acquire the authenticated inventory's domains and retain them on failure.
-- Read/Write are bounded ordered MMIO operations; Now follows the stop/reset
-- helpers' short-interval timing contract (Last means unavailable).
generic
   with function Read (Offset : Interfaces.Unsigned_32) return Interfaces.Unsigned_32;
   with procedure Write (Offset, Value : Interfaces.Unsigned_32);
   with procedure Hold (Success : out Boolean);
   with function Now return Interfaces.Unsigned_64;
   with procedure Pause;
   Poll_Limit : Positive := 100_000;
package Intel_GPU_ADLN_Reset is
   type Result is (Rejected, Forcewake_Failed, Stop_Failed, Prepare_Failed,
                   Reset_Failed, Cleanup_Failed, Complete);
   function Failure_Engine return Natural;
   -- Zero means no engine-specific failure; otherwise Engine'Pos + 1.
   -- Cleanup failure takes precedence, matching the returned status.
   procedure Execute (Vendor, Device : Interfaces.Unsigned_16;
                      Fuse : Interfaces.Unsigned_32; Status : out Result);
   -- Complete means all selected engines stopped/prepared, full GT reset
   -- acknowledged twice and settled, preparation request bits read back clear.
   -- Forcewake stays held. No engine restart, firmware upload or PTE writes.
   -- After any admitted attempt the instance cannot be reused.
end Intel_GPU_ADLN_Reset;
