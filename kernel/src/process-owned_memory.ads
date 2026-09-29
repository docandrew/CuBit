with Interfaces; use Interfaces;
-- Native implementation boundary for owned RW/NX allocation syscalls.
-- Callers own a live execution pin for PID. No caller holds addressSpaceLock.
package Process.Owned_Memory is
   Maximum_Bytes : constant Unsigned_64 := 16 * 1024 * 1024;
   -- RW/NX normal RAM only. Returns zero on failure; bytes round up to pages.
   procedure Allocate (PID : ProcessID; Bytes : Unsigned_64; Base : out Unsigned_64);
   -- Exact original base/rounded size only. Grants keep their own frame pins;
   -- releasing the owner's mapping never invalidates a receiver's acquisition.
   procedure Release (PID : ProcessID; Base, Bytes : Unsigned_64; Success : out Boolean);
   -- Mode 0 inaccessible, 1 read-only, 3 read/write; always NX. The rounded
   -- range must lie within one live allocation. Existing grants are unchanged.
   procedure Protect (PID : ProcessID; Base, Bytes, Mode : Unsigned_64;
                      Success : out Boolean);

   -- Legacy physical mappers must hold this lock from alias admission through
   -- publication. Order: mailbox/grant -> owned-memory -> address-space -> buddy.
   procedure Lock;
   procedure Unlock;
   function Physical_Conflict (Base, Bytes : Unsigned_64) return Boolean;
   -- Requires Lock. This scans retained frame inventory, not inferred PTE ownership.

   -- Requires Lock throughout process frame-list reclamation. Call after that
   -- list is freed, before unlocking or reusing PID. Caller guarantees the
   -- address space is quiescent and closed to new mapping admission.
   procedure Forget_Exited (PID : ProcessID; Retired : out Natural);
end Process.Owned_Memory;
