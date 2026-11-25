with Interfaces; use Interfaces;
with Owned_Reservation_Policy;
-- Owned memory: the one region API (ABI-002, three system calls).
--
--   Reserve       page-aligned virtual capacity in the owned aperture, no RAM;
--   Change_Range  one operation on a page range of one live region:
--                 Commit (fresh zero pages, read/write), Decommit (drop the
--                 frames, keep the range reserved), or set the access of
--                 committed pages (inaccessible, read-only, read/write);
--   Release       retire every committed page, then the region.
--
-- Each call does one thing; there are no flags. The arithmetic (range
-- admission, decoding, run splits, the access ceiling) is proved in
-- Owned_Reservation_Policy; this unit is the adapter that applies it to
-- page tables and frame lists, and is tested natively, not proved.
--
-- Commit is explicit, not on demand. Chosen (2026-11-11): a refused commit
-- is a clean failure at the call (CuAlloc returns null, the runtime raises
-- Storage_Error) instead of a kill at an arbitrary first touch; the page
-- fault path stays out of the owned-memory registry and its global lock;
-- and the records stay one per committed run instead of one per touched
-- page. Decommit therefore makes a range fault until it is committed
-- again, which then gives zero pages (madvise(MADV_DONTNEED) with explicit
-- recommit).
--
-- CPU-only memory: no GPU mapping or DMA authority is created. Grants of
-- owned pages pin their frames: decommit or release of the owner's mapping
-- never invalidates a receiver's acquisition; the frame (and its quota
-- charge) returns when the last pin goes.
--
-- Callers own a live execution pin for PID. No caller holds addressSpaceLock.
package Process.Owned_Memory is
   -- Alignment: the base's (Owned_Reservation_Policy.Valid_Alignment; 0 is
   -- a page); an invalid one is refused. Base is 0 on refusal.
   procedure Reserve (PID : ProcessID; Bytes, Alignment : Unsigned_64; Base : out Unsigned_64);

   -- The range [Address, Address + Bytes) must lie in one live region of
   -- PID. Commit: every page of the range unbacked, Bytes at most
   -- Owned_Reservation_Policy.Maximum_Commit_Bytes; all or nothing.
   -- Decommit: backed pages of the range are dropped, unbacked ones
   -- ignored; all or nothing unless a page-table retirement fails, which
   -- quarantines that run (retained until the process exits). Access
   -- operations: every page of the range committed; never executable.
   procedure Change_Range (PID : ProcessID; Address, Bytes : Unsigned_64;
                           Operation : Owned_Reservation_Policy.Range_Operation;
                           Success : out Boolean);

   -- Exact base and capacity of a live region. The region is quarantined
   -- at once, then its pages retired in steps of at most
   -- Owned_Reservation_Policy.Pages_Per_Locked_Step, the locks given back
   -- between steps. A failed run retirement retains the region and the
   -- backing not yet retired.
   procedure Release (PID : ProcessID; Base, Bytes : Unsigned_64; Success : out Boolean);

   -- Legacy physical mappers must hold this lock from alias admission through
   -- publication. Order: mailbox/grant -> owned-memory -> address-space -> buddy.
   procedure Lock;
   procedure Unlock;
   function Physical_Conflict (Base, Bytes : Unsigned_64) return Boolean;
   -- Requires Lock. This scans retained frame inventory, not inferred PTE ownership.

   -- Requires Lock throughout process frame-list reclamation. Call after that
   -- list is freed, before unlocking or reusing PID. Caller guarantees the
   -- address space is quiescent and closed to new mapping admission.
   -- Retired counts the regions the process left behind.
   procedure Forget_Exited (PID : ProcessID; Retired : out Natural);
end Process.Owned_Memory;
