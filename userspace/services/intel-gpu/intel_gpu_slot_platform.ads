with Interfaces;
with Intel_GPU_Slot_Storage;
package Intel_GPU_Slot_Platform is
   -- Per-session tables on owned CPU reservations (syscalls 123/124), not
   -- GPU memory. A commit appends at most 16 MiB (Owned_Reservation_Policy).
   function Reserve (Bytes : Interfaces.Unsigned_64) return Interfaces.Unsigned_64;
   function Commit (Base, Offset, Bytes : Interfaces.Unsigned_64) return Boolean;
   Commit_Limit : constant := 16 * 1024 * 1024;
   package Storage is new Intel_GPU_Slot_Storage (Reserve, Commit, Commit_Limit);
end Intel_GPU_Slot_Platform;
