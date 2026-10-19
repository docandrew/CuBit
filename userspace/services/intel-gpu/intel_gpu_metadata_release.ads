with Interfaces;
with Intel_GPU_Metadata_Arena.Releasing;
with Intel_GPU_Metadata_Platform;
package Intel_GPU_Metadata_Release is
   -- Per-session metadata reservations go back when the session's slot is
   -- recycled (owned reservation release, syscall 125).
   function Unreserve (Base, Bytes : Interfaces.Unsigned_64) return Boolean;
   package Releasing is new Intel_GPU_Metadata_Platform.Storage.Releasing (Unreserve);
end Intel_GPU_Metadata_Release;
