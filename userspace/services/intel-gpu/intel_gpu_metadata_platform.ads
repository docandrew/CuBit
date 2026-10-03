with Interfaces;
with Intel_GPU_Metadata_Arena;
with Intel_GPU_Metadata_Initialize;
package Intel_GPU_Metadata_Platform is
   -- Bind growth to CPU reservation syscalls, not the GPU/supervisor allocator.
   function Reserve (Bytes : Interfaces.Unsigned_64) return Interfaces.Unsigned_64;
   function Commit (Base, Offset, Bytes : Interfaces.Unsigned_64) return Boolean;
   package Storage is new Intel_GPU_Metadata_Arena
     (Reserve, Commit, Intel_GPU_Metadata_Initialize.Clear);
end Intel_GPU_Metadata_Platform;
