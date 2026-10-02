with Interfaces; use Interfaces;
package Native_GPU_Presentation is
   -- Caller serializes these operations and pins recipient endpoint slots.
   -- Parent must be acquired and owner-forwardable. This creates a terminal
   -- read-only child, not an attachment. Offset/Bytes are whole pages.
   -- 0 success, 1 invalid/denied; Output cleared on failure. Caller owns the
   -- child until confirmed retired; returning the parent does not retire it.
   function Forward
     (Desktop_Slot, Parent, Offset, Bytes : Unsigned_64;
      Output : access Unsigned_64) return Unsigned_32
     with Export, Convention => C, External_Name => "cubit_intel_forward_presentation";
   -- Completed LINEAR BGRA8888 pixels only, never tiled/compressed images or
   -- in-flight GPU output. Child remains caller-owned on every result.
   -- 0 accepted; 1..6 Desktop status; 7 local/uncertain protocol error.
   -- Success is NOT a reader-release fence. On uncertainty revoke the child
   -- and retain backing until retirement; never infer rejection or retry.
   function Attach_Linear
     (Desktop_Slot, Surface, Child, Width, Height, Pitch : Unsigned_64)
      return Unsigned_32
     with Export, Convention => C, External_Name => "cubit_intel_attach_linear";
   -- 0 retired, 1 still retained (or retirement query uncertain), 2 rejected.
   -- Poll this same child, not Forward/Attach. Does not free BOs or prove GPU
   -- quiescence. Desktop returns its read acquisition on replacement/destroy.
   function Retire (Child : Unsigned_64) return Unsigned_32
     with Export, Convention => C, External_Name => "cubit_intel_retire_presentation";
end Native_GPU_Presentation;
