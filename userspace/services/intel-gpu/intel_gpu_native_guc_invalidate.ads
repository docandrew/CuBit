with Interfaces; use Interfaces;
with Intel_GPU_Native_TLB_IO;
with Intel_GPU_GuC_MMIO_Invalidate;
generic
   -- Trusted live gate must include mapped reset pages, device ownership,
   -- forcewake/power, prior-access drain and exclusive translation updates.
   -- Use only on platforms selecting the Gen12 GuC MMIO fallback.
   with function Gate return Boolean;
   with procedure Clock_US (Value : out Unsigned_64; OK : out Boolean);
package Intel_GPU_Native_GuC_Invalidate is
   procedure Write_Request (Value : Unsigned_32; OK : out Boolean);
   procedure Read_Status (Value : out Unsigned_32; OK : out Boolean);
   package Completion is new Intel_GPU_GuC_MMIO_Invalidate
     (Gate, Write_Request, Read_Status, Clock_US);
   -- Keep each Completion.Attempt through its entire one-shot lifetime.
   -- Completion confirms the request bit cleared, not context deregistration,
   -- CPU grant retirement or permission to release the underlying RAM.
private
   package IO is new Intel_GPU_Native_TLB_IO (Gate, GuC_Only => True);
end Intel_GPU_Native_GuC_Invalidate;
