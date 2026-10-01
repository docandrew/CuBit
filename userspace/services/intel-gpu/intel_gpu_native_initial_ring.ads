with Interfaces;
with Intel_GPU_ADLN_Context_Init;
generic
   -- Stable retained CPU mapping for this instance; not mapping authority.
   CPU_Base, Backing_Bytes : Interfaces.Unsigned_64;
   -- Includes exact retained backing identity, initialization, GPU mapping
   -- and runtime authority. Exclusive additionally means never scheduled.
   with function Owner_Ready return Boolean;
   with function Exclusive_Ready return Boolean;
package Intel_GPU_Native_Initial_Ring is
   procedure Publish (Segment : Intel_GPU_ADLN_Context_Init.Segment;
                      Success : out Boolean);
   procedure Read_Marker (Value : out Interfaces.Unsigned_64;
                          OK : out Boolean);
   -- Reads the dedicated private-VM probe destination, not the ring HWSP.
   -- Caller must also verify ordered ring completion and scheduling disable.
   procedure Read_Batch_Result (Value : out Interfaces.Unsigned_64;
                                OK : out Boolean);
   -- Fixed L3 sample, separate from both completion and stencil scratch.
   -- A readable value is not admission: caller verifies ordered completion,
   -- sample freshness and the documented allocation fields.
   procedure Read_L3_Result (Value, Parameters : out Interfaces.Unsigned_32;
                             OK : out Boolean);
   -- Center, top-left, top-right, bottom-left, bottom-right of the fixed
   -- 64x64 linear BGRA8 target. Caller excludes pending target writes:
   -- before the first draw, or after draw completion and terminal disable.
   type Pixel_Samples is array (Natural range 0 .. 4) of Interfaces.Unsigned_32;
   procedure Read_Pixels (Values : out Pixel_Samples; OK : out Boolean);
   -- Diagnostic CPU snapshot, not scanout or zero-copy presentation. Caller
   -- must establish completed drawing and terminal scheduling-disable first.
   type Target_Image is array (Natural range 0 .. 4095) of Interfaces.Unsigned_32;
   procedure Read_Image (Values : out Target_Image; OK : out Boolean);
end Intel_GPU_Native_Initial_Ring;
