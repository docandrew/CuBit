with Interfaces;
package Intel_GPU_ADS_Buffer is
   type Prepared_Backing is record
      Ready : Boolean := False;
      DMA_Address, CPU_Address, Capacity : Interfaces.Unsigned_64 := 0;
   end record;
   function Prepared return Prepared_Backing;
   -- One-shot CPU backing preparation after native reset succeeds. This does
   -- not construct ADS content, flush for device access or publish any GGTT PTE.
   -- Retained on failure or driver exit, like existing firmware staging.
   function Prepare return String;
   -- Called by the serialized GGTT preparation callback after retaining the
   -- exact extent above the platform WOPCM pin bias and below GuC's limit.
   -- GPU_Start is a GPU VA, never a CPU/DMA address. Caller must have admitted
   -- the expected GuC70.49.4 blob and excluded firmware/scanout reservations.
   -- Consumes one attempt even on rejection; backing remains retained.
   -- Success means CPU contents plus x86 cache maintenance, not publication.
   procedure Initialize
     (GPU_Start, Bytes : Interfaces.Unsigned_64; Success : out Boolean);
   function Initialized_GPU_Start return Interfaces.Unsigned_64;
end Intel_GPU_ADS_Buffer;
