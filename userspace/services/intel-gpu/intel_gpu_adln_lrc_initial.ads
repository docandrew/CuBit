with Interfaces; use Interfaces;
with Intel_GPU_ADLN_LRC_Template;
with Intel_GPU_ADLN_PPGTT;
package Intel_GPU_ADLN_LRC_Initial with SPARK_Mode is
   subtype Ring_Size_Log2 is Natural range 12 .. 21;
   type Initial_State is record
      Prepared : Boolean := False;
      Registers : Intel_GPU_ADLN_LRC_Template.Register_Page := [others => 0];
   end record;
   function Admissible (Ring_GPU, Root_DMA : Unsigned_64;
                        Ring_Log2 : Ring_Size_Log2) return Boolean is
     (Intel_GPU_ADLN_PPGTT.Valid_DMA_Page (Root_DMA) and then
      Ring_GPU /= 0 and then Ring_GPU mod 4096 = 0 and then
      Ring_GPU < 16#FEE0_0000# and then
      2 ** Ring_Log2 <= 16#FEE0_0000# - Ring_GPU);
   -- Initial EMPTY ring only, with inhibited first context restore.
   -- Ring_GPU is GGTT; Root_DMA is the PML4's physical/DMA address, NOT GGTT.
   -- Caller establishes ADL-N single-slice identity, owned initialized backing
   -- and all engine/context workarounds. Prepared is numeric assembly only,
   -- NOT safe-to-submit evidence. No hardware writes or publication occur.
   function Build (Ring_GPU, Root_DMA : Unsigned_64;
                   Ring_Log2 : Ring_Size_Log2) return Initial_State
     with Post => Build'Result.Prepared = Admissible (Ring_GPU, Root_DMA, Ring_Log2)
       and then (if not Build'Result.Prepared then
          (for all Word of Build'Result.Registers => Word = 0));
end Intel_GPU_ADLN_LRC_Initial;
