with Interfaces; use Interfaces;
with Intel_GPU_ADLN_LRC_Initial;
package Intel_GPU_ADLN_Context_Image with SPARK_Mode is
   type Image_Words is array (Natural range 0 .. 16383) of Unsigned_32;
   type Prepared_Image is record
      Valid : Boolean := False;
      Words : Image_Words := [others => 0];
   end record;
   -- Numeric initial RCS0 image, NOT live mapping or submission authority.
   -- Caller owns64KiB context, ring, root and all page tables; retains backing
   -- and completes GT/engine workarounds/visibility before registration.
   -- Capacity is context backing; ring capacity follows Ring_Log2.
   function Admissible
     (Context_GPU, Capacity, Ring_GPU, Root_DMA : Unsigned_64;
      Ring_Log2 : Intel_GPU_ADLN_LRC_Initial.Ring_Size_Log2) return Boolean;
   function Build (Context_GPU, Capacity, Ring_GPU, Root_DMA : Unsigned_64;
                   Ring_Log2 : Intel_GPU_ADLN_LRC_Initial.Ring_Size_Log2)
      return Prepared_Image
     with Post => Build'Result.Valid =
       Admissible (Context_GPU, Capacity, Ring_GPU, Root_DMA, Ring_Log2)
       and then (if not Build'Result.Valid then
       (for all Word of Build'Result.Words => Word = 0));
end Intel_GPU_ADLN_Context_Image;
