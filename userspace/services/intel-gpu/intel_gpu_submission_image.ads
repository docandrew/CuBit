with Interfaces; use Interfaces;
with Intel_GPU_Submission_Backing;
package Intel_GPU_Submission_Image with SPARK_Mode is
   GGTT_Bytes : constant Unsigned_64 := 80 * 1024;
   Batch_VA : constant Unsigned_64 := 16#200000#;
   Completion_VA : constant Unsigned_64 := Batch_VA + 4096;
   Batch_Probe_Value : constant Unsigned_32 := 16#43554249#;
   Byte_Count : constant := Natural
     (Intel_GPU_Submission_Backing.After_Last - Intel_GPU_Submission_Backing.First);
   type Image_Words is array (Natural range 0 .. Byte_Count / 4 - 1) of Unsigned_32;
   type Image is record
      Valid : Boolean := False;
      Words : Image_Words := [others => 0];
   end record;
   -- Output covers allocation-relative8C000..A7000 only. DMA_Base is the
   -- retained1MiB allocation base; GGTT_Start is the RESERVED80KiB GPU range.
   -- Ring starts empty; batch stores a fixed probe at Completion_VA through
   -- the private PPGTT then ends. It is not queued here. Completion and separate
   -- engine HWSP are zero. Engine HWSP is NOT mapped in the private PPGTT
   -- or the context/ring GGTT range; it requires its own GGTT publication.
   -- No memory write, GPU publication, registration or execution is performed.
   function Build (DMA_Base, GGTT_Start : Unsigned_64) return Image
     with Post => (if not Build'Result.Valid then
       (for all Word of Build'Result.Words => Word = 0));
end Intel_GPU_Submission_Image;
