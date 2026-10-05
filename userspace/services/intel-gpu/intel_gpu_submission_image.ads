with Interfaces; use Interfaces;
with Intel_GPU_Submission_Backing;
package Intel_GPU_Submission_Image with SPARK_Mode is
   GGTT_Bytes : constant Unsigned_64 := 80 * 1024;
   Batch_VA : constant Unsigned_64 := 16#200000#;
   Draw_Batch_Offset : constant := 1024;
   Draw_Batch_VA : constant Unsigned_64 := Batch_VA + Draw_Batch_Offset;
   Draw_URB_KiB : constant := 512;
   -- Minimal occupancy for the first correctness probe, not a performance
   -- configuration. Scheduling this batch still requires live admission.
   Draw_VS_Threads : constant := 1;
   Draw_PS_Threads : constant := 1;
   Completion_VA : constant Unsigned_64 := Intel_GPU_Submission_Backing.Completion_GPU_VA;
   pragma Compile_Time_Error (Completion_VA /= Batch_VA + 4096,
                              "private completion mapping changed");
   -- Private16KiB destination for the forthcoming offscreen rendering probe.
   -- No scanout mapping, format/tiling state or drawing command is implied.
   Offscreen_VA : constant Unsigned_64 := Intel_GPU_Submission_Backing.Offscreen_GPU_VA;
   Batch_Probe_Value : constant Unsigned_32 := 16#43554249#;
   Copy_Source_Offset : constant := 256;
   Copy_Result_Offset : constant := 128;
   Copy_Probe_Value : constant Unsigned_32 := 16#43504348#;
   pragma Compile_Time_Error
     (Copy_Source_Offset mod 128 /= 0 or Copy_Result_Offset mod 128 /= 0 or
      Copy_Source_Offset < 128 or Copy_Result_Offset < 128 or
      Copy_Source_Offset = Copy_Result_Offset or
      Copy_Source_Offset + 128 > 4096 or Copy_Result_Offset + 128 > 4096,
      "copy diagnostic must use distinct owned lines outside marker/L3 data");
   Byte_Count : constant := Natural
     (Intel_GPU_Submission_Backing.After_Last - Intel_GPU_Submission_Backing.First);
   type Image_Words is array (Natural range 0 .. Byte_Count / 4 - 1) of Unsigned_32;
   type Backing_Pages is array (Natural range 0 .. Byte_Count / 4096 - 1) of Unsigned_64;
   type Image is record
      Valid : Boolean := False;
      Words : Image_Words := [others => 0];
   end record;
   -- Output covers Byte_Count bytes starting at DMA_Start, the exclusive
   -- context backing extent, not its parent firmware allocation base.
   -- GGTT_Start is the RESERVED80KiB GPU range.
   -- Ring starts empty; batch stores a fixed probe at Completion_VA through
   -- the private PPGTT, copies the visibility source to its result, then ends.
   -- Source/result offsets256/128 are disjoint from marker/L3 data on ADL-N
   -- cache lines. The source is filled by the CPU only after publication.
   -- It is not queued here. Completion and separate
   -- engine HWSP are zero. Engine HWSP is NOT mapped in the private PPGTT
   -- or the context/ring GGTT range; it requires its own GGTT publication.
   -- The four offscreen pages are zeroed and mapped only in private PPGTT.
   -- Render state page contains binding0 -> surface at offset64. Cache policy
   -- is the installed ADL-N uncached MOCS entry. An immutable drawing batch
   -- at batch-page offset1024 references this state, but is NOT queued here.
   -- Native startup must admit live URB capacity and MOCS/PAT readiness before
   -- branching to it. Marker and drawing batches never overlap.
   -- Fixed triangle vertices at256; SF/clip viewport at512, CC viewport576.
   -- Full blend state at640; RT0 writable, entries1..7 channel-disabled.
   -- Disabled CPS array at736: sixteen32-byte viewport entries.
   -- Color-calculation state at1280: alpha reference and blend constants zero.
   -- Shader binaries live in the separate instruction page at207000.
   -- No memory write, GPU publication, registration or execution is performed.
   function Build (DMA_Start, GGTT_Start : Unsigned_64) return Image
     with Post => (if not Build'Result.Valid then
       (for all Word of Build'Result.Words => Word = 0));
   -- Page addresses describe retained backing; no physical adjacency required.
   function Build (Pages : Backing_Pages; GGTT_Start : Unsigned_64) return Image
     with Post => (if not Build'Result.Valid then
       (for all Word of Build'Result.Words => Word = 0));
   -- Application context using a separately materialized VM root. The owner
   -- supplies this root, never a request payload. It must retain ALL VM pages
   -- disjoint from this extent; numeric checks can only exclude root overlap.
   -- Ring, former bootstrap tables/batch, and completion storage remain zero.
   -- No GPU publication or authorization is implied by a valid image.
   function Build_For_VM (DMA_Start, GGTT_Start, Root_DMA : Unsigned_64) return Image
     with Post => (if not Build_For_VM'Result.Valid then
       (for all Word of Build_For_VM'Result.Words => Word = 0));
   function Build_For_VM
     (Pages : Backing_Pages; GGTT_Start, Root_DMA : Unsigned_64) return Image
     with Post => (if not Build_For_VM'Result.Valid then
       (for all Word of Build_For_VM'Result.Words => Word = 0));
   -- Same numeric admission as Build_For_VM without constructing its full
   -- submission image. No mapping, writes or live authority are established.
   function Valid_For_VM
     (Pages : Backing_Pages; GGTT_Start, Root_DMA : Unsigned_64) return Boolean;
end Intel_GPU_Submission_Image;
