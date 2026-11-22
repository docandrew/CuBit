with Interfaces; use Interfaces;
package Intel_GPU_ADLN_Offscreen_Batch with SPARK_Mode is
   type Words is array (Natural range <>) of Unsigned_32;
   subtype Page is Words (0 .. 1023);
   type Image is record
      Valid : Boolean := False;
      Count : Natural range 0 .. 1024 := 0;
      Data : Page := [others => 0];
   end record;
   -- Candidate fixed offscreen triangle, NOT a submission authority.
   -- All referenced addresses are the private Submission_Backing layout.
   -- Initial RCS context only: caller must establish L3/URB, MOCS, PAT,
   -- instruction/state visibility and stepping workarounds before execution.
   -- Parent ring must flush rendering and write its completion marker AFTER
   -- this second-level batch returns. BATCH_END itself is not completion.
   -- Sample patterns and initial stencil post-sync are explicit.
   -- Submission_Image retains this at its fixed drawing-batch offset.
   -- The native caller must admit live L3 capacity before branching to it.
   function Build (MOCS : Unsigned_32; Usable_URB_KiB, VS_Threads,
                   PS_Threads : Natural) return Image
     with Post =>
       (if Build'Result.Valid then Build'Result.Count in 1 .. 1024 and then
          Build'Result.Data (Build'Result.Count - 1) = 16#05000000#
        else Build'Result.Count = 0 and then
          (for all Word of Build'Result.Data => Word = 0));
end Intel_GPU_ADLN_Offscreen_Batch;
