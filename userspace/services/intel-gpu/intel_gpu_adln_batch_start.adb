with Intel_GPU_Submission_Image;
package body Intel_GPU_ADLN_Batch_Start with SPARK_Mode is
   function Build return Command_Words is
      Address : constant Unsigned_64 := Intel_GPU_Submission_Image.Batch_VA;
   begin
      -- Linux v6.16 gen8_emit_bb_start, normal preemptible/nonsecure path.
      -- MI_ARB_ON_OFF enable, MI_BATCH_BUFFER_START (PPGTT), address,
      -- MI_ARB_ON_OFF disable, MI_NOOP. Six DWORDs preserve QWORD alignment.
      return [16#04000001#, 16#18800101#,
              Unsigned_32 (Address and 16#FFFF_FFFF#),
              Unsigned_32 (Shift_Right (Address, 32)), 16#04000000#, 0];
   end Build;
end Intel_GPU_ADLN_Batch_Start;
