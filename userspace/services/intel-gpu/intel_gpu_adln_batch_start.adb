with Intel_GPU_Submission_Image;
with Intel_GPU_Arbitration_Command;
with Intel_GPU_VA_Encoding;
package body Intel_GPU_ADLN_Batch_Start with SPARK_Mode is
   function Build_At (GPU : Unsigned_64) return Encoded_Batch is
      Address : Unsigned_64;
   begin
      if GPU = 0 or else GPU >= 2 ** 48 or else GPU mod 8 /= 0 then
         return (others => <>);
      end if;
      Address := Intel_GPU_VA_Encoding.Canonical (GPU);
      -- Linux v6.16 gen8_emit_bb_start, normal preemptible/nonsecure path.
      -- MI_ARB_ON_OFF enable, MI_BATCH_BUFFER_START (PPGTT), address,
      -- MI_ARB_ON_OFF disable, MI_NOOP. Six DWORDs preserve QWORD alignment.
      return (Valid => True, Words =>
             [Intel_GPU_Arbitration_Command.Enable, 16#18800101#,
              Unsigned_32 (Address and 16#FFFF_FFFF#),
              Unsigned_32 (Shift_Right (Address, 32)),
              Intel_GPU_Arbitration_Command.Disable, 0]);
   end Build_At;
   function Build (Kind : Batch_Kind := Marker) return Command_Words is
   begin
      return Build_At
        (if Kind = Marker then Intel_GPU_Submission_Image.Batch_VA
         else Intel_GPU_Submission_Image.Draw_Batch_VA).Words;
   end Build;
end Intel_GPU_ADLN_Batch_Start;
