with Intel_GPU_ADLN_L3_Commands;
-- SPDX-License-Identifier: MIT
-- Adapted from Linux v6.16 gen8_engine_cs.c: gen12_emit_flush_rcs,
-- gen12_emit_aux_table_inv, and intel_workarounds.c: intel_engine_emit_ctx_wa.
-- Copyright (c) 2014 Intel Corporation.
-- Full MIT permission/warranty notice: intel_gpu_adln_lrc_template.adb.
with Intel_GPU_ADLN_Context_Settings;
with Intel_GPU_ADLN_Batch_Start;
with Intel_GPU_Arbitration_Command;
with Intel_GPU_ADLN_Barrier;
with Intel_GPU_Submission_Image;
package body Intel_GPU_ADLN_Context_Init with SPARK_Mode is
   Barrier : constant Intel_GPU_ADLN_Barrier.Barrier_Words :=
     Intel_GPU_ADLN_Barrier.Flush_And_Invalidate;
   function Build (Read_Valid : Boolean; WM_Chicken2 : Unsigned_32;
                   Sequence_Value : Unsigned_32 := Completion_Value) return Segment is
      Result : Segment;
      Settings : constant Intel_GPU_ADLN_Context_Settings.Segment :=
        Intel_GPU_ADLN_Context_Settings.Build (Read_Valid, WM_Chicken2);
      Batch : constant Intel_GPU_ADLN_Batch_Start.Command_Words :=
        Intel_GPU_ADLN_Batch_Start.Build;
   begin
      if not Settings.Valid or Sequence_Value = 0 then return Result; end if;
      for I in Barrier'Range loop
         Result.Words (I) := Barrier (I);
         Result.Words (36 + I) := Barrier (I);
         Result.Words (64 + I) := Barrier (I);
      end loop;
      for I in Batch'Range loop
         Result.Words (58 + I) := Batch (I);
      end loop;
      for I in Settings.Words'Range loop
         Result.Words (22 + I) := Settings.Words (I);
      end loop;
      -- Same context-relative post-sync destination as the barriers. Full
      -- flush/invalidation is already complete; stall before the final marker.
      -- CS_STALL | STORE_DATA_INDEX | QW_WRITE | FLUSH_ENABLE.
      Result.Words (86 .. 91) :=
        [16#7A000004#, 16#00304080#, Completion_Offset, 0,
         Sequence_Value, 0];
      -- Balance Batch_Start's arbitration disable before exhausting the ring.
      -- TGL PRM vol2a pp957-958 requires paired off/on in the same dispatch;
      -- Linux gen12_emit_fini_breadcrumb_tail likewise restores arbitration.
      -- Polling replaces USER_INTERRUPT here; NOOP keeps the pair aligned.
      -- The following arbitration check supplies an explicit preemption point.
      Result.Words (92 .. 95) :=
        [Intel_GPU_Arbitration_Command.Enable, 0, 16#02800000#, 0];
      Result.Valid := True;
      return Result;
   end Build;
   function Build_Setup (Read_Valid : Boolean; WM_Chicken2 : Unsigned_32)
     return Segment is
      Result : Segment := Build (Read_Valid, WM_Chicken2);
   begin
      if not Result.Valid then return Result; end if;
      -- No batch call and no associated arbitration off/on pair. Keep the
      -- final arbitration check and ordered HWSP completion from Build.
      Result.Words (58 .. 63) := [others => 0];
      Result.Words (92) := 0;
      return Result;
   end Build_Setup;
   function Build_L3 (Sequence_Value : Unsigned_32) return Segment is
      Result : Segment := Build (True, 0, Sequence_Value);
   begin
      if not Result.Valid then return Result; end if;
      -- Replace the context settings region with L3 write + sample.
      Result.Words (22 .. 35) := [others => 0];
      for I in Intel_GPU_ADLN_L3_Commands.Initialize_And_Sample'Range loop
         Result.Words (22 + I) :=
           Intel_GPU_ADLN_L3_Commands.Initialize_And_Sample (I);
      end loop;
      -- No private-batch branch, and therefore no arbitration disable to
      -- balance. Keep the final MI_ARB_CHECK preemption point intact.
      Result.Words (58 .. 63) := [others => 0];
      Result.Words (92) := 0;
      return Result;
   end Build_L3;
   function Build_Batch
     (Read_Valid : Boolean; WM_Chicken2, Sequence_Value : Unsigned_32;
      Batch_GPU : Unsigned_64) return Segment is
      Result : Segment := Build (Read_Valid, WM_Chicken2, Sequence_Value);
      Branch : constant Intel_GPU_ADLN_Batch_Start.Encoded_Batch :=
        Intel_GPU_ADLN_Batch_Start.Build_At (Batch_GPU);
   begin
      if not Result.Valid or else not Branch.Valid then return (others => <>); end if;
      for I in Branch.Words'Range loop
         Result.Words (58 + I) := Branch.Words (I);
      end loop;
      return Result;
   end Build_Batch;
   function Build_Draw (Read_Valid : Boolean; WM_Chicken2, Sequence_Value : Unsigned_32)
     return Segment is
   begin
      return Build_Batch (Read_Valid, WM_Chicken2, Sequence_Value,
                         Intel_GPU_Submission_Image.Draw_Batch_VA);
   end Build_Draw;
end Intel_GPU_ADLN_Context_Init;
