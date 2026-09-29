-- SPDX-License-Identifier: MIT
-- Adapted from Linux v6.16 gen8_engine_cs.c: gen12_emit_flush_rcs,
-- gen12_emit_aux_table_inv, and intel_workarounds.c: intel_engine_emit_ctx_wa.
-- Copyright (c) 2014 Intel Corporation.
-- Full MIT permission/warranty notice: intel_gpu_adln_lrc_template.adb.
with Intel_GPU_ADLN_Context_Settings;
with Intel_GPU_ADLN_Batch_Start;
package body Intel_GPU_ADLN_Context_Init with SPARK_Mode is
   type Barrier_Words is array (Natural range 0 .. 21) of Unsigned_32;
   Barrier : constant Barrier_Words :=
     [16#7A000204#, 16#183070A1#, 16#D0#, 0, 0, 0,
      -- PIPE_CONTROL: HDC, L3/tile/RT/depth/DC flush, depth/CS stall,
      -- QW post-sync write through context-relative HWSP store-data index.
      16#02800101#, -- MI_ARB_CHECK: disable pre-parser
      16#7A000004#, 16#20344C1C#, 16#D0#, 0, 0, 0,
      -- Command/TLB/instruction/texture/VF/constant/state invalidation,
      -- CS stall and context-relative post-sync write.
      16#11020001#, 16#4208#, 1, -- remapped LRI, CCS_AUX_INV
      16#0E01C003#, 0, 16#4208#, 0, 0, -- register-poll until AUX_INV clears
      16#02800100#]; -- re-enable pre-parser
   function Build (Read_Valid : Boolean; WM_Chicken2 : Unsigned_32) return Segment is
      Result : Segment;
      Settings : constant Intel_GPU_ADLN_Context_Settings.Segment :=
        Intel_GPU_ADLN_Context_Settings.Build (Read_Valid, WM_Chicken2);
      Batch : constant Intel_GPU_ADLN_Batch_Start.Command_Words :=
        Intel_GPU_ADLN_Batch_Start.Build;
   begin
      if not Settings.Valid then return Result; end if;
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
         Completion_Value, 0];
      Result.Valid := True;
      return Result;
   end Build;
end Intel_GPU_ADLN_Context_Init;
