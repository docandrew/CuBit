-- SPDX-License-Identifier: MIT
-- Sequence adapted from Linux v6.16 intel_lrc.c and gen8_engine_cs.c.
-- Copyright (c) 2014 Intel Corporation
-- Full MIT permission/warranty notice: intel_gpu_adln_lrc_template.adb.
package body Intel_GPU_ADLN_LRC_Workaround with SPARK_Mode is
   function Build (Context_GPU, Capacity : Unsigned_64) return Indirect_Batch is
      Result : Indirect_Batch;
      Base : Unsigned_32;
   begin
      if Context_GPU = 0 or else Context_GPU mod 4096 /= 0 or else
        Context_GPU >= 16#FEE0_0000# or else Capacity < 16 * 4096 or else
        16 * 4096 > 16#FEE0_0000# - Context_GPU
      then return Result; end if;
      Base := Unsigned_32 (Context_GPU);
      -- Timestamp -> GPR0 -> timestamp twice; restore command-buffer control;
      -- restore saved GPR0; invalidate auxiliary tables and wait; invalidate
      -- instruction state cache. Sources: pinned Linux v6.16 intel_lrc.c and
      -- gen8_engine_cs.c, documented in intel-gpu-context-registration.md.
      Result.Words :=
        [16#14C80002#, 16#600#, Base + 16#108C#, 0,
         16#150C0001#, 16#600#, 16#3A8#,
         16#150C0001#, 16#600#, 16#3A8#,
         16#14C80002#, 16#600#, Base + 16#12DC#, 0,
         16#150C0001#, 16#600#, 16#84#,
         16#14C80002#, 16#600#, Base + 16#11D4#, 0,
         16#11020001#, 16#4208#, 1,
         16#0E01C003#, 0, 16#4208#, 0, 0,
         16#11000001#, 16#20D8#, 16#00400040#];
      Result.Valid := True;
      return Result;
   end Build;
end Intel_GPU_ADLN_LRC_Workaround;
