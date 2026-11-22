-- Register programming derived from Linux v6.16 intel_workarounds.c,
-- gen12_ctx_workarounds_init and intel_engine_emit_ctx_wa.
-- Copyright (c) 2014-2018 Intel Corporation. MIT license; full notice retained
-- in intel_gpu_adln_lrc_template.adb in this directory.
package body Intel_GPU_ADLN_Context_Settings with SPARK_Mode is
   function Build (Read_Valid : Boolean; WM_Chicken2 : Unsigned_32) return Segment is
      Result : Segment;
   begin
      if not Read_Valid or WM_Chicken2 = Unsigned_32'Last then return Result; end if;
      Result.Words :=
        [16#1100000B#,
         16#2580#, 16#00060002#, -- thread-group GPGPU preemption, masked
         16#5584#, WM_Chicken2 or 16#20#, -- preserve unrelated WM bits
         16#6604#, 16#E0040000#, -- GS224/TDS128 timers, no CPU RMW
         16#7018#, 16#20002000#, -- disable LE/GE depth-test optimization
         16#7300#, 16#00400040#, -- disable TDC load-balance calculation
         16#7304#, 16#02000200#, -- disable CPS-aware color pipeline
         0];
      Result.Valid := True;
      return Result;
   end Build;
end Intel_GPU_ADLN_Context_Settings;
