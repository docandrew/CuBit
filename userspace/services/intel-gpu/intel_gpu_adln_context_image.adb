-- SPDX-License-Identifier: MIT
-- Scratch sequence adapted from Linux v6.16 intel_lrc.c.
-- Copyright (c) 2014 Intel Corporation
-- Full MIT permission/warranty notice: intel_gpu_adln_lrc_template.adb.
with Intel_GPU_ADLN_LRC_Workaround;
package body Intel_GPU_ADLN_Context_Image with SPARK_Mode is
   function Admissible
     (Context_GPU, Capacity, Ring_GPU, Root_DMA : Unsigned_64;
      Ring_Log2 : Intel_GPU_ADLN_LRC_Initial.Ring_Size_Log2) return Boolean is
     (Intel_GPU_ADLN_LRC_Initial.Admissible (Ring_GPU, Root_DMA, Ring_Log2)
      and then Intel_GPU_ADLN_LRC_Workaround.Admissible (Context_GPU, Capacity)
      and then Context_GPU < 16#FEE00000#
      and then (if Context_GPU <= Ring_GPU then Ring_GPU - Context_GPU >= 65536
                else Context_GPU - Ring_GPU >= 2 ** Ring_Log2));
   function Build (Context_GPU, Capacity, Ring_GPU, Root_DMA : Unsigned_64;
                   Ring_Log2 : Intel_GPU_ADLN_LRC_Initial.Ring_Size_Log2)
      return Prepared_Image is
      Result : Prepared_Image;
      Initial : constant Intel_GPU_ADLN_LRC_Initial.Initial_State :=
        Intel_GPU_ADLN_LRC_Initial.Build (Ring_GPU, Root_DMA, Ring_Log2);
      Indirect : constant Intel_GPU_ADLN_LRC_Workaround.Indirect_Batch :=
        Intel_GPU_ADLN_LRC_Workaround.Build (Context_GPU, Capacity);
      Base : Unsigned_32;
      Scratch : Unsigned_32;
      type Predicate_Words is array (Natural range 0 .. 10) of Unsigned_32;
      Predicate : Predicate_Words;
   begin
      if not Initial.Prepared or else not Indirect.Valid then return Result; end if;
      if not Admissible (Context_GPU, Capacity, Ring_GPU, Root_DMA, Ring_Log2)
      then return Result; end if;
      -- Recheck locally for conversion proof; helpers already enforce this.
      if Context_GPU >= 16#FEE00000# then return Result; end if;
      Base := Unsigned_32 (Context_GPU);
      for I in Initial.Registers'Range loop
         Result.Words (1024 + I) := Initial.Registers (I);
      end loop;
      for I in Indirect.Words'Range loop
         Result.Words (14 * 1024 + I) := Indirect.Words (I);
      end loop;
      Result.Words (1024 + 19) := (Base + 15 * 4096) or 5;
      Result.Words (1024 + 21) := (Base + 14 * 4096) or 2;
      Result.Words (1024 + 23) := 13 * 64;
      Result.Words (15 * 1024) := 16#05000000#; -- per-context batch end
      -- Match the upstream scratch helper outside the length-delimited batch.
      -- ADL-N's indirect sequence does not branch here. Retain reserved space.
      Scratch := Base + 15 * 4096 - 8;
      Predicate := [16#10400002#, Scratch, 0, 0, 16#05008000#,
                    16#00800000#, 16#10400002#, Scratch, 0, 1, 16#05000000#];
      for I in Predicate'Range loop
         Result.Words (14 * 1024 + 512 + I) := Predicate (I);
      end loop;
      Result.Valid := True;
      return Result;
   end Build;
end Intel_GPU_ADLN_Context_Image;
