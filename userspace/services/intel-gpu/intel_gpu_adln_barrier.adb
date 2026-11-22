-- SPDX-License-Identifier: MIT
-- Extracted from the existing context initialization sequence adapted from
-- Linux v6.16 gen8_engine_cs.c:gen12_emit_flush_rcs/gen12_emit_aux_table_inv.
-- Copyright (c) 2014 Intel Corporation.
-- Full MIT permission/warranty notice: intel_gpu_adln_lrc_template.adb.
package body Intel_GPU_ADLN_Barrier with SPARK_Mode is
   function Flush_And_Invalidate return Barrier_Words is
     ([16#7A000204#, 16#183070A1#, Scratch_Offset, 0, 0, 0,
       -- HDC/L3/tile/RT/depth/DC flush, depth/CS stall, indexed post-sync.
       16#02800101#, -- disable pre-parser
       16#7A000004#, 16#20344C1C#, Scratch_Offset, 0, 0, 0,
       -- Command/TLB/instruction/texture/VF/constant/state invalidation.
       16#11020001#, 16#4208#, 1, -- remapped CCS_AUX_INV
       16#0E01C003#, 0, 16#4208#, 0, 0, -- poll AUX invalidation
       16#02800100#]); -- restore pre-parser
   function Build (Sequence : Unsigned_64) return Segment is
      Result : Segment;
      Barrier : constant Barrier_Words := Flush_And_Invalidate;
   begin
      if Sequence = 0 then return Result; end if;
      for I in Barrier'Range loop
         Result.Words (I) := Barrier (I);
         pragma Loop_Invariant
           (for all J in Barrier'First .. I => Result.Words (J) = Barrier (J));
      end loop;
      -- Final breadcrumb: CS_STALL | STORE_DATA_INDEX | QW_WRITE |
      -- FLUSH_ENABLE quadword to the PPHWSP timeline slot.
      Result.Words (22 .. 29) :=
        [16#7A000004#, 16#00304080#, Timeline_Offset, 0,
         Unsigned_32 (Sequence mod 2 ** 32), Unsigned_32 (Sequence / 2 ** 32),
         16#02800000#, 0];
      Result.Valid := True;
      return Result;
   end Build;
end Intel_GPU_ADLN_Barrier;
