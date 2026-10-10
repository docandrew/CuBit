with Interfaces; use Interfaces;
-- Per-process hardware status page (PPHWSP): page 0 of every CuBit logical
-- ring context image. Linux v6.16 names no hardware or GuC writer of a
-- single-LRC PPHWSP and GuC engine-reset restore excludes it, so these are
-- driver-defined slots (docs/gpu-async-submission.md, H1).
package Intel_GPU_ADLN_PPHWSP with SPARK_Mode, Pure is
   Page_Bytes : constant := 4096;
   subtype Byte_Offset is Unsigned_32 range 0 .. Page_Bytes - 1;
   -- LRC_PPHWSP_SCRATCH (i915 intel_lrc.h dword 0x34; xe
   -- LRC_PPHWSP_FLUSH_INVAL_SCRATCH_ADDR): every flush/invalidate barrier
   -- aims its quadword post-sync write here. Never a completion signal.
   Scratch_Offset : constant Byte_Offset := 16#D0#;
   -- Per-context timeline (xe's per-LRC seqno slot): a little-endian u64
   -- written ONLY by the final breadcrumb of each ring segment, so it is
   -- monotonic. Read high, low, high (H1: single-copy atomicity unproven).
   Timeline_Offset : constant Byte_Offset := 16#200#;
   Timeline_Bytes : constant := 8;
   -- DWORD indices of the timeline's halves within the page.
   Timeline_Low_Word : constant := Timeline_Offset / 4;
   Timeline_High_Word : constant := Timeline_Low_Word + 1;
   -- MI_FLUSH_DW quadword stores need 8-byte alignment with address bit 5
   -- clear (i915 intel_timeline.c hwsp_alloc, gen8_engine_cs.h), keeping
   -- the slot usable by the non-render engines' breadcrumbs.
   MI_Flush_Address_Bit_5 : constant := 16#20#;
   pragma Compile_Time_Error
     (Timeline_Offset mod Timeline_Bytes /= 0 or else
      (Timeline_Offset and MI_Flush_Address_Bit_5) /= 0,
      "timeline slot must be 8-aligned with address bit 5 clear");
   pragma Compile_Time_Error
     (Scratch_Offset + Timeline_Bytes > Timeline_Offset,
      "barrier scratch quadword must not overlap the timeline slot");
end Intel_GPU_ADLN_PPHWSP;
