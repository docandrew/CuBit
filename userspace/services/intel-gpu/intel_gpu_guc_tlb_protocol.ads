with Ada.Unchecked_Conversion;
with Interfaces; use Interfaces;
with System;
with Intel_GPU_GuC_Context_Event;
package Intel_GPU_GuC_TLB_Protocol with SPARK_Mode is
   -- Firmware ABI, not an MMIO register. Pinned Linux v6.16:
   -- gt/uc/abi/guc_actions_abi.h and intel_guc_submission.c
   -- guc_send_invalidate_tlb / intel_guc_tlb_invalidation_done.
   -- Request includes HXG, NOT the CT header. Completion is matched by its
   -- sequence payload, NOT the CT fence or a context ID. Caller must establish
   -- firmware support, reserve response space, retain backing and impose a
   -- deadline. Encoding a request does not establish any of those conditions.
   -- NOT the ADL-N path: v6.16 i915_pci.c assigns ADL-N to adl_p_info,
   -- without has_guc_tlb_invalidation. mtl_info explicitly enables it.
   -- Do not infer support from GuC readiness or firmware major version alone.
   type Target is (Engines, GuC);
   type Bits_4 is mod 2 ** 4 with Size => 4;
   type Bits_19 is mod 2 ** 19 with Size => 19;
   type Bit is mod 2 with Size => 1;
   type Options is record
      Kind : Unsigned_8 := 0;        -- engines=0, GuC=3
      Mode : Bits_4 := 0;            -- heavy=0, lite=1
      Reserved : Bits_19 := 0;
      Flush_Cache : Bit := 1;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Options use record
      Kind at 0 range 0 .. 7;
      Mode at 0 range 8 .. 11;
      Reserved at 0 range 12 .. 30;
      Flush_Cache at 0 range 31 .. 31;
   end record;
   function Encode is new Ada.Unchecked_Conversion (Options, Unsigned_32);
   function Decode is new Ada.Unchecked_Conversion (Unsigned_32, Options);
   subtype Words is Intel_GPU_GuC_Context_Event.Words;
   subtype Request_Words is Words (0 .. 2);
   -- Heavy invalidation plus cache flush. No lite-mode shortcut for reclaim.
   function Build (Sequence : Unsigned_32; Domain : Target) return Request_Words;
   type Completion is record
      Valid : Boolean := False;
      Sequence : Unsigned_32 := 0;
   end record;
   -- Exact pinned event layout: GuC EVENT 7001 + one full-width sequence.
   -- A valid event is not sufficient: lifecycle must match an outstanding
   -- generation and reject stale/duplicate events before freeing anything.
   function Decode_Completion (Payload : Words) return Completion
     with Post =>
       Decode_Completion'Result.Valid =
         (Payload'Length = 2 and then Payload (Payload'First) = 16#90007001#)
       and then
         (if Decode_Completion'Result.Valid then
            Decode_Completion'Result.Sequence = Payload (Payload'First + 1)
          else Decode_Completion'Result.Sequence = 0);
end Intel_GPU_GuC_TLB_Protocol;
