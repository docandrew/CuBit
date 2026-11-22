with Interfaces; use Interfaces;
-- ADL-N video-decode (VCS) ring segments: the MI_FLUSH_DW counterpart of the
-- RCS PIPE_CONTROL builders in Intel_GPU_ADLN_Barrier/_Context_Init.
-- Encodings from Linux v6.16 (hardware facts only, no code):
--   i915/gt/gen8_engine_cs.c:362-414  gen12_emit_flush_xcs: pre-parser off,
--     MI_FLUSH_DW STORE_INDEX|OP_STOREDW|INVALIDATE_TLB|INVALIDATE_BSD to the
--     PPHWSP scratch, AUX table invalidation, pre-parser on
--   i915/gt/gen8_engine_cs.c:168-221  per-engine AUX_INV (VD0 0x4218,
--     VD2 0x4298), LRI with MMIO remap then a register-poll semaphore
--   i915/gt/gen8_engine_cs.c:576-597  gen8_emit_bb_start (arbitration on,
--     BB_START bit 8 non-privileged PPGTT, arbitration off, NOOP)
--   i915/gt/gen8_engine_cs.c:784-810  gen12_emit_fini_breadcrumb_xcs: stalling
--     MI_FLUSH_DW without post-sync, then MI_FLUSH_DW store; user interrupt,
--     arbitration on, MI_ARB_CHECK, NOOP
--   i915/gt/gen8_engine_cs.h:118-140  MI_FLUSH_DW address: 8-aligned, bit 5 0
--   xe/instructions/xe_mi_commands.h:55-64  MI_FLUSH_IMM_QW (length 5-2)
--   i915/gt/intel_gpu_commands.h:37-188 opcodes and flag bits
-- The breadcrumb is a QUADWORD immediate store of the 64-bit timeline value.
-- Linux only uses the dword form; the qword form's single-copy atomicity is
-- unproven (gpu-async-submission.md H1), so readers keep high-low-high.
-- Encoding alone is not evidence of execution, engine ownership, or of the
-- target slot being retained; callers own ring publication and sequencing.
package Intel_GPU_ADLN_Video_Segment with SPARK_Mode is
   type Video_Engine is (VCS0, VCS2);   -- ADL-N physical VDBOX 0 and 2

   subtype Command_Word is Unsigned_32;

   -- MI opcodes: MI_INSTR(op, len) = op << 23 | len.
   MI_NOOP : constant Command_Word := 16#0000_0000#;
   MI_User_Interrupt : constant Command_Word := 16#0100_0000#;   -- 0x02
   MI_ARB_Check : constant Command_Word := 16#0280_0000#;        -- 0x05
   MI_ARB_On_Off : constant Command_Word := 16#0400_0000#;       -- 0x08
   MI_ARB_Enable : constant Command_Word := 16#0000_0001#;
   Preparser_Disable_Bit : constant Command_Word := 16#0000_0100#;
   MI_Flush_DW : constant Command_Word := 16#1300_0000#;         -- 0x26
   Flush_DW_Length_DW : constant Command_Word := 2;  -- 4 dwords
   Flush_DW_Length_QW : constant Command_Word := 3;  -- 5 dwords
   Flush_Store_Index : constant Command_Word := 16#0020_0000#;   -- bit 21
   Flush_Invalidate_TLB : constant Command_Word := 16#0004_0000#; -- bit 18
   Flush_Op_Store_DW : constant Command_Word := 16#0000_4000#;   -- bit 14
   Flush_Invalidate_BSD : constant Command_Word := 16#0000_0080#; -- bit 7
   Flush_Use_GTT : constant Command_Word := 16#0000_0004#;       -- bit 2
   MI_LRI_One_Remapped : constant Command_Word := 16#1102_0001#; -- 0x22, bit 17
   AUX_Invalidate : constant Command_Word := 1;
   MI_Semaphore_Poll_Register_EQ : constant Command_Word := 16#0E01_C003#;
   MI_BB_Start_PPGTT : constant Command_Word := 16#1880_0101#;   -- 0x31, bit 8

   function AUX_Invalidate_Register (Engine : Video_Engine) return Command_Word is
     (case Engine is when VCS0 => 16#4218#, when VCS2 => 16#4298#);

   -- PPHWSP layout shared with the async design (H1): barriers dump their
   -- post-sync writes at +0xD0; the timeline value lives at +0x200 and only
   -- the final breadcrumb writes it.
   PPHWSP_Scratch_Offset : constant Unsigned_32 := 16#D0#;
   PPHWSP_Timeline_Offset : constant Unsigned_32 := 16#200#;
   PPHWSP_Bytes : constant Unsigned_32 := 4096;
   Flush_Address_Bit_5 : constant Unsigned_32 := 16#20#;
   Slot_Alignment : constant := 8;

   -- Where the breadcrumb stores. Index: an offset in this context's PPHWSP
   -- (STORE_INDEX). GGTT: a driver-owned GGTT timeline page (USE_GTT); never
   -- a client PPGTT address (H3).
   type Target_Kind is (PPHWSP_Index, GGTT_Address);
   type Timeline_Target is record
      Kind : Target_Kind := PPHWSP_Index;
      Address : Unsigned_64 := 0;
   end record;

   function Valid_Target (Target : Timeline_Target) return Boolean is
     (Target.Address mod Slot_Alignment = 0 and then
      (Target.Address and Unsigned_64 (Flush_Address_Bit_5)) = 0 and then
      (case Target.Kind is
         when PPHWSP_Index =>
           Target.Address < Unsigned_64 (PPHWSP_Bytes) and then
           Target.Address /= Unsigned_64 (PPHWSP_Scratch_Offset),
         when GGTT_Address =>
           Target.Address /= 0 and then Target.Address < 2 ** 32));

   PPHWSP_Timeline : constant Timeline_Target :=
     (Kind => PPHWSP_Index, Address => Unsigned_64 (PPHWSP_Timeline_Offset));

   -- Pre-batch: TLB + BSD invalidation and AUX table invalidation inside a
   -- pre-parser disable. Its post-sync store goes to the scratch only.
   type Invalidate_Words is array (Natural range 0 .. 13) of Command_Word;
   function Invalidate (Engine : Video_Engine) return Invalidate_Words
   with Post => Invalidate'Result (2) = PPHWSP_Scratch_Offset and
     Invalidate'Result (6) = AUX_Invalidate_Register (Engine) and
     Invalidate'Result (10) = AUX_Invalidate_Register (Engine);

   -- Stalling flush, qword timeline store, user interrupt and tail.
   -- Even length keeps the ring tail qword aligned.
   type Breadcrumb_Words is array (Natural range 0 .. 13) of Command_Word;
   Value_Low_Index : constant := 7;
   Value_High_Index : constant := 8;
   function Breadcrumb (Target : Timeline_Target; Value : Unsigned_64)
     return Breadcrumb_Words
   with Pre => Valid_Target (Target) and Value /= 0,
     Post => Breadcrumb'Result (Value_Low_Index) =
         Unsigned_32 (Value mod 2 ** 32) and
       Breadcrumb'Result (Value_High_Index) = Unsigned_32 (Value / 2 ** 32) and
       Breadcrumb'Result (5) =
         (Unsigned_32 (Target.Address mod 2 ** 32) or
            (if Target.Kind = GGTT_Address then Flush_Use_GTT else 0));

   -- Batch dispatch segment: invalidate, BB_START, breadcrumb.
   type Command_Words is array (Natural range 0 .. 33) of Command_Word;
   Batch_Value_Low : constant := 20 + Value_Low_Index;
   Batch_Value_High : constant := 20 + Value_High_Index;
   type Segment is record
      Valid : Boolean := False;
      Words : Command_Words := [others => 0];
   end record;
   -- Batch_GPU: raw48, qword aligned, in this context's private PPGTT.
   -- Value: the context's next timeline value, nonzero, never reused.
   function Build_Batch
     (Engine : Video_Engine; Batch_GPU : Unsigned_64;
      Target : Timeline_Target; Value : Unsigned_64) return Segment
   with Post => Build_Batch'Result.Valid =
       (Batch_GPU /= 0 and Batch_GPU < 2 ** 48 and Batch_GPU mod 8 = 0 and
        Valid_Target (Target) and Value /= 0) and then
     (if Build_Batch'Result.Valid then
        Build_Batch'Result.Words (Batch_Value_Low) =
          Unsigned_32 (Value mod 2 ** 32) and
        Build_Batch'Result.Words (Batch_Value_High) =
          Unsigned_32 (Value / 2 ** 32) and
        Build_Batch'Result.Words (15) = MI_BB_Start_PPGTT and
        Build_Batch'Result.Words (16) = Unsigned_32 (Batch_GPU mod 2 ** 32) and
        Build_Batch'Result.Words (17) = Unsigned_32 (Batch_GPU / 2 ** 32)
      else (for all Word of Build_Batch'Result.Words => Word = 0));

   -- Timeline-only signal (no batch): breadcrumb words then NOOP padding.
   type Signal_Segment is record
      Valid : Boolean := False;
      Words : Breadcrumb_Words := [others => 0];
   end record;
   function Build_Signal (Target : Timeline_Target; Value : Unsigned_64)
     return Signal_Segment
   with Post => Build_Signal'Result.Valid =
       (Valid_Target (Target) and Value /= 0) and then
     (if Build_Signal'Result.Valid then
        Build_Signal'Result.Words = Breadcrumb (Target, Value)
      else (for all Word of Build_Signal'Result.Words => Word = 0));
end Intel_GPU_ADLN_Video_Segment;
