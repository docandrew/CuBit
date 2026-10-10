with Interfaces; use Interfaces;
with Intel_GPU_ADLN_PPHWSP;
package Intel_GPU_ADLN_Context_Init with SPARK_Mode is
   -- Final breadcrumb destination: the PPHWSP timeline slot. Barrier
   -- post-syncs go to PPHWSP scratch instead (Intel_GPU_ADLN_Barrier).
   Timeline_Offset : constant Unsigned_32 := Intel_GPU_ADLN_PPHWSP.Timeline_Offset;
   Completion_Value : constant Unsigned_32 := 1;
   type Command_Words is array (Natural range 0 .. 95) of Unsigned_32;
   -- First word of each of the three flush/invalidate barriers.
   type Barrier_Start_List is array (Positive range 1 .. 3) of Natural;
   Barrier_Starts : constant Barrier_Start_List := [0, 36, 64];
   type Segment is record
      Valid : Boolean := False;
      Words : Command_Words := [others => 0];
   end record;
   -- ADL-N RCS: barrier, settings, barrier, private-VM probe, barrier.
   -- Caller must publish/retain the submission image and its private VM.
   -- Final stalled PIPE_CONTROL (the breadcrumb) writes the supplied nonzero
   -- sequence as a quadword (upper DWORD zero) to the PPHWSP timeline slot;
   -- it is the slot's only writer, so the slot is monotonic. The barriers
   -- write zeroes to PPHWSP scratch, never to the timeline. Caller must
   -- serialize work and validate the previous completion before publish;
   -- never reuse/wrap sequence values in a context lifetime.
   -- Caller must own the context's retained HWSP/ring and supply an owned,
   -- forcewake-held MCR read of WM_CHICKEN2. This is a ring segment, not a
   -- standalone batch or evidence of GPU execution. A marker is not a
   -- scheduling-disable acknowledgment or permission to free backing.
   -- AUX register polling requires external execution timeout/reset handling.
   function Build (Read_Valid : Boolean; WM_Chicken2 : Unsigned_32;
                   Sequence_Value : Unsigned_32 := Completion_Value) return Segment
     with Post => Build'Result.Valid =
       (Read_Valid and WM_Chicken2 /= Unsigned_32'Last and Sequence_Value /= 0) and then
       (if Build'Result.Valid then Build'Result.Words (90) = Sequence_Value
          and Build'Result.Words (88) = Timeline_Offset
          and (for all Barrier_Start of Barrier_Starts =>
                 Build'Result.Words (Barrier_Start + 2) = Intel_GPU_ADLN_PPHWSP.Scratch_Offset
                 and Build'Result.Words (Barrier_Start + 9) = Intel_GPU_ADLN_PPHWSP.Scratch_Offset)
          and Build'Result.Words (91) = 0
          and Build'Result.Words (92) = 16#04000001#
          and Build'Result.Words (93) = 0
          and Build'Result.Words (94) = 16#02800000#
          and Build'Result.Words (95) = 0) and then
       (if not Build'Result.Valid then
          (for all Word of Build'Result.Words => Word = 0));
   -- Separate ring dispatch after initial context setup has completed.
   -- Application initialization without any bootstrap/private batch branch.
   -- Uses only driver ring/context-relative storage, no application VA.
   function Build_Setup (Read_Valid : Boolean; WM_Chicken2 : Unsigned_32)
     return Segment
     with Post => Build_Setup'Result.Valid =
       (Read_Valid and WM_Chicken2 /= Unsigned_32'Last) and then
       (if Build_Setup'Result.Valid then
          Build_Setup'Result.Words (90) = Completion_Value and
          Build_Setup'Result.Words (92) = 0 and
          (for all I in 58 .. 63 => Build_Setup'Result.Words (I) = 0)
        else (for all Word of Build_Setup'Result.Words => Word = 0));
   -- Separate ring dispatch after initial context setup has completed.
   -- Programs L3 under a preceding stalled barrier, samples the register,
   -- and publishes a fresh completion after the trailing barriers.
   -- Does not branch to the private batch or submit drawing. Caller must
   -- admit ownership/topology and validate the readback before drawing.
   function Build_L3 (Sequence_Value : Unsigned_32) return Segment
     with Post => Build_L3'Result.Valid = (Sequence_Value /= 0) and then
       (if Build_L3'Result.Valid then
          Build_L3'Result.Words (90) = Sequence_Value and
          Build_L3'Result.Words (91) = 0 and
          Build_L3'Result.Words (92) = 0 and
          (for all I in 58 .. 63 => Build_L3'Result.Words (I) = 0)
        else (for all Word of Build_L3'Result.Words => Word = 0));
   -- Trusted driver-owned immutable batch, reached after initial context
   -- setup and any required resource/topology admission. The caller supplies
   -- a validated raw48/QWORD-aligned VA in this context's retained PPGTT.
   -- Validation here is encoding only, not extent/contents/ownership checking.
   -- Same barriers and completion rules as Build; not a public IPC endpoint.
   -- The breadcrumb writes the 64-bit Sequence_Value: low DWORD in word 90,
   -- high DWORD in word 91 (GPU-001 step 2: 64-bit timelines end to end).
   Breadcrumb_Low : constant Natural := 90;
   Breadcrumb_High : constant Natural := 91;
   function Build_Batch
     (Read_Valid : Boolean; WM_Chicken2 : Unsigned_32; Sequence_Value : Unsigned_64;
      Batch_GPU : Unsigned_64) return Segment
     with Post => Build_Batch'Result.Valid =
       (Read_Valid and WM_Chicken2 /= Unsigned_32'Last and Sequence_Value /= 0
        and Batch_GPU /= 0 and Batch_GPU < 2 ** 48 and Batch_GPU mod 8 = 0) and then
       (if Build_Batch'Result.Valid then
          Build_Batch'Result.Words (88) = Timeline_Offset and
          Build_Batch'Result.Words (Breadcrumb_Low) = Unsigned_32 (Sequence_Value mod 2 ** 32) and
          Build_Batch'Result.Words (Breadcrumb_High) = Unsigned_32 (Sequence_Value / 2 ** 32)
        else (for all Word of Build_Batch'Result.Words => Word = 0));
   -- Immutable drawing batch, reached only after live capacity admission.
   -- Same before/after barriers and sequencing as the marker dispatch.
   function Build_Draw (Read_Valid : Boolean; WM_Chicken2, Sequence_Value : Unsigned_32)
     return Segment
     with Post => Build_Draw'Result.Valid =
       (Read_Valid and WM_Chicken2 /= Unsigned_32'Last and Sequence_Value /= 0) and then
       (if Build_Draw'Result.Valid then Build_Draw'Result.Words (90) = Sequence_Value
        else (for all Word of Build_Draw'Result.Words => Word = 0));
end Intel_GPU_ADLN_Context_Init;
