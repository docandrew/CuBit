with Interfaces; use Interfaces;
package Intel_GPU_ADLN_Context_Init with SPARK_Mode is
   Completion_Offset : constant Unsigned_32 := 16#D0#;
   Completion_Value : constant Unsigned_32 := 1;
   type Command_Words is array (Natural range 0 .. 95) of Unsigned_32;
   type Segment is record
      Valid : Boolean := False;
      Words : Command_Words := [others => 0];
   end record;
   -- ADL-N RCS: barrier, settings, barrier, private-VM probe, barrier.
   -- Caller must publish/retain the submission image and its private VM.
   -- Final stalled PIPE_CONTROL writes the supplied nonzero sequence to
   -- context-relative HWSP scratch D0 after barriers write0 there. Caller
   -- must serialize work and validate the previous completion before publish;
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
          and Build'Result.Words (91) = 0
          and Build'Result.Words (92) = 16#04000001#
          and Build'Result.Words (93) = 0
          and Build'Result.Words (94) = 16#02800000#
          and Build'Result.Words (95) = 0) and then
       (if not Build'Result.Valid then
          (for all Word of Build'Result.Words => Word = 0));
end Intel_GPU_ADLN_Context_Init;
