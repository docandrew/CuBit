with Interfaces; use Interfaces;
package Intel_GPU_ADLN_Context_Init with SPARK_Mode is
   Completion_Offset : constant Unsigned_32 := 16#D0#;
   Completion_Value : constant Unsigned_32 := 1;
   type Command_Words is array (Natural range 0 .. 91) of Unsigned_32;
   type Segment is record
      Valid : Boolean := False;
      Words : Command_Words := [others => 0];
   end record;
   -- ADL-N RCS: barrier, settings, barrier, private-VM probe, barrier.
   -- Caller must publish/retain the submission image and its private VM.
   -- Final stalled PIPE_CONTROL writes1 to context-relative HWSP scratch D0
   -- after barriers write0 there. One-shot context only: caller must verify
   -- initial zero and never accept an old marker as a new completion.
   -- Caller must own the context's retained HWSP/ring and supply an owned,
   -- forcewake-held MCR read of WM_CHICKEN2. This is a ring segment, not a
   -- standalone batch or evidence of GPU execution. A marker is not a
   -- scheduling-disable acknowledgment or permission to free backing.
   -- AUX register polling requires external execution timeout/reset handling.
   function Build (Read_Valid : Boolean; WM_Chicken2 : Unsigned_32) return Segment
     with Post => Build'Result.Valid =
       (Read_Valid and WM_Chicken2 /= Unsigned_32'Last) and then
       (if not Build'Result.Valid then
          (for all Word of Build'Result.Words => Word = 0));
end Intel_GPU_ADLN_Context_Init;
