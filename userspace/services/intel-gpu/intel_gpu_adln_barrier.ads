with Interfaces; use Interfaces;
package Intel_GPU_ADLN_Barrier with SPARK_Mode is
   -- ADL-N RCS sequence shared with context initialization. Requires a valid,
   -- retained context-relative HWSP at scratchD0 and an initialized engine.
   -- Caller serializes ring publication and completion sequence allocation.
   -- AUX polling requires an external timeout/reset policy. Encoding alone is
   -- NOT proof of engine quiescence or permission to update page tables.
   Completion_Offset : constant Unsigned_32 := 16#D0#;
   type Barrier_Words is array (Natural range 0 .. 21) of Unsigned_32;
   function Flush_And_Invalidate return Barrier_Words;
   type Command_Words is array (Natural range 0 .. 29) of Unsigned_32;
   type Segment is record
      Valid : Boolean := False;
      Words : Command_Words := [others => 0];
   end record;
   -- Barrier followed by stalled post-sync marker and arbitration check.
   -- No context settings, probe batch, or scheduling-disable operation.
   -- Sequence must be nonzero and never reused in the context lifetime.
   function Build (Sequence : Unsigned_32) return Segment
     with Post => Build'Result.Valid = (Sequence /= 0) and then
       (if Build'Result.Valid then Build'Result.Words (26) = Sequence
        else (for all Word of Build'Result.Words => Word = 0));
end Intel_GPU_ADLN_Barrier;
