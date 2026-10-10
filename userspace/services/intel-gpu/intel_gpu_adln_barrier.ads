with Interfaces; use Interfaces;
with Intel_GPU_ADLN_PPHWSP;
package Intel_GPU_ADLN_Barrier with SPARK_Mode is
   -- ADL-N RCS sequence shared with context initialization. Requires a valid,
   -- retained context-relative PPHWSP and an initialized engine. Both
   -- barrier post-syncs dump into PPHWSP scratch (+0xD0), never the timeline.
   -- Caller serializes ring publication and completion sequence allocation.
   -- AUX polling requires an external timeout/reset policy. Encoding alone is
   -- NOT proof of engine quiescence or permission to update page tables.
   Scratch_Offset : constant Unsigned_32 := Intel_GPU_ADLN_PPHWSP.Scratch_Offset;
   Timeline_Offset : constant Unsigned_32 := Intel_GPU_ADLN_PPHWSP.Timeline_Offset;
   type Barrier_Words is array (Natural range 0 .. 21) of Unsigned_32;
   -- Words 2 and 9 are the two post-sync destinations.
   function Flush_And_Invalidate return Barrier_Words
     with Post => Flush_And_Invalidate'Result (2) = Scratch_Offset and
       Flush_And_Invalidate'Result (9) = Scratch_Offset;
   type Command_Words is array (Natural range 0 .. 29) of Unsigned_32;
   type Segment is record
      Valid : Boolean := False;
      Words : Command_Words := [others => 0];
   end record;
   -- Barrier followed by the stalled final breadcrumb (the only timeline
   -- write: the 64-bit Sequence as a quadword, low DWORD in word 26) and an
   -- arbitration check. Also the queue's timeline-only Signal segment.
   -- No context settings, probe batch, or scheduling-disable operation.
   -- Sequence must be nonzero and never reused in the context lifetime.
   Breadcrumb_Low : constant Natural := 26;
   Breadcrumb_High : constant Natural := 27;
   function Build (Sequence : Unsigned_64) return Segment
     with Post => Build'Result.Valid = (Sequence /= 0) and then
       (if Build'Result.Valid then
          Build'Result.Words (2) = Scratch_Offset and
          Build'Result.Words (9) = Scratch_Offset and
          Build'Result.Words (24) = Timeline_Offset and
          Build'Result.Words (Breadcrumb_Low) = Unsigned_32 (Sequence mod 2 ** 32) and
          Build'Result.Words (Breadcrumb_High) = Unsigned_32 (Sequence / 2 ** 32)
        else (for all Word of Build'Result.Words => Word = 0));
end Intel_GPU_ADLN_Barrier;
