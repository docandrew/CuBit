with Interfaces; use Interfaces;
package Intel_GPU_ADLN_Batch_Start with SPARK_Mode is
   type Command_Words is array (Natural range 0 .. 5) of Unsigned_32;
   -- Ring-only branch to the fixed driver-owned submission image batch.
   -- Uses the context's private PPGTT, never GGTT or a caller-supplied VA.
   -- Caller must retain and publish that image and its page tables before
   -- scheduling. This encoding grants no authority and proves no execution.
   -- Batch END returns to the ring; subsequent completion must be ordered
   -- after the batch's stores. No completion/fence is emitted here.
   function Build return Command_Words;
end Intel_GPU_ADLN_Batch_Start;
