with Interfaces; use Interfaces;
package Intel_GPU_ADLN_Batch_Start with SPARK_Mode is
   type Command_Words is array (Natural range 0 .. 5) of Unsigned_32;
   type Encoded_Batch is record
      Valid : Boolean := False;
      Words : Command_Words := [others => 0];
   end record;
   -- Trusted command encoder, not an IPC submission interface. Initial
   -- dynamic batches use QWORD-aligned starts. Caller validates ownership,
   -- immutable command contents, entire batch extent and context isolation.
   -- Raw48 GPU addresses only: CPU/DMA addresses and canonical sign extension
   -- are not accepted as allocator input. No mapping or authority is created.
   -- TGL PRM Vol 2a (12.21), MI_BATCH_BUFFER_START pp. 971-972:
   -- DW0 bit 8 selects non-privileged PPGTT execution. Hardware confines
   -- chained/nested batches to PPGTT too. Requires PPGTT enabled in context;
   -- this encoding alone is NOT a proof of application command isolation.
   function Build_At (GPU : Unsigned_64) return Encoded_Batch
     with Post => Build_At'Result.Valid =
       (GPU /= 0 and GPU < 2 ** 48 and GPU mod 8 = 0) and then
       (if Build_At'Result.Valid then
          Build_At'Result.Words (1) = 16#18800101#) and then
       (if not Build_At'Result.Valid then
          (for all Word of Build_At'Result.Words => Word = 0));
   -- Ring-only branch to the fixed driver-owned submission image batch.
   -- Uses the context's private PPGTT, never GGTT or a caller-supplied VA.
   -- Caller must retain and publish that image and its page tables before
   -- scheduling. This encoding grants no authority and proves no execution.
   -- Batch END returns to the ring; subsequent completion must be ordered
   -- after the batch's stores. No completion/fence is emitted here.
   type Batch_Kind is (Marker, Drawing);
   function Build (Kind : Batch_Kind := Marker) return Command_Words;
end Intel_GPU_ADLN_Batch_Start;
