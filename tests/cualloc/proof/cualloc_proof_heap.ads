--  The instance GNATprove analyzes: CuAlloc over a provider whose answers
--  are unknown, with the kernel's commit cap (cualloc_proof_odd_heap.ads:
--  an odd one).
pragma SPARK_Mode (On);
with CuAlloc;
with CuAlloc_Proof_Provider;
package CuAlloc_Proof_Heap is new CuAlloc
  (CuAlloc_Proof_Provider.Reserve, CuAlloc_Proof_Provider.Commit, CuAlloc_Proof_Provider.Release,
   CuAlloc_Proof_Provider.Kernel_Commit, CuAlloc_Proof_Provider.Attached);
