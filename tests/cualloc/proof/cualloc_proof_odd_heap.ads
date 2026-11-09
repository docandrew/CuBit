--  As cualloc_proof_heap.ads, with a commit cap that is not a page multiple.
pragma SPARK_Mode (On);
with CuAlloc;
with CuAlloc_Proof_Provider;
package CuAlloc_Proof_Odd_Heap is new CuAlloc
  (CuAlloc_Proof_Provider.Reserve, CuAlloc_Proof_Provider.Commit, CuAlloc_Proof_Provider.Release,
   CuAlloc_Proof_Provider.Odd_Commit, CuAlloc_Proof_Provider.Attached);
