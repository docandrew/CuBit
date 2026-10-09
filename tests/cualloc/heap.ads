with CuAlloc;
with Linux_Provider;
package Heap is new CuAlloc
  (Linux_Provider.Reserve, Linux_Provider.Commit, Linux_Provider.Release, Linux_Provider.Maximum_Commit);
