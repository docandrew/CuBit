# Experimental free-set tree — NOT used by the kernel

This prototype explores replacing intrusive free lists with a packed membership
tree. It has focused proofs and host tests, but **is not the current CuBit
allocator**, and its proof does not establish whole-allocator correctness.

The measured improvement from approximately 45 ns to 19 ns per operation was
between the root-search and hinted-search versions of this prototype. The
idealized hot indexed-list baseline was approximately 1 ns. These are hosted
microbenchmarks, not allocator-wide throughput or latency measurements.

Following review, kernel work continues with the existing constant-time linked
lists and [inlined splice primitives](../intrusive-list-splices/README.md).
The prototype is retained for reference, not enabled or selected at runtime.
