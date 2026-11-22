# CCL CBOR evaluation

Status: hosted experiment completed; the native development control app now
uses a restricted, float-free CBOR profile. See
`userspace/ccl/remote/README.md` for the selected profile and the Observatory
README for real browser-to-CuBit tests. The findings below describe the
original dependency evaluation, not an internet-readiness claim.

The Nix flake pins an immutable `cbor_ada` 0.3.0 source revision. The experiment
exercises typed CCL request/error/value/reference fixtures, bounded adversarial
decoding, SPARK analysis, stack/copy behavior, and a compile-only CuBit userspace
runtime probe. It does not implement a remote session or activate any service.

See [the experiment and reproducible commands](../tests/ccl-cbor/README.md).

Key findings:

- Upstream's normal 697 tests pass. The local contract-enabled corpus and
  sample schema validator pass; the validator separately proves cleanly.
- Three upstream float-payload proof assertions remain unproved with our
  toolchain, including at the documented timeout. Do not repeat upstream's
  “100% proved” claim as a locally reproduced result.
- Upstream all-contracts testing exposes anonymous-array lower-bound mistakes
  in its fixtures. The immutable upstream source has not been patched.
- Generic decoding does not reject duplicate map keys or enforce our types;
  a restricted profile and schema validation are still required.
- The generic tree result and decoder consume appreciable stack space, and
  return-array encoders use the GNAT secondary stack. Source-level no-heap
  claims are not a complete runtime allocation or latency bound.

Next: assess a schema-generated bounded codec API and resolve the dependency
verification discrepancies. The current native profile has no remote object
references or authority transfer. Decoding a future reference must
never itself acquire authority, and message tracing must be explicitly scoped
and redact secrets. Nothing here changes the kernel IPC ABI.
