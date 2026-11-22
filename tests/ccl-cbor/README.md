# Hosted CCL / CBOR evaluation

This is an isolated experiment, **not an adopted remote-control ABI**. It opens
no sockets, performs no CCL evaluation, resolves no endpoint references, and
does not modify any native application's dependencies or ELF manifest.

## Pinned input and toolchain

The flake pins `b-erdem/cbor_ada` revision
`ce9897cdd80dea21112c59b80a5c42f2921f59f0` (upstream version 0.3.0), including its
Nix content hash. `CBOR_ADA_SRC` points at the immutable source. No upstream
source or proof contract is patched. Upstream license: Apache-2.0; its LICENSE
and source notices remain in the pinned source and must accompany any future
redistribution as required. This is not yet a runtime Alire dependency.

Recorded toolchain: GNAT Native 16.1.0, GNATprove FSF 15.0, Why3 1.7.1+git,
CVC5 1.3.4, Z3 4.16.0. Compiler and proof-driver versions are recorded
separately; upstream's published proof counts need not match these.

All builds and proofs run through `alr exec` from `kernel/`, inside Nix:

```sh
nix develop -c make -C kernel test-ccl-cbor
nix develop -c make -C kernel prove-ccl-cbor-core
nix develop -c make -C kernel measure-ccl-cbor
nix develop -c make -C kernel check-ccl-cbor-native
```

The last command is a **compile-only CuBit userspace runtime probe**, not a
linked app, VM boot, kernel build, or end-to-end networking test.

The broader upstream property proof is available separately and currently
fails on three float-payload assertions (see below):

```sh
nix develop -c make -C kernel prove-ccl-cbor
```

## Small experimental profile

Messages are definite arrays `[version, request-id, kind, payload...]`, with
version 1, nonzero unsigned-64 request IDs, a typed message-kind enum, and an
overall 512-byte limit. This is one fixed sample schema, not schema negotiation.

| Kind | Payload | Encoded fixture size |
| --- | --- | --- |
| Evaluate request | UTF-8 source, at most 256 bytes | 14 bytes for `(+ 20 22)` |
| Evaluation error | Error enum, one-based uint32 line/column, UTF-8 diagnostic up to 128 bytes | 24 bytes |
| Endpoint reference | Exactly 16 opaque bytes | 21 bytes |
| Unsigned result | Full uint64 value | 13 bytes for `2**64 - 1` |

Endpoint bytes are **unresolved data**, not proof of authority. A later broker
must validate session ownership, generation/liveness and permitted operations.
There is no endpoint resolver in this experiment.

The sample excludes maps, semantic tags, floats, indefinite-length containers,
unknown kinds/versions and trailing bytes. It calls the strict full-tree decoder
with UTF-8 checking, depth 1 and explicit string limits, then validates the
message shape and field ranges. The library's single-item `Decode` validates
only the header of a container; it is not a complete-message validator.

Fixtures print readable descriptions and actual hex bytes. They contain only
public test values, including a deliberately fake endpoint reference. This is
not a live tracing API; future tracing must require inspection authority and
redact sensitive fields rather than routinely dump payload bytes.

## Test results and proof boundaries

The pinned upstream test suite passes **697 tests** with its original split
assertion policy: test code enables assertions, the library does not. Our local
evaluation enables assertions/contracts throughout the library and test code;
it passes **67,102 checks**, including:

- an independent golden request encoding and sample payload round trips;
- every truncated prefix of each sample, plus trailing data;
- uint64 preservation, valid UTF-8, invalid/overlong UTF-8 and surrogates;
- source/diagnostic/reference bounds, malformed headers, wrong types and enums;
- depth and item exhaustion, huge declared maps and oversized frames;
- positive non-one input bounds and rejection of negative input bounds;
- all 256 one-byte inputs and 65,536 two-byte inputs;
- 10,000 deterministic random 64-byte frames and 3,584 single-byte mutations.

This corpus is not coverage-guided fuzzing or proof of full RFC conformance.

`Sample_Profile.Valid` independently discharged 18 analysis items with no
unproved or justified checks. Its proof covers runtime safety and termination
under upstream API contracts, **not** a complete semantic equivalence theorem
or any authority/session state machine. The core target additionally analyzes
the upstream encoder, decoder and ghost wire model.

The combined core/schema run discharged **865/865** analysis items, with no
unproved or justified checks. The separate full property run below remains a
failing gate; a clean core run does not hide or resolve its three open checks.

### Upstream issues preserved, not suppressed

1. **All-contracts upstream fixture failure.** Enabling library assertions in
   the upstream tests raises a precondition failure at `cbor-decoding.ads:139`.
   `Test_Decode_Raw` passes anonymous positional `Storage_Array` aggregates,
   whose default lower bound is negative; `Decode` requires a nonnegative
   lower bound. Our tests use explicit bounds and leave this upstream issue
   visible. This is a test-fixture/API-precondition mismatch, not evidence that
   validly bounded hostile input crashes the decoder.

   Reproduce independently:

   ```sh
   nix develop -c bash -c 'cd kernel && alr exec -- gprbuild -p -P../tests/ccl-cbor/upstream_tests.gpr -XCBOR_UPSTREAM_CONTRACTS=on && ../tests/ccl-cbor/build/upstream-on/test_cbor'
   ```

2. **Three float property assertions not reproduced.** At level 2, including
   upstream's documented 30-second per-check timeout, the full run reports
   1,080 analysis items: 1,077 discharged, three unproved, none justified.
   The remaining checks are `cbor-properties.adb:148`, `:176`, and `:207`,
   quantifying that encoded float payload bytes equal the input bytes. All
   runtime checks and functional contracts in that run discharged. We have
   not established why the local result differs from upstream's published
   1,082/1,082 result; do not claim to have reproduced that result. No
   assumptions, exclusions inside upstream code, or weakened contracts were
   added. Floats are outside the experimental control-message profile.

3. **Generic decode is not canonical/schema validation.** A duplicate-key map
   is accepted by the generic decoder; the sample rejects maps altogether.
   Shortest integer/length encodings are enforced by upstream (stricter than
   unrestricted CBOR). Full deterministic map/float rules are not established
   by that check. Any future map-based protocol needs explicit rules.

4. **Text encoding is not Unicode conversion or validation.** The String
   encoder copies Character bytes; Latin-1 above ASCII can produce invalid
   UTF-8. The UTF8 encoder also copies supplied bytes without validating them.
   Use a validated UTF-8 boundary in generated encoders. Our tests demonstrate
   that the receiving profile rejects invalid UTF-8.

## Stack, copying and timing observations

On the current x86-64 toolchain, without executable ghost contracts:

- `Decode_All_Result` is 6,168 bytes, including space for 128 decoded items.
- GCC reports a 13,408-byte static frame for hosted `Decode_All`, plus a
  6,208-byte frame for the sample validator. These stack frames can coexist;
  the result size is not the whole call-chain budget.
- The CuBit-runtime compile probe reports 13,424 bytes for `Decode_All` and
  112 bytes for `Decode_At`. Exact figures are compiler/target dependent.
- Native objects reference `system__secondary_stack__ss_allocate`, `memcpy`,
  `memset`, and the last-chance handler. No explicit malloc is imported, but
  runtime secondary-stack growth can allocate: do not equate source-level
  “no heap allocation” with no runtime allocation or bounded secondary-stack
  consumption. The probe does not prove the complete link/runtime behavior.
- String encoders return newly constructed arrays; `Get_String` copies a
  referenced payload. Concatenating encoded fields can introduce additional
  temporaries. Reading validated buffer references can avoid extraction copies,
  provided the buffer's lifetime is retained.
- One hosted run measured about 0.31 microseconds per validation of the 14-byte
  request, averaged over 20,000 iterations. This is not a p99 latency result,
  a contention-controlled benchmark, or a native networking measurement.

Compiler `.su` reports live under `build/off` and `build/native-probe`; proof
reports under `build/on/gnatprove` and `build/upstream-off/gnatprove`.

## Recommendation / next experiment

CBOR remains a promising control-plane encoding, but do not integrate this
test schema as a permanent ABI. First settle schema identity/version negotiation
and session-bound reference semantics. Compare generated schema-specific
traversal and bounded destination-buffer encoding against the generic tree and
array-concatenation APIs. Reproduce/resolve the upstream proof and fixture
discrepancies before representing the dependency as fully independently proved.
Keep bulk data on streams/grants rather than turning all local IPC into CBOR.
