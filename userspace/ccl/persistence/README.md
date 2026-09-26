# CCL object persistence boundary

`CCL.Objects.Persistence` encodes shared native CCL objects for disk/network
transport. It is separate from `src/` so embedding the ordinary CCL core does
not pull in CBOR. Consumers opt into this directory and the Nix-pinned
`CBOR_ADA_SRC/src` library. It is pure SPARK code, but not an Ada `Pure` package.

This package never runs stored CCL, reconstructs authority, invokes constructors,
or chooses a schema based on data supplied by the sender. The caller supplies
the approved `CCL.Objects.Binding`. The result is owned data, not a live resource.

## Version 1 envelope

```text
[1, schema : bytes32, cells : [[u64, u64], ...], text : bytes]
```

Every container has a definite length; integers/lengths use shortest CBOR
encoding. No tags, maps, floats, negative CBOR integers, or trailing data are
allowed. Signed CCL integers use the native object model's two's-complement
unsigned cell bits, not the CBOR negative-integer major type. Interpretation
comes from the trusted CCL schema, not self-described CBOR kinds.

The four schema words are encoded most-significant byte first in array order.
Native machine endianness, struct padding, unused cells, and local CCL registry
indices do not appear on disk. Identity keys still need an authenticated
registry/digest-binding mechanism; the codec does not supply that policy.

The text arena is a byte string intentionally: current CCL `Character`/`String`
values can contain NUL and bytes above 127 without a UTF-8 invariant. This is
not a new public CCL byte-value type or an implicit string conversion. A future
Unicode language model should define its semantics explicitly and version the
representation if needed.

The envelope preserves the native model's bounded cells and text: 1–256 cells,
0–8192 text bytes, at most 13120 encoded bytes. Schema-ordered preorder cells
represent records/tuples, variants/enums and primitive values. `Validate` checks
the complete reconstructed object, including nested shapes, before exposing it.
Failure returns `Empty(Contract)`, never a partially decoded object.

Decoding walks the fixed envelope incrementally using the upstream checked
single-head decoder, then validates native semantics. It does not use
`Decode_All`'s 128-item bound: a valid 256-cell object can exceed that bound.
Indefinite heads are rejected explicitly; a single-head decoder does not prove
that an indefinite container is complete.

Encoding validates the native image and emits only occupied cells and text.
Byte-string heads and bodies are appended separately, avoiding another full
text-sized temporary. The API still owns bounded buffers and copies: this is
not zero-copy IPC. Local IPC uses the native representation, not this codec.

## Evidence and integration status

See [the hosted tests](../../../tests/ccl-objects/README.md). The round trip law
is regression-tested, not formally proved as a quantified encoder/decoder inverse.
Focused SPARK analysis checks runtime safety and stated contracts; dependency
contracts are part of its assumptions.

The experimental Turso backend understands the bounded envelope and stores it
transactionally with revision and schema metadata. It deliberately does **not**
duplicate the CCL type checker in Rust. A Rust `EncodedObject` has passed only
structural/canonical validation: Config must apply the expected CCL binding
before publication. Schema tags, valid CBOR and SQL commits confer no authority.

This is not wired into the live Config service, its IPC handlers, the storage
worker, or the normal image. Durable service integration also needs authorized
backend attachment and acknowledgement/publication/lifetime handling.
