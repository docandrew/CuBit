# Signed executable admission: integration starting point

Status: design and local API inspection, not implemented enforcement.

A valid signature is provenance evidence, not executable authority. Launch
policy still chooses a subset of the authenticated manifest's requests. No
signer identity or certificate subject becomes a superuser identity.

## Local primitives inspected

The sibling SPARKTLSCrypto project exposes direct signature primitives, so
binary verification need not instantiate a TLS connection or certificate
parser. Its current Ed25519 API has `Open` for a signature-prefixed message and
public key, returning validity and message bytes. P256.ECDSA exposes `Verify`
over a 32-byte hash and public-key/signature components. Ed25519 currently
depends on the Fiat_25519 and SPARKNaCl/SHA512 packages. These APIs were read,
not linked into CuBit or independently audited in this task.

Before integration, pin a reviewed source revision and inspect the transitive
runtime/assembly dependencies and compiler switches. Do not inherit a host
`native` CPU target or unrelated AES/AVX options into a baseline verifier.
Measure stack use on worst-case untrusted inputs and test under CuBit's runtime.
Package declarations saying SPARK_Mode On are not evidence that all required
properties have been proved at the selected revision.

## First vertical slice

1. Define one bounded, versioned signing envelope, with no algorithm negotiation
   initially. Choose the algorithm explicitly after reviewing the primitive.
2. Cover the exact complete ELF bytes, including `.cubit.*` manifests, identity,
   interface descriptors and loader-relevant headers. A detached envelope avoids
   ambiguous exclusions or a signature field signing itself.
3. If signing a compact record containing the ELF digest, specify the domain
   separator, version, algorithm, byte length and canonical encoding. This is a
   CuBit envelope protocol, not an implicit switch to Ed25519ph. Bind any package
   version/rollback metadata used by policy into authenticated content too.
4. Verify against explicitly installed trusted public keys. Claimed key IDs,
   namespaces and package names merely select/check candidates; none supplies
   its own trust root. A raw-key initial policy can work offline without X.509.
5. Bind approval to the **same immutable bytes actually loaded**, not a path or
   a previous verification of a writable buffer. Decide the sealing/copy boundary
   before wiring a verify-then-SPAWN sequence. Kernel enforcement and trusted
   loader authority must prevent an ordinary app from bypassing signed admission.
6. Record digest, signer, policy decision and requested/granted authority for
   System Inspector. Development-mode unsigned admission must be an explicit,
   visible policy exception, never fallback after a failed verification.

## Tests and decisions still needed

- Trusted key, wrong key, bit flips in code and in each metadata section,
  truncation, unsupported versions, malformed signatures and excessive lengths.
- Attempts to substitute the executable between verification and loading.
- Whether unsigned development builds are allowed, which policy principal may
  approve them, and whether exceptions are per-artifact or per-installation.
- Key rotation/revocation and rollback protection. A valid old signature does
  not prove freshness; rollback counters themselves need durable protection.
- Coverage of the bootstrap path: GRUB/kernel/devmgr and early drivers load
  before normal process-manager policy. Application signature enforcement alone
  is not verified boot, and the two claims must remain distinct.
- Certificate expiry, if certificates are later adopted: RTC-derived wall time
  is not authenticated time. Avoid quietly relying on network time for offline
  launch or on unauthenticated time to establish signer trust.

This extends the existing [security model](security-model.md) and
[package design](ccl-packages.md); it does not introduce a second authority model.
