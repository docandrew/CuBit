# Kernel hardware catalog policy tests

Run from the repository root:

```sh
nix develop -c bash -c 'cd kernel && alr exec -- gprbuild -p -P ../tests/aml-core/hardware-catalog/catalog.gpr && ../tests/aml-core/build/hardware-catalog/hardware_catalog_tests && alr exec -- gnatprove -P ../tests/aml-core/hardware-catalog/catalog.gpr -u hardware_catalog.adb hardware_authority.ads --mode=all --level=2 --prover=cvc5,z3 --checks-as-errors=on -j2'
```

The production kernel/src/hardware_catalog unit is compiled and exercised on the
host. Its state is not yet instantiated in boot initialization. This test does
not register hardware, install capabilities, invoke a syscall or perform I/O.

The catalog bounds inventory to 64 descriptors, rejects duplicate IDs across
classes, checks scalar transaction geometry, freezes membership while active,
and rejects stale inventory epochs. Can_Select uses the shared per-resource
attenuation predicate and requires the selected resource to belong to the
specified category. Group/token arguments must eventually come from an
already-authenticated kernel capability; passing them as user assertions would
not provide security. `Hardware_Grants` implements parent-child revocation
linkage, and `Hardware_Grants.Cspace` checks installed capability type, registry
identity, generation and rights before reserving an access. Those separate
policy units are exercised by the hardware-grants and hardware-cspace targets;
this catalog target alone does not validate them. Boot admission, current-caller
syscall dispatch, synchronization and real hardware access remain unwired.

Trusted platform admission must establish resource ownership and safe operation
semantics independently. Valid only checks metadata geometry and masks. Revoke
invalidates selection but does not free backing or complete in-flight accesses.

2026-10-01: 126 hosted checks and 151 SPARK analysis checks passed, with no unproved
or justified checks. The standard AML runner now includes the hosted target;
`--prove` includes its contracts and the shared hardware authority unit. Both
targets passed again after integration under the shared build lock.

Resolve now returns an internal descriptor only after current-epoch, installed
permission subset and operation checks. It does not demand delegation rights
merely to use installed authority. A caller-supplied Permission is not accepted
as an authentication mechanism: eventual syscall glue must obtain it from
kernel-owned cspace/authority records and retain backing through completion.

Private checked compilation against the actual kernel runtime passed in
/tmp/cubit-hardware-catalog-native-yr7b4gy6; all 85 recorded input hashes matched
the checkout afterward. The Resolve frame was 272 bytes, not a whole-call-chain
stack bound. This static library check does not establish live kernel integration.

Begin_Access now reserves one serialized transaction and returns a private,
nonwrapping completion ticket. Revocation preserves that reservation; inventory
replacement and new admissions fail until matching trusted completion. Tests
cover wrong/duplicate/delayed tickets and replacement attempts while busy.
Actual mappings/pins and synchronization must honor this metadata lifecycle.

Updated private checked kernel-runtime compile passed in
/tmp/cubit-hardware-reservations-native-en0qrhy4 (85 inputs unchanged). Checked
Begin_Access/Finish_Access frames were 9552/6384 bytes; these include contract
snapshots and do not establish a safe whole-call-chain kernel stack bound.
