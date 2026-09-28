# Native CCL schema metadata

```sh
nix develop -c bash tests/ccl-objects/run-schemas.sh --prove
```

`CCL.Objects.Schemas` carries approved type metadata between the Config
dispatcher and its storage worker. It also provides the bounded metadata form
needed by eventual `Config.create(type)`. It is not that creation operation,
not a policy decision, and not a serialization step imposed on ordinary
get/set clients.

The seven-page native image contains a schema key, root, and at most 32 named
product/sum definitions with 16 components each. Primitive IDs and record
offsets are explicit. Private Ada Registry memory is never copied across IPC;
all incoming scalars admit every bit pattern before validation. Shared images
must be copied into owned memory before import. This is a native ABI, not a
portable on-disk or network encoding.

Import rebuilds through the existing type constructors: no cycles or forward
references, invalid/duplicate names, oversized layouts, or handler/resource-containing
persisted values. Unused storage must be zero. All alternatives must be safe,
not just the currently selected variant. Definitions unrelated to the root
may be accepted by the native importer; they do not create live handlers or
authority. Export now emits only the root dependency closure with translated
IDs, never an unrelated resource declaration from the process's visible catalog.
Native/persisted metadata supports data products and sums, not the Resource
shape used by CCL's discoverable type descriptions.

Authentication comes FIRST in the eventual provisioning IPC adapter. A valid
image and a schema key do not establish provenance; the key is not computed or
cryptographically verified here. Only the held Config authority may provision
the worker, and conflicting definitions for an existing key must be rejected.
Clients and Config retain their own approved bindings for value validation.

4,223 hosted checks cover builtins, nested product/sum values, canonical
round-trips, maximum registry/components, every header-padding byte and hostile
counts/references/names. The dependency-closure change is covered by the current
six-unit 281-check SPARK run documented in `tests/ccl-types/README.md`, plus
62 resource metadata tests and the portable codec/interoperability suites.
Semantic round-trip equivalence and
provenance are not proved. No assumptions or disabled-proof sections were added.
The real Turso fixture now automatically provisions the worker through the
private schema request/ack protocol before loading data. The actual receiver
enforces source/tag policy and refuses unprovisioned or conflicting bindings.
Syscalls and their authenticated identities are still modeled, not live IPC.
