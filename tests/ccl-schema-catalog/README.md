# Approved object schemas

```sh
nix develop -c bash tests/ccl-schema-catalog/run.sh --prove
```

`CCL.Objects.Catalog` is an append-only, host-owned catalog for one authorized
discovery view. It shares a single `CCL.Types.Registry` across up to 32 approved
schema keys. Each entry contains only a key and translated root reference;
there is no native object image or repeated registry per entry. On this x86-64
host the catalog occupies 23,304 bytes, compared with 22,057 bytes for one full
binding (Ada `Size`, not a cross-platform ABI promise).

Publication accepts an existing approved `CCL.Objects.Binding`. Only its root's
dependency closure is imported. Repeated publication of the same identity and
definition is idempotent, including when full. A key cannot be rebound to a
different nominal definition; a different key cannot replace an existing name's
definition. Different approved keys may share one identical definition. Table
or type capacity failure publishes nothing. Missing keys resolve to an unbound
contract, not a guessed/default schema. No reset/removal operation is provided.

The caller must validate discovery provenance and authority **before** supplying
bindings. This catalog does not compute or authenticate schema keys, grant
Config access, allocate handles, or make constructors executable. Build/filter
the catalog per authorized view: `Visible_Types` exposes its complete registry
and must not be used to reveal a global catalog to an unprivileged application.
Ordinary object validation still requires the resolved binding; the key in a
received object's header is not independently trusted.

5,467 hosted checks cover every table occupancy, repeated publication at
capacity, conflicting identities and names, shifted local numbering, shared
definitions, unreachable handler-containing metadata, rejection of unbound or
nonpersistable roots, type-table exhaustion, unchanged state after rejection,
and actual object validation under resolved contracts. The focused SPARK run
discharges 16 checks: runtime safety, initialization/termination/dependencies,
successful count growth, unchanged state on every non-new publication, and
the returned binding's key matching the lookup key. Complete nominal graph
preservation is regression-tested with the shared import/correspondence suites,
not established by this package's small contract set.

The native Config discovered-type fixture now publishes and resolves its
approved Reading binding here before exposing the description to compilation.
Its Config IPC client still owns a full bound contract; the compact schema
catalog prepares host-import metadata sharing, not a new client serialization
format. General aggregate CCL host imports and async source execution remain
separate integration work.

Native validation (2026-09-25): the KVM writer and independent KVM/TCG readers
pass with this catalog in the actual CuBit fixture. Independent SQLite/WAL and
read-only ext2 checks confirm three declarations and six exact revisions, with
no new writes during read-only recovery. Logs:
`/tmp/cubit-schema-catalog-create.log`, `/tmp/cubit-schema-catalog-reopen.log`,
and `/tmp/cubit-schema-catalog-{create,reopen,reopen-tcg}.serial`.
The seed is `/tmp/cubit-schema-catalog-seed/disk.img`. The normal ISO was restored.
These are clean restart tests, not arbitrary power-loss/crash-consistency proofs.
