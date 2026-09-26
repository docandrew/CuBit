# Config inspection checks

Run the hosted namespace checks and focused runtime-error proof:

```sh
nix develop -c bash tests/config-inspection/run.sh --prove
```

The SPARK units are the pure component-boundary matcher, owned
`Config_Authority` table, `Config_Protocol` wire-length decoder, and owned
`Config_Store`, not the IPC service, ACL parser, or kernel grants. Matcher cases include non-1 string
bounds and one-character strings at `Positive'Last`.

The authorization tests cover default deny, separate subjects, scoped read
versus write, explicit wildcard, empty-set denial, grant replacement,
revocation, invalid subjects, capacity exhaustion without mutation, updates
while full, and slot reuse without stale rights. The proof checks failure
preservation, successful profile installation, revocation, and isolation of
other subjects using a Ghost predicate. Production builds do not enable
assertions; hosted tests do.

Native integration:

```sh
nix develop -c make -C kernel config procmgr devmgr config-check
nix develop -c tests/headless/run.sh --test config-inspection --accel kvm --cpus 4 --timeout 35
```

Before the two normal clients, a test-only ELF with invalid Config rights must
be rejected by the scope installer. The launcher must report failed admission
and never resume that child. The runner creates an exclusive temporary copy;
staged applications are untouched. FS/Config installation failure now stops
launch, cleaning partial policy before killing the suspended child while its
PID is still occupied. TLS/network approval behavior is unchanged.

Two normal processes have different manifest scopes. The first has global read plus
write only to its test namespace. The second has read only to a different
namespace and cannot discover the global inspector, enumerate all keys, or read
outside its scope. The test also runs a CCL expression through the real Config
service, checks explicit oversized responses, and sends malformed requests,
including an unauthorized ACL install with a maximal 64-bit count (which must
be denied before a narrowing conversion) and unauthorized revocation.

The data-protocol fixture covers the full 128-byte key plus 4096-byte value,
empty values versus missing keys, delete, listing overflow without partial
publication, malformed lengths/slots, stale grants, insufficient mappings,
read-only input versus output grants, and completed grant retirement. Retired
load/save opcodes are rejected. The runner installs an obsolete `config.dat`
overlay and checks that the CCL timezone seed is unchanged and no hidden
`config.store` key exists. Persistence is not implemented; updates are volatile.

Global read is not granted to the default plaintext remote-control app. The
Linux Workbench preview does not expose a fake Config connection.

## Native tree inspector

```sh
nix develop -c bash tests/config-inspection/run.sh
nix develop -c make -C kernel config-inspector desktop test-ui-fonts
nix develop -c tests/headless/run.sh --test config-tree --accel kvm --cpus 4 --timeout 55 --keep-logs
```

The hosted tree tests cover grouping nonadjacent namespace keys, nodes that
have both values and children, collapse/refresh, invalid names, and capacity
failure leaving the previous snapshot intact. Shared widget pixel tests check
every tree icon and clipping. The native test uses real Apps-menu launch and
Config IPC, then compares screenshots across keyboard and mouse interactions. These are
regression tests, not SPARK proofs of the inspector.

Validation on 2026-09-22: all 12 runtime checks in `Contains` proved without
assumptions; hosted boundary cases passed; native authorized/denied fixtures and
the remote HTTP/CBOR suite passed (including denial of Config discovery for the
default remote host). These results predate the data-wire migration and are not
a proof of the Config IPC handlers. Follow-on hardening is tracked as SEC-019.

The subsequent authority extraction proves 24 runtime checks, six assertions,
and three functional contracts (33 proof checks), plus six flow/termination
checks, with no assumptions or unproved checks. This is not a proof of the
whole Config service or the procmgr reset path.

The data-wire decoder additionally proves all five runtime checks and both
initialization checks, without assumptions or unproved obligations. The migrated
native `config-inspection` test passed under QEMU/KVM with four virtual CPUs,
including the full-size payload, overflow, grant-lifetime, permission, CCL, and
obsolete-overlay cases above. These native results are regression evidence,
not additional SPARK-proved properties.

The native `config-tree` regression also passed after the wire migration:
Apps-menu launch, keyboard/mouse tree expansion and selection, live values,
scrolling, and refresh preservation. Its captured guest screenshot was inspected.

## Owned store extraction (2026-09-23)

The native service now uses `Config_Store` for read/put/remove/enumeration; callers
cannot mutate its private entries or retain table pointers. Hosted tests cover
full keys/values, invalid lengths, full capacity, replacement at capacity,
deletion/slot reuse, empty values, stale-byte clearing, non-1 strings and a key
whose index is `Positive'Last`.

GNATprove reports 35 proved runtime checks, two proved functional contracts, and
eight initialization/termination checks, with no unproved checks or assumptions.
Successful Put contains the requested pair; rejected Put and a missing Remove
preserve state. The successful-write predicate is Ghost; native object inspection
confirms it is not emitted. This is not a proof of global key uniqueness, all
functional store behavior, IPC authority, or durability. Native `config-inspection`
also passes with the extracted implementation.
