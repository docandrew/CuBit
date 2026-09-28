# Opaque resource values in the CCL VM

```sh
nix develop -c bash tests/ccl-resource-values/run.sh
nix develop -c bash tests/ccl-resource-values/run.sh --prove
```

Linux-hosted tests exercise the shared VM and host registry, not kernel IPC:

- Factory result initializes a must-handle local, is borrowed by a read call,
  then moved to a closing call. Every call has its own import lifecycle.
- Foreign, null, wrong-type, retired and stopped references are rejected.
- Full nominal correspondence accepts equivalent declarations with different
  local type numbers, and rejects same-name/same-number parameter mismatches.
- Ordinary scalar completion cannot inject a resource. Data conversion rejects
  resources, including resource metadata hidden in an otherwise scalar value.
- Verification rejects unrestricted resource types, copying, mismatched owned
  arguments and incomplete must-handle lifetimes.
- `receiver_tests` passes a native product from an owned receiver's read result
  to a separate write argument. It checks exact snapshots, receiver identity,
  borrow acknowledgement, premature completion, receiver clearing on the next
  local-only call, and rejection of copied/mismatched/missing receiver/data.
  Filling all 16 object snapshots rejects the next offered borrow before any
  host effect, retains a well-formed terminal machine and preserves the
  `Object_Storage_Exhausted` diagnosis on repeated debugger steps.

`tests/config-object-client/resource_vm_tests.adb` combines this with the actual
Config client under modeled IPC/grants. Its private service handle never enters
the VM. `native-app/resource_fixture.adb` exercises the same acquire/read/close
flow using real Config IPC inside CuBit, reading the existing Integer collection
without adding durable revisions. Neither fixture is a source-level
`Config.create(type)` compiler test: they build a trusted in-memory program.

`native-app/receiver_fixture.adb` additionally acquires the existing nested
Preferences collection, reads its first value, writes its replacement through
an owned receiver plus independent native-object argument, reads it back and
closes it. It replaces the native fixture's second nested write, so the existing
independent database/reboot oracle still requires exactly two durable revisions.
The fixture explicitly pairs one stable client with its exact live reference;
general fixed-client dispatch rejects owned calls rather than ignoring them.

Remaining boundary work: approved portable resource signatures, static type
arguments, source ownership lowering, resource-bearing outcomes, and the host
pool associating each live resource reference with its stable Config client.
Receiver/data calls now have separate typed arguments/results; the existing
native-object snapshot path remains distinct
from non-persistable resources. Host completion still requires authenticated
receipt/run correlation, and stop/drain/close/grant retirement remain host duties.

Validation (2026-09-25): 58 focused checks and 29 Config-client integration
checks pass. The native `config-objects-resource-vm` marker and the full writer
test pass under KVM, with independent SQLite/WAL/ext2 verification. Normal
desktop ISO rebuilt. This native fixture uses a host-built in-memory program,
not source compilation or portable resource-import linkage.

GNATprove level 2 discharges 298 checks for the VM, resource completion bridge,
registry and native value conversion, with none unproved or justified. The
`From_VM` postcondition explicitly proves successful persistence conversion
excludes both resource values and hidden resource-reference metadata. Existing
native-object wrapper/host-value proof adds 126 discharged checks. No Assume
or SPARK exclusions were added. These are selected-unit safety/contracts, not
proofs of the complete interpreter, codec, IPC shell or external handle cleanup.

Receiver/data update (2026-09-25): 67 new focused checks pass alongside the
original 58 resource-value checks. The six-unit proof (VM, native-object and
resource bridges, host values, registry, native value conversion) discharges
433 checks with none unproved or justified. The capacity regression first
reproduced an offered-borrow/waiting-state mismatch; rejecting the unsubmitted
offer fixes the transition without new guards or proof assumptions. The
completion-ready helper states its actual machine/lifecycle condition directly
so the prover can use it, rather than adding an external precondition.
Final native writer, independent SQLite/WAL/ext2 inspection and fresh-boot
recovery all pass; the normal desktop ISO was restored. Logs:
`/tmp/cubit-receiver-final-native.log`, `/tmp/cubit-receiver-final-host.log`,
`/tmp/cubit-receiver-exhaustion-proof.log`, `/tmp/cubit-receiver-core.log`.
