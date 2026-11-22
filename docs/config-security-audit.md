# Config authority boundary: 2026-09-26 audit

Status: source audit, focused hosted proofs/tests and native desktop integration.
Not a claim that Config is policy-complete or that the full service is verified.
Use the [security vocabulary](security-vocabulary.md) and
[verification plan](security-model-verification.md).

## Paths and current enforcement

| Path | Enforcement in this checkout | Limit / remaining work |
|---|---|---|
| Typed collection open/create | `Config_Object_Receiver.Begin_Definition` snapshots the descriptor, checks machine context, installed subject rules, requested operations and namespace before storage admission. | Sender authentication is supplied by the IPC adapter. No user-selectable contexts or live grant broker. |
| Typed read/write | `Config_Collections.Resolve` checks holder, handle operations, current installed authority and exact grant revision. `Config_Object_Dispatch` checks access before submission; writes use expected revisions. | These local checks do not establish process-incarnation safety or cross-service atomic revocation. |
| Typed close | Holder and live token required; failed acquisition reply delivery closes the newly opened handle. | Unknown delivery/retirement outcomes still need quarantine; close is not revocation of future open authority. |
| Scalar get/set/delete | `main.Handle_Data` validates wire framing, snapshots caller input and uses `checkAccess`; write protects reserved system keys. | Scalar settings still use the owned in-memory `Config_Store`; this is not automatic Turso persistence for all settings. |
| Scalar list/inspection | Requested scope and each returned key are checked; global probe requires wildcard read except for the administrative shortcut. | This is scalar inspection, not a complete typed-object or system-authority inspector. |
| Authority install/revoke | `main.isAdmin` compares the authenticated sender with current devmgr/procmgr registry roles; install snapshots scope bytes, revoke invalidates subject state/handles. | Broad trusted bootstrap administration, not the final scoped issuer model. An authorized zero-rule wire request deliberately installs wildcard read/write; an empty ordinary `Rule_Set` grants nothing. |
| Worker attachment | Registered procmgr and a valid attachment frame are required. | Role-based bootstrap trust is explicit; worker/provider restart binding is not proved by the pure collection model. |

The legacy scalar `checkAccess` grants registered devmgr/procmgr unrestricted
access before consulting subject rules. That is a concrete transitional
exception, not evidence that every Config access already follows the target
scoped-grant model. Typed collection access does not call that shortcut; it
requires installed rules. Track replacement under SEC-019/SEC-020, with explicit
bootstrap grants before removing the old path. Do not broaden ordinary app
grants to make startup pass.

## Lifetime boundary

Within one service lifetime, handle tokens and installation revisions do not
wrap or silently reuse. Revoking or replacing a subject's rules invalidates
old handles, including after the same rules are reinstalled. Capacity denial
does not fall back to wildcard authority.

The current Config subject is still the IPC PID, not a kernel-authenticated
process-instance pair. Procmgr's reset-before-resume and failed-launch cleanup
reduce reuse risk but are not a proof for arbitrary death/restart/reuse traces.
Config restart also needs an explicit service epoch/client rebinding protocol.
These are existing open boundaries, not solved by a monotonic counter that
resets when its owning service restarts. Coordinate the sender-incarnation ABI
with the kernel/thread owner before changing it locally.

Already accepted writes and grant withdrawal need a documented ordering rule;
revocation must not be presented as rollback of an accepted durable transaction.
The current hosted tests cover revision conflicts, stale sessions and worker
failures, not every distributed interleaving or arbitrary power failure.

## Evidence from this round

Follow-up: the native service now calls the pure `Config_Authority_Wire`
counted-rule decoder instead of its old inline parsing loop. Exhaustive hosted
rights/length/reserved-byte and boundary tests pass 134,962 checks. GNATprove
discharges 18 checks, including rejection returning exactly an empty rule set;
none unproved or justified, with Z3/CVC5 invoked. This establishes the decoder's
bounds and failure-output property, not administrator authenticity or global
non-amplification. Exact valid decoding remains regression-tested. Existing
scalar and typed Config tests/proofs still pass. The explicit bootstrap
zero-count wildcard path remains outside this decoder and is not removed.
The native QEMU/KVM `config-inspection` regression also passes, including
rejected malformed launch policy, authorized operations and denied-client tests.

All commands used the Nix environment. Shared native builds and QEMU testing
held `coordination/build.lock`. No existing browser/user disk was replaced.

* Hosted collection handles: 168 checks; typed publication: 97 checks.
* Hosted worker channel: 42 scenarios; receiver/channel: 86 scenarios; native
  type channel: 680 checks; worker protocol: 4,573; worker execution: 602.
* Scalar inspection suites pass namespace component boundaries/non-1 indices,
  default deny, isolation, replacement/revocation/capacity, malformed wire
  bounds/legacy framing, owned-store replacement/reuse and inspection-tree cases.
* Focused GNATprove for `Config_Authority`, `Config_Collections` and
  `Config_Typed_Store`: 158 checks, including 10 functional contracts; none
  unproved or justified. CVC5 and Z3 were invoked. Three existing warnings concern
  an unused resolved ID and specialized branches with no effect. Native syscall,
  shared-memory overlays, Rust/Turso and GUI code are outside this proof.
* Scratch disk tests: 15 pass at 1/4 KiB block sizes, including 85 MiB content,
  base preservation, failed writes, corruption and failure-before-publication.
* Native `--apps-test`: ordinary desktop startup, keyboard Apps launch, editor
  compilation, two typed Config writes, then a fresh boot and guest assertion
  that the recovered value is 42. Both stopped-disk checks pass SQLite integrity,
  exact schema/value/revisions and `e2fsck`.

Local logs: `/tmp/cubit-config-authority-regression-r1.log`,
`/tmp/cubit-config-inspection-audit-r1.log`,
`/tmp/cubit-config-desktop-staging-r2.log`,
`/tmp/cubit-config-desktop-native-r1.log`.
Native artifacts were preserved and moved out of quota-limited `/tmp` to
`tests/config-workbench/build/artifacts/cubit-config-desktop.oJb2Sw/apps`.
Decoder test/proof logs are in `tests/config-inspection/build/authority-wire-*.log`.
Repeatable commands and UI instructions are in
[the playground README](../tests/config-workbench/README.md).

## Remaining Config milestone

1. Kernel-authenticated instance identity and explicit service-restart epochs,
   with native stale-handle/replay tests, not only pure subject-table tests.
2. Replace broad registry-role administration with scoped issuance authority;
   preserve explicit bootstrap and inspectable decision reasons.
3. Connect legacy scalar/default settings to the durable typed model without
   silently changing types, authority or bootstrap precedence. Typed collection
   delete/list and additional profiles are not implemented by this demo.
4. Reuse the shared execution adapter in the interpreted REPL and remote shell;
   the Workbench integration currently runs compiled native-object bytecode.
5. Validate workload performance and failure recovery separately from clean
   reboot persistence. ext2 remains non-journaled.

Ordinary desktop launchers recreate their scratch disk on each invocation.
Use the explicit playground `--reuse` path to retain work across launches.
