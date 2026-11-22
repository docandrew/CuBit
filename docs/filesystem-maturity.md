# Filesystem maturity and application-scoped access

Status: implementation progress and remaining design work, September 2026.

## Usable now

Files can enter folders with Open or Enter, refresh the current folder, and
return with Back or Backspace. It keeps already-authorized parent handles; no
`..` resolution is needed. Back is disabled at the initial root. Navigation
retains at most 16 directory handles and shows at most 128 entries with an
explicit truncation indicator. A failed read/open leaves the previous listing
visible and reports failure. Closing the app releases its retained handles.

The first root remains explicitly manifest-scoped NVMe storage, falling back to
the live memory workspace. CPIO remains a flat bootstrap archive. ISO9660 is
still future work; this change does not add a CD filesystem or writable desktop
file operations. Files is not yet a trusted cross-process Open/Save chooser.

The shared filesystem protocol adds:

| Request | Meaning |
|---|---|
| `Open_Child_Directory` | Owned parent handle plus one name in a generation-checked input loan; produces a read-only directory handle on the same backend. |
| `Rewind_Directory` | Refresh metadata and restart enumeration of the same owned object, without reopening its pathname. |

Child lookup rejects path components that could change namespace or traverse a
parent. It validates the resolved inode type rather than trusting the listing's
kind hint, and does not follow symlinks. It checks both parent handle ownership
and the application's current policy. Metadata read and child lookup now have
typed outcomes; device and malformed-directory errors propagate through the new
navigation path. Older callers of the compatibility overloads still collapse
some failures to zero/not-found and remain migration work.

## ACLs are application policy, not Unix permissions

The current model already ignores ext2 uid/gid/mode for authorization. The
filesystem checks its authenticated caller, the installed per-process scope
profile, and the rights retained by a service-issued file/directory handle.
The endpoint capability only authorizes reaching the service.

`CuBit.File_Access` now supplies a bounded, pure SPARK decision core:

- Named read/write/execute/create rights, using the existing manifest bit layout.
- Scope matching at a complete path-component boundary.
- One matching entry must supply all requested rights; independent entries are
  not merged into broader authority.
- Empty policy, invalid paths, and unknown rights do not grant access.
- Oversized scopes are rejected, never truncated into a different scope.
- Decode into a candidate; publish only after validation and return of the loan.
- Successful profile replacement/revocation invalidates the owner's handles;
  malformed/unauthorized updates preserve the current policy and handles.

The current devmgr/procmgr registry identities remain the policy installers.
Ordinary applications cannot call the administrative messages to grant
themselves access. The legacy zero-entry administrative bootstrap operation is
still an explicit wildcard request; it is not the meaning of an empty Policy.

Before resuming any newly spawned app, procmgr clears that PID's filesystem
profile/handles and only then installs its validated manifest scopes. This also
applies when the access section is missing or malformed. A failed reset aborts
launch. It prevents a normally launched app inheriting a previous PID occupant's
filesystem authority; it does not replace generation-bound service identities
or timely cleanup following process death.

### Next policy shape (proposal, not implemented)

Keep the same three levels rather than adding another permission system:

1. **Declared request ceiling:** what this signed/admitted application may ask
   for. Installation and launch policy may narrow it further.
2. **Concrete handles:** selected projects, documents, application data, or
   temporary storage, each with explicit rights, issuer, reason, and lifetime.
3. **Replies and loans:** request completion and temporary data access, never
   an implicit right to acquire unrelated files or forward a borrowed mapping.

Friendly launch roots such as `project`, `documents`, `pictures`, `app-data`,
and `temporary` should bind to supplied handles, not public machine-wide paths.
The same name may refer to different objects for different apps. Knowing a name
does not acquire its handle. Backend selectors such as `@nvme:0/` should not be
part of an application's permanent security identity.

The existing coarse rights should eventually become explicit verbs:
inspect metadata, enumerate, read content, append, replace content, create child,
remove child, rename, subscribe to changes, and delegate. Do not overload
"write" to mean all of these. A read-only chooser result must not acquire
create/delete/rename/watch rights incidentally. Watch subscriptions reveal names
and activity and need their own bounded, revocable authorization.

Runtime approval should issue an attenuated object handle through the existing
trusted approval path. It must not edit a requesting app's broad path profile as
a shortcut. A chooser binds the request to the authenticated recipient instance,
object, rights, and decision; it returns a handle, not just a filename. Closing
a UI is not proof that a noncancelable write has stopped.

Policy persistence remains undecided: a signed declarative system config plus a
protected object/approval store is a candidate. App identity metadata is not yet
signature-authenticated. Bind durable policy to stable publisher/application and
volume/object identities, never a PID, displayed title, or mutable path alone.
Expose WHAT/WHO/WHEN/WHERE/WHY in the future Inspector, including the authority
ceiling, actual grants, denials, exceptions, expiry, and effects of revocation.

## Before exposing replacement Save in Workbench

Workbench now supports named, non-overwriting source files and a shared
Open/Save picker within its explicit work folder; see
[CCL workspace milestones](ccl-workspace.md). Its Linux adapter is memory-only.
That limited workflow does not provide replacement Save, a cross-scope trusted
chooser, or crash consistency.

Priority findings from the code audit:

1. **Ordinary rename repaired; safe replacement still pending.** Rename no
   longer unlinks the old name before attempting insertion. It validates and
   prepares a separate directory block, preserving inode identities and
   rejecting an existing destination. Only then does it write the block with
   a checked outcome. A failed write triggers restoration of the original
   block, even if the device may have partially or completely changed it.
   Failed restoration reports `RECOVERY_REQUIRED` and write-quarantines that
   filesystem instance. The central physical-write path rejects subsequent
   writes; this is not an on-disk recovery record or permission to remount RW.
   Rejections during preparation make no changes. Supported renames stay in
   one parent and fit within the source block after compaction; operations
   needing additional blocks or cross-parent movement explicitly return
   `FILE_RANGE_UNSUPPORTED`. Flagged directories (including hash-indexed ones)
   are rejected rather than mutating records without maintaining the index.
   Nested paths resolve their parent first. This is
   deliberately **non-overwriting rename**, not POSIX replacement or safe-save.
   Crash/reboot recovery and atomic replacement remain unimplemented.
2. **Directory mutation parsing needs the same discipline as enumeration.**
   Add/remove still walk raw variable-length records more loosely than the
   checked page reader. Extract one bounded record iterator/ADT and use it for
   both reading and mutation. Do not merely copy checks into each loop.
   The mount-time superblock check validates geometry, but does not yet decode
   and enforce all feature masks. Reject unsupported filesystem layouts before
   exposing mutation; do not treat the shared ext2/ext4 signature as proof that
   extents, checksums, indexing, or journal recovery are supported.
3. **Object/session lifetime needs a real generation/death path.** The normal
   launch reset is in place; profiles and handles are still PID-owned inside the
   service. Prompt cleanup of dead clients, stale IPC/async completions, inode
   reuse, rename across authorized roots, directory hard links on malicious
   media, and independent raw-volume writers need explicit treatment. Audit
   configuration-service PID/profile reuse as well.
4. **No journal yet.** Successful operation tests are not power-loss tests.
   Typed durable-commit outcomes and a journal/recovery protocol remain separate
   work. Never claim `rename`, `close`, or a completed block write implies a
   durable transaction today.
5. **Authority integration is still incomplete.** Manifest ceiling versus user
   approval, recipient-bound chooser delegation, watch rights, policy provenance,
   and persistent policy are design work, not implemented guarantees.
   In particular, validating the syntax of an app's requested scopes does not
   decide whether an untrusted app deserves those scopes. Signed admission and
   installed-policy ceilings must constrain procmgr's grants before arbitrary
   downloaded applications can be claimed safe by default.

After these boundaries are sound: trusted Open/Save chooser and safe replacement
for Workbench projects. Independent CCL widgets can first exercise the limited
revision workflow. Read-only ISO loading remains a separate
backend project and must reuse the same application protocol and authority model.

## FS policy implementation and package lifecycle

This is a proposed implementation sequence, not a claim that current manifest
requests pass a complete installation-policy gate. It specializes the security
model's existing endpoint/handle/loan rules rather than adding another kernel
capability class or Unix ownership model.

### Keep three questions separate

| Question | Representation and enforcement |
|---|---|
| What may this app request? | Manifest declarations narrowed by an approved installation policy; procmgr may not exceed that ceiling when admitting a process. |
| Which particular object did this instance receive? | FS-issued, recipient-bound, generation-checked handle with explicit verbs and scope. Friendly launch names bind to these handles. |
| May this operation proceed now? | FS checks the current owner/session, handle generation, requested verb, object/scope, revocation, and limits before accepting the operation. |

A publisher signature authenticates evidence used by admission policy. It is
not itself any of these grants. A successful package build, installation, or
discovery query does not imply permission to activate or broaden app access.

### First implementation slice: approval ceiling before more verbs

1. Define a bounded, pure admission decision over requested scopes and an
   already-approved installation profile. Missing approval denies, except for
   explicit, separately recorded bootstrap profiles. Distinguish automatic
   startup grants from rights that merely permit requesting a chooser result.
2. Have procmgr install only this decision for a suspended child. A malformed
   request, denied required request, stale approval, or failed installation
   prevents resume. An optional denied request stays unavailable; it does not
   trigger a fallback to broader authority.
3. Bind the profile to the admitted artifact/installation and authenticated
   process generation. Extend service-session teardown so PID reuse and stale
   replies cannot attach old grants to a new process. Ordinary app launch
   must not be able to impersonate the administrative issuer.
4. Prove one property at a time: effective rights do not exceed request or
   approval; a rejected decision leaves active policy unchanged; session
   mismatch denies; derived handles cannot widen scope or rights.

The existing `File_Access` policy representation can support initial bounded
scope intersection, but two prefix checks are not an object-containment proof.
Reject ambiguous path/backend aliases during this transition; eventually bind
approved roots to a volume incarnation and directory object, not a selector
such as `@nvme:0/`. Migrate older path resolvers and mutation parsers to the same
checked boundary before claiming complete mediation on hostile media.

Keep policy compilation and approval on the control path. Routine I/O should
validate a local handle-table entry and its current generation/rights, not make
an extra policy-service IPC or walk a textual ACL for each buffer. Root/object
containment is established during derivation and preserved by the mutation
rules. Revocation must invalidate that cached authority; it must not rely on
clients voluntarily repeating discovery. The existing shared-memory data path
remains independent of these control decisions.

### Then introduce object-root grants

Use private, typed service records for installation identity, process instance,
root grant, and open handle. A root grant contains a stable object binding,
enum-indexed rights, issuer/decision reference, recipient, generation, and
limits. An open handle derives from that grant and remembers its revocation
lineage. It does not obtain authority by reinterpreting a saved pathname.

Separate `read`, `enumerate`, `create-child`, `rename`, `replace`, `remove-child`,
`watch`, and `delegate` as the interfaces mature. A rename requires authority
over the affected directory binding(s); modifying a file's contents alone must
not allow deleting or replacing its name. Cross-root moves need an explicit
rule for both roots and retained handles before they become available.

Watch events use the same root/handle lineage, bounded queues, and revocation
rules. A subscription must not leak names outside its scope; queued events
must also respect invalidation. Raw block writers remain a separate powerful
authority: filesystem handles cannot provide isolation against an independent
writer modifying the mounted volume underneath the service.

### Package-store and app-data split

| Resource | Intended authority |
|---|---|
| Verified package realization | Immutable store object, readable only by admitted consumers; creation/finalization belongs to the store service. |
| Build workspace | Bounded temporary root for one build, not the installer's or eventual app's full authority. |
| App data | Private RW root attached to a stable installed-application identity, not its current executable digest or PID. |
| Chosen project/document | Separately approved, recipient-bound handle; not an automatic consequence of installing the app. |
| Deployment activation record | Explicit activation authority; storing a package cannot switch the active deployment. |

The candidate stable app identity is **admitted publisher + application ID +
local installation instance**. Different versions have distinct artifact
digests but may share that installation's data root. Separate installation
instances can have separate roots. Neither an unsigned `.cubit.id` string nor
a familiar display name is proof of continuity.

An upgrade re-evaluates requested authority and shows additions before
activation. New requests are not automatically approved because the old
version was approved. Key rotation, publisher changes, reinstall, and data
adoption need explicit continuity decisions. The UI should display artifact
identity, stable installation identity, approved ceiling, concrete roots, and
the decision that connects them: WHAT/WHO/WHEN/WHERE/WHY.

Data migration is a separate, confined operation with declared old/new data
roots and explicit commit authority. No unrestricted privileged install script.
Code rollback does not undo mutable data changes; require a compatible schema,
retained data generation, or an explicit irreversible-migration decision.
Package finalization and deployment activation need their own durable commit
protocol. The repaired ordinary rename is not that protocol.

### Decisions to review before implementing persistence

- Should each local installation get private data by default, with sharing
  only through explicit shared roots? Recommended: yes.
- Which approved issuer can establish identity continuity across signing-key
  changes, reinstalls, or a change of publisher?
- How are declarative policy and temporary chooser approvals composed and
  persisted? Candidate: declarative ceiling plus a protected grant/decision
  store, never silently rewriting the manifest or widening the ceiling.
- What survives revocation for already-accepted, noncancelable writes? Deny
  new submissions immediately, retain borrowed memory until completion, and
  report outstanding effects; do not claim completed work was undone.

## Verification and reruns

Host tests and proof (Linux, not guest code):

```sh
nix develop -c make -C kernel test-filesystem-policy prove-filesystem-policy
```

The target includes the bounded path/policy helpers and `Directory_Blocks`.
The latter proves 73 checks, including unchanged input after failed rename
preparation. No assumptions or SPARK-Off sections were added to these packages.
This is not yet a functional proof of every successful directory transformation.
The native filesystem, ext2, and procmgr are **not** thereby proved end to end.

Hosted rename tests cover collisions, missing/invalid names, insufficient space,
malformed and duplicate records, 1/2/4 KiB blocks, and maximum-length names.
An injected writer exercises the same generic commit helper used by ext2:
failure before any bytes change, partial/full changes reported as failure,
successful restoration, and failed restoration. These are reported-I/O failure
tests, not power-loss or remount-recovery tests. QEMU additionally exercises the
native rename IPC, nested paths, collision preservation, unsupported moves and
selectors, and open-handle continuity across rename.

Native builds and guest regressions:

```sh
nix develop -c make -C kernel filesystem procmgr files storage-check
nix develop -c tests/headless/run.sh --test storage-grants --accel kvm --timeout 40
nix develop -c tests/headless/run.sh --test files --accel kvm --timeout 40
nix develop -c tests/headless/run.sh --test capability-security --accel kvm --timeout 30
nix develop -c tests/headless/run.sh --test ccl-workbench-virtio-vga --accel kvm --timeout 25
```

The native scope-check app has a read-only subtree and a separate read/write/
create subtree. It proves by execution that an allowed mode-000 ext2 file can be
read, while writes through a read-only handle, sibling-prefix access, root access,
and administrative self-grant attempts fail. The guest navigation test cycles
handles repeatedly and exercises malformed input, stale generations, and
malformed media. Fixtures are temporary copies, not the interactive data image.

The storage/scopes, Files, capability-security, and native CCL Workbench tests
passed under KVM after this pass. Storage/scopes also passed under TCG against
the small capability-security fixture disk. CI now includes both pure helper
tests/proofs and that native scoped-storage test without requiring KVM.

Validation logs for this working-tree pass are in `/tmp/cubit-fs-host-validation.log`,
`/tmp/cubit-fs-maturity-validation.log`, and `/tmp/cubit-fs-ci-validation.log`;
guest serial files use `/tmp/cubit-fs-maturity-*.serial`. Changes remain
uncommitted, including the earlier kernel work that was already in the tree.

The rename follow-up uses `/tmp/cubit-rename-policy-proof.log` for the host
tests/proofs, `/tmp/cubit-rename-native-final.log` for the Files UI regression,
and `/tmp/cubit-rename-storage-final.log` for the final KVM/TCG storage runs.
Guest serial files are `/tmp/cubit-rename-{storage,files,tcg}.serial`.
