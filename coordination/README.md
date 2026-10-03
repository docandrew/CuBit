# Shared-checkout agent coordination

This is an advisory handoff between independent sessions, not an automatic
message channel. Each agent owns its own note; read the other notes before
touching shared files and at the start of each work chunk. Timestamps describe
the last update, not a lease or proof that an agent is still running.

- `filesystem.md`: filesystem, storage and Turso work.
- `networking.md`: networking drivers, TLS and NetSurf work.

Record scope, exact shared files, active build/test commands, and requests.
Do not edit another agent's note or treat silence as permission to edit a file
they have claimed. For overlaps, post a request in your own note and wait for
acknowledgment, or ask the user to relay it. Unrelated work can continue.

## Shared build/test lock

From this checkout's root, wrap commands that touch shared build outputs or
boot artifacts as follows:

```sh
flock --exclusive --nonblock coordination/build.lock nix develop -c make -C kernel filesystem
flock --exclusive --nonblock coordination/build.lock nix develop -c bash tests/headless/run.sh --test storage-grants --accel tcg,thread=multi --timeout 60
```

For a dependent build and test, hold the lock over BOTH:

```sh
flock --exclusive --nonblock coordination/build.lock nix develop -c bash -c 'make -C kernel filesystem storage-check && bash tests/headless/run.sh --test storage-grants --accel tcg,thread=multi --timeout 60'
```

`--nonblock` exits if another command holds the lock: report/publish that state
and do independent work. Do not remove the lock file or kill the other agent's
process to acquire it. The OS releases the lock when all holders close it; the
empty file remaining on disk is normal and does not mean it is locked.

Use this lock for native/runtime builds, ISO creation, headless QEMU tests and
anything modifying shared staging (including temporary GRUB configuration).
Also HOLD this lock while editing shared build/test scripts or build definitions,
especially `tests/headless/run.sh`: Bash reads scripts incrementally, and an
in-place save can break another agent's already-running script even when the
edited branch belongs to a different test. A one-time lock availability check
followed by an unlocked edit is not mutual exclusion. If the lock cannot be held
through the edit, defer it and coordinate an idle window instead.
Hosted tests/proofs with disjoint output directories may run concurrently, but
coordinate CPU-heavy workloads. Do not edit shared sources while the other
agent is building them: the build lock alone does not prevent source races.

Both agents must follow this convention for it to provide mutual exclusion.
Existing commands are not automatically wrapped. Use unique log, socket and
test-image paths. Never modify the user's base disk or kill unrelated VMs.

## Parallel private build workspaces

The user authorized isolated native builds on 2026-09-26. The shared lock is
still mandatory for shared outputs, but NOT for work wholly inside a private
snapshot. `tools/build-workspace.py` implements the first kernel/live-image
workflow:

```sh
nix develop -c python3 tools/build-workspace.py create boot-debug --seed-live
# Substitute the unique path printed above:
nix develop -c python3 tools/build-workspace.py run .build-workspaces/boot-debug-XXXXXXXX -- make -C kernel cubit_kernel
nix develop -c python3 tools/build-workspace.py run .build-workspaces/boot-debug-XXXXXXXX -- bash -c 'bash tests/usb-optical/build-live.sh "$DOOM_WAD" --uefi && python3 tests/usb-optical/run-live.py --uefi --cpus 4'
```

Creation acquires the main lock briefly to copy sources plus optional live
seed artifacts. Do not wrap `create` in another acquisition of the same lock.
It includes modified and untracked, nonignored sources, excludes untracked
build trees/backups, and records each input's hash. Ignored dependencies are
not automatically copied. `--seed-live` copies staged service binaries, the
live RAM-disk seed, CCL image/config tools and the test ROM. These binaries
are explicitly recorded as seeds, not represented as freshly built sources.
Private ROMs are not copied automatically. This is not yet a full `make world`
workspace: external/ignored port dependencies may require explicit seeding.

The source snapshot and artifacts are regular independent files, never hard
links or symlinks back into the checkout. Internal leaf source symlinks are materialized as regular
copies, recording link text and resolved in-repository target; both provenance
and content are rechecked. External/directory links, linked parents, and seed
artifact symlinks are rejected. Source hashes are rechecked before
marking it complete; concurrent source editing still requires coordination.
No Git commit, branch or shared index change is made. Do not run `git` mutations
inside the snapshot: Git would otherwise discover the enclosing main checkout.

`run` requires Nix, sets a private TMPDIR, and holds only that snapshot's lock.
Independent snapshots may compile/package/boot concurrently. This is isolation
of build state, NOT a security sandbox: use trusted commands with relative
paths; don't point tools at shared outputs or the user's development disk.
For a stable toolchain, continue using the main checkout's pinned Nix shell;
the snapshot also records the flake files used when it was created.

Artifacts stay private until explicitly published under the shared lock. Prefer
handing the user a link to the private image instead of overwriting a shared one.
Edits made privately must be reviewed and applied back to the appropriate owned
source files; never blindly synchronize the whole snapshot over the checkout.
Preserve the input manifest for reproducibility. Failed snapshots remain marked
incomplete and cannot run through the helper. They are not silently deleted.

Functional tests can overlap. Performance measurements still need an otherwise
quiet host (no competing compiler/prover/VM jobs), or explicitly documented CPU
isolation; per-workspace locks do not isolate CPU, cache or memory bandwidth.
