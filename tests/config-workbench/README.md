# Native Config playground

Developer integration checks (Linux-hosted, no VM or user disk):

```sh
nix develop -c python3 tests/config-workbench/test-desktop-disk.py
nix develop -c bash -c 'cd kernel && alr exec -- gprbuild -p -P ../tests/config-workbench/diagnostics.gpr && ../tests/config-workbench/build/diagnostics/diagnostic_tests'
```

The staging helper in `tools/prepare_desktop_disk.py` prepares a verified
disposable copy of an existing development disk. It never opens the base image
writable, grows only the temporary copy, verifies payload hashes, runs e2fsck,
and publishes only on success. Fifteen real-ext2 tests include an oversized worker,
base-file preservation, bad paths/aliases, failed writes returning exit zero,
same-size corruption, and a failed final filesystem check. Both 1 KiB and 4 KiB
base images are covered, including an 85 MiB payload and preserved 4 KiB geometry.
The ordinary desktop Make targets now share this verified staging recipe.
It must not be used to overwrite the persistent playground disk below.

The diagnostic fixture covers all VM/interpreter statuses and parser diagnostics
with 72 checks. Workbench now uses shared readable messages rather than native
GNAT enum images, so Completed/Waiting for service are not shown as integers.
These label changes have been built on Linux and in the native Workbench.
Focused GNATprove analysis of the three pure message functions reports three
termination checks, none unproved; this is not a proof of GUI rendering.

This is **CuBit in QEMU**, not the Linux Workbench preview. It starts the real
Config storage worker (Turso), Config service, desktop, and Workbench. A fresh
ext2 disk contains the applications and samples; no existing user disk is used.
The usual `run-desktop`, `run-desktop-fast` and `run-desktop-inspect` sessions
now start the storage worker too. `run-desktop` builds it; the fast/inspect
launchers use already staged binaries and fail if a required payload is missing.

## Ordinary desktop

After building, launch normally and open CCL Workbench from Apps. The workspace
contains `config-counter.ccl` and `config-counter-read.ccl`; use Ctrl+F5 to compile
and run them. Workbench and the worker are refreshed in the fast-launch overlay.

These launchers **recreate `desktop_disk.img` from `nvme_disk.img` each time**.
The database survives a guest reboot using the same disk, not another invocation
of the scratch launcher. Use the persistent playground below for retained work.
The scratch helper preserves the base's block geometry and existing Servo/fonts
assets; it never reformats the base or edits it in place.

## Persistent playground

From the repository root, choose a results directory that does not exist:

```sh
flock --exclusive --nonblock coordination/build.lock nix develop -c bash tests/config-workbench/run.sh /tmp/cubit-config-demo
```

Workbench opens automatically. Load `config-counter.ccl` from the workspace,
focus its source editor, then press **Ctrl+F5** (compile and run bytecode).
It opens its approved collection, reads the current revision, writes `42`,
and closes the handle. Load `config-counter-read.ccl` and use Ctrl+F5 again;
it matches the typed read outcome and returns `42`. F5 alone interprets and
does **not** use this asynchronous bytecode adapter.

The value is stored in `com.cubit.ccl-workbench.demo.counter`, in the machine
profile. Workbench may write only beneath its own demo namespace. The older
global Config inspector grant is read-only. The example's `config-values`
alias is a trusted bootstrap binding to that one collection, not arbitrary
namespace selection or unauthenticated service enumeration.

After quitting QEMU, reopen the exact disk without restaging it:

```sh
flock --exclusive --nonblock coordination/build.lock nix develop -c bash tests/config-workbench/run.sh /tmp/cubit-config-demo --reuse
```

`--reuse` retains the staged application versions as well as the database. Use
a new directory when testing rebuilt applications. The interactive runner uses
KVM and GTK/X11; it requires access to `/dev/kvm` and a graphical session.
Do not run two VMs against the same writable disk.

## Regression test

```sh
flock --exclusive --nonblock coordination/build.lock nix develop -c bash tests/config-workbench/run.sh /tmp/cubit-config-test --test
```

To test the actual ordinary desktop startup and Apps-menu launch instead of the
direct Workbench startup fixture (requires an existing development base disk):

```sh
flock --exclusive --nonblock coordination/build.lock nix develop -c bash tests/config-workbench/run.sh /tmp/cubit-config-apps-test --apps-test
```

This stages into the new result directory using `prepare-desktop-disk`, never
the interactive browser disk. Both boots open Apps with Super and launch its
first entry, CCL Workbench, through Desktop/procmgr. If the configured menu order
changes, update the test explicitly rather than substituting trusted startup.

The headless test defaults to TCG; set `CONFIG_WORKBENCH_ACCEL=kvm` for KVM.
It sends real keyboard events through QEMU, desktop input, the editor, compiler,
and native VM. Two writes must produce exactly two revisions. A fresh boot
executes the checked-in read/match sample. After each VM stops, `e2fsck` checks
the image and `debugfs` exports the database and WAL. An independent SQLite
reader verifies the schema key, typed CBOR value, revisions, and integrity.
Screenshots and serial logs stay in the results directory. No guest file is
mounted or inspected by SQLite while the VM is running.

This checks acknowledged writes and recovery, not arbitrary power-loss crash
consistency, all possible scheduling interleavings, or a formal proof of the
whole UI/service stack. Stop/late completion/conflict/grant-retirement cases
are covered separately by `tests/config-object-client/runs.gpr`.

## Remaining UX work

Aggregate result and local-value inspection still displays `<native object>`;
the read sample deliberately extracts the integer using normal typed `match`
and `field`. Collection acquisition failures currently terminate the VM call;
read/write failures are typed outcomes. The REPL/interpreter and remote web
shell do not yet use this execution adapter. Both should reuse authorized
catalog metadata; remote sessions must receive their own grants and contexts,
never the desktop Workbench's process-local handles.
