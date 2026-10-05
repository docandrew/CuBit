# Processes agent (formerly "coreutils agent")

Spawned by the CCL agent, 2026-10-02. Scope (user, via the CCL agent):
argv/environment delivery end to end, exit status to the launcher,
posix_spawn/waitpid over procmgr (no fork, no shell), the user-approved
launch design (one entry point, launch tables instead of PATH, children hold
a subset of their launcher's authority), headless test, docs. Design and
status: docs/process-arguments.md.

The uutils/coreutils port was cancelled before any file was created for
it; nothing to revert.

## Claimed files (new, mine)

- runtime: `cubit-launch_arguments.ad[sb]`, `cubit-launch_arguments_c.ad[sb]`,
  `cubit-launch_authority.ad[sb]`, `cubit-child_exits.ads`, `a-comlin.ad[sb]`
- `kernel/src/process_launch.ads`
- libc overlay: `src/cubit/process.c`, `src/process/{posix_spawn,waitpid,system}.c`,
  `src/linux/wait4.c`, `src/stdio/popen.c`, `include/cubit/launch.h`
- `tests/launch-arguments/`, `tests/process-spawn/`,
  `tests/headless/init-processes.ccl`, `docs/process-arguments.md`

## Shared files edited (narrow)

- kernel: `process.ads/.adb` (launch phase, termination report, exit event
  now 4 words: PID, kind, code, generation), `syscall.ads/.adb` (syscall 126,
  exit code kept), `syscall-admin.ad[sb]` (install handler, resume marks
  started), `ipc_labels.ads` (OP_LAUNCH 0x0106). The uncommitted grant-slot
  hunks of others and syscall-ipc.adb untouched.
- `userspace/services/procmgr/main.adb`: OP_LAUNCH, launch tables,
  attenuation of launched children, per-process launch state (~0.4 MiB BSS),
  EVENT_CHILD_EXIT validated via CuBit.Child_Exits (4 words).
- `userspace/c/crt0.S` (keeps the launch length; Ada exit status),
  `userspace/libc/crt/crt1.c`, `userspace/libc/build.sh`.
- `tests/headless/run.sh`: `--test processes` (edited under the lock).
- Earlier slips, all fixed within minutes: two 80-column lines in the
  runtime and one procmgr compile error. Everything since is compile-checked
  privately before the next shared build.

## Active commands

None. Last: 2026-10-02 ~15:45 `run.sh --test processes` PASS (TCG,
serial3.log in tests/net-tcp/build-tmp/processes-agent/); hosted tests and
gnatprove level 1 (257 checks, 0 unproved) PASS.

## Requests to the CCL agent

1. Manifest form `(may-launch "as" "ld" ...)` emitting the `.cubit.launch`
   section in the layout of `CuBit.Launch_Authority` (magic "LNCH", u16
   version 1, u16 count <= 32, then length-prefixed names, <= 512 bytes).
   `Valid` there is the proved reference decoder. Until then the test links
   a section from `tests/process-spawn/launch-table.py` (interim fixture,
   to delete once the form exists).
2. Place delegation: the approved design hands a child a declared subset
   of its launcher's grants (e.g. the build directory's place). Today
   procmgr only checks the child's own manifest scopes against the
   launcher's (refuses on excess); passing places down needs the
   filesystem service.
3. Launchers: CCL `run` / startup `(start ... (arguments ...))` can build
   blocks with `CuBit.Launch_Arguments.Builder` and call OP_LAUNCH; exit
   events decode with `CuBit.Child_Exits` (match PID and generation).
