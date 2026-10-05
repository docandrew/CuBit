# Process arguments, exit status and spawning

Status (2026-10-02): implemented. This is the "Spawn path" step of
[ccl-launch-parameters.md](ccl-launch-parameters.md), reduced to plain
strings: an argument vector and an environment. Typed `.cubit.parameters`
descriptors are later work; the CCL agent's typed wrappers render into
these strings.

Arguments are data, never authority. Nothing in an argument block changes
what a child may do.

Design (approved by the user, 2026-10-02):

1. **One way to start a program**: `posix_spawn`/`posix_spawnp` (over
   procmgr's `OP_LAUNCH`). `fork`, `vfork`, `execve`, `system` and `popen`
   fail with `ENOSYS`; there is no `/bin/sh`.
2. **No PATH search.** A program may start only the names its own manifest
   declares in its launch table, compared exactly. Anything else is refused
   before anything is read or runs (`Not_Granted`, reported through
   `CuBit.Failures` with the manifest line as the remedy; `EACCES` in C).
3. **The child gets less authority, never more** (see "Launch authority").
4. **argv is plain data**: a list of strings, no shell, no quoting.

## End to end

1. **Launcher.** It encodes a *launch block* (below), lends procmgr one
   read-only grant holding the program name followed by the block, and
   calls `OP_LAUNCH` (0x0106) on its process-manager endpoint.
2. **procmgr** (`handleLaunch`) checks the request fields, acquires the
   grant with the requester as the expected owner (the kernel checks the
   generation, range and access), copies name and block into its own
   memory, returns the grant, and validates its copy with the proved
   `CuBit.Launch_Arguments.Validate`. It spawns the child suspended as
   usual, then, before granting anything or resuming it, calls
   `SYSCALL_INSTALL_LAUNCH_ARGUMENTS`.
3. **Kernel** (`Syscall.Admin.handleInstallLaunchArguments`): the caller
   needs `CAP_PROCESS` with `RIGHT_EXECUTE` for the target; the target must
   be suspended and never resumed (`launch = Unstarted`), so arguments go in
   once, before the first instruction. The length must be 1 .. 64 KiB. The
   kernel copies the bytes from the caller's own memory into fresh, zeroed
   pages mapped read-only and non-executable at `0x0000_5A00_0000_0000`
   (owned by the child, freed with it), and sets the main thread's entry
   `RDI` to the length. It does not interpret the bytes. `SYSCALL_RESUME`
   moves the process to `Started`; installation is refused after that.
4. **Child.** The start code reads `RDI` and validates the block again
   before use:
   - libc (`userspace/libc/crt/crt1.c`): argv and the environment for musl,
     strings copied to the stack (writable, as POSIX allows). Without a
     valid block: `argv = { "cubit-program" }`, empty environment.
   - Rust std (the Unix-family std over the libc): `std::env::args`,
     `env::var` and `env::vars` work through musl's argv and environ.
   - Ada (`userspace/c/crt0.S` keeps the length;
     `Ada.Command_Line` in the user runtime): `Argument_Count`,
     `Argument`, `Command_Name`, `Set_Exit_Status`. The environment is not
     exposed to Ada yet.
5. **Exit status.** `SYSCALL_EXIT`'s argument is now kept: the kernel
   records `Exited` with the low 8 bits (as POSIX), unless the process was
   already stopping. Any other end (killed, faulted, main thread ended) is
   `Stopped`. `EVENT_CHILD_EXIT` goes to the parent (the launcher procmgr
   named) and to procmgr with four words: PID, kind (1 Exited, 2
   Stopped), code, and the process's generation, which `OP_LAUNCH` also
   returns, so a launcher matches exits exactly even when a PID is reused
   (`CuBit.Child_Exits`, `<cubit/launch.h>`).

## Launch authority (`CuBit.Launch_Authority`)

- **Launch table.** An executable's `.cubit.launch` section names the
  programs it may start: magic `LNCH`, u16 version 1, u16 count (at most
  32), then per name a length byte (1 .. 255) and the bytes, nothing after
  (512 bytes at most). procmgr keeps each process's table, validated with
  the proved `Valid`, from when it spawned it; without a table a process
  may start nothing with `OP_LAUNCH`. The intended manifest form is
  `(may-launch "as" "ld" "cc1" "collect2")`; until the CCL manifest
  compiler emits it, the test links a section made by the interim fixture
  `tests/process-spawn/launch-table.py` (requested from the CCL agent).
- **Attenuation.** For a child started with `OP_LAUNCH`, procmgr checks
  every request in the child's manifest against what the launcher holds:
  a service endpoint only if the launcher was granted the same service
  role; filesystem scopes only if one of the launcher's scopes covers them
  (same service, rights a subset, prefix matching as a path:
  `Scope_Covered`, which uses `CuBit.File_Access.Scope_Matches`); Config
  and TLS scopes only if identical. Framebuffer, I/O ports, notifications,
  render and network are never passed on, and startup-only approvals never
  apply. Any excess refuses the whole launch (`Not_Granted`) rather than
  silently dropping it, so the failure is explained up front.
- **Delegated places** (2026-10-04): a launcher may also hand the child
  places it holds, each checked the same way (`CuBit.Launch_Grants`, in the
  request beside the launch block; `CuBit.Launching` for Ada launchers). See
  docs/self-hosting.md, item 4.

## Launch block (`CuBit.Launch_Arguments`)

Little-endian, at most 64 KiB and 4096 strings:

| offset | size | field |
| --- | --- | --- |
| 0 | 2 | format version, 2 |
| 2 | 2 | directory count: 0, or 1 when a working directory follows |
| 4 | 4 | argument count (argv[0], the program name, included) |
| 8 | 4 | environment count |
| 12 | 4 | string bytes (block length - 16) |
| 16 | ... | argument strings, then environment strings, then the working directory, each NUL-terminated |

The working directory is a qualified CuBit name (`@nvme:0/src`). Like the
arguments, it is data: relative names start there, and the child's
filesystem scopes still decide what it may open. Version 1, which had no
directory, is gone (it was never deployed).

A block is *well formed* when the header is valid, the counts are within
the limits, the string bytes end with a NUL, and they contain exactly as
many NULs as declared strings. Together that means exactly the declared
strings, each terminated, and nothing else.

## OP_LAUNCH (procmgr)

Request, four words: `(0)` the grant reference (`CuBit.Grant_References`
wire form: generation << 32 | slot), `(1)` name bytes (1 .. 255), `(2)`
block bytes (0 for no block, else 16 .. 65536), `(3)` priority (0:
default). The grant holds the name, then the block. Reply `REPLY_OK` with
the child's PID in word 0 and its generation in word 1, or `REPLY_ERR` with a `Launch_Failure`:
1 malformed request, 2 grant unavailable, 3 arguments rejected, 4 spawn
failed (no such program, bad ELF, policy). The requester becomes the
child's parent and receives its `EVENT_CHILD_EXIT`.

The older `OP_SPAWN` (name and working directory, no arguments) is
unchanged for its current callers (desktop, shell); they can move to
`OP_LAUNCH` and `OP_SPAWN` can then be retired.

## libc: posix_spawn and waitpid

`userspace/libc/overlay/src/cubit/process.c` (musl's `posix_spawn.c`,
`waitpid.c` and `wait4.c` replaced by thin overlays):

- `posix_spawn`/`posix_spawnp` encode `argv`/`envp`, check the block with
  the proved validator, and call `OP_LAUNCH` through the fixed
  process-manager slot 12. The program needs
  `(request-service process-manager read-write process-manager)` in its
  manifest; without it, `EPERM`. Errors: `EACCES` (not in the launch
  table, or the child would hold more than the caller), `E2BIG` (over the
  limits), `ENAMETOOLONG` (name over 255 bytes), `ENOENT` (procmgr could
  not start it), `ENOTSUP` (file actions).
- Names are launch-table names, compared exactly (a leading `/` is
  dropped). There is no `PATH` search: `posix_spawnp` behaves as
  `posix_spawn`.
- `waitpid`, `wait`, `wait4`: `pid > 0`, `-1`, `0` and `< -1` (no process
  groups: any child), `WNOHANG`. Exited children report
  `WIFEXITED`/`WEXITSTATUS`; stopped ones `WIFSIGNALED` with `SIGKILL`.
  `ECHILD` when the program has no such child. `wait4`'s resource usage is
  zeroed.
- `fork`, `vfork`, `execve`: `ENOSYS` (the libc syscall layer has none).
  `system` and `popen`: `ENOSYS` (overlays; `system(NULL)` returns 0, no
  shell).

### Toolchain audit (2026-10-02)

- **GCC driver (libiberty `pex-unix.c`)**: the `posix_spawn` path was added
  by "[PATCH v3] libiberty: Use posix_spawn in pex-unix when available"
  (gcc commit 879cf9ff45, 2023-11-10), first released in **GCC 14.1**; it
  is selected when configure finds `HAVE_POSIX_SPAWN` and
  `HAVE_POSIX_SPAWNP` (unless `spawnve`/`spawnvpe` exist, which musl does
  not have). GCC 13 and older use `vfork`+`execvp` and need a patch. It
  always creates (possibly empty) file actions, which this libc accepts;
  it adds `dup2`/`close` actions only when redirecting (`-pipe`,
  `PEX_STDERR_TO_STDOUT`, output files), which give `ENOTSUP` here. It
  waits with `wait4` (for `-time`) or `waitpid`. `PEX_SEARCH` selects
  `posix_spawnp`, otherwise `posix_spawn` with the path the driver found;
  either way the name must be in the driver's launch table exactly as
  passed (often a full libexec path for `cc1`).
- **gprbuild**: `GNAT.OS_Lib` spawns through `__gnat_portable_no_block_spawn`
  in `adaint.c`, which uses `fork`/`execv` on Unix; a CuBit GNAT runtime
  must route it to `posix_spawn`.

## What is proved, tested, and live

- **Proved** (gnatprove level 1, 162 checks, 0 unproved,
  `tests/launch-arguments/run.sh --prove`): `Validate` accepts exactly the
  well-formed blocks (functional postcondition against the ghost
  `Well_Formed`); `Next_String` stays inside a well-formed block and returns
  a terminated string; `Locate`, the encoder and the header readers are free
  of run-time errors. The kernel only copies bytes; it needs no decoder.
- **Hosted tests** (`tests/launch-arguments/run.sh`): encoder round trip,
  every rejection reason, limits (4096 strings, a block of exactly
  64 KiB), the C entry point, and 400,000 random and mutated blocks against
  an independent reference decoder.
- **Proved** too: `CuBit.Launch_Authority.Valid` accepts exactly the
  well-formed launch tables (functional postcondition against a ghost
  recursive definition); `Contains` and `Scope_Covered` are free of run-time
  errors.
- **Live on CuBit** (`tests/headless/run.sh --test processes`):
  spawn-check (C) starts args-check (C), ada-args-check (Ada) and
  rust-args-check (Rust std) with arguments and environment; checks exit
  codes 42/43/44, three concurrent children with `waitpid(-1)`, exit codes
  0/255/300 (low 8 bits: 44), `WNOHANG`, a faulting child reported as
  stopped, 1000 arguments, `E2BIG`, `ENOENT`, `ECHILD`, `EACCES` for a
  name outside the launch table and for a child asking for a service its
  launcher lacks, `ENOSYS` for `fork`/`execve`/`system`/`popen`; and a
  program started from the startup profile without a block.
- **Assumed, not checked here**: procmgr's request decoding (bounds checks,
  not proved); the kernel handler (`SPARK_Mode Off`, as the rest of the
  syscall integration).

## Results (2026-10-02)

- `tests/launch-arguments/run.sh --prove`: 61 hosted checks PASS, 400,000
  random blocks agree with the reference; gnatprove level 1: 257 checks,
  0 unproved (CuBit.Launch_Arguments, CuBit.Launch_Authority,
  CuBit.Child_Exits, the kernel's Process_Launch).
- `tests/headless/run.sh --test processes` (QEMU TCG, 4 CPUs): `headless:
  PASS processes`, all 25 markers, ending `PROCESS-SPAWN: PASS`.

## Gaps

- Events are not unforgeable: another process could send a fake
  `EVENT_CHILD_EXIT` naming one of our children, and `waitpid` would report
  it ended. procmgr guards against this with the process list; the libc
  does not yet.
- The parent's event ring is bounded: exit events can be dropped if many
  children end while the parent never drains its events (the kernel counts
  drops: `SYSINFO_EVENT_DROPS_SELF`). `waitpid` would then wait forever.
- `waitpid` blocks on "any IPC activity" and pauses 1 ms after a wakeup
  that brought no exit event, because the kernel has no wait for events
  alone that returns the whole message (`SYSCALL_RECEIVE_EVENT` returns only
  the tag).
- Non-exit events reaching a libc program are dropped by `waitpid`
  (procmgr's `OP_STREAM_AVAILABLE` for children that declare streams).
- No descriptor inheritance, by design: a child's stdout/stderr are its own
  CuBit streams, not the parent's descriptors; `posix_spawn_file_actions_*`
  give `ENOTSUP`. A launcher reads or wires its children's streams instead
  (docs/self-hosting.md, item 4); there are no Unix pipes.
- Only libc programs read the working directory so far. Ada programs and
  the Moto-based Rust runtime ignore it.
- `OP_LAUNCH` carries no sandbox override (the `OP_SPAWN` sandbox modes
  are not offered); the working directory travels in the launch block.
- The launch table comes from an interim test fixture until the
  `(may-launch ...)` manifest form exists; no shipped program has one, so
  only the test can use `OP_LAUNCH` today.
- Attenuation compares a launched child's requests with what procmgr
  granted the launcher; authority a launcher obtained another way
  (delegated endpoints, grants) is not counted, which only errs toward
  refusing.
- Ada sees no environment; the Motor-based Rust std (`target_os = "cubit"`,
  non-Unix) still has empty `env::args`.

## Files

- `userspace/runtime/gnat/cubit-launch_arguments.ad[sb]`,
  `cubit-launch_arguments_c.ad[sb]`, `cubit-launch_authority.ad[sb]`,
  `cubit-child_exits.ads`, `a-comlin.ad[sb]`
- `kernel/src/process_launch.ads`, `syscall-admin.adb`
  (`handleInstallLaunchArguments`), `process.adb` (`recordExit`, exit
  event), `syscall.adb` (number 126, exit code)
- `userspace/services/procmgr/main.adb` (`handleLaunch`)
- `userspace/c/crt0.S`, `userspace/libc/crt/crt1.c`,
  `userspace/libc/overlay/include/cubit/launch.h`,
  `userspace/libc/overlay/src/cubit/process.c` and the overlays
  (`posix_spawn.c`, `waitpid.c`, `wait4.c`, `system.c`, `popen.c`)
- Tests: `tests/launch-arguments/`, `tests/process-spawn/` (with the
  interim `launch-table.py`),
  `tests/headless/init-processes.ccl`

## Build and run

```sh
export TMPDIR=$PWD/tests/net-tcp/build-tmp
nix develop -c bash tests/launch-arguments/run.sh --prove
flock --exclusive coordination/build.lock nix develop -c bash -c \
  'make -C kernel user_runtime ccl-manifest && bash tests/headless/run.sh --test processes --timeout 90'
```

The `processes` case rebuilds the libc and the test programs
(`tests/process-spawn/build.sh`), and, like every case, the kernel and the
stage-1 services (procmgr included).
