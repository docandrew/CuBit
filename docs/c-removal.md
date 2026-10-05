# Removing CuBit-written C

User decision, 2026-10-04: "anything that we write ourselves SHOULD NOT be C.
Period. Start rewriting all the C we have in the tree." musl stays as the
C-compatibility layer for ported C programs (gcc, binutils, git, games); its
printf, strcpy and the rest are acceptable. Everything CuBit itself writes is
Ada (SPARK where it parses or decides anything) or Rust.

## Scope: only CuBit-written C

Only C written for CuBit is ported. Third-party code stays as it is: musl,
Mesa, the SameBoy emulator, doomgeneric's
engine, Servo and its crates. For a ported program, only CuBit's own layer
between it and the system (display, input, sound, timing) is rewritten. The
homegrown freestanding libc under Doom and SameBoy is replaced by musl, not
translated. Public C headers that declare the Ada functions for C programs
(`cubit/launch.h`, `cubit/debug.h`) are interfaces, not implementations, and
stay.

## The pattern

- **libc internals** become Ada packages exported with
  `Export, Convention => C, External_Name => "__cubit_..."`, compiled into
  libc.a by `userspace/libc/build.sh`. Examples: `CuBit.Launch_Arguments_C`
  and `CuBit.Path_Names_C`.
- **The libc's Ada has no Ada run-time library.** No exceptions, no secondary
  stack, no tasking, no elaboration code. Built with `-gnatp`, so it must be
  proved (or at least obviously free of checks) where it decides anything.
- **Calls into musl** (malloc, pthread_create, snprintf) are Ada imports with
  `Convention => C`. **The syscall instruction** is
  `System.Machine_Code`.
- **Assembly files** (`.S`, `.asm`) are not C and stay where assembly is the
  right tool: entry points, boot code.
- **Each step:** an Ada unit with a hosted test against an independent
  reference, proofs where they apply, the guest test for the feature, and
  then the C deleted in the same change. No C kept alongside as a backup.

## Inventory (git-tracked, 2026-10-04)

About 19,500 lines in 140 files, excluding NetSurf, Mesa and other
third-party code.

| Area | Files | Plan | Owner |
| --- | --- | --- | --- |
| libc layer, `userspace/libc/overlay/src/cubit` | file.c 1569, net.c 1397, fd.c 935, syscall.c 854, process.c 320, debug.c, network/lookup_name.c, headers | Ada with C exports, file by file | mine (process.c: processes agent's area, user-assigned to me) |
| libc start code | crt/crt1.c 108 | Ada, plus a small `.S` for `_start` | mine |
| Homegrown freestanding libc, `userspace/c` | libc_stubs.c 995, string.c, stdio.c, cubit_mem.c, iconv.c, include/*.h, test_malloc.c, hello.c, spin.c | **Delete**: move its users onto musl | mine |
| Platform glue for ported programs | doomgeneric_cubit.c, doomgeneric_sound.c, cubit_video.c, cubit_input.c, cubit_streams.c, cubit_time.c, cubit_syscalls.c, sameboy/main.c, sameboy/compat.c | Ada exporting the hooks each program calls (DG_DrawFrame and so on) | mine |
| Compositor Vulkan glue, `userspace/lib/compositor` | vulkan_*.c/h, softpipe.c, compositor.h | Ada over Vulkan/Mesa C APIs | graphics agent; coordinate first |
| Test programs | libc-check, args-check, spawn-check, fs-bench (also runs on Linux), net-bench, sched-latency, filesystem-journal/native, grant-forward/native, ccl-input-budget/flood, ccl-file-dialog/workbench_events, desktop-protocol/input_c, runtime-string/check, servo/secondary_stack_host, compositor host tests | Ada or Rust. C-ABI checks import the libc functions they check | mine; compositor tests: graphics agent |
| Host tool | ccl/tools/ccl-ui-preview/native_window.c | Ada | mine |
| Manifest byte fixtures | tests/ccl-manifests/fixtures/*.c (30, data only, never compiled into a program) | Binary fixture files | mine |

## Order

1. **The libc layer and start code.** Every C program links them.
   - process.c first: small and self-contained, so it sets the pattern.
   - Then syscall.c, fd.c, file.c, net.c, crt1.c.
2. **`userspace/c`.** Move Doom, SameBoy and NetSurf's freestanding
   dependencies onto musl, port their platform glue to Ada, then delete the
   homegrown libc.
3. **Test programs and the host tool.**
4. **Manifest fixtures** become data files.
5. **Compositor Vulkan glue,** with the graphics agent.

## Progress

- 2026-10-04: path resolution moved to `CuBit.Path_Names` (SPARK, level 2);
  the C copy is deleted.
- 2026-10-04: **process functions.** `posix_spawn`, `posix_spawnp`, `wait4`,
  `waitpid`, `system`, `popen` and `cubit_debug_write` are now Ada:
  `userspace/libc/ada/cubit-libc_process.adb` (glue), with the
  child-table bookkeeping in `CuBit.Child_Table` (SPARK, level 2, nothing
  unproved). Launch blocks are encoded by the proved
  `CuBit.Launch_Arguments` builder instead of C, and exit events are
  checked with `CuBit.Child_Exits.Valid`.
  - Foundation units: `CuBit.Kernel_ABI` (system calls and the message
    layout; tests/libc-ada checks it against `CuBit.Messages`),
    `CuBit.Kernel_Calls` (the syscall instruction), and `CuBit.Libc_ABI` /
    `CuBit.Libc_Imports` (errno and flag values; musl's `__lock`,
    `__errno_location`, `mmap`).
  - `userspace/libc/libc-ada.adc` forbids elaboration code, the secondary
    stack, exception handlers, finalization, tasking and allocation.
    `replaced-by-ada.txt` removes musl's versions before it is built.
  - Deleted: `overlay/src/cubit/process.c`, `debug.c`, and the
    `posix_spawn.c`, `waitpid.c`, `system.c`, `wait4.c` and `popen.c`
    overlays.
  - Tested: tests/libc-ada (14 checks plus the ABI comparison), and the
    processes, libc and bench-fs guest tests.
- 2026-10-04: **system-call dispatcher, start code, descriptors, files.**
  - `syscall.c`, `crt1.c`, `fd.c` and `file.c` are deleted. In Ada now:
    `CuBit.Libc_System_Calls` (`__cubit_syscall`), `CuBit.Libc_Start` with a
    six-instruction `crt/crt1.S`, `CuBit.Libc_Descriptors` and
    `CuBit.Libc_Files`.
  - Proved SPARK units hold every decision and all arithmetic:
    - `Libc_Time`: saturating timespec and deadline arithmetic.
    - `Libc_Select`: fd_set to and from pollfds.
    - `Libc_Reports`: diagnostics formatting.
    - `Libc_Start_Layout`: launch-block string starts.
    - `Libc_Rings`: the pipe ring, FIFO order proved.
    - `Libc_Directory_Entries`: getdents records from untrusted pages.
    - `Libc_Descriptor_Rules`: open flags, overflow-safe lseek, fcntl.
    - `Libc_File_Cache`: the page cache's chains and clock.
    - `Libc_Dirty_Map`: buffered-write entries.
    - `Libc_Park_Table`: parked handles, names kept inline instead of
      malloc'd.
    - Totals: gnatprove level 2, 459 checks, nothing unproved. tests/libc-ada
      runs 178 hosted checks.
  - The filesystem queue uses the proved `CuBit.Submission_Queues` client
    the service's side already used, instead of the C's own index
    arithmetic.
  - ABI constants (231 musl macros, 23 kernel system calls, the
    filesystem protocol's numbers, struct stat's and struct pthread's
    layout) are checked against their sources by
    tests/libc-ada/check_constants.py.
  - Bugs the port removed:
    - Signed overflow in the timespec arithmetic, lseek and the
      most-negative `long` in diagnostics.
    - Unchecked negative `dup` targets.
    - getdents' unaligned casts of service-supplied pages.
    - `lseek` on pipes and sockets "succeeding" (now ESPIPE).
    - futex, nanosleep, ppoll and pselect accepting invalid timespecs
      (now EINVAL).
  - Big tables live in zero-filled `.bss` (`pragma
    Suppress_Initialization`), with all-zero memory as their empty state.
- 2026-10-04: **networking, streams, threads: libc has no CuBit-written C.**
  - `net.c`, the `lookup_name.c` and `pthread_getattr_np.c` overlays, the
    copy of `userspace/c/cubit_streams.c` and `cubit_fd.h` are deleted.
  - `CuBit.Libc_Net` (TCP over netstack channels and `__lookup_name`)
    uses the proved `Channel_Rings`, `Datagram_Rings` and
    `Net_Control_Queues` directly, instead of their C exports.
  - Its own decisions are SPARK:
    - `Libc_Net_Addresses`: scope decoding and prefix matching.
    - `Libc_Net_Targets`: `@net:` targets, refused rather than cut when too
      long.
    - `Libc_Net_Names`: placeholder names, kept inline instead of
      `realloc`/`strdup`, capacity 1024.
  - `CuBit.Libc_Streams` produces stdout and stderr, with entry placement
    in SPARK (`Libc_Stream_Rings`). `CuBit.Libc_Threads` provides
    `pthread_getattr_np`.
  - `Kernel_Calls.Submit` is the one place the endpoint-submit
    instruction sequence lives.
  - Totals: 608 checks proved at level 2, 178 hosted checks.
    check_constants.py covers 284 musl macros, 28 kernel system calls,
    struct stat, struct pthread, musl's attribute macros, the filesystem
    and stream protocol numbers, and OP_NET_SHUT.
- 2026-10-04: **DOOM and SameBoy run on the CuBit libc, frontends in Ada.**
  - `userspace/ports/doom` and `userspace/ports/sameboy`: the upstream engines
    are unmodified C built with `cubit-cc` against musl; each port's own
    layer is Ada, compiled without an Ada run-time library like the libc's.
    Manifests are typed CCL (`manifest.ccl`) instead of hand-written
    section bytes.
  - Deleted: `doomgeneric_cubit.c`, `doomgeneric_sound.c`, `Makefile.doomgeneric`,
    SameBoy's `main.c`, `compat.c` and stub headers, `Makefile.sameboy`,
    `cubit_audio.h` with its only implementation `CuBit.Audio_C`, and the
    unused `hello.c`, `spin.c` and `test_malloc.c`.
  - SPARK, gnatprove level 2, nothing unproved:
    - DOOM (44 checks): `Doom_Keys` (scan codes, bounded key queue),
      `Doom_Lumps` (DMX sound lumps from the WAD), `Doom_Mixer` (panning,
      resampling, saturating mix).
    - SameBoy (48 checks): `SameBoy_Keys`, `SameBoy_Frames` (3x scaling,
      cycle-to-time conversion), `SameBoy_Batches` (audio batch with
      partial writes).
  - Tests: tests/doom-port (33,430 hosted checks; key codes, screen size
    and the sound module records checked against doomgeneric's headers),
    tests/doom-audio (the stream's partial-write regression, now against
    the restructured engine), tests/sameboy-port (162 hosted checks; key
    order, models, sample layout and imported signatures checked against
    the core's headers).
  - Guest tests: `desktop-doom` passes, and the USB live boot test passes
    (`run-live.py --sameboy --quiet-xhci`). SameBoy reads its cartridge from
    `@cd:0`; DOOM reads `doom1.wad` from `@cd:0`, renders in its window and
    takes keyboard input. Both now print status lines to the debug console;
    the engines' own printf output is their stdout stream, so the tests wait
    for the frontends' lines. Both tests now pick Apps entries by label or
    position, matching the current menu order (CCL Console and Logs were
    added before DOOM).
  - Bugs the port removed:
    - DOOM's lump parser computed `data_len + 8` in 32 bits; a huge declared
      count wrapped and passed its bound check, reading past the lump.
    - DOOM's 16.16 sample position was 32 bits and wrapped after 65,536
      samples, restarting long sounds.
    - SameBoy released every held button whenever the desktop reported more
      pending events (it read the more-pending flag as a reset). It now
      resets on the resynchronization event.
  - Live images: the libc always names a volume, and the bootstrap archive
    and the optical disc had none. They are now `@boot` and `@cd:0` (user
    decision, 2026-10-04; `Volume_List`, proved, tests/volume-names); `@cd:0`
    is the disc's `apps/` tree, the part the filesystem service mounts.
    Names without a volume keep their old search order for the services
    that use them. DOOM looks for `doom1.wad` on NVMe, ATA, `@cd:0`, then
    `@boot`; SameBoy reads `@nvme:0/sameboy/`, else `@cd:0/sameboy/`.
  - Still on `userspace/c` then: the NetSurf engine (since retired), and
    `crt0.S`/`link.ld`, which every Ada service links.
- 2026-10-04: **manifest fixtures are data.** The 30 C byte-array fixtures in
  tests/ccl-manifests are now `fixtures/<name>/<section>.bin`, extracted once
  from the old C; test-migrations.py and test-manifests.py read them. Results
  are unchanged: test-manifests passes; test-migrations still stops at the
  files app, whose manifest gained a capability after its fixture was
  recorded (it failed the same way before the conversion).
- 2026-10-04: **NetSurf retired; the homegrown libc is gone; C test
  programs stay** (user decisions).
  - NetSurf was the last user of `userspace/c`'s freestanding libc; Servo
    (Penny) is the browser. Deleted: `userspace/apps/netsurf`, its kernel
    targets and disk-image entries, the `netsurf-https` headless case,
    tests/netsurf-frame and tests/compositor/test-browser-invalidation.py,
    and its Apps menu entries (`system.ccl`, `Desktop_Launch.Defaults`).
    `netsurf.app` no longer gets the desktop's browser network approval
    (`CuBit.Launch_Policy`), so a program installed under that name gets
    nothing. The git-ignored local port (`userspace/c/netsurf`,
    `netsurf_deps`, `netsurf_src`) is left on disk for its owner to delete.
  - Deleted from `userspace/c`: `libc_stubs.c`, `stdio.c`, `string.c`,
    `cubit_mem.c`, `iconv.c`, `cubit_input.c`, `cubit_video.c`,
    `cubit_time.c`, `cubit_syscalls.c`, `cubit_streams.c`, `include/` and
    `cubit_tls.h`. What remains: `crt0.S` and `link.ld` (every Ada service's
    start code and link script) and the interface headers C programs and
    tests include (`cubit.h`, now without declarations of the deleted
    functions, `cubit_desktop.h`, `cubit_fs_queue.h`, `cubit_net_channel.h`).
  - The C test programs (libc-check, args-check, spawn-check, fs-bench,
    net-bench, sched-latency and the rest) stay C: they are clients checking
    what C programs see, not CuBit's implementation.
  - Tests: network-authority (launch policy, now refusing `netsurf.app`) and
    desktop-launch pass. desktop-protocol's hosted test stops at an
    attachment-decoding assertion in code this change does not touch.
- Unavoidable C-syntax shims: `overlay/arch/x86_64/syscall_arch.h` (musl
  includes it; it only forwards to `__cubit_syscall`), and the public
  headers that declare the Ada functions for C programs.
