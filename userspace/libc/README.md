# CuBit libc

A C library for CuBit: **musl 1.2.6** (MIT; `build/musl-src/COPYRIGHT`)
with its Linux system-call layer replaced by an in-process implementation
over CuBit syscalls and services (`overlay/src/cubit/syscall.c`). Plus a C++
runtime: nixpkgs' musl cross gcc's `libstdc++`, linked against this libc.
Part of the Servo port (`docs/servo-port.md`, decision B: a WASI-style
libc). Native checks: headless case `libc` (`libc-check` in C,
`cxx-check` in C++).

## Build and use (Nix shell)

    make -C kernel libc                  # -> userspace/libc/build/sysroot
    userspace/libc/cubit-cc  -O2 -pthread -o prog.app prog.c   --manifest manifest.o
    userspace/libc/cubit-c++ -O2 -pthread -o prog.app prog.cpp --manifest manifest.o

The wrappers link statically with the CuBit start code, gcc's static-link
frame (`crti`, `crtbeginT` … `crtend`, `crtn`, from the musl cross
toolchain), the libc, libgcc (and libstdc++), using `link.ld`: ELF headers loaded (the start code finds
the TLS template through them), code R+X, data R+W on separate pages, and a
stack-size contract (`CUBIT_STACK_SIZE`, default 1 MiB). Nix's gcc hardening
is disabled: the start code runs before the thread pointer, and so the
stack canary at `%fs:0x28`, exists.

## What maps to what

| Linux call | CuBit |
| --- | --- |
| `clone` (threads) | THREAD_CREATE: entry, stack, TLS as FS base, the child-clear-tid word as the exit word (`overlay/src/thread/x86_64/clone.s`) |
| `exit` / `exit_group` | THREAD_EXIT / EXIT |
| `futex` wait, wake | FUTEX_WAIT / FUTEX_WAKE; requeue wakes all waiters instead |
| thread pointer | `wrfsbase` (FSGSBASE); the kernel keeps FS per thread |
| `brk`, anonymous `mmap` | SBRK (page-aligned heap growth, zero-filled) |
| `munmap`, `mprotect`, `madvise` | accepted, no effect yet (no region API): memory is not returned |
| `clock_gettime`, `nanosleep` | the kernel's millisecond clock (1 ms resolution; `CLOCK_REALTIME` is time since boot until the clock service is wired in) |
| descriptors 1, 2 | the program's `stdout`/`stderr` CuBit streams (typed text lines, created on first write; subscribers see them if the manifest declares them). Not terminals: `ioctl` is `-ENOTTY` |
| descriptor 0 | none (no input stream is granted) |
| `open`, `stat`, `read`, `pread`, `lseek`, `getdents64`, `close` | files and directories through filesystem.svc (`overlay/src/cubit/file.c`), read-only; the service checks each path against the program's `filesystem-scope`s. `@vol:N/…` paths are CuBit names; POSIX absolute paths name the system volume (`/fonts/a.ttf` is `@nvme:0/fonts/a.ttf`); no working directory |
| `mmap` of a file | a private copy of its bytes |
| `pipe`, `pipe2`, `socketpair(AF_UNIX, SOCK_STREAM)` | in-process rings (both ends in one address space: thread wakeups such as mio's waker, tokio's signal self-pipe). Across processes these should become CuBit IPC objects (an endpoint capability or stream granted at launch, never a filesystem path); not implemented yet |
| `poll` | readiness of the descriptors' CuBit objects (streams writable, files readable) |
| `getrandom` | RDRAND (not yet the entropy service) |
| signals | masks and handlers accepted; none are delivered; `kill`/`raise` exit |
| `sched_getaffinity` | four CPUs |
| `prctl` thread names | kept per thread in the process (the kernel has no thread names yet) |
| sockets, processes | not yet |

Unknown calls return `-ENOSYS` and are reported once on the console
(`cubit-libc: unimplemented system call N`).

Diagnostics that must reach the kernel console (test markers) use
`cubit_debug_write` from `<cubit/debug.h>`, explicitly; nothing is mirrored
there implicitly.

The start code (`crt/crt1.c`) builds the argument block musl expects, since
CuBit's loader passes none: `argv = { "cubit-program" }`, an empty
environment, and auxiliary entries for the program headers, page size and
16 random bytes.

## Licenses

musl: MIT (portions BSD-2-Clause and public domain), see its COPYRIGHT.
libstdc++/libgcc: GPLv3 with the GCC Runtime Library Exception. The CuBit
overlay and wrappers: GPLv3, as CuBit. All GPLv3-compatible.

## Not yet

Writing files, sockets over netstack, a precise and wall clock, the
entropy service, `munmap`/`mprotect` (needs the address-space region API),
real `FUTEX_REQUEUE`, signals, `fork`/`exec` (by design: procmgr starts
programs).
