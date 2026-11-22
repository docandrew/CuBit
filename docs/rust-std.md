# Rust `std` for CuBit

Status: implemented as a prototype; one merged std for every CuBit Rust
program (see "Status" at the end). Date: 2026-09-25.

This is step 2 of the plan toward Servo (`docs/threads.md`): kernel threads
and futexes are step 1 and now exist.

## Why `std`

Servo, SpiderMonkey's Rust parts and nearly every crate they depend on use
`std`: threads, `Mutex`/`Condvar`, thread-locals, `Instant`/`SystemTime`,
files (fonts, resources, caches), environment and, through its dependencies,
sockets. The existing `userspace/rust` bootstrap is `no_std` (`core` +
`alloc` on `x86_64-unknown-none`), which is enough for fonts and probes but
not for Servo.

## Two ways to get there

1. **A CuBit platform in `std` itself** (`target_os = "cubit"`, a
   `library/std/src/sys/pal/cubit` implementing the platform traits over
   CuBit syscalls and services). This is how Hermit, Xous, UEFI, SOLID and
   others are supported. Xous showed it can be shipped before upstreaming:
   build `std` for a custom target JSON with `RUSTC_BOOTSTRAP=1` and
   `-Zbuild-std`, and install the `.rlib`s in a sysroot.
2. **A POSIX C library on CuBit** (a musl or relibc port) and Rust's existing
   Unix `std` on top. Redox took this route with relibc. It also serves the
   C and C++ code Servo pulls in (SpiderMonkey, HarfBuzz, FreeType-style
   dependencies), but it means emulating a Unix process model (fds, `fork`
   semantics, signals, `mmap`, `poll`) that CuBit deliberately does not have.

**Recommendation: (1) for Rust, with a small C runtime shim for the C/C++
dependencies, grown from `userspace/c` as needed.** The `std` platform layer
maps cleanly onto what CuBit has: threads and futexes (step 1), `sbrk`, the
time syscalls, stdout, and IPC services for files and sockets. It keeps
capabilities explicit instead of hiding them behind ambient Unix paths.
SpiderMonkey needs its own platform layer either way (it has one per OS).

## First milestone

A Rust program using `std::thread`, `Mutex`, `Condvar`, `thread_local!`,
`Instant`, `println!` and `Vec`/`String` runs natively on CuBit, with the
futex tests of step 1 as its foundation. Files and sockets return
`Unsupported` at first.

| `std` area | CuBit mapping |
| --- | --- |
| alloc | the existing SPARK-core allocator (`userspace/allocator`) over `sbrk`; needs a thread-safe front (a futex mutex, then per-thread caches) |
| thread | `THREAD_CREATE` with a heap stack, `exit_word` join, `THREAD_EXIT` |
| thread-local | FS-base TLS: the runtime allocates each thread's TLS block (ELF TLS layout, variant II) and passes it as `fs_base` |
| locks | `std`'s futex-based `Mutex`/`Condvar`/`RwLock`/`Once` over `FUTEX_WAIT`/`FUTEX_WAKE` (the protocol `tests/futex-queues/explore.py` checks) |
| time | monotonic ms today; `Instant` wants a finer clock (TSC-based, calibrated by the kernel) |
| stdio | `SYSCALL_WRITE` for stdout/stderr; stdin unsupported |
| env, args | from the launch message (procmgr), empty at first |
| fs | the filesystem service over IPC, later (capability-scoped, no ambient root) |
| net | netstack over IPC, later; Servo's networking may go through the embedder instead |
| process | unsupported (spawning is procmgr's, by capability) |

## Open questions for review

- Toolchain: `-Zbuild-std` needs `rust-src` of a pinned toolchain in the
  flake (nightly, or stable with `RUSTC_BOOTSTRAP`).
- Floating point and SIMD: the bootstrap target is soft-float, no SIMD.
  Servo needs the native FPU/SSE ABI; the kernel already saves FPU state per
  thread.
- Where the `std` patch lives: a patch series applied to the pinned
  `rust-src` in the Nix build, upstreamed later.

## Status (2026-09-25)

One std for CuBit, in `userspace/rust/std` (details in its README):

- `target_os = "cubit"` takes Motor OS's std arms (a mechanical cfg rewrite
  of 31 files); Motor's thin runtime table (`moto-rt`, forked as
  `moto-rt-cubit`) is filled from CuBit syscalls.
- Runs natively: headless case `rust-std` (threads, futex locks, condition
  variables, RwLock, channels, `thread_local!`, `HashMap`, sleep, `Instant`).
- Merges the Turso bring-up port: its hooks (`cubit_std_allocate`/`release`,
  `cubit_std_time`, `cubit_std_random`) are used when a program links them;
  the runtime installs lazily, so std works in a Rust library linked into an
  Ada program. Turso (`--features turso`) builds and links with it; its
  native run and the switch of its build are coordinated with the
  filesystem agent.
- The choice between keeping Motor's runtime ABI and writing a CuBit
  platform layer inside std stays open; the Motor route kept the std patch
  mechanical.
- Not yet: precise monotonic time (1 ms today), args/environment, files and
  sockets over CuBit services, ELF TLS, stack guards, unwinding.

## Status (2026-09-25, afternoon): the Unix-family target

A second std, for the native `x86_64-unknown-cubit` Unix-family target
(`userspace/rust/std/unix/`), now runs natively: the same `rust-std` checks
pass with Rust's standard Unix std over the CuBit libc (userspace/libc),
including native ELF thread-locals.

- `target_os = "cubit"`, `target_env = "musl"`, `target_family = "unix"`.
  CuBit's libc presents the Linux/musl C ABI, so std and the `libc` crate
  take their Linux arms (`prepare-std-unix.py`, a mechanical cfg rewrite
  of 70 std files and 11 libc files). These are ABI facts. Linux-specific
  *behavior* std relies on reaches CuBit's syscall layer, which implements
  it deliberately or returns ENOSYS and reports the call on the console.
  The first such report (`poll` on the standard descriptors at startup)
  was found and implemented this way.
- Build: `bash userspace/rust/std/unix/cargo-cubit-unix.sh cargo build
  -Zbuild-std=std,panic_abort -Zjson-target-spec --target "$CUBIT_RUST_TARGET"`;
  linking goes through `userspace/libc/cubit-cc`.
- The Motor-based std (above) stays until the Unix one reaches parity
  (the Turso hooks: allocator, wall clock, secure randomness move into the
  libc) and the switch is coordinated with the Turso work.
- Crate tail next, only what Servo pulls in: getrandom, mio (its `poll`
  backend over CuBit rather than epoll emulation), socket2, rustix.
