# Rust `std` for CuBit (prototype)

Step 2 of the Servo plan (`docs/rust-std.md`). A Rust program using `std` —
threads, `Mutex`, `Condvar`, `RwLock`, channels, `thread_local!`,
`HashMap`, `Instant`, `thread::sleep`, `println!` — builds for
`x86_64-unknown-cubit` and runs natively on CuBit (headless case `rust-std`).

## How it works

- **Target:** `x86_64-unknown-cubit.json`, `target_os = "cubit"`, SSE
  enabled (the kernel saves FPU/SSE state per thread), static, no PIE,
  `panic = abort`, key-based thread-locals (no ELF TLS yet). Linked with
  `userspace/c/link.ld`, a 1 MiB stack contract and a CCL manifest, like
  every native program.
- **`std` itself is unmodified except for its platform switch.**
  `prepare-std.py` copies the pinned toolchain's `rust-src` to
  `userspace/rust/build/std-src` and makes `target_os = "cubit"` take
  Motor OS's arms (31 files, a mechanical cfg rewrite), plus a
  `check-cfg` entry and a `[patch]` of `moto-rt`.
- **Motor OS's `std` layer calls a thin runtime table (`moto-rt`).** Motor
  fills it from a VDSO. `moto-rt-cubit/` is a fork of `moto-rt` 0.16
  (MIT/Apache) whose table lives in the program and is filled before `main`
  by `src/cubit.rs` from CuBit syscalls:

  | Table area | CuBit |
  | --- | --- |
  | memory | `dlmalloc` over `SBRK`, behind a futex lock |
  | threads | `THREAD_CREATE` (heap stack, a TLS block as FS base), join on the exit word, `THREAD_EXIT` after thread-local destructors |
  | futexes | `FUTEX_WAIT`/`FUTEX_WAKE` (std's own futex `Mutex`/`Condvar`/`RwLock`/`Once`/parking) |
  | thread-locals | 256 keys in a per-thread block at `fs:0` |
  | time | the kernel's millisecond clock (coarse: `Instant` has 1 ms resolution) |
  | sleep, yield | timed futex waits |
  | stdout, stderr | the debug output (`SYSCALL_WRITE`) |
  | randomness | RDRAND, else a TSC-seeded mixer (for `HashMap`; not cryptographic) |
  | exit | `SYSCALL_EXIT` |
  | files, sockets, processes, polling, args, environment | not implemented yet (errors, empty args and environment) |

- **Entry:** `_start` aligns the stack and calls std's `motor_start`, which
  fills the table, sets the main thread's FS base and calls `main`.

**Building any program:** run cargo through `cargo-cubit.sh`, which
prepares the patched sources once per input hash and exports
`CUBIT_RUST_TARGET`:

    bash userspace/rust/std/cargo-cubit.sh cargo build --release \
        --target "$CUBIT_RUST_TARGET" -Zjson-target-spec -Zbuild-std=std,panic_abort

The test program: `make -C kernel rust-std-hello` (`build-hello.sh`).

**Optional hooks** (weak symbols, used when a program links them, as the
Turso probe and Config storage worker do in
`userspace/services/config-storage/std_hooks.rs`):
`cubit_std_allocate`/`cubit_std_release` (std's `System` allocator),
`cubit_std_time` (wall clock; without it `SystemTime::now` fails loudly)
and `cubit_std_random` (secure randomness; without it RDRAND/TSC, not
cryptographic).

**Libraries in other programs:** the runtime installs itself on the first
std call, and `_start`/`main` are weak defaults, so a Rust library linked
into an Ada program keeps the Ada entry point. The runtime sets a thread's
FS base only if it is zero.

## Status and limits

- Native: `rust-std` headless case PASS (4 CPUs). Turso builds and links
  with this std; its native run is pending its build switch.
- Borrowing Motor's runtime ABI keeps the `std` patch mechanical. The
  decision to keep it, or to write a CuBit platform layer inside `std`
  instead, is open (docs/rust-std.md).
- Not yet: a precise clock (TSC), wall-clock time, files and networking over
  CuBit services, args/environment from procmgr, ELF TLS, stack guards,
  unwinding.
