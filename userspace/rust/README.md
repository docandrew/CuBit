# Native Rust on CuBit: first milestone

This is native userspace, not a Linux executable running inside CuBit.
All commands below run from the repository root through the Nix environment.

```sh
nix develop -c make -C kernel rust-probe
nix develop -c bash tests/rust-native/run.sh
```

The second command needs the existing NVMe development image and KVM. Use
`QEMU_ACCEL=tcg nix develop -c bash tests/rust-native/run.sh` for emulation,
although slow hosts may need a longer timeout in the headless runner.

## Build and ABI

- `flake.lock` pins rust-overlay and its stable Rust toolchain, including
  prebuilt `core`/`alloc` for `x86_64-unknown-none`. Cargo dependencies are locked;
  the bootstrap has no external crates and builds offline.
- This is a freestanding bootstrap target, not yet a named `*-cubit` target.
  It uses the small static code model, no red zone, and the target's default
  no-SIMD/soft-float configuration. Font rendering will require evaluation of
  floating-point performance and the native FPU ABI before choosing a target.
- Rust emits a static library. GNU `ld` links its assembly entry point with
  CCL-generated manifest sections using the existing userspace ELF layout.
  Neither libc nor the C or GNAT startup runtime is linked.
- The loader initializes BSS and supplies the requested 1 MiB stack through
  `PT_GNU_STACK`. The entry point aligns it before calling Rust.
- `ccl-manifest --rust-output bindings.rs` emits slot constants from the same
  validated declarations that produce `.cubit.caps`. Names are not duplicated
  in handwritten Rust magic slot constants.

## Initial runtime surface

The `cubit` crate supplies an exactly laid-out 48-byte IPC message, endpoint-slot
references, checked synchronous calls, and process exit. Unsafe code is confined
to the machine boundary; pointers passed to the kernel refer to initialized,
aligned Rust-owned buffers that remain alive across the call.

An `EndpointSlot` is a reference, not an authority minted by Rust. Constructing
a slot number cannot bypass the kernel's capability checks. It is not yet an
ownership/delegation API or a typed service binding. `Unavailable` reflects the
current kernel's null-tag result, which does not distinguish missing authority,
stale authority, and an unavailable peer. Never present that alone as a precise
policy explanation.

Threads and futex locks exist (`cubit::thread`, behind the `alloc` feature,
and `cubit::sync`: `Mutex`, `Condvar`, raw futexes; `docs/threads.md`), and
the probe checks them natively (`RUST-THREADS: PASS`). The lock logic is also
tested on host threads, where futex calls degrade to yielding. There is no
`std`, thread-local storage, async completion API, grants,
typed log publisher, or widget binding yet. `alloc` now uses the opt-in bounded
SPARK-core allocator in the probe. Panics abort the probe
after attempting a test-service failure report; they do not unwind into Ada.
No SPARK or other formal proof is claimed for this Rust boundary.

The [portable SPARK allocator](../allocator/README.md) supplies slab and extent
metadata through a small Ada ABI, linked statically with the Rust GlobalAlloc
adapter. Its small-object arena is backed incrementally in independent 1 MiB
chunks through `sbrk`; the large-object arena is acquired separately on first
use. Heap payload no longer occupies BSS. Neither this
boundary nor its spinlock/payload copying is covered by the metadata proofs.
The allocator/probe still have no external Cargo dependencies or C allocator.
The shared font library now uses pinned pure-Rust rasterizer/parser dependencies;
see [runtime typography](fonts/README.md).

## Native test

Both probes contain identical loaded code and data. One requests Clock and the
test endpoint; the other requests only the test endpoint. Both send a full-width
four-word `Hello from Rust!` echo through IPC. Each runs Vec/String/Box growth,
over-alignment, exhaustion, zeroing and reuse checks before the observer accepts
its outcome. One then reads the real Clock
service twice, while the other encounters the absent Clock authority. The
observer holds the first report's reply capability until both processes finish
their checks, preventing PID reuse from confusing the fixture. It then releases
both replies so the probes can exit. This test service is not normal logging.

After that barrier, the Clock-authorized probe requests 256 MiB three times in
the dedicated 128 MiB guest. Each must fail without killing it or moving its
break, preserve a live allocation, and allow a subsequent zero-filled 4 KiB
growth. This exercises real partial-growth rollback and reuse of unmapped
addresses. Do not run this resource-pressure fixture as a normal desktop app.
The ELF checker requires less than 1 MiB of zero-filled storage so payload
backing cannot silently return to BSS.

The native Rust API does not expose `WRITE`. Only the existing Ada test observer
uses its current diagnostic path to make test results visible in the host log.

## Next steps

1. Incremental large-object backing and additional arenas/reclamation.
   Small-object backing is incremental; native threading remains separate.
2. Async IPC/completions, ownership-aware grant wrappers, and typed log output.
3. Extend the integrated TrueType renderer with per-output DPI and text shaping.
   The shared native/hosted path uses immutable bounded glyph caches.
4. A dedicated CuBit target and broader SDK/`std` support as the native APIs mature.

### Legacy diagnostic syscall audit (follow-up, not implemented)

`SYSCALL_WRITE` is not dead: `CuBit.Messages.debugPrint`, C `cubit_write`, and
the console branch of `fwrite` still use it. It handles STDOUT only and feeds
kernel TextIO (serial and/or boot video), not the filesystem or typed logstore.
Its direct userspace-buffer reads also need hardening.

The discussed replacement is a narrowly named `DEBUG_OUTPUT`, without a file
descriptor argument, enabled by an explicit boot option and diagnostic authority.
It needs bounded, validated user copying and resource limits. Normal application
logging should move to authorized typed streams; early boot diagnostics need a
separate safe fallback. Do not delete WRITE or just rename it while leaving its
callers, unchecked pointer handling, and boot-test observability unaddressed.
