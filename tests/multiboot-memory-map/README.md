# Multiboot-v1 memory-map admission

This target exercises the production `Multiboot_Memory_Map` pure SPARK decoder,
not a duplicate test parser. Run from the repository root, after entering Nix:

```sh
nix develop -c bash tests/multiboot-memory-map/prove.sh
nix develop -c bash -lc 'cd kernel && alr exec -- gprbuild -p -P ../tests/multiboot-memory-map/map_tests.gpr && ../tests/multiboot-memory-map/build/main'
nix develop -c make -C kernel cubit_kernel
nix develop -c python3 tests/multiboot-memory-map/check-codegen.py
```

## Evidence

GNATprove discharges **72 checks**, none unproved or justified: bounded byte
reads, safe record advancement with strict progress, valid nonempty address
intervals, output bounds and zero published entries on failure. Exact decoded
byte values and tag mapping are tested, not claimed as separate functional
theorems. No `Assume` or `SPARK_Mode => Off` is used in this decoder.

The checked hosted executable passes **75,085 cases**, including every prefix
of a standard record, extended records, unknown tags, zero lengths, overflowing
physical spans, UINT32_MAX record size, arbitrary/high array lower bounds,
exact/full/empty output capacity, and randomized raw and structured inputs.
The byte oracle uses radix arithmetic independently of the decoder's bit shifts.

The optimized integrated kernel also passed the four-vCPU KVM
`capability-security` fixture (eight partial-load rollbacks and successful PID
reuse) and `desktop-doom` with `CUBIT_DOOM_MULTIAPP=1` (game pixels and responsive
Apps menu). These are native smoke regressions, not malformed-loader-input
injection tests or proof of the whole boot path. Existing firmware admission,
boot bitmap, buddy metadata and list-splice codegen checks continue to pass.

The old adapter counted fixed 24-byte entries but traversed variable-sized
entries, and read a complete header before validating the remaining extent.
The replacement validates the size prefix before reading any payload. Extended
tails are skipped; trailing partial records fail the entire publication.
Multiboot specifies that the size excludes its own four bytes and may exceed
the standard payload size. See the [GNU Multiboot specification](https://www.gnu.org/software/grub/manual/multiboot/multiboot.html).

## Kernel integration and limits

`Count` is the publication boundary, not a promise to scrub partially filled
private output after failure. The kernel uses the output only after success.
Its configured workspace holds 1,024 records plus two synthesized reservations;
excess records fail explicitly rather than being silently truncated. This is a
kernel boot-workspace budget, not a limit inherent in the decoder or wire format.

Decoded, translated and normalized maps use static boot workspace. Normalization
writes a caller-provided array and runs once for both allocators. Neither the
firmware byte length nor record count sizes a kernel stack allocation. Production
stays optimized with assertions disabled; hosted tests enable runtime checks.
The codegen check checks per-function stack reports, not complete call-chain
stack bounds, and allows the decoder's existing `memcpy` dependency.

The raw-address adapter remains outside the focused SPARK proof. It validates
the map and module-descriptor/name extents against the first-GiB bootstrap
mapping before overlaying them, uses the correct 16-byte module stride, and
retains the map/module metadata in the reserved boot prefix. Being inside that
mapping is not proof that an address actually names readable RAM.

Initial numeric pointer admission and optional-field sanitization are now
handled by the [entry boundary](../multiboot-entry/README.md).
Module snapshotting and permanent payload retention are now handled by the
[module boundary](../boot-modules/README.md).
Still trusted/open: the complete assembly path, firmware truthfulness and source stability, physical
backing, complete module payload/metadata lifetime and overlap validation,
mapping/cache-mode consistency, and DMA mutation. A proved array decoder does
not discharge those machine-level obligations.
