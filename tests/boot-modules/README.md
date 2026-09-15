# Sealed boot modules and retained payloads

The production `Boot_Modules` core is pure SPARK. Raw loader pointers remain
inside the Multiboot adapter; `Modules.setup` consumes only catalog values.

```sh
nix develop -c bash tests/boot-modules/prove.sh
nix develop -c bash -lc 'cd kernel && alr exec -- gprbuild -p -P ../tests/boot-modules/modules_tests.gpr && ../tests/boot-modules/build/main'
nix develop -c make -C kernel cubit_kernel
nix develop -c python3 tests/boot-modules/check-codegen.py
nix develop -c python3 tests/boot-modules/native-rejection.py
```

## Admission and lifetime

1. Decode the firmware map, then validate module descriptor/name sources against
   the bootstrap window and firmware RAM coverage, excluding the kernel/stack
   reservation. Copy descriptors before following any of their name pointers.
2. Read names byte-by-byte through the terminator, validating each byte before
   access. Retain a bounded copy, not a borrowed pointer or a fixed-width read
   past the terminator. Limits are 64 modules and 64 name bytes plus NUL; excess
   input fails explicitly. These are configured implementation budgets, not
   Multiboot protocol limits.
3. Reject empty/reversed/out-of-window/unaligned payloads, reserved holes,
   overlapping payload pages and duplicate names. Payloads must start above
   the kernel/bootstrap-stack reservation. RAM coverage accepts adjoining or
   overlapping usable regions in any order; reserved regions win.
4. Reject payload-page overlap with loader header/map/descriptors/names or the
   framebuffer. Complete all validation before modifying any padding bytes.
5. Clear final-page slack without changing payload bytes. Publish the completed
   snapshot only after the reservation map and sanitization are ready. No module
   entries are externally visible during construction or on a fatal rejection.
6. Both allocators receive the existing reserved boot prefix, now explicitly
   covering every accepted payload's rounded page end. This retains payloads
   for the entire boot, including CPIO indexes and userspace mappings. There is
   deliberately no reclaim/release operation in this catalog.

Names and descriptors may cease to be usable without affecting later module
selection: `Modules.setup` never reads those raw buffers again. Transport
metadata remains conservatively covered by the boot reservation too; this change
does not introduce reclamation. `init.img` selection is exact, not an eight-byte
prefix match, and duplicate names fail instead of silently picking the last.
Initrd startup is one-shot. Failed page mapping stops boot before granting or
resuming the device manager; a CPIO entry shorter than an ELF header is rejected
before constructing that header overlay.

The userspace initrd view remains read-only, and the resident-initrd range query
still recognizes only payload bytes, not page padding. LiveCD/ISO service loading
and the CPIO format are unchanged. This is not reference-counted reclamation or
an ownership transfer of payload pages to userspace.

## Evidence

GNATprove discharges **62 checks**, none unproved or justified, including:

- Valid page-aligned payload spans and pairwise-disjoint page coverage.
- Unchanged catalog state after every failed append.
- No append to a sealed catalog; repeated sealing preserves a sealed value.
- Every published payload page end is covered by `Reserved_End`.
- Padding clearing preserves the payload prefix and zeros the suffix.
- Bounded RAM-coverage traversal and progress.

`Consistent` is Ghost. No `Assume` or `SPARK_Mode => Off` exists in this core.
The functional meaning of RAM-map coverage is regression-tested with an
independent byte ledger, not a separate universally quantified coverage theorem.

Hosted tests pass **1,000,460 catalog/RAM/padding cases**, including failure
atomicity, capacity, adjacent payload pages, sealed-state behavior, input-copy
independence and overlapping/adjacent/reordered RAM regions with reserved holes.
The real-GRUB rejection fixture boots isolated temporary ISOs with duplicate,
overlong and over-capacity module declarations and checks rejection before
allocator startup. It preserves logs under its printed `/tmp` directory.

Native four-vCPU security rollback/PID-reuse and multi-app desktop/DOOM
regressions pass. The rebuilt `kernel/cubit_laptop_usb.iso` also passes
`tests/usb-optical/run-live.py --cpus 4 --timeout 180`: the USB-only optical
boot loads its 7-file initrd, then launches apps from the CD payload, without
ATA/NVMe fallback. The image build audits 7 bootstrap files and 18 CD payload
files. No application image was moved back into the initrd by this change.

The optimized native kernel passes its 2 KiB per-function stack check. Catalog
and capture workspaces live in BSS. Codegen allows only the ordinary `memcmp`
and `memset` dependencies in the core, with no runtime Ghost/postcondition code,
and checks that the module consumer contains no raw descriptor/name traversal.
The final adapter uses 640 bytes of per-function stack, append uses 288 bytes,
and padding clearing uses 8 bytes. The sealed catalog occupies 6,152 bytes of
BSS and the transient descriptor/name capture workspace 6,656 bytes. These
are per-function/codegen observations, not a whole-call-chain stack proof or
an end-to-end latency benchmark.

## Remaining trusted boundaries

This closes the software snapshot/lifetime gap for the current permanently
resident initrd model, not the entire physical-memory proof. Actual RAM backing,
firmware truthfulness and stability during capture, raw address overlays,
hardware/DMA writes, effective page-table permissions and correct application of
reservations by machine-level adapters remain assumptions. Numeric firmware
coverage is not independent hardware discovery. Container/ELF parsing and process
rollback retain their separate proof/test boundaries.

The malformed-module fixture also exposed an existing panic diagnostic weakness:
the former `Last_Chance_Handler.printCallStack` assumed frame-pointer chains
despite optimized code not guaranteeing them. That walk is now removed. The
handler disables local interrupts before output, preserves the rejection reason,
reports the absence of a stack trace, and halts directly instead of raising another
software interrupt/exception through `x86.panic`.

`native-rejection.py` additionally checks a single panic banner and, through QMP,
observes CPU 0 twice (half a second apart) with HLT set and IF clear. Serial output
must remain unchanged and QEMU must remain alive with `-no-reboot`. Register dumps
are retained beside each fixture's serial log. All three cases pass with the
optimized kernel. This tests a stable local fatal stop, not a proved unwinder,
SMP-wide shutdown, or safety under arbitrary corruption of the stack, runtime
message pointer, or diagnostic output machinery.
