# Firmware frame admission

`Firmware_Frames` is the shared production numeric policy used by both boot
allocators, through `MemoryAreas.Allocation_Map`. Raw `MemoryArea` byte endpoints
are inclusive. Normalized page spans are half-open. Usable entries contribute
only complete pages; any non-usable entry excludes every page touched by even
one of its bytes, regardless of entry ordering.

The earliest usable entry containing a whole page owns that page. This prevents
duplicate usable entries from admitting the same RAM twice. Adjacent partial
usable entries are not stitched together: requiring one whole-page covering
entry is intentionally conservative.

`Classify` returns `Admit`, `Reject` or `Split`. Whole conflicts can be skipped
cheaply; boundary conflicts are split to smaller aligned blocks. A singleton
can never require splitting. Buddy setup tiles the normalized usable spans,
replacing the old alignment helpers that discarded already-aligned boundaries
and trailing capacity. Boot-reserved frames still use the previously proved
inclusive `Buddy_Boot_Admission.Source_Of` rule before any bitmap query.

## Proof and tests

The focused pure core passes **51 checks, none unproved or justified**:

- Whole-page rounding stays within inclusive usable byte endpoints.
- Touched-page rounding covers inclusive excluded byte endpoints.
- Selected blocks are aligned, bounded by the candidate span and maximum order.
- Admitted blocks intersect neither reserved pages nor earlier usable owners.
- Rejected blocks have a whole-span conflicting entry as a witness.
- Partial conflicts require a span larger than one page.
- Two different usable entries cannot both admit the same page.
- Focused arithmetic, indexing and termination obligations are discharged.

No `Assume`, suppressed checks or `SPARK_Mode => Off` in the numeric core. The
uniqueness lemma is Ghost. An initially overbroad three-valued local scan state
was replaced by a Boolean recording partial conflict; `Reject` is only an early
return. No runtime defensive guards were added to make that proof pass.

Hosted tests independently evaluate raw byte intervals, passing **57,346 byte
ranges, 2,008 maps and 401,354 candidate blocks**. They check exact rounding,
single-byte exclusions across page boundaries, duplicate usable entries,
reserved entries before/after usable entries, unique owner selection, all
candidate spans in small maps, and full-width arithmetic edges. A setup traversal
harness tiles each map against a per-frame ownership oracle, checking no lost or
duplicate admissions. This harness exercises the same policy, but is not a proof
of the kernel's physical overlay/mapping adapter.

Run from the repository root using Nix:

```sh
nix develop -c bash tests/firmware-frames/prove.sh
nix develop -c bash -lc 'cd kernel && alr exec -- gprbuild -p -P ../tests/firmware-frames/firmware_tests.gpr && ../tests/firmware-frames/build/main'
nix develop -c make -C kernel cubit_kernel
nix develop -c python3 tests/firmware-frames/check-codegen.py
nix develop -c python3 tests/buddy-boot-admission/check-codegen.py
nix develop -c python3 tests/buddy-metadata/check-codegen.py
nix develop -c python3 tests/intrusive-list-splices/check-codegen.py
nix develop -c tests/headless/run.sh --test capability-security --disk kernel/capability_security_disk.img --accel kvm --cpus 4 --timeout 90 --serial /tmp/cubit-firmware-native-serial.log --keep-logs
nix develop -c env CUBIT_DOOM_MULTIAPP=1 tests/headless/run.sh --test desktop-doom --accel kvm --cpus 4 --timeout 90 --serial /tmp/cubit-firmware-desktop-serial.log --keep-logs
```

Run the native fixtures sequentially; they stage shared boot inputs.
Hosted assertions are enabled. Kernel builds remain optimized `-O2 -gnatp` with
no `-gnata`. The new policy runs during boot, not ordinary alloc/free. Compiler
geometry checks tie its page size and byte range to the kernel layout.

The final optimized kernel passed both four-vCPU KVM fixtures: eight partial-load
rollbacks and successful PID reuse in the security test; DOOM game pixels and
responsive Apps-menu input in the multi-app desktop test. The earlier 27-check
boot reservation/handoff proof still passes. Production codegen checks confirm
no Ghost/assertion code in the firmware core, unchanged checked buddy insertion
and descriptor lookup, and the existing packed boot bitmap/leaf reservation.

## Adapter hardening and remaining boundaries

- `MemoryAreas.Empty_Area` explicitly represents absent/zero-length entries;
  an absent framebuffer previously left its address fields uninitialized.
  Other reversed ranges are rejected before allocator mutation.
- Multiboot range construction handles zero lengths and rejects out-of-range
  or overflowing physical spans before inclusive-end arithmetic/narrowing.
- Framebuffer pitch/width/height arithmetic is widened and bounded before
  multiplication and endpoint construction; zero-sized buffers are empty.
- Boot setup derives free memory from whole-page policy decisions. Buddy setup
  applies the same map's reserved precedence and duplicate-owner rules.

These adapter changes have **not** been established by the 51-check core proof.
Raw Multiboot pointer validity, map length/entry-count consistency, variable
entry sizes, retained boot-module extents, and duplicate direct-map/cache-mode
handling still need a dedicated parser/mapping review. Physical mappings and
aliases, the complete traversal-to-free-list refinement, metadata/sentinel
exclusion and SMP remain end-to-end obligations. This is not a claim that the
whole allocator or firmware parser is formally verified.
