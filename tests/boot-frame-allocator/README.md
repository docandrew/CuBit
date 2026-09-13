# Boot-frame reservation core

`kernel/src/boot_frame_allocator.*` is the production implementation, not a
parallel model. `BootAllocator` instantiates it for PFNs 0..16,383. The private
state contains a one-bit-per-frame availability map and an inclusive high-water
mark. PFN zero is never admitted or allocated. The physical-address/firmware
adapter remains outside this focused proof.

The old bitmap mutations incremented/decremented a separate free counter even
when the bit was already in the requested state. Repeated usable firmware
entries could therefore inflate the count. There is now no cached counter:
diagnostic counts are computed from the bitmap, outside the allocation path.
Admission is idempotent and permitted only before a successful reservation.
The unused public boot `free` API and old private search/bit-mutation routines
were removed; existing kernel callers only reserve and query boot memory.

The boot adapter now scans only its bounded arena rather than traversing all
firmware-listed RAM beyond 64 MiB. It checks external bitmap-query bounds before
indexing, including in the unchecked kernel. It retains the existing firmware
region/page admission policy; conflicting memory classifications and partial
pages are **not** solved by idempotent admission.

## Proven properties

The focused target passes **27 checks, none unproved or justified** (20 in the
production-sized core instantiation, seven in Ghost composition/handoff tests).

- Initialization makes every frame unavailable and clears the high-water mark.
- Admission changes exactly one nonzero frame and leaves the high-water mark.
- A successful reservation claims exactly the requested, previously free span;
  all other bits are preserved and the high-water mark covers the last frame.
- Exhaustion leaves the entire state unchanged, with zero as the result.
- Two successful reservations cannot overlap; subsequent reservation attempts
  cannot reduce coverage of the first allocation by the high-water mark.
- Newly reserved frames select the bitmap-checked branch of the actual buddy
  handoff rule and are marked unavailable there.
- Arithmetic, array indexing and loops satisfy the focused runtime-safety and
  termination obligations.

No `Assume`, suppressed proof obligations or `SPARK_Mode => Off` in this core or
its composition proofs. Proof statements and snapshots do not execute in the
kernel. The proof establishes safety/failure atomicity, not a formal theorem of
search completeness or lowest-address first fit; those and exact diagnostic
counts are independently regression-tested here.

## Reproduction

From the repository root, using the pinned Nix environment:

```sh
nix develop -c bash tests/boot-frame-allocator/prove.sh
nix develop -c bash -lc 'cd kernel && alr exec -- gprbuild -p -P ../tests/boot-frame-allocator/reservation_tests.gpr && ../tests/boot-frame-allocator/build/main'
nix develop -c make -C kernel cubit_kernel
nix develop -c python3 tests/boot-frame-allocator/check-codegen.py
nix develop -c python3 tests/buddy-boot-admission/check-codegen.py
nix develop -c python3 tests/buddy-metadata/check-codegen.py
nix develop -c python3 tests/intrusive-list-splices/check-codegen.py
nix develop -c tests/headless/run.sh --test capability-security --disk kernel/capability_security_disk.img --accel kvm --cpus 4 --timeout 90 --serial /tmp/cubit-boot-reservation-native-serial.log --keep-logs
nix develop -c env CUBIT_DOOM_MULTIAPP=1 tests/headless/run.sh --test desktop-doom --accel kvm --cpus 4 --timeout 90 --serial /tmp/cubit-boot-reservation-desktop-serial.log --keep-logs
```

Run the native fixtures sequentially: they stage shared boot inputs.

Hosted tests pass **206,022 requests**, using an independent per-frame oracle
that tests candidate intervals instead of duplicating the production run
counter. All maps and pairs of request sizes are exhausted in the small arena;
additional tests cover a singleton arena, the production-sized arena, repeated
admission, exact exhaustion, the final PFN, cross-byte/word spans, fragmentation,
first-fit results, exact counts/high-water and failure-state equality. Hosted
assertions are enabled; kernel builds remain optimized `-O2 -gnatp`, without
`-gnata`.

Pinned x86-64 codegen checks show a 2,048-byte bitmap plus a four-byte high-water
field. Reservation is a leaf routine with no helper calls, division, bulk copies
or proof code, and eight-byte static stack usage (return-address accounting).
The bitmap query uses direct shift/mask indexing. This is codegen evidence, not
a latency benchmark or a claim that steady-state buddy allocations got faster.
The existing buddy insertion check still reports 32 instructions with its
original call count; descriptor lookup remains within its prior size with the
indexed LEA and no helper/Ghost code.

The final optimized kernel passed both four-vCPU KVM fixtures: the security
test verified eight partial-load rollbacks and successful PID reuse; the
multi-app desktop test verified DOOM game pixels and responsive Apps-menu input.

## Remaining boundary

The caller must correctly admit firmware memory and serialize the boot phase.
The contract forbids reopening admission after reservation; kernel setup is its
only production caller. Physical address translation, actual mappings/aliases,
partial-page and conflicting firmware regions, and complete payload/metadata/
sentinel exclusion remain integration obligations. Single-order list refinement
still needs a complete physical address-to-ID mapping. This is not an end-to-end
proof of the buddy allocator, SMP machinery or compiler representation lowering.
