# Allocation-failure and stack-admission regression

Run from the repository root, using the project's Nix / Alire toolchain:

```sh
nix develop -c bash -lc 'cd kernel && alr exec -- gprbuild -p -P ../tests/allocation-failures/allocation_tests.gpr && ../tests/allocation-failures/build/main'
nix develop -c bash -lc 'cd kernel && alr exec -- gnatprove -P ../tests/allocation-failures/allocation_tests.gpr -u page_admission.adb allocation_proof.adb elf_admission.adb --mode=all --level=1 --checks-as-errors=on -j2'
nix develop -c make -C kernel capability-security-image
nix develop -c tests/headless/run.sh --test capability-security --disk kernel/capability_security_disk.img --accel kvm --cpus 4 --timeout 90
```

The host suite compiles the **actual kernel SlabAllocator, LinkedLists and
Page_Allocation implementations**, not copies of their algorithms. Fixtures
replace physical allocation and IRQ locks with bounded memory and checked lock
ownership. The shared page-acquisition sequence is instantiated with a resource
model that rejects each acquisition stage. Checks cover:

- Physical backing failure during setup and repeated slab expansion attempts.
- No lock retained on either a fallible return or the legacy allocating wrapper's exception.
- Recovery through deallocation while physical allocation is still disabled.
- Slab block-count exhaustion without an out-of-range block-list write.
- Full lists rejected before allocating a node; failed insertion leaves links/count unchanged.
- Circular forward/backward links, empty/singleton transitions, and 20,000 mixed operations against an array model.
- Page acquisition failure at physical allocation, tracking, ownership and mapping; exactly-once cleanup and successful retries.
- Exclusive stack/heap ends, empty/reversed ranges, the lower guard boundary, frame capacity/quota and the lower canonical user-address limit.
- ELF program-header geometry, file/memory extents, alignment and image-to-heap guard space using raw unsigned wire metadata.

GNATprove checks the pure admission predicate and a SPARK instantiation of
acquisition with arbitrary candidate/success inputs. Generic bodies need an
instantiation to generate proof obligations; analyzing only the generic package
does not establish its result invariant. The proof checks that a frame is
returned exactly on success; stage-specific errors and release sequencing are
covered by the checked resource-model tests. It does **not** prove hardware callbacks, pointer-backed
list, physical allocator, mapping teardown, or SMP safety. Host assertions are
enabled only for this project; kernel runtime assertions remain disabled.

The native four-vCPU test touches and revisits 64 KiB of stack, rejects wrapping
and oversized heap requests, checks a successful zeroed/writable allocation,
and continues the capability non-amplification tests. It does not inject global
physical exhaustion into a running kernel or exercise every teardown race.
It also runs the [unpublished construction regression](../process-construction/README.md)
before the valid app is launched.

The kernel build also runs its existing 2048-byte per-function stack-usage
limit. That is not a worst-case call-chain / nested-interrupt stack proof.

See [remaining allocation work](../../docs/kernel-heap-admission-issue.md).
