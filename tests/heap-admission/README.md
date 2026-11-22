# Whole-request heap admission

Run through Nix from the repository root:

```sh
nix develop -c bash -lc 'cd kernel && alr exec -- gprbuild -p -P ../tests/heap-admission/heap_tests.gpr && ../tests/heap-admission/build/main'
nix develop -c bash -lc 'cd kernel && alr exec -- gnatprove -P ../tests/heap-admission/heap_tests.gpr -u heap_admission.adb --mode=all --level=1 --checks-as-errors=on -j2'
nix develop -c make -C kernel capability-security-image
nix develop -c tests/headless/run.sh --test capability-security --disk kernel/capability_security_disk.img --accel kvm --cpus 4 --timeout 45
```

The planner checks the full unsigned request before allocating pages. Host
assertions check every page offset and growth through two pages, plus tracking,
quota and virtual-address boundaries. The native authorityless app submits
wrapping and oversized requests, verifies no partial break advance, and then
checks a successful allocation is zeroed and writable.

Heap growth also preserves existing frame-tracking headroom. Previously, growing
the limit to `max(old capacity, used + new pages)` could leave no slots for a
later demand-mapped stack page. Mixed-resolution Desktop buffers reproduced this
as a rejected fault at the secondary-stack base. `Expanded_Capacity` now adds
the new heap-page count to the old capacity, rejecting an unrepresentable sum;
the syscall restores the old capacity on allocation rollback. Capacity is only
a list limit, not preallocated physical memory or an override of resource quotas.
Its exact-sum/rejection contract is checked by GNATprove. Hosted tests cover
boundary values and preservation of unused slots across repeated growth; the
native mixed-output arrangement fixture exercises the originally failing path.

The postcondition covers successful range/non-wrap, exact page accounting,
tracking capacity and quota admission. This is not a proof of `Process.addPage`,
the slab/buddy allocators, page tables, or failure cleanup. Runtime frame
acquisition and stack admission have additional
[failure-injection tests](../allocation-failures/README.md), and initial stack/ELF
construction has [native rollback tests](../process-construction/README.md).
Complete exhaustion containment and syscall source-buffer validation remain open. See
[the remaining issue](../../docs/kernel-heap-admission-issue.md).
