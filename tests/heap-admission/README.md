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

The postcondition covers successful range/non-wrap, exact page accounting,
tracking capacity and quota admission. This is not a proof of `Process.addPage`,
the slab/buddy allocators, page tables, or failure cleanup. Runtime frame
acquisition and stack admission have additional
[failure-injection tests](../allocation-failures/README.md), and initial stack/ELF
construction has [native rollback tests](../process-construction/README.md).
Complete exhaustion containment and syscall source-buffer validation remain open. See
[the remaining issue](../../docs/kernel-heap-admission-issue.md).
