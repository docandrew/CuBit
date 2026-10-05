# Process-owned mapping records

The old kernel had 4,096 mapping descriptors shared by all processes. Penny
reproducibly exhausted them with about 4,090 live mappings. Kernel diagnostics
confirmed allocation rejection before abort, with no failed releases or
quarantined records in the measured run. This did not prove absence of
application-level retention leaks.

`Owned_Record_Tables` allocates blocks on demand, maintains stable record IDs
and references while live, and links records in address order. Empty blocks
and the last empty directory are freed immediately under the caller's registry
lock. Iteration prefetches the successor, allowing release of the current
record; releasing the successor during that iteration is forbidden.

`Process.Owned_Memory` has one table per process, each bounded at 65,536 records.
IDs are kernel-private, not capabilities. Caller generation checks, mapping
ownership, frame inventory, quarantine, TLB retirement, and RW/NX permissions
remain in the mapping adapter. Address-order iteration makes first-fit a single
pass, including overlapping reservation and chunk records.

The ceiling isolates record-ID admission between processes. It does **not**
reserve physical RAM for other processes or prove resistance to resource denial
of service. Metadata uses kernel allocator backing. Frame quotas, global memory
pressure, browser OOM handling, and page-process isolation remain separate work.
There is no claim that this change makes Penny crash-proof.

## Hosted test

Run from the checkout root in the pinned Nix shell. Outputs are private:

```sh
out=$(mktemp -d /tmp/owned-record-tests.XXXXXX)
gnatmake -q -gnat2022 -gnata -gnato -g -fsanitize=address \
  -I"$PWD/kernel/src" -D "$out" -o "$out/test" \
  tests/owned-record-table/table_tests.adb -largs -fsanitize=address
"$out/test"
```

The test exercises 8,193 live records (including a partial final block), an
independent table while the first is full, stable references, ordered hole
reuse, equal keys, directory/block allocation failures, expansion rollback,
removal during iteration, and complete metadata reclamation. The ordered
payload check uses a separate key-presence oracle. This is an implementation
test with assertions and AddressSanitizer, not a SPARK proof.

## Native test

Use an isolated build workspace. Build the kernel and `user_runtime` there,
then run `bash tests/owned-record-table/run-native.sh` through the workspace
helper. The script replaces devmgr only inside a disposable boot image and
never writes the production disk. Its startup program uses caller-owned
mapping syscalls; it does not test unprivileged manifest admission.

The native test allocates 8,193 pages twice, checks zero fill and sentinels,
reuses holes, rejects duplicate releases, changes protections, and retires
interleaved reservation chunks while retaining unrelated mappings. It then
exits with 64 live mappings and requires the kernel retirement marker.
Cross-process kernel security, low-memory injection, and PID reuse are not
proved by this fixture.

The large-mapping regression crosses the former 16 MiB admission limit with
four 20 MiB allocations, verifies page contents and whole/subrange protection,
and checks release, double-release rejection, 16 MiB + 1 byte rounding, the
256 MiB maximum, and oversized/wrapping request rejection. The libc adapter
is separately exercised by `userspace/libc/tests/test-large-mmap.py`.
The 256 MiB value bounds a single eager mapping; it neither reserves that RAM
nor changes per-process quotas or the 16 MiB reservation commit bound.
Larger eager allocations still hold the existing memory locks while mapping
pages; this change makes no allocation-latency or general OOM-recovery claim.
