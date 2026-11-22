# Retained DMA allocation checks

Run hosted checks with `nix develop -c bash tests/retained-dma/run.sh`.
They exercise the production budget arithmetic, stable block-backed record
pool, and bounded ownership retirement policy, with mocked physical callbacks.
They are not kernel scheduling or GPU retirement evidence.

The metadata guard fixture is compiled with assertions disabled, like the
kernel. Explicit Metadata_Error checks reject linked-record mutation/release,
double insertion/release, wrong-pool release, self-list transfer and reading a
released record before list mutation. Internal references are still not
generation-bearing external handles: stale aliases after legitimate reuse
remain a caller invariant, not a guarantee established by these guards.

Malformed retirement identity is a terminal rejection, not ordinary pending
work. The kernel moves its remaining records to quarantine, logs once, stops
queueing that PID and keeps it unreusable. Hosted tests verify a poisoned cursor
cannot be revived by restoring an earlier identity or by repeated retries.
Native poison injection/scheduler observations remain separate coverage.

Budget proof: `nix develop -c gnatprove -P tests/retained-dma/proof.gpr --level=2 --report=all -j2`.
This proves the arithmetic/contracts, not allocator concurrency or device safety.

Process_Memory_Budget is the common page-charge core used by process and DMA accounting. Hosted tests
mix ordinary, DMA and metadata charges, reject over-limit quota adoption
without side effects, preserve zero-as-unlimited and check overflow/underflow.
Its SPARK contracts cover charge deltas, failed-operation preservation and
quota adoption; they do not establish caller locking, authentic ownership,
exactly-once refunds or hardware retirement.

The regular runner also checks retired account identities across storage reuse,
135 quota-width boundary cases, and allocation/track/claim/bind/map rollback.
Its negative control deliberately refunds a failed map twice; the runner
requires both the mutation marker and the resulting assertion failure.
The proof project includes the owner-account lifecycle contracts. Unique token
issuance and native storage/locking are tested rather than established by that
proof. Process startup installs the physical-refund callback; ordinary pages,
DMA backing and owner-slab metadata share one account. The actual-adapter hosted
fixture injects allocation/binding failure and checks no charge remains.
Native tests cover owner death, pinned grants, quota adoption, and PID reuse.
Authenticated whole-allocation GPU release and allocation/exit concurrency remain
open; retained backing is not reclaimed merely because an owner exits.

The actual buddy charge callback has a separate native early-boot fixture:

```sh
nix develop -c python3 tools/build-workspace.py create buddy-charge-refunds --seed-live
# Use the completed path returned by create; never run against the main tree.
nix develop -c python3 tools/build-workspace.py run <workspace-path> -- python3 tests/retained-dma/run-buddy-native.py
```

Use a fresh snapshot: the runner inserts a test-only halt in private `kmain`,
builds a kernel and disposable ISO, and stops only its own QEMU process.
It records hashes, build/serial logs and `result.json` in `charge-evidence-*`.
It tests deferred unpin refunds, rejected charge replacement, repeated frame
reuse, whole-block release, growth with 600 simultaneously charged frames,
and callback re-entry into the allocator after unlocking. This is single-CPU
kernel execution, not GPU, TLB-shootdown, grant, SMP or NUC validation.

The contention fixture runs eight Linux-hosted Ada tasks with one protected
ledger, 48,000 mixed-charge attempts and a four-page ceiling. It checks no
oversubscription and a zero final balance. This exercises the shared accounting
core under host serialization, not the kernel's spinlocks or allocation/exit
races.

Native disposable QEMU fixtures (hold coordination/build.lock, build the
current kernel first; no production image/staging replacement):

    nix develop -c bash tests/intel-gpu/native/run-demand.sh dma-growth
    nix develop -c bash tests/intel-gpu/native/run-demand.sh dma-retirement

Growth checks 96 simultaneous retained allocations/192 MiB, both CPU mapping
modes, 49,152 page sentinels, and partial mapping rollback. Retirement checks
40 retained records on a killed child, a live read-only grant holding the dead
owner's PID after report consumption, readable backing until grant return,
report-aware exact PID reuse, retained backing exclusion from new ordinary
allocations, and 160 ordinary-DMA owner lifecycles with metadata reclamation.
The runner requires the quota and lifecycle success markers, not just process
exit. Neither points a GPU at memory.
Concurrent allocation/exit stress and kernel-owner review remain release gates.

The global retained backing ceiling is half of buddy-managed RAM.
It is separate from the driver's
per-client policy, GPU virtual addresses and DMA addressability. Metadata is
charged independently at one thousand twenty-fourth of managed RAM, in actual
page-rounded blocks. Neither budget preallocates that amount.

Cleanup processes at most 64 ownership tags per worker step, outside grantLock.
The PID remains reserved until all DMA records and outstanding process reports
retire. Retained backing and its metadata remain in an orphan ledger: owner
death is not evidence of GPU retirement and never refunds retained-byte quota.
