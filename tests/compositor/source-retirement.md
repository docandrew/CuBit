# Asynchronous client source retirement

Desktop owns a fixed table of 24 source acquisitions independently of its eight
surface records. Each surface can hold two publication buffers, with one extra
replacement-acquisition allowance per surface. Closing a surface or replacing
its visible source clears aliases without dropping a pending acquisition.
This adds metadata, no pixel allocation or copy. The slot limit bounds held
acquisitions; it is not a measurement of total Mesa/process memory.

Desktop reserves a slot before either authenticated source-acquisition syscall.
Exhaustion returns Resources_Exhausted without entering the kernel. Acquisition
failure cancels only that reservation. Successful acquisition records the actual
grant and address, then activates the ticket. Existing sender authentication and
read-only source access remain unchanged.

Retirement proceeds through Renderer_Pending, Grant_Pending and Released.
Source_Busy preserves all acquisition metadata and allows a later bounded poll.
Only Source_Retired allows a kernel return. Only confirmed kernel return frees
the table entry. Unknown renderer or return results quarantine the entry and
require Desktop restart. No successful publication-retirement receipt is emitted
while the corresponding acquisition remains held. Generational tickets prevent
an old surface alias from releasing a newer occupant of the same table slot.

The event loop visits at most 24 entries per pass and includes pending source
retirement in its existing 1 ms fallback wake deadline. Input can wake it earlier.
This is a polling bound, not a measured input-latency guarantee. Renderer calls
must be nonblocking. Both current software backends complete synchronously;
the typed interface permits a future GPU backend to report ordinary busy work.

## Evidence and boundaries

- Concrete 24-slot SPARK instantiation: 25 checks, none unproved or justified.
  Contracts preserve other slots and ticket identities on every transition;
  Busy leaves the entire policy state unchanged.
- Hosted policy: 24,000 busy observations, table exhaustion, ordered release,
  stale tickets, canceled acquisitions and quarantine.
- Exact Desktop acquisition/retirement routines with controlled callbacks:
  another 24,000 busy observations retain actual address/grant records;
  full-table rejection precedes the syscall, and stale tickets cannot return a
  newer loan. Renderer and kernel-return uncertainty preserve the records.
- Exact surface-release and receipt routines: publication replacement, pending
  receipts, close/reset while held, legacy replacement and idempotent cleanup.
- Native CuBit/QEMU delayed-evidence fixture: 11 real acquisitions returned only
  after 33 injected busy observations, including three pending detachments.
  Primary-display, fractional-DPI, arrangement and Desktop interaction groups
  passed. The fixture withholds evidence after the real software renderer has
  retired; it does not simulate GPU memory accesses or prove fence truth.
- Typed software facade proofs: Mesa 85 checks, legacy 25 checks, none unproved
  or justified. Both native Desktop variants link; the native output test uses
  the typed Source_Release result.

The legacy main procedure remains an audited/tested integration boundary, not a
whole-service SPARK proof. Kernel acquisition ownership, mapping validity and
renderer retirement evidence remain trusted boundaries. Output-target teardown
is separate and still requires known renderer quiescence. Older native protocol
checks that expect immediate retirement need bounded receipt polling before
being used with a genuinely asynchronous renderer. This change does not select
the GPU backend, establish scanout sharing or measure physical display latency.

## Reproduce

Run inside the repository's Nix environment:

```
gprbuild -p -P tests/compositor/source_loans.gpr
tests/compositor/build/source-loans/source_loans_tests
gnatprove -P tests/compositor/source_loans.gpr -u source_loans_check.ads --level=2 --report=all
python3 tests/compositor/test-source-retirement.py
python3 tests/compositor/check-desktop-completion.py --self-test
```

The caller test snapshots the actual main routines into a fresh private build
folder with controlled callbacks, and records source hashes. It must not be
represented as native kernel execution.

For the native fixture, hold coordination/build.lock across build and run:

```
python3 tests/compositor/build-desktop-completion-fixture.py MESA_BUILD --source-retirement
python3 tests/compositor/run-desktop-completion-fixture.py FIXTURE_DIR
```

The builder prints FIXTURE_DIR. The runner restores the exact previous staged
Desktop and checks both delayed-source observations and all four interaction
groups. Use a fresh fixture directory for each run; never overwrite old evidence.
