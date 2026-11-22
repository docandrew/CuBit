# Checked archive pages for CCL

`Observatory_Trace_View` validates the complete archive while retaining one
64-event page. `Observatory_Trace_CCL` exposes that page to the existing CCL
interpreter. Neither package performs filesystem I/O or IPC. File checksums detect
accidental corruption; querying a file does not authenticate its publisher. This is the
query layer for a trace viewer, not a native graph window or capture-control UI.

## Reproduce

Use Nix and the repository's Alire toolchain. Hold the shared build lock while
snapshotting shared sources, or use an independent frozen source tree:

```sh
flock --exclusive --nonblock coordination/build.lock nix develop -c python3 tests/observatory-metrics/trace-view/run.py --prove /absolute/private/trace-view-check
```

The output directory must be new and outside the source checkout. The runner
records and rechecks all copied input hashes. `--root` and `--toolchain-root`
allow explicit frozen sources and an existing toolchain. Omitting `--prove`
runs compilation and hosted tests only.

The tests load the retained 78-event native CuBit capture, exercise CCL queries
and authority/type/index rejection, test all five event phases, preserve
64-bit identities above the signed integer limit, and check first/second/last
pages of a maximum-size 4,096-event archive. Missing footers, corrupt chunks,
trailing bytes and data after EOF cannot publish a ready view. Tests also
preserve the difference between requested stop, budget reached and observer
failure. See `evidence.json` and `proof.txt` for the accepted run.

## Integration and queries

Call `Start` with the header and a page number from 0 to 63. Feed every remaining
chunk in order, then call `Finish` with the actual trailing-byte count at EOF.
All row queries remain unavailable until the entire archive passes validation.
A new `Start` invalidates the previous view immediately. A page beyond the last
event is an empty page of a valid capture. Page selection requires another
bounded file scan; it is not telemetry loss. The model stores at most 64
captures and the archive framing state, with no dynamic allocation.

Publish both CCL interfaces, then explicitly grant the bindings needed by the
session. `Namespace`, `Name` and `Binding` enumerate them. The CCL callback uses
`in out` to match `Submit_With_Values`; it does not modify the view.

Basic queries use `trace`:

```lisp
(trace.ready)
(trace.count)
(trace.total)
(trace.page)
(trace.lossy)
(trace.stop-reason)
(trace.kind 0)
(trace.time-us 0)
(trace.has-duration 0)
(trace.duration-us 0)
(trace.event-id 0)
(trace.pid 0)
(trace.publisher 0)
(trace.observer 0)
(trace.output 0)
(trace.surface 0)
```

Detailed fields use `trace-detail` (each takes a zero-based row index):

| Queries | Meaning |
|---|---|
| `source-epoch`, `source-ticket` | Complete source identity with surface and publisher namespace |
| `writer-buffer`, `writer-epoch`, `writer-serial` | Output writer identity |
| `session`, `frame` | Submission/completion frame identity |
| `input-serial`, `input-watermark` | Dequeued input serial or client-declared watermark |
| `producer-dropped`, `batch-gaps` | Explicit cumulative transport loss counters in this observation |
| `history-sequence`, `batch` | Raw transport history position and publication batch |

IDs, timestamps, durations and counters are decimal strings, preserving all
64 bits. Only bounded row/page counts use CCL integers. An inapplicable field
returns `unavailable`; an invalid index, argument type or missing authority
fails the query. Before a file is ready, row counts are zero and its stop reason
is `unavailable`. A zero input watermark means unknown, not evidence of a
zero-latency input response.

`ready` describes validated file structure, not successful collection or disk
flush. A valid `observer-failed` footer remains visible as that stop reason.
`lossy` reports any observed nonzero producer-drop/batch-gap counter or footer
skipped/rejected/abandoned/mismatched-endpoint count across the entire file,
including events outside the selected page. It does not quantify missing
whole events or prove the absence of upstream loss when false.

Input, source acceptance, draw and submission timestamps are observations.
Completion records expose their recorded submission-to-collection duration;
they do not prove a separate submission record is present. No cross-event
join, input causality, CPU stack, display latch or photon time is invented.
Clocks are boot-local with unknown boot identity; separate captures cannot be
assumed to share a time domain.

## Proof boundaries

Both new package specs and bodies use SPARK. Scoped proof checks bounds,
initialization, termination, encoding access and the stated view contracts,
using the imported archive and CCL package contracts. Hosted checks exercise
the actual CCL interpreter and native capture bytes. This does not prove the
entire interpreter or an eventual native file adapter. No native viewer,
hardware acceleration or refresh-rate benchmark is claimed by this suite.
