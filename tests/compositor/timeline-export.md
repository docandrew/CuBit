# Compositor observation timeline

`export-timeline.py` converts one validated Desktop-incarnation capture into
Chrome JSON supported by Perfetto. It reuses the strict input/publication,
source/draw/writer and submission/completion validators before writing output.
It rejects incomplete/lossy/invalid batches, preserves unmatched-work counters,
and carries retained-close retry ambiguity into the report metadata.

```sh
nix develop -c python3 tests/compositor/export-timeline.py capture.serial timeline.json
```

Open the resulting local file in the Perfetto UI. Format reference:
https://perfetto.dev/docs/getting-started/other-formats

Only measured software submission-to-release intervals become duration slices.
Input delivery, publication acceptance, successful draw and writer submission
are instant observations. No input-to-photon bar, inferred draw execution
interval, causal flow arrow or CPU stack is fabricated. Tracks are synthetic
lanes named by output/surface, not OS thread identities. All events originate
from the same captured Desktop clock domain. Large object identities and the
absolute clock origin are strings; event timestamps are rebased to the earliest
observation. Captures exceeding the exact JSON integer timestamp range are
rejected. These rules preserve 64-bit identities without JavaScript rounding.

The exporter validates before creating output and atomically replaces the
output only after serialization. It refuses to overwrite its input capture.
This tool currently consumes diagnostic serial captures; it does not implement
the pending raw subscription/archive transport and is not a CPU flame profiler.

## Validation

```sh
nix develop -c python3 tests/compositor/test-timeline-export.py
nix develop -c python3 tests/compositor/validate-timeline-import.py timeline.json \
  --processor /path/to/trace_processor
```

Regression 87337 passed observed slice/instant counts, large identity/clock
values, retry ambiguity and preservation of an existing output after rejected
input. The native capture from the earlier render-pipeline gate was exported
as `/tmp/cubit-compositor-native-timeline.json`: 87 software-completion slices,
247 input instants, 84 publication instants and 255 render checkpoints.

Perfetto Trace Processor v58.2 independently imported the JSON. Validation
80656 compared every imported event's name, timestamp and duration with the
export: all 673 matched exactly and the processor reported no error/data-loss
statistics. The binary was downloaded from the official launcher's manifest
and verified against its size and SHA-256 before execution; no global install
was needed. Hash:
`58042408e6cc861fb1a731c26bb082dc222285561eaa4e12a48a8b2b90dca7b9`.

Evidence: `/tmp/cubit-timeline-export.log`, `/tmp/cubit-timeline-import.csv`,
`/tmp/cubit-timeline-import.log`, and
`/tmp/cubit-timeline-import-validation.log`. This is export/import validation of
an existing native capture, not a new native compositor run or hardware
measurement. The retained serial capture still has the timing boundaries
specified in `render-pipeline-trace.md`.

## Native CCL viewer direction

The preferred interactive frontend is a native CCL Observatory. JSON/Perfetto
is an optional offline interoperability path, not the producer or capture ABI.
The existing validator's distinctions between observed instants, measured
intervals, ambiguous input watermarks and missing data must carry through to
that viewer; a renderer must not turn those gaps into apparent causality.

Current source offers more than the older UI proposal describes:
`CCL.Host_Values` carries owned `Resource_Value` and typed `Object_Value`
results, and `workbench-ui.ccl-interface` includes button handlers and bounded
output updates. These are useful building blocks, not an implemented trace
viewer. A native `CCL.Objects.Image` occupies 16 KiB; materializing one such
object per trace event would defeat the bounded, lean capture design.

The intended split is:

- The collector/archive retains compact binary events with explicit clock,
  producer incarnation, sequence and loss information. Summary histograms
  cannot reconstruct this history; raw subscriptions remain a prerequisite.
- A native capture resource owns indexed storage. CCL borrows the resource for
  bounded queries over a selected time range and lane set; it does not receive
  raw pointers or acquire observation authority merely by knowing an ID.
- CCL controls filters, dashboard composition and selection. Native chart
  components render a clipped viewport, cap primitives per refresh and aggregate
  dense regions explicitly. Capture loss and visual aggregation are distinct.
- Refresh coalesces at a separately bounded cadence. A hidden or closed viewer
  stops UI work and releases subscriptions; it never holds producer buffers
  while waiting for user interaction. Save/reopen uses a scoped archive resource.

Initial coverage should include real latency distributions, drop/gap counts,
and input/publication/draw/submission/completion lanes. CPU flame graphs require
actual stack samples and symbols, and must remain unavailable until those data
exist. None of the current software completion records proves physical photons.

Acceptance needs a native capture/query/UI path on CuBit without serial, an
archive round trip, loss/slow-reader tests, bounded memory and viewport work,
and proof of the new policy components with explicit adapter/FFI boundaries.
The existing JSON exporter and import checks do not satisfy those gates.
