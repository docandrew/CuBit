# Desktop draw-to-completion correlation

The opt-in `CUBIT_COMPOSITOR_TIMING=on` diagnostic connects accepted client
publications to successful draw work, output submission and validated software
Display completion. `check-render-pipeline.py` also attaches a matching input
dequeue when the publication's client-supplied watermark identifies one.

The writer identity contains output, buffer slot, epoch and write serial. A
slot alone is insufficient: it is reused after completion. A submission record
is appended only after successful asynchronous submission. The existing frame
trace provides completion after Desktop validates the Display reply. Draw
records include surface, publication epoch and publication ticket, and require
a matching visible, acquired source buffer.

These are associations between **work records**. A recorded draw can later be
overwritten or occluded; retained pixels may require no fresh draw. Repeated
draws or repeated input watermarks do not establish independent input responses.
Completion is software buffer release, not display latch, scanout or photons.
TCG timings and instrumented serial output are not hardware latency benchmarks.

Direct primary-output and native per-output rendering have writer identities.
The compatibility logical-canvas copy path and legacy unprotected publications
increment an unsupported counter rather than guessing an association. Extending
coverage requires an explicit region-copy/retained-content bridge.

`Compositor_Render_Trace` is a bounded SPARK policy with 64 records, saturating
invalid/drop/unsupported counters, exact append and reset contracts, and no
allocation or IPC. Its level-2 proof discharged 33 checks (8 flow, 25 prover),
zero unproved or justified. Hosted policy tests exercise 1,000 bounded batches,
invalid phase/identity/clock fields, saturation and reset. This proof covers the
policy, not the SPARK-off Desktop glue, renderer, kernel clock or Display.

The actual-helper hosted test extracts Desktop's writer/source selection,
successful-submission append and diagnostic drain. It uses the real pool and
surface policies with mocked addresses, clock and IPC outcome. It checks the
disabled path avoids clock reads, invalid identities fail, unsupported paths
are counted, and successful submission preserves the held writer identity.
It does not execute rasterization or establish kernel capability enforcement.

The offline checker requires closed printed batches, rejects invalid/lost or
unsupported records, and joins by complete identities independently of the
print order of the different trace families. It rejects reversed clocks,
duplicate writer submissions, frame timestamp disagreement and source draws
before acceptance. Unsubmitted, uncompleted or unobserved-source draws are
counted separately. At least one complete association is required. Capture
can end before an in-memory batch is printed; closed printed batches do not
prove capture of every event before shutdown.

Startup or idle render captures may have no input records. The render checker
reports missing input associations and unknown/unmatched watermarks explicitly;
it still rejects any malformed, lossy or unclosed input batch that is present.
The standalone input/publication checker continues to require an input match.

Run hosted checks in Nix:

```sh
nix develop -c gprbuild -q -p -P tests/compositor/render_trace.gpr
nix develop -c tests/compositor/build/render-trace/render_trace_tests
nix develop -c gnatprove -P tests/compositor/render_trace.gpr -u compositor_render_trace.adb --level=2
nix develop -c python3 tests/compositor/test-render-trace-integration.py
nix develop -c python3 tests/compositor/test-render-pipeline.py
```

For native validation, hold `coordination/build.lock`, build default Desktop
and the timing variant using the pinned Alire compiler in `kernel`, and use
the absolute timing binary path as `CUBIT_DESKTOP_IMAGE` for the 120-second,
four-CPU TCG `ccl-workspace` test. Check the captured serial output with:

```sh
python3 tests/compositor/check-render-pipeline.py SERIAL_LOG > REPORT.json
```

The serial exporter is temporary integration-test machinery. A subscribable
raw trace transport, controlled instrumentation overhead and supported-hardware
measurements remain necessary. The metrics service's aggregated summaries do
not by themselves supply flame-graph events or complete presentation tracing.

## Native verification, 2026-10-01

Session 54528 completed with exit 0 after both Desktop variants built and the
120-second, four-CPU TCG `ccl-workspace` regression and final fault scan passed.
There were 255 render records in 22 closed batches: 168 draws and 87 submissions.
All 168 draws joined to accepted source publications, submitted writer tickets
and observed completions. These cover 84 distinct publications and 84 output
frames with client draws; the frame trace includes 87 completed submissions.
All associated source watermarks matched the 247-record input capture. Missing
links, unknown/unmatched watermarks, invalid records, drops and unsupported
paths were zero in the exported capture.

Do not treat the 168 draws as 168 independent input responses: repaired buffers
and later redraws repeat publications and their old input watermarks. In this
capture, a later redraw of publication 84 carries an input watermark more than
12 seconds old. That is elapsed association age, not a 12-second response-time
measurement. Determining the first visible response still needs pixel/region
provenance and display-latch evidence.

All nine source/staged-binary hashes matched at exit:
`/tmp/cubit-render-pipeline-native-inputs.sha256`. The tested kernel hash is
`/tmp/cubit-render-pipeline-native-kernel.sha256`. Other evidence:
`/tmp/cubit-render-pipeline-native.log`,
`/tmp/cubit-render-pipeline-native.serial`, and
`/tmp/cubit-render-pipeline-native-report.json`.

This exercises the direct software primary-output path. Native Mesa, two-output
rendering, logical-canvas copy bridging, GPU fences and scanout remain outside
this native gate. The normal staged Desktop has tracing disabled; the separate
timing build enabled this capture. Actual-helper test 81522 additionally rejects
unacquired sources. Offline regression 34692 covers idle/no-input rendering and
partial associations without weakening loss or malformed-input rejection.


The pipeline report propagates input delivery counts, retried close identities
and ambiguous watermarks. A valid retained-close retry no longer invalidates
otherwise complete source/output work evidence. Its input-to-completion join
is withheld because the client watermark does not distinguish delivery
attempts; publication/draw/submission/completion associations remain available.
See `input-publication-trace.md` for the retry validation rules and checks.


Validated captures can now be exported with `export-timeline.py` into a local
Perfetto timeline. The export uses measured software completion durations and
instant observations for other stages, preserving identity and ambiguity
without inventing execution stacks or input causality. See
`timeline-export.md` for commands and exact independent import validation.
