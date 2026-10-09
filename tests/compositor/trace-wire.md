# Compositor trace encoding, version 1 

A packet is sixteen unsigned 64-bit words, 128 bytes. A future byte adapter
must encode each word little-endian. The pure codec does not access memory,
perform IPC, allocate, read clocks, or change rendering state.

Words 0/1/2 hold magic 0x4354524300000001, kind (1=input, 2=source,
3=render, 4=frame), and a nonzero event identifier. Event identifiers must be
allocated without wrapping by the producer; that lifecycle rule is not
established by this stateless codec. Transport must retain authenticated
producer incarnation. Event identifiers from different incarnations cannot
be joined. Reserved words are zero and a decoder rejects nonzero reserves.

| Kind | Words starting at 3 |
| --- | --- |
| Input | surface, input serial, input kind, dequeue timestamp |
| Source | surface, source epoch, source ticket, client input watermark, acceptance timestamp |
| Render | phase (0=draw, 1=submit), output, buffer, writer epoch, writer serial, surface, source epoch, source ticket, session, frame, observation timestamp |
| Frame | output, session, frame, submission timestamp, completion timestamp |

All timestamps use the existing monotonic microsecond clock. Unavailable
sentinels, invalid identities, impossible phase fields and reversed frame
timestamps are rejected by existing trace validity predicates. Client input
watermarks remain untrusted metadata. A draw does not establish visibility;
a software completion does not establish a hardware latch or photons.

The encoder contract proves exact Decode(Encode(event)) equality for valid
input, preserving all 64 bits of every identity. The ghost Lemma_Decoded_Valid contract
establishes decoder validity on success. It does not prove telemetry delivery,
producer identity authenticity, fragment assembly, or buffer retirement.

The hosted fixture exercises 54,012 checks with full-width identities,
zero/maximum timestamps, all reserved fields, out-of-range enum/output
values, invalid writer/source identity and reversed completion times.
A separately compiled negative control replaces writer epoch with 1; the
round-trip postcondition rejects it at runtime.

Desktop now exports these packets through the metrics service's bounded raw
trace groups. See [native capture](trace-native/README.md) and the
[metrics service contract](../../docs/metrics-service.md#desktop-trace-export).
CCL capture archives/viewers and kernel event export remain separate work.

## Reproduce

From the repository root, inside `nix develop`, run from `kernel`:

```sh
alr exec -- gprbuild -P ../tests/compositor/trace_wire.gpr -p -XTRACE_WIRE_OUTPUT=/absolute/private/output
/absolute/private/output/trace_wire_check
alr exec -- gnatprove -P ../tests/compositor/trace_wire.gpr -XTRACE_WIRE_OUTPUT=/absolute/private/output -u compositor_trace_wire.adb --level=2 --timeout=30 -j2 --counterexamples=off --checks-as-errors=on --report=all
```

Use a fresh disjoint output directory. The accepted proof has 42 checks
(4 flow, 38 prover), zero unproved or justified obligations. Earlier attempts
with a procedural decoder could not discharge the round trip through its
contract; the expression decoder exposes the same validation to the prover.
This is hosted/SPARK evidence, not native capture or hardware timing evidence.

## Cross-package proof follow-up

The expression decoder is completed in the private specification so GNATprove
can inline it in callers. The implementation and wire format are unchanged.
The original 54,012 hosted checks pass against this arrangement. A private
trace-fragment assembler also passes 29,007 mixed-identity/sequence tests and
77 combined proof checks (10 flow, 67 prover), including preservation of all
fragment payload words, successful-decoding validity, and the original codec
round trip. Evidence is in `trace-wire-evidence/cross-package.json` and
`cross-package-proof.out`. The metric-record type, assembler and streaming collector now have native
transport validation. Desktop callsite validation is recorded separately in
`trace-wire-evidence/desktop-native188.json`.

## Stateful publication policy

`Compositor_Trace_Publication` allocates a nonzero event ID for each valid
attempt, before SDK admission. A refused event never reuses its ID. It reserves
the sequence limit and refuses further events instead of wrapping. Invalid
content consumes no ID. Refusal, invalid and unsupported counters saturate.
Its contract preserves the complete input payload except the assigned ID.

`Compositor_Metric_Batch_Policy` accounts for four-record trace groups alongside
ordinary samples. If fewer than four slots remain it requests an early flush;
Desktop's ordinary pump performs the submission. There is no additional queue,
page allocation, IPC or waiting in the event append itself. The existing SDK
continues to own two pages. Held pages cause bounded shedding, not rendering
backpressure. Trace event attempts occupy four 64-byte records (256 bytes).

The portable publication suite exercises sequence exhaustion, invalid content,
10,000 ordinary IDs, every page-capacity boundary, urgent flush/reset and the
existing 1,000-page batch tests. From `kernel`, inside Nix:

```sh
alr exec -- gprbuild -p -P ../tests/compositor/trace_publication.gpr -XTRACE_PUBLICATION_OUTPUT=/absolute/private/output
/absolute/private/output/trace_publication_check
/absolute/private/output/metric_batch_policy_tests
alr exec -- gnatprove -P ../tests/compositor/trace_publication.gpr -XTRACE_PUBLICATION_OUTPUT=/absolute/private/output -u trace_publication_instances.ads compositor_metric_batch_policy.adb --level=2 --timeout=30 -j2 --counterexamples=off --checks-as-errors=on --report=all
```

The production, short-sequence and empty-sequence instances plus batch policy
have 90 proved checks (33 flow, 57 prover), no unproved or justified checks.
The native `Desktop_Metric_Publisher` adapter and Desktop event loop are outside
this proof. The actual adapter's 20 hosted fault/overload modes include both SDK
pages held, immutable submitted pages, four-record refusals, recovery without
ID reuse, and early flush when a group cannot fit. Run inside Nix:

```sh
python3 tests/compositor/test-desktop-metric-publisher.py
```

Clock correctness, mapped memory, kernel authority, Mesa, and physical display
behavior remain foreign assumptions or separately tested boundaries.
