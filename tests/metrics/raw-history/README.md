# Raw metric history

Run from the repository in its pinned Nix environment, using a new output path:

```sh
nix develop -c python3 tests/metrics/raw-history/run.py --output /absolute/fresh/output
```

The runner snapshots inputs and checks their hashes after execution. It runs
24,340 existing metrics checks, 414 raw-store checks, 4,276 query checks,
wrap/sequence-exhaustion tests, 54 observer boundary checks, and 520 malformed
packet checks. A compiled negative control removes publisher-tag validation;
the packet tests must reject it. `--no-prove` skips only GNATprove.

The selected proof has 128 checks (62 flow, 66 prover), zero justified/unproved.
Contracts cover retained event identity/payload, exact accepted-record sequence
advancement, publisher-source isolation, rejected-batch stability, cursor/gap
semantics and bounded pagination. The packet validator's proof covers generated
safety/termination checks; semantic packet rejection is regression-tested.
Counters from overlapping earlier proof reports are not additive.

Observer tests compile the actual runtime adapter against mocked IPC, endpoint
inspection and grants. They cover stale/replaced service identity, malformed
reply envelopes/pages, rejected grant creation, and pending or rejected revoke.
Revoke acceptance never substitutes for `Retirement_Confirmed`. These mocks do
not prove kernel authentication or native failure behavior.

## Native evidence

The private native run in `evidence/native.json` rebuilt runtime, metrics service
and the extended `userspace/apps/metrics-check` app. It used explicitly recorded
prebuilt kernel/initrd/allocator inputs. Real CuBit IPC/grants delivered the last
256 of 1,005 accepted records with exact latency/counter/span values and
correlations, reported the 749 overwritten records, rejected publisher-only raw
query authority and confirmed observer grant retirement. Synthetic test timestamps
are protocol data, not performance measurements. Normal native endpoint replacement
and fault injection are not covered by this run.

The normal `tests/headless/run.sh --test metrics` profile selects this acceptance
app; rebuild the runtime, service and app together before running it. Do not run
the fixture against a service already collecting unrelated producers: its exact
counts intentionally require an isolated metrics service. Keep the shared build
lock for shared build/staging commands, or use an independent private workspace.

No GPU, scanout, 240 Hz, physical latency, Desktop causal export, or CCL capture
claim follows from these tests. See `docs/metrics-service.md` for query semantics.

The suite also covers raw-only trace fragments and atomic four-record event
admission (`trace_group_check`, 18,107 checks), and compositor reconstruction
(`trace_assembly_check`, 29,007 checks). The proof now includes the record
codec, batching policy, compositor wire codec and fragment assembler. The streaming collector now reconstructs events across arbitrary partial raw
pages; Desktop callsites do not yet publish these events. Native acceptance in
`userspace/apps/metrics-check` covers real SDK/IPC/grant publication, two busy
pages, explicit loss and complete-group recovery. See the trace evidence for
exact build scope and binary identities.

`trace_stream_check` adds 89,443 checks for page splits, partial starts,
malformed/mixed envelopes, replay and explicit endpoint changes. The proof
includes `compositor_trace_stream.adb`; native metrics-check now exercises an
initial orphan group and events crossing page boundaries. Streaming evidence
uses the `stream-*` prefix. The `stream-negative.json` result records a
successfully compiled endpoint-guard mutation that the hosted fixture rejects.
