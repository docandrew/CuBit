# Bounded, self-describing compositor metric batches

`Compositor_Metric_Batch_Policy` tracks the successful appends to the current
SDK filling page. It stores only a record count and the first append timestamp;
it adds no record queue or page buffer. The existing metrics SDK retains its
two 4 KiB payload pages. The metrics-enabled Desktop adapter now uses this policy;
the complete aligned native SDK publisher object reserves 16 KiB.

Every page begins with the output-0 and output-1 declarations from
`Compositor_Release_Metrics`, followed by at most 61 measurements. This makes
each batch independently usable after collector source eviction or a lost
earlier declaration batch. Metadata is not itself counted as a measurement.

The policy declares a nonempty measurement batch due when the page is full
or 100 ms have elapsed since its first append. A missing/backward flush clock
also makes it due, avoiding indefinite retention. Metadata-only pages are not
submitted. The 100 ms value is an initial collection interval, not a rendering
delay, guaranteed export deadline or hardware-tuned optimum. The caller still
must pump at suitable event-loop boundaries and arrange an idle wakeup.

The native adapter must obey these integration obligations:

- Advance policy only after the actual SDK accepts an append. Failed appends
  preserve the next required declaration or measurement position.
- If both SDK pages are busy, drop/count the offered measurement without
  attempting spurious declarations. Never wait for a completion.
- Reset policy only after successful SDK sealing/submission. Preserve state
  and stop publication on a failed or ambiguous nonempty submission.
- Use Desktop's global non-reused token allocator and bounded completion
  routing. Only a validated release makes an SDK page writable again.
- Stop attempts when the publisher is disabled; do not spin or allocate tokens
  repeatedly for a collector that has failed.

The updated SPARK policy proof discharged 21 checks: one initialization, nine
runtime, four functional contracts and seven termination checks; zero
unproved/justified. This includes remaining-delay and rounded/saturating
millisecond wake arithmetic used by Desktop's idle wait.
This covers the policy state transitions, capacity and decision functions,
not the above service-loop obligations or native IPC. The first proof reported
an intermediate-state predicate failure during separate count/timestamp writes;
the implementation now assigns the state atomically, and the rerun discharged
the check.

```sh
nix develop -c gprbuild -q -p -P tests/compositor/metric_batch_policy.gpr
nix develop -c tests/compositor/build/metric-batch-policy/metric_batch_policy_tests
nix develop -c gnatprove -P tests/compositor/metric_batch_policy.gpr -u compositor_metric_batch_policy.adb --level=2
nix develop -c gprbuild -q -p -P tests/compositor/metric_batch_stream.gpr
nix develop -c tests/compositor/build/metric-batch-stream/metric_batch_stream_tests
```

Hosted policy tests exercise 1,000 full batches, exact interval boundaries,
missing/backward clocks and near-maximum timestamps. The stream fixture uses
the real portable SDK page builder, codecs and compositor metric declarations.
It fills both pages with 122 measurements plus four declarations, withholds
completion, then drops 878 more measurements while comparing both retained
pages word-for-word. Releasing page 2 first admits a new independently declared
batch, keeps page 1 unchanged, and publishes the accumulated drop count in the
next header. The fixture supplies the release directly; it does not validate
kernel completions or execute the native SDK adapter.

Evidence, 2026-10-01: corrected proof/tests 87452 completed with exit 0,
`/tmp/cubit-metric-batch-policy-v2.log`; stream fixture 5800 completed with
exit 0, `/tmp/cubit-metric-batch-stream.log`. Initial proof 52423 had one
unproved predicate check despite its zero process exit status. No native
publisher integration or supported-hardware performance claim follows.
