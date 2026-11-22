# Metrics publisher prerequisite for Desktop integration

The current `CuBit.Metrics` adapter has two integration defects:

1. A matching token, success status and valid reply tag can recycle a page even
   when `Completion.valid` is false. With both pages in flight, the hosted test
   observes that invalid completion making room available again.
2. After a failed submission disables the publisher, `Has_Room` still reports
   room and `Put` accepts records. This contradicts the disabled-drop API and
   can hide loss from a producer that checks the returned acceptance flag.

`test-metrics-publisher-boundary.py` compiles the complete current adapter body
and spec with the real portable record, protocol and batching packages. Only
IPC and grant operations are mocked. Separate executions reproduce invalid
completion, disabled-room and disabled-Put failures. Normal page completion
and unrelated-token rejection pass before and after the fix. The disabled-Put
case checks 1,000 dropped records and no further submission.

```sh
nix develop -c python3 tests/compositor/test-metrics-publisher-boundary.py --expect-known-bugs
```

The proposed `metrics-publisher-boundary.patch` is **not applied** to the shared
runtime. It checks completion validity before page recycling, rejects Put when
disabled, and includes those drops in the existing saturating public count.
It adds one 64-bit counter and no pages, allocation, IPC or wait. A disabled
publisher never resumes, so these terminal drops are exposed through `Dropped`
rather than a future submitted batch. Keep the publisher object alive while
its uncertain grants remain outstanding.

To test the candidate without modifying shared sources, copy the two adapter
files into a private directory, apply the patch there, and run:

```sh
nix develop -c python3 tests/compositor/test-metrics-publisher-boundary.py --adapter-dir PRIVATE_DIRECTORY
```

On 2026-10-01, session 64787 passed the before/after checks. Evidence:
`/tmp/cubit-metrics-publisher-before-after.log`; candidate files are in
`/tmp/cubit-metrics-publisher-candidate`. Session 7952 also compiled the
candidate body with the pinned Alire native compiler and real CuBit runtime
interfaces, using private objects and the shared build lock. Evidence:
`/tmp/cubit-metrics-publisher-native-compile.log`. `git apply --check` passed
against the shared sources. The initial hosted harness failed to compile a
mock tag aggregate with mixed field types; that test fixture was corrected
before either reported pass.

These checks do not prove kernel completion authenticity, service grant return,
all reply-payload validation, concurrency or physical transport behavior. The
adapter itself is not SPARK-proved. Applying the owner-reviewed patch, rebuilding
the runtime and consumers, and exercising native failure paths remain required
for unguarded SDK callers. Desktop now has a separately tested
[caller boundary](desktop-metric-publisher.md) that rejects invalid completions
and never appends after disable/quarantine, avoiding these defective paths.
The original publisher's documented unique live-token requirement remains a
caller obligation.
