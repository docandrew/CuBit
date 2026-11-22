# Service-owned power-source observations

Run `bash tests/acpi-power-owner/run.sh` in the pinned Nix environment with
TMPDIR=/home/doc/cubit-build-tmp set before and inside Nix. The runner uses
disjoint output directories, exact source/tool/fixture hashes, actual ALI
profiles,180s builds,30s tests,5s iASL,nice19/j1,64MiBstack,core0 and1MiB logs.
`--validate-only` verifies the manifest without running functional tests.

Actual Requests/Bootstrap/Core state discovers the first exact ACPI0003 device
only after successful device initialization. The service-selected evaluator
invokes only fixed _HID/_PSR members through the existing typed consumer; no
mutable Core, arbitrary method or register address escapes. Multiple-source
selection and whole-system AC/battery policy remain separate work. PSR0 means
this source is offline,1online; other integers and wrong result types are
Unknown. Online does not establish physical cable presence.

Prepare evaluates once into owned private data; notification fanout precedes
Publish. Reads copy cached observations without AML effects. Two local caches
exercise queued and retained-event freshness, same-owner staged receipt routing,
deduplication, no busy/publication/read replay, and ACK ordering through
ACPI_Requests.With_Notification_Routing. Failed fanout preserves the broker
receipt/revision. All local consumers must participate before ACK. Each cache
belongs to one serialized Server for its lifetime and uses one clock epoch/unit.

The real AML fixture counts PSR calls, notifies on its third call, returns2 on
its fifth and a named String on its sixth. Invalid results retain diagnostics
but cannot become valid online/offline cached samples. Budgets, missing/regressing
clocks, expiry, revision ceiling and an unconfigured EmbeddedControl declaration
are covered. Normal/ceiling cases75checks each,hardware15:330checks/profile across
AML revisions1/2. The declared region is never physically accessed.

This is hosted behavior, not full SPARK proof, native execution, real SCI/GPE/EC
handling, device presence guarantees or laptop validation. Semantic-only checking
uses the actual native-clock/runtime API snapshot and emits no objects. Native
provider provisioning, event-loop hooks, cached-result IPC/subscriptions and
power policy routing remain integration work. The initial private direct-String
PSR fixture was rejected by normal iASL6105; that diagnostic is retained. Current
fixture compilation uses normal options without forced AML or error suppression.
