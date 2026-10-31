# Service-owned lid cache

Run `bash tests/acpi-lid-owner/run.sh` inside the pinned Nix environment, setting
TMPDIR=/home/doc/cubit-build-tmp before and inside Nix. `--validate-only` checks
source pins without compiling or executing. The runner creates disjoint TMPDIR
outputs, verifies source/tool/fixture hashes, enforces actual ALI profiles and
runs each lifecycle in a fresh process. Limits: build180s, runtime30s, iASL5s,
nice19/j1,64MiBstack,core0 and1MiB command logs.

ACPI_Requests.Lids is an internal generic owner bridge. It uses the actual
Requests-owned Bootstrap/Core with a service-selected fixed-method evaluator.
It discovers PNP0C0D only after device initialization. Prepare invokes _LID once;
Publish makes a copied sample available only after healthy notification routing.
Read never invokes AML. Missing/failed/stale clock or sample outcomes are Unknown,
never an invented closed lid. Firmware method evaluation may have effects and
requires owner authority; this is not an observer invocation endpoint.

Observe_Claimed routes the same Server's actual retained notification, at most
once per active receipt, without claiming, draining or acknowledging it. All
local consumers must receive that event before the owner acknowledges its broker
receipt. Read suppresses stale freshness while the Core FIFO is nonempty or an
active receipt is unseen, including after Claim emptied the FIFO. Publish also
refuses an unseen staged receipt. Keep each Cache with one Server for its whole
lifetime; serialize all mutation and use one monotonic clock epoch and unit.

The hosted fixture's real _LID method counts calls and emits Notify on its third
invocation. Two cache consumers demonstrate queued and staged-event freshness
suppression, duplicate-receipt handling, stale publication, broker receipt
preservation, no busy/read/publication replay, budget failure, unknown/regressing
clocks and expiry. An unconfigured EmbeddedControl region blocks active binding;
no physical register access occurs. A generation-ceiling lifecycle verifies
cache operations do not consume Requests revisions. Both AML widths are used.

These tests do not establish full SPARK proof, real SCI/GPE/EC delivery, kernel
IPC, native startup, native clock/allocator behavior, laptop compatibility or
hardware validation. Native event-loop fanout, typed cached-result messages,
provider provisioning and power-policy routing remain integration work. Existing
Bootstrap cache and AML source proofs retain their original recorded scope.

The suite also composes ACPI_Requests.With_Notification_Routing with the actual
lid consumers and retained notification protocol. Ten invalid/unauthorized ACK
cases retain normal outcomes without routing. Failed local fanout returns
Incomplete without discarding the receipt or advancing revision. Successful
Claim routes immediately; successful ACK routes before discarding the receipt.
Repeated ACK does not route or mutate again. The native owner must use this
wrapper on ALL relevant requests and supply bounded idempotent fanout to ALL
local consumers; no bypass ACK or reentrant/custom protocol handler is allowed.
Normal/ceiling cases71checks each,hardware15:314checks/profile across both widths.
Callback failure and retry remain explicit; no AML, IPC or reset is permitted
in the routing callback. This adapter adds no wire selector or native binding.
