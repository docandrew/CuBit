# Service-owned battery observations

Run `bash tests/acpi-battery-owner/run.sh` inside the pinned Nix development
environment, setting TMPDIR=/home/doc/cubit-build-tmp before and inside Nix.
`--validate-only` checks source pins. The runner copies into a disjoint output,
verifies tools/sources/fixtures, builds checked and release profiles with
warnings as errors, validates ALI flags and checks profile result agreement.
Limits: build180s, runtime30s, iASL5s, nice19/j1, stack64MiB, core0, log1MiB.

ACPI_Bootstrap.Batteries and ACPI_Requests.Batteries are trusted generic owner
bridges to the SAME Core used by service requests, EC events and notification
receipts. They reuse the actual battery collector/cache; no duplicate parser or
sample policy is introduced. Multiple caches can bind owner-selected devices
from that Server's namespace. Node metadata grants no IO authority. Collector
callbacks keep their existing charge, ownership, initialization and non-reentry
obligations; native provisioning remains separate.

Bind/Prepare require completed device initialization and enabled notifications.
Not-ready Prepare invalidates without evaluating AML; its Admission qualifies
the default refresh report. Publish consumes the cache's pending update and
fails closed if readiness, queued-event or routing health checks fail. Read
returns only owned data and suppresses all payload while unready, with queued
Core events or with an unseen staged broker receipt. Observe_Claimed uses only
that Server's actual retained event, at most once per receipt/cache. Owner must
fan out to ALL consumers before ACK through With_Notification_Routing. Keep the
Server/cache/receiver lifetime serialized; never reset/replace a live Core.
No wire selector, observer AML call, endpoint/grant, retry or receive loop added.

Hosted tests use real PNP0C0A _STA/_BIX/_BST methods and the real five-stage
collector: identity, presence, information, status, presence confirmation share
one budget. Counted evaluator callbacks show that pending preparation and pure
observer reads do not rerun AML. The real EC receiver executes Q01 changing
remaining capacity with Notify80, and Q02 toggling presence with Notify81.
Two battery caches receive the same actual service broker event. Queued/staged
events hide old bundles; failed fanout holds the receipt; routing invalidates a
pre-routing pending bundle; absence publishes Missing; reinsertion requires a
fresh bundle; zero fuel publishes Failure without the previous payload.

Normal and unconfigured-region lifecycles run in separate processes for both
AML widths. The hardware fixture deliberately has no admitted provider: catalog
completion does not permit battery binding or sampling. Test-only node indices
describe the fixed fixture layout, not production discovery. Find enumerates matching batteries in this same completed namespace; the owner
chooses which returned devices to bind.

`oracle.mjs COMPILED_AML` independently observes _BST, presence and EC-handler
Notify behavior with ACPICA on the exact same fixture. Results retain any exact
recognized four-allocation exit diagnostic and a false clean_tool_log flag;
other ACPI errors fail. This is semantic comparison, not a clean teardown or
validation of our collector/cache/broker implementation.

These are hosted tests, not a new SPARK proof, native execution, laptop battery
validation, atomic hardware snapshot or a whole-system charging decision.
Existing scoped pure proofs retain their original scope. Native discovery wiring,
provider/GPE/source admission, component/receive-loop wiring, typed observer
transport and the full verified AML/service objective remain incomplete.


## Bounded owner discovery

Batteries now takes the actual Identity package plus Store, with formal matching
requiring Store.Dynamic=Identity. Find searches strictly after a snapshot-local
node cursor (Root starts enumeration). It shares one AML budget across identity
queries, never retries firmware, returns Root unless Found, rejects cursors
beyond the current namespace and checks the whole captured topology after each
identity query. The finite namespace bounds scanning/metadata checks separately
from AML charged work. No claim that Charged measures all CPU or wall time.

Missing/nonbattery identities are skipped; exhausted AML returns Incomplete;
other identity failures or topology changes return Failed. Incomplete is not a
resumable query token: a caller must explicitly decide whether to search again.
No binding, presence check, hardware admission or wire authority is implied.

Hosted tests find BAT0 then BAT1 past an unrelated EC, reach No_Match, bind a
returned node to the actual cache, and reject zero budget, bad cursor and early
readiness without evaluating AML. Existing collection/event regressions remain.
Current checked/release each170checks (normal68,hardware17 per AML width),
348source/test pins,14commands. No new proof or native/hardware acceptance.
Fresh ACPICA comparison on this revised exact fixture retains its narrower
status/BST/Notify scope and explicit four-allocation exit diagnostic. It does
not validate our enumeration implementation.


## Bounded owner notification pump

Added ACPI_Requests.Notification_Pump for serialized local owner work. Each
batch has an explicit limit of at most32claims and ACK attempts. Existing
guarded routing runs on both claim and ACK. Partial fanout or ACK fanout
failure retains the exact receipt; retry must invalidate all consumers
idempotently. No AML is re-executed by the pump. Limit_Reached never means
queue empty; Queue_Empty is only an explicit local empty observation. It
does not establish source health,hardware acknowledgement or cache freshness.
The callback must be bounded,local and non-reentrant; that remains a caller
obligation. No native source enrollment or automatic live refresh is enabled.

Hosted checked/release:262 EC/lid/AC checks and180battery checks per profile,
32/64bit AML,14commands per suite. Tests exercise zero limit,unready owner,
partial fanout,retained-receipt retry,ACK fanout failure,batch boundaries,
explicit empty observation and stale-cache refusal. Runtime semantic gate:
30selections/29ALIs/zeroobjects per profile. Startup230file inventory is
metadata-only.20previous source-pin changes explicitly reconciled to
approved current afterimages or unchanged frozen baseline. Initial EC exact
count gate refused114vs old94; corrected catalog and full rerun passed.
Existing fixtures compile with normal iASL; no new ACPICA interpreter run,
SPARK proof,native linking/boot or hardware validation. Full goal incomplete.
