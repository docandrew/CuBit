# Service-owned EC event dispatch

`bash tests/acpi-ec-owner/run.sh` runs hosted checked/release tests of the actual
Requests-owned Bootstrap/Core, EC receiver/method executor, lid caches and
notification receipt broker. Set TMPDIR=/home/doc/cubit-build-tmp before and
inside the pinned Nix environment. `--validate-only` verifies the source catalog.
The runner copies inputs into disjoint private outputs, pins source/tools and
fixtures, compiles with warnings as errors, and checks ALI profile switches.
Limits: nice19/j1, build180s, runtime30s, iASL5s, stack64MiB, core0, log1MiB.

ACPI_Bootstrap.EC_Events and ACPI_Requests.EC_Events are trusted serialized owner
bridges. They keep event execution in the SAME private AML Core used by the
existing lid/power caches and broker. Install/Dispatch require Complete plus
Devices_Completed; catalog completion alone is insufficient. Not-ready delivery
does not consume a token or Delivery. Close remains callable without readiness
and delegates retained-result ownership checks. Owner operations do not consume
Requests revisions; the existing receipt protocol still owns its revisions.

Enrollment selects a current node from the owner's completed snapshot. A node
number is not a capability: the owner must independently establish PNP0C09,
resources, provider readiness, source provenance and authenticated full-width
sender metadata. No wire selector, hardware grant, arbitrary method invocation
endpoint, receiver reset or readiness override is added. Keep the Receiver,
Server and deliveries together for their lifetime and preserve source FIFO order.
Receiver.Revoke remains the existing trusted route-revocation operation.

The normal fixture has no hardware region. Its Q01 increments lid state and
issues Notify(LID0,0x80); it tests actual event-to-cache integration without
pretending to validate EC transport. Both 32-bit and 64-bit AML widths run in
fresh processes, each with 94 normal and 16 blocked-hardware checks. The separate
hardware fixture contains an unconfigured EmbeddedControl region: enrollment
and dispatch remain barred even after snapshot completion. Native provider/_REG
startup is tested elsewhere and is not supplied or bypassed here.

The normal lifecycle checks: not-ready delivery reused after readiness; full
process generation and epoch rejection; exact local Q01; duplicate suppression;
Core-queued and broker-staged events hiding stale closed samples; failed fanout
preserving the receipt; fanout to two caches before ACK; fresh open samples;
observer reads without AML; missing handler and zero-budget consumption without
Q01 effects. The fixture's first user node follows seven predefined root nodes;
its test-only index is not a production discovery policy.

`oracle.mjs COMPILED_AML` independently observes LID/PSR values 0,0,1,1,2,0 and two notifications each for lid and AC
with ACPICA. The current pinned ACPICA build also reports four outstanding cache
allocations at exit. The oracle records this exact diagnostic and a false
clean_tool_log flag; it rejects other ACPI errors. This is a semantic comparison,
not a claim of a clean ACPICA teardown or validation of our broker/cache code.

These are hosted functional checks, not new SPARK proof or native/hardware
acceptance. Existing pure EC policy proofs retain their recorded scope; the full
receiver/interpreter proof remains unfinished. Native main/component wiring,
authenticated source admission, EC transport/leases/GPE routing, lid hardware,
typed power events and suspend policy remain integration work. This bridge adds
no polling loop, receive loop or synchronous upward IPC.


## Mixed lid and AC-source notification batches

The current fixture also contains ACPI0003 with fixed _PSR. Q02 toggles the
source and notifies AC; Q03 changes both lid and source and queues two distinct
notifications. Three actual Requests-owned consumers (two lid caches and one
source cache) receive each retained event before ACK. Matching events invalidate
the target cache; an unrelated cache retains its existing sample after routing.
All caches suppress freshness while any queued or unseen staged event remains.

A source sample collected between the two batch receipts is refused as stale
after the second event is routed. A new collection publishes the final offline
state. Duplicate Q02 cannot toggle the source twice. Tests preserve the previous
readiness, sender/epoch, failed-fanout, missing-handler and zero-fuel checks.
Checked/release each220checks across both AML widths;344source/test pins.

The independent exact-fixture ACPICA trace checks LID/PSR 0,0,1,1,2,0 and
two Notify(0x80) observations for each target. Its previously recorded exact
four-allocation exit diagnostic remains explicit; clean_tool_log is false.
Reference scope is firmware method semantics, not our broker/cache code.
No production code, new proof, native execution or hardware validation is
added by this regression expansion. No battery sample or power policy claim.


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
