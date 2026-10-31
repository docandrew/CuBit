# Backend microsecond clock and bounded pause adapter

    export TMPDIR=/home/doc/cubit-build-tmp
    nix develop --command bash tests/acpi-ec-clock/run.sh
    nix develop --command bash tests/acpi-ec-clock/semantic.sh

Use the pinned project shell. Behavioral runner accepts --source-root DIR;
semantic runner accepts --source-root DIR and --unit-root DIR. No native
component,launcher,capability,kernel or runtime change is made.

CuBit.Monotonic.Read may use a published microsecond clock sharing the IPC
millisecond epoch,or fall back to HPET's different epoch. Dividing that adaptive
value by1000 is not an established conversion to an IPC deadline. The new
ACPI_EC_Native_Clock therefore consistently uses existing kernel raw-microsecond
read and absolute sleep calls,whose source uses the same Time.Read_Monotonic
clock. It never converts to IPC milliseconds or changes the existing AML clock.
This is source-reviewed epoch pairing,not hardware or cross-suspend validation.

ACPI_EC_Clock_Policy validates unsigned raw readings before conversion to the
EC63-bit nonnegative time type. Unavailable and representation-limit observations
remain distinct; zero is a valid sample. Stateless Slice avoids addition overflow
by comparing remaining distance first. ACPI_EC_Clock_Adapter provides fresh Read,
last-read diagnostic status and at most one raw pause per Pause_Until call.

The native default caps requested pauses to1000microseconds ahead,and never
past the supplied target. Scheduling may return later; no wall-clock latency
bound is claimed. Pause_Returned means the raw call returned normally,not that
time reached the request. Re-read time and let the transaction engine check its
own deadline,ownership and budget before IO. Already-due/unavailable/invalid
clock paths do not pause. Last_Status describes the latest read,not current
clock health or proven monotonicity. Engine still detects reversed clocks.

Every positive-budget pause call consumes one independent attempt. Exhausted
budget performs no clock or pause callback. A stopped clock can therefore exhaust
an episode instead of causing unbounded retries. The outer owner must preserve
Remaining across the episode,never reset it automatically,and handle exhaustion/
refusal/clock failure explicitly while retaining reply/hardware obligations.
Exhaustion is not transaction completion or quiescence. The engine's work budget
is separate. This library does not implement that supervisory receive loop.

Both hosted checked/release profiles pass70056checks over actual policy/adapter
code with modeled raw read/pause callbacks. Covers raw ranges,0,max63-bit values,
near-maximum slices,deadline equality,unavailability,refusal,zero budget and a
stopped clock with finite attempts. Checked postconditions execute. SPARK_Mode
and tested assertions are not new solver proof. Callback normal return,boundedness,
same epoch,truthfulness and absence of reentry/budget aliasing remain assumptions.

The separate semantic profiles instantiate the real native wrapper and dispatcher
against222actual runtime/capability specs,244sources total,default saved-reply
callbacks and actual opaque PID types. Strict -gnatc/ALI checks,zero object/binder
output,no link or execution. native-semantics.json preserves original source
origins/hashes/commands. No behavioral kernel-call,physical timing,stack,launch
or hardware acceptance is claimed. Existing engine/policy/comparison proof inputs
remain unchanged. Full verified ACPI service and native integration stay open.


## Successor bounded wait supervision

ACPI_EC_Backend_Waits combines one bounded pause and one owning adapter check,
so a stopped clock cannot defer engine budget forever. Pause failure/exhaustion
requests stop without claiming cancellation or clearing recovery. New2021joined+
1063geometry/profile cases cover advancing/frozen clocks and exactly-once saved
reply completion. See tests/acpi-ec-backend-waits for assumptions/integration gaps.
Its246-source actual-runtime semantic gate covers the changed dispatcher and
real clock callback. Earlier native-semantics records remain historical snapshots
for that dispatcher; no old hash record is relabeled as current. No new proof,
native launch,hardware execution or recovery reset is claimed.


## EC clock policy contract milestone

The pure EC clock policy now states and proves exact timestamp conversion/refusal and the exact bounded-slice result. SPARK levels 1/2 each discharge14checks (10solver,4flow), with no skipped work, pragma Assume or external claim dependencies. The portable tests/acpi-ec-clock-proof suite audits exact entities/checks and rejects20altered reports per level. This proof covers Decode/Slice under the declared types/precondition, not physical time, callback behavior, the whole interpreter or native/hardware execution.

Fresh checked/release hosted clock, receive and backend-wait regressions pass147374checks; semantic-only checking covers244sources/222actual runtime specs with zeroobjects in both profiles. Historical native-semantics records remain historical; current declaration evidence is /home/doc/cubit-build-tmp/ec-progress-semantic-Llt3pC/COMPLETE.json. Runtime behavior is unchanged; only contracts and a redundant body visibility declaration changed. Native authority/launch/region/lifecycle integration and the full verified service/interpreter remain unfinished.
