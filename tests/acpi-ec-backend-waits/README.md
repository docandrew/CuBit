# Bounded backend wait supervision

    export TMPDIR=/home/doc/cubit-build-tmp
    nix develop --command bash tests/acpi-ec-backend-waits/run.sh
    nix develop --command bash tests/acpi-ec-backend-waits/semantic.sh

Use the pinned project shell. Behavioral --source-root and semantic
--source-root/--unit-root options support private snapshots. Both hosted profiles
pass2021joined+1063geometry checks,276actual source pins and normal iASL.
Existing AML/startup/lid/receiver/fairness cases remain. New cases connect the
actual catalog,saved-reply owner,dispatcher,clock adapter and wait wrapper with
modeled hardware/clock/pause callbacks. No replacement Core is introduced.

ACPI_EC_Backend_Waits.Advance performs at most one bounded pause and one owning
adapter step. Nonwait work uses the ordinary dispatcher. A pause return never
proves target reached: Progress.Advance_Waiter forces one owning engine clock/
budget check even if advisory wake remains future. Original minimum-delay,
deadline,ownership and at-most-one-IO guards stay inside the unchanged engine.
Now is the owning controller's current same-epoch clock observation; the engine
samples its own clock before IO. No internal polling/receive loop is added.

Pause budget exhaustion/refusal/clock failure requests stop,then advances the
same owner once. Stop is a request,not cancellation: already written work must
drain or remain retained for recovery. Each caller tick stays bounded,and the
original finite engine work budget still advances if time stops. Preserve the
separate pause budget through one admitted operation; do not reset on retries.
The native outer loop must drain IPC between ticks,handle idle waits explicitly,
retain leases and supervise faults. This does not register or launch that loop.

In the advancing-clock test,two bounded pauses lead to a successful Field read
and exactly one saved reply. In the frozen-clock test,two pause callbacks return
without advancing time; exhaustion requests stop and engine work reaches
Budget_Exhausted. Only initial status+command touched hardware; no early address/
data access occurs. Saved reply completes once,No_Work retains hardware recovery.
That model instance intentionally retains its recovery obligation; no global
quiescence or hardware reset is claimed. Per-tick EC access count is at most one.

Callbacks must return normally,remain bounded/nonreentrant,share clock epoch,
preserve controller identity and obey pause-budget semantics. These facts and
physical behavior are assumptions. No new SPARK proof is claimed. Existing
selected engine/event-policy/comparison proof inputs are unchanged.

The new246-source semantic profiles use222actual runtime/capability specs,the
real native clock pause callback and default saved-reply callbacks. Strict -gnatc
and ALI gates,zero object/binder output,no link/execution. native-semantics.json
records original input/command provenance. Older backend-progress/clock semantic
records describe their original snapshots and are historical for the changed
dispatcher; they are preserved,not silently repinned. The new check is current.

ASL fixture is byte-identical to the prior independent ACPICA startup/lid
reference. That reference was not rerun and covers method semantics,not wait
supervision,physical clocks or IPC. Native admission,receive routing,launch,
hardware acceptance and whole AML/service verification remain unfinished.


## Extended wait-outcome composition

The joined matrix now runs advancing,frozen,refused,clock-unavailable,
clock-representation-limit and due-during-pause scenarios through the actual
clock adapter/catalog/saved-reply/dispatcher/wait owner. Checked/release each
pass2138joined+1063geometry checks. Production code is unchanged.

Unavailable/range faults arise during the pause's fresh read after initial
status+command IO,and stay latched through the engine's own fresh read. Stop
is requested,one saved reply reports Clock_Unavailable and recovery remains
retained. Pause_Report keeps unavailable versus representation-limit detail.
No raw pause or early address/data IO occurs. Refused pauses exhaust their
finite episode,then the original engine work budget; one reply and recovery
remain. A target that becomes due between advice and pause read makes no raw
pause,does not request stop,and still finishes the Field read once.

Every scenario checks at most one EC access per tick,one reply,No_Work versus
retained recovery and the exact pause budget. Failed model instances deliberately
retain recovery; no global quiescence or hardware reset is claimed. Observation
wrappers inject clock outcomes around actual policy/adapter code; hardware and
clock behavior remain modeled. The existing complete AML/startup/lid/fairness
regressions remain. Fixture unchanged; prior independent ACPICA reference and
native semantic records retain their original scopes. No new native execution,
SPARK proof,production change or hardware acceptance is claimed.


## Current dependency closure recheck (2026-10-29)

The behavioral catalog now includes the device-status and resource-name helper
specifications and bodies required transitively by the current lid/namespace
code. No production or test assertions changed. Fresh checked/release runs pass
2138 joined plus 1063 geometry checks per profile (6402 total, four runtime
processes). ACPICA iasl compiles the unchanged fixture; this is not a new
ACPICA execution comparison. Hardware, clock and IPC callbacks remain modeled.
No new proof, native execution or hardware acceptance is claimed.


## Bounded backend controller

The optional ACPI_EC_Backend_Controller owns finite request deadlines and one
pause budget per accepted Field/query transaction. Invalid policy, unavailable
clock and deadline overflow refuse before saving/submitting work. Busy/refused
calls and queries never replenish an active pause budget. A failed outer clock
sample requests stop and gives the existing owner one fresh engine check; it
does not claim cancellation or release recovery. Sender/request inputs remain
exact authenticated kernel metadata, and query sources require independent
admission. All actual packages must be the same exclusively serialized owner.

Fresh checked/release runs each pass2296joined+1063geometry checks (6718 total,
four runtime processes), including cold stop, busy query, no-event query and
retained recovery. Hardware/clock/IPC remain modeled. The updated semantic main
instantiates the controller and its default reply callback against222actual
runtime specs/248sources in both -gnatc profiles, with zero object output.
controller-native-semantics.json records that snapshot; native-semantics.json
remains historical for its earlier semantic main. ACPICA iasl compiles the
unchanged fixture; no new independent ACPICA execution comparison is claimed
for this controller milestone. No new SPARK proof, native execution or hardware
acceptance. Native backend launch, named-register provisioning, complete EC
ownership/Global Lock, real clocks, receive/event draining and teardown remain
independent integration obligations. The AML service remains snapshot-only.
