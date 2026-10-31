# Direct ACPI observer runner

Run `bash tests/acpi-native-server-loop/run.sh --mode checked` or `--mode release`
inside the pinned Nix shell with TMPDIR=/home/doc/cubit-build-tmp. The default is
release. `--validate-only` checks inputs, closed Ada directory inventory and exact
production/API mirrors without compiling.

This replaces the obsolete provider-configure/grant-loop suite. It runs actual
ACPI Requests/Core and the exact production observer endpoint/native adapter and
server, using modeled IPC/log transport and actual bounded Refusal_Notes logic.
The original711 checks plus event-lane cases cover known/unknown/zero badges, denial of upload/receipt mutations,
canonical replies, all metrics pages, malformed envelopes, payload/PID claims,
repeated/refused logging, a full64-bit process identity, lost replies, spurious
wakes, explicit Wait_Forever and termination on unexpected completion.

Strict source ALIs/profile are audited; checked gnata+gnato vs release gnatp. Build180s,
runtime30s, stack64MiB, logs1MiB, nice19/j1. Exact input/mirror/artifact hashes are
recorded. These tests do not authenticate real kernel stamps or exercise native
transport, logger delivery, service registration, hardware or a SPARK proof.

The optional prove.sh targets only the pure observer policy wrapper. Its first
bounded level2 attempt stopped during Global-contract generation on a GNAT
front-end assertion in the existing AML namespace dependency, before producing
flow/proof results. No new observer proof is claimed. Do not automatically retry
or investigate that compiler failure; revisit after a relevant compiler/source
change or explicit user direction. The regular hosted test runner does not run
the prover. The AML core --prove hook surfaces this pending proof as a failure.


Event-lane regression: one event and one IPC opportunity per turn; kernel Stop
precedes new dispatch; readable events are consumed before waiting. Actual system
classification checks full publisher identity, framing and grant generation.
Channel notices reach a modeled registry; logger notices exercise the actual
ACPI_Log_Lifetime policy with a modeled disconnect boundary. Global logger is
unchanged. Native real-log wrapper and current runtime need separate zero-object
semantic validation; this hosted runner does not execute Native_Instance/Main.
Future hardware owners require bounded drain/lease cleanup before stopping; the
current observer loop submits no async work and owns no hardware transaction.


## Native loop bounded owner work

ACPI_Native_Server.With_Dispatch now accepts one bounded serialized owner
step per turn. Kernel Stop and unexpected completion win before owner work.
Pending runnable local work prevents idle sleep; one IPC opportunity remains
between batches. Owner refusal terminates this serving attempt before a new
dispatch. The default observer returns idle and enables no hardware source.
The callback must be bounded/non-reentrant,avoid blocking peer IPC,and retain
cleanup/uncertain state; Pending cannot mean waiting for hardware. These are
caller obligations,not newly proved properties. No automatic replay/reset.

Hosted checked/release actual loop753checks/profile covers pending drain,IPC
fairness,explicit idle wait,refusal,control stop and completion precedence,
and event-before-owner ordering. Startup1228checks/profile/76fresh processes
passes with updated model API. Actual-runtime semantic30selections/29ALIs,
zeroobjects/profile. No AML semantic changes,new ACPICA interpreter run,
SPARK proof,native linking/boot,hardware provider/source enrollment or laptop
validation. Full verified ACPI/AML goal remains incomplete.


The controlled module-startup milestone is recorded in MODULE-STARTUP-EVIDENCE.json.
Its current-source input/closure refresh does not renew historical proof results.
Fresh hosted checks cover both profiles; no native execution or hardware
authority/region connection follows from these tests. The native API gate
selects 59 units and produces 56 ALIs with zero objects per profile.


## Typed service receive preserves queued events

The actual server now uses Poll_Service_Request after its bounded event/owner
turn. Poll_Any_Ipc can consume a second event as a request, bypassing its typed
handler. The old hosted model masked that difference by making mixed receives
service-only. The model now exposes both operations separately. Self-controls
check mixed event consumption and typed event preservation. A two-event/two-
request regression ensures a second queued kernel stop is handled after the
first request and before the second; the old server fails this regression.
Existing event flood, owner fairness and completion precedence checks remain.

Fresh actual-loop checked/release runs pass758checks/profile (1516total), plus
4936 notification/EC owner checks across both AML widths (38runtime processes
combined). The actual-runtime API gate selects60units/57ALIs with zero objects
per profile. No new SPARK proof, native queue/kernel execution or hardware
acceptance. ACPICA iasl compiles the unchanged notification fixtures; no new
ACPICA execution comparison is claimed. SERVICE-RECEIVE-EVIDENCE.json preserves
the exact private evidence; older evidence retains its historical scope.
