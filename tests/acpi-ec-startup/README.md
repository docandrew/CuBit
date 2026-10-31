# EC startup and event integration

Run with the pinned CuBit Nix shell and TMPDIR=/home/doc/cubit-build-tmp:

    bash tests/acpi-ec-startup/run.sh

The portable runner supports --source-root DIR and --validate-only. It pins
268 actual input sources and three tests; it uses normal iASL and actual
Core/AML/device/region initialization,EC Field remote provider,transaction
catalog,native saved-reply owner and event receiver. No replacement Core is used.
Hardware,IPC authentication,clock,exclusive ownership and queue behavior remain
modeled. It does not add a native component or grant firmware-declared access.

Startup case: the trusted controller installs named EC cell backing and the
matching provider/Field binding. Device_Initialization.Begin_Initialization
executes the root prepass. Complete_Initialization calls the actual region
barrier; its _REG(EmbeddedControl,1) reads LIDS through the EC protocol. The
following _STA/_INI walk sees that effect; later query/_Q01/Notify/_LID also
passes. The callbacks share the initializer's remaining AML budget.

An independent fully loaded service instance with unavailable backing cannot
invoke _REG or touch hardware. Its failed session cannot retry. The successful
service uses one initialization/region session; repeated calls cannot replay
_REG,_INI or hardware operations. Startup_Context.Provider_Installed is a
trusted test observation after admission,not a production readiness oracle.

Checked/release each pass 1753 joined checks and1063geometry
checks. Existing event identity,replay,cleanup and uncertainty regressions stay
in this suite. No new SPARK proof is claimed; prior scoped policy proofs do not
prove this composition or the complete interpreter.

Independent reference (after run.sh emits its private output path):

    bash tests/acpi-ec-startup/oracle.sh OUTPUT/ec-fields.aml

ACPICA uses repair-disabled explicit root/_REG/_STA/_INI calls,simulated
zero-filled EC storage and the exact normal-iASL fixture. Expected integers:
16,1,15,2,0,1,0,0; LID0 Notify0x80. This checks AML fixture semantics,not ACPICA
automatic initialization ordering,physical EC behavior or native event delivery.
The raw reference log retains four outstanding ACPICA cache allocations at
shutdown,also present in the earlier alias-method fixture reference. Functional
acceptance is not a clean-shutdown or leak-free ACPICA claim.

Native integration still needs independently admitted controller backing,
firmware sharing/GlobalLock,clock conversion,a launched backend and authenticated
event/receipt routes. Early ECDT bootstrapping and namespace _HID/_CRS discovery
must establish the initial EC binding without assuming providers are already
ready. This test starts after that admission boundary; it does not solve it.
The existing snapshot-only native service still reports hardware-pending.

The future native controller should emit bounded logstore+serial stage lines
after admission,region connection and device initialization,and one final
summary. Include the exact typed failing phase once; no per-poll logging.


## Catalog progress scheduling

Registers.Next_Progress(Now) is a serialized,read-only advisory observation of
the catalog's single accepted operation. No_Work means no catalog advancement
or uncollected completion,Run_Step means call the owning adapter's bounded
Advance,Wait_Until carries a monotonic microsecond timestamp no later than the
accepted request's original deadline. It exposes no hardware or clock callback.
Nonwait recommendations have At_Time=0. Repeated observation does not move wake
time,consume work budget or perform IO. Newly accepted work replaces the cached
deadline only after Engine.Start succeeds.

After writing,Run_Step allows the engine's fresh postwrite timestamp sample.
The next observation exposes its earliest safe poll. At/after deadline,Run_Step
allows timeout/collection rather than sleeping past it. Terminal completions
also need the owning route's Advance/Collect; calling Step alone cannot deliver
them. Field saved-reply and query/event owners must remain serialized and use
their own completion routes. Engine checks its own fresh clock,ownership and
budget even if an advisory observation was stale or ignored.

No_Work is catalog-specific,not global quiescence. Delivered hardware recovery
can remain occupied and retained while Next_Progress says No_Work. Saved replies,
pending publication/receipts,AML roots and Field uncertainty have independent
lifetimes. Pending publication work must run even when catalog progression is
empty. Recompute advice after incoming work; OS wait/deadline conversion,fairness
and polling cadence remain the native owner's job. This adds no native loop,
launcher,capability or hardware access.

Hosted startup/Field/native-reply/event integration now drives virtual time from
these recommendations:1868joined+1063geometry checks per checked/release profile.
Original EC suite passes24375engine+1043catalog+645query checks per profile.
Deadline/wake tests at ordinary and maximum clock values verify no IO on repeated
observations,immediate terminal collection,and No_Work beside retained recovery.
No new proof or native/hardware execution claimed; existing engine source and
its proof inputs are unchanged.
