# Native grant transport probe

## Launcher and saved-reply broker probe

`GPU_Launch_Probe` exercises `Intel_Render_Launch_Client` and
`Intel_Render_Broker` over native kernel IPC in the private workspace. It uses
a separate launcher process and co-locates the synthetic GPU and broker in the
server; it is not an Intel emulator or production bootstrap policy. Private
procmgr hooks install the tagged launcher endpoint61, server self-control31,
and fixture-only CSPACE authority58. The synthetic server's first-request
binding must never be copied into production.

Earlier tests retain recipient slots40..44. The probe consumes failed launch
reservations using a non-grantable source, then installs its broker source at55;
five inactive synthetic GPU reservations move its actual recipient to45.
The application destination is38. Pressure-test reply slots skip self-control31
and use62 instead. These disjoint reservations preserve the old test caps,
rather than deleting or overwriting them to make the new test pass.

The expected marker is `GPU-LAUNCH-IPC PASS saved reply and reciprocal admission`.
It requires real saved-reply delivery, reciprocal endpoint installation,
activation, launch completion validation, endpoint identity inspection and
kernel rejection of onward delegation. Existing admission and full async-ipc
regressions must also pass. No Mesa rendering or cache coherence is exercised.
The first run `/tmp/cubit-launch-native.U3kP7pXs/` failed on fixture slot
collisions; the corrected run uses `/tmp/cubit-launch-native-fixed.lO21IGrp/`.

Hosted launch-side regressions are reproducible with
`alr exec -- gprbuild -p -P ../tests/mesa-anv/memory-fixture/launch_client.gpr`
from `kernel/` inside `nix develop`, then run
`tests/mesa-anv/memory-fixture/build-launch-client/launch_client_tests` from the
repository root. Hosted success is separate from the native marker above.

The admission fixture also exercises memory discovery through the production
`Native_GPU_Query.Execute` FFI. Its synthetic server dispatches device-query
label `0x0A20` to `Intel_GPU_Device_Query.Respond`, advertising explicit-WB
maintenance. Selector3 must return `[0,1,1,0]`; selector4 travels over IPC but
returns `[2,1,0,0]` (private-VM policy unavailable in this synthetic service).
Selector5 must be rejected locally and clear all output words. Require `TEST: PASS GPU-MEMORY-QUERY-IPC`
in addition to the runner's existing checks. This verifies native capability
IPC and the query ABI, not Intel hardware coherence. Production Mesa coherent
allocation remains disabled.

`GPU_Admission_Probe` uses the production async admission adapter and control
core with a synthetic server at label `0x0A21`. Native IPC reserves a session;
the unprivileged client's endpoint delegation must be denied by the kernel,
after which the adapter sends Abort and drains its completion. Activation is
not reached in this negative test. The reply destination is poisoned before each poll;
the full sender ID must authenticate and reserved bytes 81..87 must be zero.

On 2026-10-01 this exposed a completion ABI mismatch: the kernel used its
narrow internal ProcessID while userspace consumed a 64-bit sender. Both now
declare the same 88-byte layout, and the kernel explicitly zeroes its reserved
tail. The private snapshot rebuilt kernel and test apps; run
`/tmp/cubit-completion-abi.Ug8OqV/` exited 0 with GPU-ADMISSION-IPC PASS,
GPU-BUFFER-IPC PASS (128 cycles), GPU-BUDGET-IPC PASS, baseline async-ipc PASS,
and final fault scan success. Tested changed sources compare equal to main.
This verifies native CuBit IPC in QEMU, not positive delegation, production
startup integration, Mesa device creation, or Intel hardware rendering.

Private test hooks add `../../lib/display` and `../../services/intel-gpu` to
both IPC app source directories, call `GPU_Admission_Probe.Client` after the
budget probe, and dispatch `0x0A21` to `GPU_Admission_Probe.Server` before the
other test labels. Main production startup and public admission remain closed.

`Authorized_Client (31)` adds the positive path using the same native adapter
and control core. Private procmgr setup grants IPC-test consumers self-scoped
CAP_CSPACE/GRANT in slot30 and the test server endpoint READ|WRITE|GRANT in
slot31, preserving the ordinary manifest endpoint's authority tag. This is
private test policy, not a production manifest permission or kernel bypass.
The ordinary source remains non-grantable, so the negative test still aborts.
The server additionally dispatches status label0A2F. The positive client must
reach Active, use its READ|WRITE-only slot32 for a session status call, fail to
delegate that slot onward, abort, then receive Denied using the retained slot.
No GPU allocation or submission occurs in this synthetic service.

The reciprocal-delegation adapter now additionally requires a READ|GRANT
endpoint naming the client's captured incarnation in broker slot37, and
driver-scoped CSPACE authority (slot34 in the private setup). It first installs
that endpoint READ-only in driver slots40..55, then installs the render
endpoint in the client. These driver slots must be reserved exclusively; do
not run the older IPC pressure fixture that also writes slots30..45 alongside
this version. Cross_Process uses the existing grantable server endpoint as
the recipient source because that fixture combines driver/client roles.
The historical runs below predate this reciprocal step; their PASS markers do
not validate it.

Reciprocal native run on 2026-10-01:
`/tmp/cubit-reciprocal-current.sZ4sWkoT/` completed with runner exit0, all four
admission PASS markers, GPU-MEMORY-QUERY-IPC, baseline async-ipc and final fault
scan success. Private procmgr supplied endpoint37 and driver CSPACE34; private
server reply-pressure slots moved to24..39, leaving recipient40..55 immutable.
Activation used the actual kernel endpoint inspection, not a mocked ready bit.
All12 tested broker/control/query/probe source files compare equal to main.
The first attempt `/tmp/cubit-reciprocal-admission.M40X4f45/` exited1 because
the snapshot retained old selector3-only query code; admission passed, but that
run is not a passing regression. Current query dependencies were synchronized
before the successful rerun. This validates native CuBit delegation/completion
in QEMU with a synthetic GPU service, not production startup, Mesa rendering,
cache coherence, or physical Intel hardware.

Authorized run on 2026-10-01: `/tmp/cubit-authorized-admission.Q4eQty/`
completed with runner exit0 and both admission PASS markers, baseline IPC and
buffer/budget regressions, and the final fault scan. Private procmgr and probe
apps were rebuilt. Positive delegation here targets the broker's own process;
cross-process startup supervision and production driver activation remain
unverified and are not implied by this test.

`Authorized_Client (31, Cross_Process => True)` targets the captured server
incarnation instead of the broker itself. Private bootstrap additionally grants
the broker a CSPACE capability scoped to that server in slot34. Delegation uses
server slot59 (not slots30..45 reserved by the IPC pressure test). The test-only
0A31 control asks the recipient to submit a status request through that endpoint
and drain its own completion; the service therefore sees the recipient's real
kernel-stamped PID and delegated tag. It repeats the request after broker abort
and requires Denied. This combines driver and recipient roles in one process;
it is not a separate production application or startup-supervisor integration.

The first cross-process run (`/tmp/cubit-cross-admission.oKK4Rp/`) passed its
new marker but failed the baseline pressure test because it incorrectly used
server slot32. Its final runner exit1 is a failure, not a passing regression.
Corrected run `/tmp/cubit-cross-admission-fixed.KsM6Jq/` completed with exit0,
all three admission markers, baseline async-ipc PASS and final fault scan.
The tested probe body compares equal to the main checkout.

`Dispatch_Client (31)` uses the bounded production dispatcher with two
concurrent admissions to client slots35/36. Both must activate and answer
status; cancelling one must leave the other usable; cancelling the second
must close its access too. Real completion routing and token allocation are
used. The current probe uses the kernel monotonic millisecond clock and
`Wait_For_Activity_Until`, guided by dispatcher `Runnable`/`Next_Deadline`,
instead of periodic sleeps. A separate five-second test watchdog bounds each
pump; expiry never releases authority or claims retirement. The historical run
below used deterministic test ticks.
The activity-wait revision passed on 2026-10-01 in
`/tmp/cubit-admission-activity.WHfxMvWr/` with runner exit0, all four admission
markers, memory query, baseline IPC and final fault scan. It recorded three
actual activity waits. Hosted tests additionally cover cancellation of an
expired pending receipt without repeatedly advertising its past deadline,
and scheduling abort when that receipt arrives late. Production managers have
not yet adopted this loop; the probe owns its completion queue exclusively.
Run `/tmp/cubit-dispatch-admission.AjOQ5P/` on 2026-10-01 completed with exit0,
the dispatcher PASS marker, all earlier admission markers and baseline IPC,
and final fault scan. Dispatcher/probe bodies match the main checkout. This
is native CuBit in QEMU with a synthetic GPU service, not startup deployment
or physical rendering. The private client hook follows the cross-process test.

`GPU_Budget_Probe` exercises `Native_GPU_Query.Budget` over native IPC using
the synthetic server's label0A2E. It saves each incoming reply capability into
slot60, then replies through that saved capability. Four calls check successful
budget words, a well-formed busy status, a wrong label, and a short envelope;
the latter two must clear client output and fail transport validation.
This tests neither the production Intel supervisor allocator nor its async
deadline/ownership handling. No GPU execution is implied.

The existing private `graphics-grant-cycles-43bkvqow` test apps now call this
probe after the buffer probe and dispatch0A2E before ordinary IPC operations.
The production query bridge is copied verbatim; its selector2 timestamp support
is included as well. These hooks remain private test-app changes, not production
startup changes. Require `GPU-BUDGET-IPC: PASS`, baseline `TEST: PASS async-ipc`,
runner success, and no `TEST: FAIL` in the final serial log.

Run75735 on 2026-10-01 passed all three requirements and the existing128-cycle
grant test. Logs: `/tmp/cubit-budget-ipc.eMAnop/`. The runner used a fresh
scratch ext2 disk and exited0 after its final fault scan. Native app build9898
passed; tested query/probe sources compare equal to the main checkout. Other
private-workspace services are seeds or rebuilt dependencies, not a world build.

`GPU_Buffer_Probe` additionally tests the production `Native_GPU_Buffers`
create/map/retire/close calls with actual CuBit IPC and ordinary RAM grants.
Its synthetic server implements labels `0x0A22`/`0x0A23` only inside the IPC
test application. It checks read-only denial, shared marker contents, pending
retirement while borrowed, completion after return, and stale acquisition
rejection. Require `GPU-BUFFER-IPC: PASS` and no `TEST: FAIL` in addition to
the baseline runner result. This is deliberately not the Intel service: its
reply-authorized `Create_For_Process` does not test production session admission,
recipient endpoint association, GPU backing, or drawing.

The buffer probe now repeats 128 mapping lifetimes over the same retained
ordinary RAM page. Each new grant must have a different generation-qualified
reference; the preceding reference must fail acquisition while the replacement
is live. Old synthetic mapping IDs must not retire the replacement. Each cycle
also repeats the read-only check and pending-revoke/return/confirmed-retirement
sequence. This exercises kernel grant reuse, not the production service's
64-slot mapping table (covered separately by hosted sharing tests).

The fresh `graphics-grant-cycles-43bkvqow` private snapshot wires only this probe
into the current IPC client/server. Their private GPR source directories include
`../../mesa/anv` and `../../../tests/mesa-anv/native-integration`, with
`-gnat2022` for the bridge/probe's Ada 2022 aggregates. The client calls
`GPU_Buffer_Probe.Client (CAP_SLOT_IPCTEST)` after its initial FPU check; the
server dispatches labels `0x0A22` and `0x0A23` to the probe before normal IPC
operations. These hooks are test-only and are not installed in the main apps.

The 128-cycle run passed on 2026-09-30 with a freshly rebuilt kernel and probe
apps: runner exit 0, `GPU-BUFFER-IPC: PASS 128 native grant cycles`, baseline
`TEST: PASS async-ipc`, and final fault scan passed. Logs are in
`/tmp/cubit-grant-cycles.rB1MqzuE/`. Probe source SHA-256 in the main checkout
and tested snapshot matches:
`dd8ffa8219bc9923165139705190854d2ba0b86dd0b0db8f4cf2f1a4ab50d77d`.
Other services were seeded or rebuilt as dependencies, not a full world build.

The private IPC client/server now call this probe as well as the earlier memory
probe below. These hooks are confined to test applications, not devmgr/procmgr
or the production Intel driver. The published NUC image has not been rebuilt.

Native buffer probe run on 2026-09-30 completed with exit 0, both GPU probe
markers, baseline async-ipc PASS and the runner's final fault scan. Evidence:
private `intel-presence.F8KpDB/tmp/gpu-buffer-native.uOh1HS/` (`run.log` and
`cubit-headless-async-ipc-serial.log`). The run used a new scratch ext2 disk,
not the user's development disk.

`GPU_Memory_Probe` exercises the production `Native_GPU_Memory` bridge in
CuBit, not a mocked runtime. The test server allocates one anonymous page,
writes a marker and makes a read-only grant to its synchronous caller. The
client uses its existing server endpoint to acquire the grant. This verifies:

- an empty endpoint slot cannot acquire it;
- write access to a read-only grant is rejected;
- a successful acquisition reads the actual shared marker;
- two acquisitions defer owner-requested revocation;
- revocation rejects new acquisitions;
- the first return does not complete retirement; the second does;
- extra returns and acquisitions using the retired reference fail.

The test label is `0x0A30`, confined to the synthetic IPC test server. This is
not a production GPU protocol. The server's `Create_For_Process` relies on
the test's synchronous reply authority; it does not test the production
session-to-recipient capability association. The test page is ordinary RAM,
not a GPU BO. It does not execute Intel hardware, Mesa rendering, or the
`Intel_GPU_Buffer_Views` wrapper.

Private integration on 2026-09-30: `intel-presence.F8KpDB` IPC test client
calls `GPU_Memory_Probe.Client` with its authorized and empty slots. The IPC
server dispatches label `0x0A30` to `GPU_Memory_Probe.Server`. Both helper files
and the unchanged production memory bridge are copied to the private
`userspace/lib/graphics`; the client GPR includes that directory (the server
already did). Existing query tests remain enabled.

Build under that workspace's Nix/private lock:

```sh
make -C kernel ipctest-server ipctest-client
```

Run `tests/headless/run.sh --test async-ipc --disk <fresh-ext2-scratch-image>
--accel tcg,thread=multi --timeout 90`, with a fresh private TMPDIR. Require
`GPU-MEMORY-IPC: PASS` in the serial log in addition to the runner's original
IPC markers and fault scan; its baseline marker list does not yet include
this new probe. Never use the user's disk for this test.

Observed serial evidence is in private
`tmp/gpu-memory-native.wcDFph/cubit-headless-async-ipc-serial.log`.
The runner completed with exit 0 and its final fault scan passed. The
production bridge body matches the tested private copy (SHA-256
`9a411ffcadecb509611a7d003d373d06d4a2942f04c27e72eb549ba7bd1b6f61`).
