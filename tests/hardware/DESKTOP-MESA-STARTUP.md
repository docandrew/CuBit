# Opt-in Desktop Mesa startup checkpoint

## Drawing-enabled compositor packaging (2026-10-08)

The legacy `--desktop-mesa-startup` option below intentionally accepts only an
initialization-only artifact. For a drawing-enabled runtime-dispatch artifact,
use `CUBIT_DESKTOP_VULKAN_DIR=/absolute/path/to/verified/artifact` with
`--uefi --desktop-vulkan-compositor` instead. Its verifier is
`tools/verify_desktop_vulkan_compositor.py`; it requires drawing-enabled flags
and matching binary/source hashes. The default output is separately named
`cubit_live_desktop_vulkan_compositor.img`.

Both options use the explicit render-approval startup profile and start the
separate triangle-window demo. The default USB profile remains unchanged and
does not approve Desktop rendering. Hosted wrapper tests check the three routes
and reject drawing-disabled artifacts before any build. This is not a native
image or hardware acceptance result. Verify the matched runtime/Mesa/driver
stack, embedded startup and binaries, and exact-image boot before a handoff.

## v38 sustained-service failure capture (2026-10-02)

Ready-to-test image:
`/tmp/cubit-cleanup-health.f3U0OB/cubit_live_mesa_failure_v38_rebuilt.img`
SHA256: `c1a2d86a709292888aab6acd0e4fdb66e80128211e19952072eae976bd52b8b2`.
The adjacent `v38-rebuild-inputs.json` records payload provenance. This retains
the v37 kernel/services/initrd and replaces only `/apps/mesa-triangle.app`
with the new growable-lifetime adapter and first-failure/cleanup-health probe.
It is a 256-cycle offscreen workload: do not expect a new triangle window.

Exact-image QEMU UEFI USB-flash, four CPUs, without PS/2 passed desktop and
boot-log delivery (session 35800, exit 0). Logs are in
`/tmp/nix-shell.TGtbUj/cubit-usb-live.6uw3x647`. This does not validate Intel
hardware execution or resolve the NUC cycle-11 device-lost failure.

On the NUC, capture `MESA-TRANSPORT first-failure` with operation, status and
handle; `MESA-SERVICE cleanup health` if present; the sustained completed /
requested / result summary; and retirement. Success requires all 256 cycles
and completed retirement, not merely one successful draw. Missing diagnostic
lines alone are not success evidence, especially if logs have aged out.

`init-desktop-mesa-startup.ccl` is a frozen startup-v1 fixture. It retains the
existing hardware triangle-window startup sequence and adds explicit rendering
approval to Desktop. It does not change normal startup, production manifest
syntax, or the application's declared authority. Typed CCL migration remains
owned by that agent.

The fixture must be paired with a Mesa-linked Desktop carrying an optional
render request. A checked candidate is
`tests/compositor/build/desktop-admitted-startup-r2/desktop-vulkan-link.svc`,
SHA256 `f56a53344cd9144208b01b0cf7d77a58766f37255c00d9b887c3c7c822e202e6`.
Its fixture slot is 62. It initializes Mesa/context, but still draws the Desktop
in software. Newer compositor candidates require their own artifact checks.

Packaging selects this document through `images/desktop-mesa-startup.ccl` and
the `--desktop-mesa-startup` live-wrapper option. Set `CUBIT_DESKTOP_MESA_DIR`
to the selected link-artifact directory. The wrapper verifies its recorded
hash, byte size and admitted-startup flags, then supplies the executable as an
explicit input rather than overwriting the staged normal Desktop. This is a
local artifact consistency check, not a signature or proof of driver compatibility.
An ordinary triangle-window image still uses its original startup document and
does not approve Desktop. Do not infer hardware execution from a visible Desktop.

After preparing the matching staged driver/broker and other normal live inputs,
run in Nix under the shared build lock:

```sh
CUBIT_DESKTOP_MESA_DIR=/absolute/path/to/verified/candidate \
  bash tests/usb-optical/build-live.sh /absolute/path/to/doom1.wad \
  --uefi --desktop-mesa-startup
```

The default output is `kernel/cubit_live_desktop_mesa_startup.img`; use the
existing `CUBIT_LIVE_OUTPUT` override for a new numbered checkpoint.

Before handing off an image:

- Rebuild devmgr and Intel together: the current reservation reply carries the
  driver recipient slot. Do not combine the new driver with v32's older broker.
- Preserve the matched logstore/viewer pair and the separate Mesa demo.
- Verify the exact embedded Desktop, startup document, driver and broker against
  the selected artifacts; preserve previous hardware images and staged files.
- Run the image's UEFI/USB QEMU boot regression. GPU-unavailable recovery in
  QEMU is not Intel execution evidence.

On the NUC, require `DESKTOP-VULKAN: admitted endpoint; starting Mesa`, followed
by `startup=READY` and `initial health=TRUE`, plus responsive software Desktop
and boot logs. `SOFTWARE` validates fallback only. `QUARANTINED` requires logs
and retained resources, not a blind retry. The separate demo verifies its own
render path; neither result establishes GPU-composited Desktop presentation.

Validation so far: the existing `ccl-config --dump-startup` tool accepts the
fixture and reports `render=declared` for Desktop and the demo, preserving all
seven service/application entries. This is a host tool check, not a fresh CCL
source rebuild, image realization, or NUC run.

The existing image compiler also accepts the new catalog/profile pairing and
selects the supplied Desktop plus the correct bootstrap startup file. Artifact
guard regression covers the accepted fixture and nine invalid metadata/content
cases. Shell syntax checks pass.

## Separate sustained-rendering candidate v34 (2026-10-02)

Private candidate (not the optional-render Desktop profile):
`.build-workspaces/graphics-sustained-nrmhy060/kernel/cubit_live_mesa_sustained_v34.img`.
SHA256 `a5330e8d4d5e86d8610a56a1a5579b3c30ddf8f273f7f170015b52003072347d`.
It uses `init-mesa-triangle.ccl`: software Desktop and Boot diagnostics, plus
one admitted-service Mesa device running 256 **offscreen** triangle cycles.
No triangle window is expected. The separate v33 image remains unchanged.

The candidate contains the isolated six-route FAST-fence experiment and the
published libc thread-startup handshake, rebuilt privately. It excludes the
private endpoint-disposal syscall and session-reclamation experiment. The
adjacent image plan records selected payload hashes; native probe inputs are
in the snapshot's `tests/mesa-anv/target/native-instance-link.relunm10/inputs.json`.
All other seeded services are recorded by `.cubit-build-workspace.json`, not
represented as freshly rebuilt. Kernel, driver, broker, logstore/viewer and
the probe were built in this snapshot. Image audits passed. Run21322 exited
zero: the exact image passed four-CPU UEFI/USB-flash startup with no PS/2 and
quiet xHCI, plus Boot diagnostics collector/clock log delivery. Logs remain in
the snapshot's `tmp/cubit-usb-live.xo0lgfte/`. This is not Intel GPU execution.

NUC evidence required after boot-regression acceptance:

- `MESA-SERVICE startup=0`, followed by health `0` and advancing cycle counts.
- `MESA-TRIANGLE service sustained completed=256 requested=256 result=0`.
- `MESA-SERVICE retirement=...` followed by `MESA-SERVICE result=0`.
- Responsive Desktop and Boot diagnostics throughout.

On failure, capture the last cycle, its result, and surrounding Intel/Mesa
errors. Do not repeatedly relaunch an uncertain session. Finishing the draw
count without final service result does not establish completed retirement.
This tests sustained offscreen rendering/readback/cleanup, not GPU Desktop
composition, scanout ownership, VRAM support, or safe context-ID reuse.

## Launch-diagnostic candidate v35 (2026-10-02)

Private image:
`.build-workspaces/graphics-launch-log-la3mv1zq/kernel/cubit_live_mesa_launch_v35.img`.
SHA256 `c995fa76d2a14cbe9ff046486a3f67103bc01a632ccbab8a9271c31343f257d7`.
The v34 image remains unchanged. This clean clone adds bounded launcher and
Intel admission diagnostics, not the experimental endpoint-disposal syscall
or bootstrap authority changes. The Mesa payload is unchanged: 256 offscreen
cycles, no triangle window expected, software Desktop.

Exact-image run59439 passed UEFI/USB-flash/four-CPU/no-PS2 boot and log delivery.
Actual viewer readback includes `procmgr: boot launch diagnostics ready` and
`procmgr: render launch state=REJECTED decision=DISCARD incarnation= 4294967330`.
Logs: snapshot `tmp/cubit-usb-live.lxizz5zv/`. Rejection is expected without
Intel hardware in QEMU; this does not validate physical GPU admission/rendering.

On the NUC collect:

- `procmgr: boot launch diagnostics ready`, render launch state/decision, and
  child resumed or resume rejection if present.
- Intel `render control`, `render context allocation`, `render activation`,
  and `render control reply` records surrounding that launch.
- If admitted, Mesa startup and sustained-cycle/final retirement results as
  specified for v34 above. Do not blindly replay an uncertain session.

Launcher records are captured during startup and flushed after startup
processing finishes; their absence alone does not prove no request occurred.
The shared procmgr implementation is unchanged pending its owner's review.

NUC feedback: `render launch state=REJECTED decision=DISCARD
incarnation=4294967331`. This establishes non-admission, not a failed Mesa
device constructor. The frozen launch client has two ways to reach REJECTED:
failure delegating the captured application endpoint/submitting the broker
request, or a validated broker rejection carrying the matching nonce and
captured child identity. It does not identify which path happened.
An invalid completion becomes UNCERTAIN; an uncompleted timeout remains PENDING.
The broker can also reject before sending any Intel control request, so absence
of Intel control logs must not be interpreted as a GPU firmware failure.
The next evidence is the Intel control/allocation/activation/reply records;
if absent, investigate launcher submission and broker admission before touching
GPU reset or firmware sequencing. No new timeout or admission bypass is justified.

## Broker authority namespace fix v37 (2026-10-02)

Hardware result: the NUC completed ten cycles, then reported
`MESA-SERVICE health=-4 before cycle=11`, sustained `completed=10 requested=256
result=-4`, and `retirement=1`. This is a failed sustained run with pending
retirement, not an out-of-memory diagnosis. A void Vulkan cleanup callback can
mark the device lost after the draw result was computed; the next health check
can then fail locally without querying the Intel service.

The newer **source**, not this v37 image, adds an immediate post-cleanup health
check and a one-shot diagnostic:

```text
MESA-TRANSPORT first-failure operation=... status=... handle=...
MESA-SERVICE cleanup health=-4 after cycle=...
```

For the next explicitly verified image containing these changes, capture that
first-failure line, the cleanup/cycle line, sustained totals, and retirement
transitions. Operation names distinguish `session-health`, `vm-bind`,
`vm-unbind`, `close-cpu-view`, `close-buffer`, and `unmap-cpu-view`. Status values
are operation-specific; preserve the exact number rather than treating every
nonzero status as allocation exhaustion. The handle is a diagnostic BO identity,
not authority. Capture performs no IPC under the adapter mutex; the probe logs
after returning. Only the first captured failure is reported, so later teardown
failures cannot overwrite it. A missing line does not prove the session healthy:
not every device-loss path uses this diagnostic, and log retention is bounded.

Candidate: `.build-workspaces/graphics-broker-tag-6cc_atrz/kernel/cubit_live_mesa_launch_v37.img`.
SHA256 `ed103cb75d2180a355840ed9adef44324e8509a35caa7d978c581643fb779618`.
Native services/kernel and packaging pass. Exact-image QEMU70466 passes
UEFI/four-CPU/USB-flash/no-PS2/quiet-xhci/log delivery; logs are in
`/tmp/cubit-usb-live.1uo_e56l/`. QEMU does not validate Intel hardware rendering.

The real boot broker tag `4750_4252_4F4B_0001` was inside the reserved
application-session range. `Intel_GPU_Render_Control.Bind` correctly rejected
it, leaving render admission denied while normal GPU initialization continued.
Broker-gated admission logs were consequently absent. The corrected broker tag
is `4751_4252_4F4B_0001`; neither session validation nor authentication is relaxed.
Both devmgr and intel-gpu are rebuilt together. Actual-tag hosted regression
fails before this change and passes afterward; existing render-control tests pass.
The original fixture's tag99 did not expose this integration mismatch.

The Mesa payload remains the 256-cycle offscreen service probe: no triangle
window is expected. On the NUC collect launcher admission/reason, Intel render
control/activation records, then Mesa startup, cycle results and final retirement.
This is not a claim of verified physical execution or GPU Desktop composition.
Older images remain unchanged; input overrides are in `broker-tag-inputs.json`.

## Early launch-reason candidate v36 (2026-10-02)

Image: `.build-workspaces/graphics-launch-reason-ys5yi225/kernel/cubit_live_mesa_launch_v36.img`.
SHA256 `b94bebee5bc4abfbba3c82d0ffa7dde8f9ab80e530d6a4c386dfa5410d607972`.
Adds observation-only launcher reasons and raw endpoint-delegation status;
admission policy and the offscreen Mesa payload are unchanged from v35.
The clean snapshot excludes experimental kernel endpoint disposal and the
growable Mesa lifetime prototype. Four modified source hashes are recorded
in its `launch-reason-inputs.json`.

Hosted launch-client tests and native procmgr build pass. Exact-image QEMU
run59132 passes UEFI/four-CPU/USB-flash/no-PS2/log delivery. Actual viewer
readback reports `render launch reason=BROKER-REJECTED` and
`render delegation status= 0` without an Intel GPU. Logs:
`/tmp/cubit-usb-live.1rhh1yic/`. This is diagnostic transport evidence, not a
NUC fix or successful GPU admission. Initial VM attempt83783 failed before
boot on socket path length; the retry uses a unique shorter temporary path.

Request those two procmgr lines on the NUC. DELEGATION-FAILED identifies the
local authority handoff; SUBMISSION-FAILED identifies request submission;
BROKER-REJECTED establishes a matching explicit broker refusal. Broker refusal
still needs admission-stage diagnosis if no Intel control records appear.

## v33 packaged checkpoint (2026-10-02)

`kernel/cubit_live_desktop_mesa_startup_v33.img` has SHA256
`2ea8e93fbcfdb2941ea50efa4c8911dbc396f68f5b40ddb15a719040a5cf8e7e`.
The adjacent `.img.plan.json` records the selected inputs. The realizer checked
ISO payload bytes and the bootstrap archive; staged driver, broker and explicit
Desktop candidate were compared against the selected binaries.

The exact image passed UEFI, four-CPU, USB-flash, no-PS/2 startup and boot-viewer
collector/clock delivery checks. QEMU reported `DESKTOP-VULKAN: startup=SOFTWARE`
and an empty optional render slot, as expected without Intel hardware. This is
fallback evidence only. Physical admitted-device startup remains to be tested;
the Desktop continues drawing in software even if that startup succeeds.
