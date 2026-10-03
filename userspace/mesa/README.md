# CuBit Mesa integration

## Authorized Vulkan device bootstrap

`device-bootstrap.h` is the shared Vulkan-only device/queue constructor used
by the native authorized Intel scene/triangle/teapot fixture. The caller supplies
an already-authorized instance and physical device, a queue family, and required
queue flags. It validates the queue inventory before creating one queue and
returns the device, queue, family, and destruction function together. Missing
dispatch and unsuitable families reject before consuming device admission;
device creation is never retried. Existing output ownership cannot be overwritten.

This is not a discovery/session broker or a complete Desktop GPU backend.
It enables no optional features/extensions. Calls require external serialization. Before
`cubit_mesa_device_destroy_retired`, the owner must finish GPU work and retire
all image consumers. Destruction does not certify driver endpoint retirement;
the native backend's independent retirement callback/polling remains required.

Hosted regression (real Mesa Vulkan, not physical Intel validation):

```sh
nix develop -c nix-shell tests/mesa-anv/triangle-host-shell.nix --run '
  task_out=$(mktemp -d /tmp/cubit-device-bootstrap.XXXXXX)
  cc -std=c11 -Wall -Wextra -Werror tests/mesa-anv/device-bootstrap-test.c \
    -lvulkan -o "$task_out/test" &&
  VK_DRIVER_FILES=$(find "$MESA_DRIVER_ROOT/share/vulkan/icd.d" -name "*lvp*") \
    VK_INSTANCE_LAYERS=VK_LAYER_KHRONOS_validation "$task_out/test"
'
```

The regression covers 32 real device lifetimes, invalid queue selection,
missing dispatch, empty inventory, zero queue count, missing graphics support,
creation failure, missing returned queue, output overwrite rejection, and
double-destroy prevention. Native scene-teapot compile/link also passed after
adoption; neither check proves physical GPU execution or permission isolation.

### Launch-supplied session ownership

`launch-session.h` supplies the reusable ANV provider callbacks now used by the
native authorized fixture. The owner zero-initializes persistent storage once,
starts it with an already-admitted endpoint, and passes a separately retained
per-process/GPU budget to `cubit_mesa_launch_provider`. It does not acquire a
capability. Provider operations must be externally serialized; it is not yet a
general concurrent session broker. Its address and endpoint must remain stable
until both discovery references and transferred logical-device cleanup retire.

Opening a device consumes the one attempt, even on failure. Pin transfer is
tracked separately from the creation result: an attachment can fail after the
backend has acquired ownership. Discovery reference release never releases that
pin. Its retirement callback only atomically publishes a notification; the
owner must keep driving `anv_cubit_memory_poll` and retain the slot while cleanup
is pending or quarantined. No reset/reuse API is provided. If no pin transferred,
the original owner still handles one-shot close of its admitted session.

The native fixture retains its diagnostic wrappers and scheduling loop;
Desktop still needs explicit startup/teardown integration, rather than calling
the demo's main function. See `tests/mesa-anv/launch-session-test.c` for actual
provider-code tests with mocked transport against configured Mesa types. It is
part of the existing regression command:

```sh
nix develop -c python3 tests/mesa-anv/test-native-memory-policy.py \
  tests/mesa-anv/target/state-table-native.sthIHk/build
```

The checks cover untransferred failure, transferred failure, successful attach,
deferred retirement after discovery release, one-shot refusal, reference
underflow poisoning, and clearing invalid transport replies. These are hosted
lifetime regressions, not concurrency proofs or native IPC execution.

`cubit_mesa_launch_finish` performs one cleanup pass after Vulkan device and
instance destruction and external image-consumer retirement. It returns
`CUBIT_MESA_RETIRED`, `CUBIT_MESA_RETIRE_PENDING`, or
`CUBIT_MESA_RETIRE_UNSAFE`. Pending and unsafe both require retaining the owner
and capability. Unsafe is sticky and never retries an uncertain close.
Untransferred sessions now require confirmed retirement, not merely successful
close. Transferred sessions require their exact retirement notification, not
just a zero global pending count. New provider use is denied once finishing
starts. No internal sleep is performed, but synchronous transport calls can
still block; this is not a hard-latency/asynchronous IPC guarantee.

The demo uses this same pump, sleeping between pending passes and retaining its
process indefinitely on unsafe cleanup. A Desktop event loop can instead
schedule passes while continuing unrelated work. This does not authorize GPU
backing reclamation or capability-slot reuse. Six hosted retirement paths cover
the reference gate, pending-to-retired, failed/invalid close, uncertain polling,
transferred notification, and missing tracker evidence.

### Admitted-service startup bridge

`service-device.h/.c` composes both helpers behind an opaque, process-static
owner for a trusted service. It is the intended compositor startup interface,
not yet wired into Desktop. Compile it against the same configured ANV headers
and archives as the native backend. The public header needs only Vulkan types.
No test application's entry point, logging callback, or manifest is required.

1. Pass an already-admitted launch slot to `cubit_mesa_service_start` with a
   NULL output pointer. A non-NULL returned owner means ownership was accepted,
   even if the Vulkan result reports initialization failure. Failed instance
   creation outputs are not treated as valid handles.
2. On success, `cubit_mesa_service_device` borrows instance, physical device,
   logical device, queue, family and dispatch facts for trusted C adapters.
   The bridge requests Vulkan 1.1, graphics queue family 0 after validation,
   and no optional features. It does not enable imports or allocate surfaces.
3. After all child Vulkan objects, GPU work and image consumers retire, call
   `cubit_mesa_service_close`. Repeated calls only advance retirement; each
   device/instance is destroyed once. Handle lookup is disabled immediately
   when closing starts. Pending or unsafe keeps the endpoint retained.

Provider and accounting storage never moves or resets. This first bridge
accepts one GPU session per process; multi-GPU services require an authenticated
per-device inventory and shared budget ownership rather than duplicating this
singleton. It does not change the Desktop manifest, grant capabilities, retry
admission, wait for GPU idle, or certify physical scanout. Calls require external
serialization, and synchronous teardown/IPC may block.

`cubit_mesa_service_status` exposes the existing native ANV session-health
observation without submitting work or waiting for idle. Any backend failure
makes bridge device loss sticky and disables further handle borrows. Already
borrowed handles and pending work still require explicit retirement; a healthy
observation is neither an authority lease nor a completion fence. Invalid,
not-ready and closing owners reject without transport calls. The native service
probe now checks status before each drawing cycle and logs `MESA-SERVICE health`.

`tests/mesa-anv/service-device-test.c` covers nineteen fresh-process scenarios
(arguments 0 through 18) using the actual bridge and configured Mesa types with
mocked Vulkan/IPC. Successful startup, memory-policy/instance/provider/enumeration
failure, untransferred/transferred device failure, missing queue and uncertain
close all passed. It also rejects explicit-only memory, missing dispatch,
non-graphics queues, and empty/multiple/invalid device inventories. The existing
`test-native-memory-policy.py BUILD` command now runs every scenario in a fresh
process as part of its fourteen hosted fixtures. Added cases cover sticky device
loss, unexpected health failure, no recovery from a later successful reply and
no health IPC after closing. The suite also executes the actual
`anv_cubit_check_status` implementation for denied/unavailable/protocol failures.
Evidence: `tests/grant-storage/build/service-health-policy-r2.log`.

`test-native-instance-link.py` accepts `--service-link-check` alongside
`--authorized-discovery --retain-transport`. This compiles and retains all four
bridge entry points (including status), checks their presence in the final native
ELF, and records
source hashes and `service_link_only=true` in `inputs.json`. It does **not** invoke
the service bridge. Native link gate passed in
`tests/mesa-anv/target/native-instance-link.ec49up4s` (ordinary teapot fixture),
including the queue/property dispatch implementations. Execution and Desktop
wiring are still pending; neither hosted tests nor native linking prove GPU
execution of this new bridge.

The status-enabled native service probe also linked successfully in
`native-instance-link._8jbh7rm` (`service-health-native.log`). It has not been
executed or packaged; the v30 image below predates these extra health logs.

`--service-smoke` selects a different native entry path that actually calls
the shared service bridge, then runs the requested transfer/triangle/teapot
callbacks and retires the owner. It requires authorized discovery, logical
device and retained transport; it implies service linkage and records
`service_smoke=true`, `service_link_only=false`. Output is `mesa-service.app`.
The existing diagnostic-provider entry path remains the default. The same
manifest and launch slot are used; this does not expand admission.

Hardware checkpoint v30 is `kernel/cubit_live_mesa_service_teapot_v30.img`,
SHA256 `7a7e72b4581e3c539dd9b9f90f157479fdbdea39a398abef8334d15670dc95f7`.
It packages `native-instance-link.q678lpp5/mesa-service.app` and passed image
audits plus QEMU UEFI four-CPU boot/log-delivery/no-PS2 USB/Desktop gates.
QEMU denied Intel render admission; the GPU probe did not execute there.
Physical NUC validation remains required: expect `MESA-SERVICE startup=0`,
`owner-retained=1`, queue family 0, three teapot cycles retired and cleaned,
`MESA-SERVICE retirement=0`, and `MESA-SERVICE result=0`. This is plain teapot
rendering through the service startup bridge with CPU readback presentation,
not the scene overlay or full Desktop GPU compositor. v29 remains unchanged.

The simultaneous scene-link attempt rejected a runtime hash mismatch with
frozen `native-scene-oahh3gi0`. That guard was not bypassed. A refreshed matching
scene archive is required for future scene relinks; previously linked v29 is
unchanged.

## Native Mesa software cube

Build from the repository Nix environment, holding the shared build lock:

```sh
flock --exclusive --nonblock coordination/build.lock nix develop -c make -C kernel mesa-cube
```

This resolves the pinned Mesa source, prepares a writable copy with the CuBit
platform patch, configures native softpipe, builds the CuBit ELF and stages
`kernel/isodir/boot/mesa-cube.app`. The source/configuration cache is keyed by
the pin, patch and configuration inputs. Earlier cache directories are retained;
the script never modifies the Nix store. Target objects use the CuBit libc;
host build tools run in the Mesa Nix environment.

The build also prints the path of a fresh upstream notice bundle under
`userspace/mesa/build/notices.*`. It preserves the source pin, platform patch,
upstream license overview, version and complete license-text directory.
The normal build includes a linker map with archive extraction reasons and
symbol cross-references, plus the ELF hash. This connects the packaging audit
to the actual linked binary, including the non-Mesa runtime dependencies.
`LINKED-SOURCES.json` resolves Mesa archive members through compilation records
and hashes their source files, separating generated sources and unresolved
runtime members. It fails on missing Mesa mappings. It does not inventory
included headers, generators or direct link inputs.
The complete adapted Mesa source tree is also retained as `MESA-SOURCE.tar.gz`,
preserving per-file notices even for headers and generator inputs. This adds
about 106 MiB at the current pin. Separate runtime dependencies remain outside
that archive; the bundle is not a general distribution-license audit.
`build/notice-path` identifies the successful build's bundle for image tooling.

Check notice integrity and rejection behavior with:

```sh
nix develop -c bash tests/mesa-software/test-notices.sh /path/to/prepared-mesa-source
```

The demo renders nine frames of a textured, depth-tested OpenGL cube, then
retains the final image. Space resumes/pauses continuous rotation; Escape closes
it, including during animation. Each replacement retires the previous CPU
attachment before that buffer is rendered again. Present is not a retirement
fence. Only Desktop authority is requested.
It is software rendering, not Intel GPU acceleration. LLVM/JIT, EGL and a
general public app GL API are not supplied by this target yet.

The implementation still shares the validated frontend/winsys sources with
`tests/mesa-software`. The live USB image packages the app and its source/notice
bundle under `licenses/mesa`. Config supplies the ninth Apps entry, **Mesa Cube
(software)**. `usb-live-iso` builds the app dependency; the image audit checks
the packaged ELF against the bundled hash and requires the source/notices.

The UEFI live image passed a four-CPU native QEMU regression with optical Apps
launch, 194673 independently checked pixels, a complete animated rotation,
pause and Escape shutdown. Evidence: `tests/mesa-software/target/mesa-live-boot.log`.
Run this path with `tests/usb-optical/run-live.py --uefi --cpus 4 --mesa` inside
Nix under the shared build lock. This remains CPU softpipe rendering; it does
not validate Intel reset, firmware execution or GPU command submission.

The fresh-cache target and its staged ELF were validated with native QEMU:
`tests/mesa-software/target/mesa-staged-run.log` records 194673 independently
checked cube pixels plus Escape shutdown. This is not a NUC hardware test.

Set `MESA_WINDOW_ANIMATION=1` for the headless `mesa-window` test to inject
Space after the deterministic screenshot, require 36 more frames with buffer
isolation checks, pause, then close. Keep `MESA_WINDOW_SCENE=cube` and point
`MESA_WINDOW_IMAGE` at the staged app. This is a functional test, not a frame
rate benchmark; rendering is deliberately paced.
