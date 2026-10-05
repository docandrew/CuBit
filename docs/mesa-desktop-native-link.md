# Native Mesa link contract for Desktop

Status: native bundle and link-only consumer verified, 2026-10-02. This is not
live Desktop GPU activation. The v32 native service demo remains the tested
link checkpoint; its physical NUC behavior is not yet confirmed. This document
answers the compositor owner's request for the dependencies hidden in
`tests/mesa-anv/test-native-instance-link.py`.

## Reusable tooling

Under the shared build lock, inside Nix:

```sh
python3 tools/build_mesa_service_bundle.py MESA_BUILD NEW_OUTPUT_DIRECTORY
python3 tests/mesa-anv/test-service-bundle-verification.py NEW_OUTPUT_DIRECTORY
python3 tools/verify_mesa_service_bundle.py NEW_OUTPUT_DIRECTORY
```

The last command returns JSON with `link_prefix` and `link_args`; pass these
as argument arrays, not shell-evaluated strings. Supply Desktop objects and
its own manifest before the bundle arguments. Hold the lock from verification
through final linking. The default stack contract still needs an explicit
Desktop override as described below.

Verified artifact: `tests/mesa-anv/target/service-bundle-production` against
`tests/mesa-anv/target/state-table-native.sthIHk/build`. Its `inputs.json` records
source/header/runtime/archive and object hashes, commands, link-check ELF hash,
and `executed=false`. `build.log` contains the native compiler/linker output.
The authority-free `link-check.app` retains all four production entry points
and checks common dispatch definitions plus absence of undefined symbols.
It is not executed or staged. Six isolated corruption/status controls verify
the bundle verifier rejects altered inputs, objects, ELF, argument JSON, and
incomplete or execution-mislabelled metadata.

These are local integrity checks, not signatures or a proof that every Mesa
archive was rebuilt from the current upstream sources. The verifier must not
be used to authenticate an untrusted downloaded manifest. No CPU-readback
presenter, demo entry point or diagnostic linker wrappers enter the bundle.

## Bundle boundary

### VulkAda candidate: source audit (2026-10-02)

VulkAda is a candidate for the Ada compositor's Vulkan calls, not a replacement
for `Mesa_Service` admission/retirement or the Intel GPU service. No VulkAda
code is adopted into the production build by this audit.

The live [official page](https://phasercat.com/vulkada/) links the September 7,
2026 release, despite a cached page advertising February 25. The old archive
returned HTTP 404. Audited archive:
`https://phasercat.com/wp-content/uploads/2020/08/vulkada_090726.zip`, SHA-256
`f3de1c162a954c23491df6b63ef37b2fad22f22877bf76569db18dae9d4fa524`.
The archive's source headers identify LGPL-3.0-or-later. Distribution/static-link
license review remains required; this note does not establish compliance.

Concrete integration findings (paths relative to the archive's `vulkada/` source
directory):

- `vulkan-c.ads:1511` imports core functions by global C names, for example
  `vkCreateInstance`; `vulkan-core.adb:87` and `vulkan-devices.adb:58` obtain
  function pointers through imported `vkGetInstanceProcAddr` and
  `vkGetDeviceProcAddr`. Our `service-device.c` instead supplies
  `anv_GetInstanceProcAddr` in the borrowed device view. `nm --defined-only` on
  the existing production `link-check.app` confirms `anv_GetInstanceProcAddr`
  and `anv_CreateBuffer`, but not the corresponding global `vk*` entry points.
  Therefore merely adding VulkAda sources is not a complete link strategy.
- `vulkan-extensions-khr_swapchain.adb:111` loads extension pointers through
  `Core.Get_Proc_Addr` into package-level variables. A port must account for
  dispatch lifetime and multiple devices; do not assume these are per-owner
  dispatch tables or that loading a pointer enables an extension.
- `vulkan-c_arrays.adb:39` keeps allocation metadata in a protected object
  containing a doubly linked list. `Allocate` allocates an Ada array and appends
  metadata; freeing searches that list. Callback marshalling additionally uses
  protected maps/vectors (`vulkan-callback_marshallers.ads:58`). These are real
  allocation/runtime dependencies, not allocation-free thin declarations.
- `vulkan-objects_common.adb` offers result-returning creation overloads as well
  as convenience overloads calling `Vulkan.Exceptions.Check`. Prefer explicit
  results in Desktop integration. Conversion allocation can still fail even
  with those overloads: selecting them does not prove exception-free behavior.
- `vulkan.ads:127` defines 64-bit opaque handle types. These remain process-local
  Vulkan handles, not CuBit capabilities. Check C/Ada representations against
  the actual native target before converting the service's borrowed addresses.

An isolated Nix compile probe starts at `vulkan-images.adb` with `gnatmake -c
-gnatA -gnat2022 -O2 -mno-red-zone -fno-pic --RTS=userspace/runtime` (absolute
runtime/source paths in the actual invocation). Working directory:
`/tmp/cubit-vulkada-native.V3GgWF`. The image-wrapper object compiled; its `nm -u`
output concretely includes `vkCreateImage`, `vkDestroyImage`, `vkBindImageMemory`,
exception/unwind hooks, finalization primitives, and secondary-stack hooks.
The transitive dependency compile completed successfully (117 output files,
empty diagnostic log, command exit 0). This is native compile-only evidence
for the image API's dependency closure, not all VulkAda packages. No complete
native link, C/Ada ABI equivalence, runtime behavior or GPU execution is claimed.

Recommended first gate: a small offscreen consumer borrowing the existing
`Mesa_Service` owner, with a deliberately scoped dispatch adapter. Do not create
or destroy a second instance/device through VulkAda, replace the owner with an
automatic finalizer, or introduce a generic loader that bypasses admission.
Exercise creation, command recording, completion, explicit result failures,
and teardown using the same device. Test native runtime/container dependencies
and allocator failure cleanup before placing calls on the frame path. Do not
promise zero-allocation recording without measuring the selected wrappers.

Window-system bindings (X11/Wayland/Win32) are not CuBit presentation. Keep the
current offscreen/private-image boundary until native presentation and sharing
have their own authority/synchronization contract. Header coverage for Vulkan
1.4 likewise does not establish support by our current Mesa device.

### Ada consumer adapter

`userspace/mesa/mesa_service.ads/.adb` provides a narrow Ada adapter to the
service bridge, not a replacement for VulkAda or a general Vulkan API binding.
Add these sources to the consuming Ada project so its own binder sees the
unit. The bundle still supplies the C service implementation and transport.

`Owner` is limited (noncopyable), one-attempt, with no reset or finalizer.
Check `Accepted` even when `Start` returns a failing Vulkan result. `Borrow`
returns an explicitly laid out 48-byte native C device view and clears output
on rejection. Handles are borrowed process-local addresses, not capabilities.
`Close(Object, Consumers_Retired)` makes the caller's synchronization duty
explicit; false does not call C teardown. Once teardown begins, Ada refuses
further borrows and health queries. Pending, unsafe, and retired all retain
the owner identity; none permit another start or capability-slot recycling.

The adapter is SPARK_Mode Off at the foreign boundary. A Boolean caller claim
is not proof that consumers retired. The C implementation remains responsible
for destroying device/instance once and polling native retirement thereafter.

`test-service-ada.py MESA_SOURCE --bundle BUNDLE` checks the Ada representation
against the real C header with compile-time offsets/size/alignment assertions,
then exercises three mock-C lifecycle paths (accepted, unaccepted failure,
accepted failure) including stale output and unknown retirement codes. Explicit
Ada test checks remain enabled under `-gnatp`. It also compiles the production
adapter against the native runtime, links it to the real bundle and checks all
five Ada entry points plus no undefined symbols. Evidence:
`tests/mesa-anv/target/service-ada.gn2c2gv5/result.json`. This is hosted ABI
testing and native link evidence, not hardware execution or proof.

The production bundle should contain these objects, compiled from current
sources against the same configured native ANV build:

| Object | Source | Purpose |
|---|---|---|
| service-device.o | userspace/mesa/service-device.c | One admitted, process-lifetime Mesa owner |
| native_build_id.o | userspace/mesa/anv/native_build_id.c | Static build-ID validation |
| native_build_id_link.o | userspace/mesa/anv/native_build_id_link.c | Linker-symbol build-ID lookup |
| native_gpu_buffers.o | userspace/mesa/anv/native_gpu_buffers.adb | Allocation/submission/retirement FFI |
| native_gpu_memory.o | userspace/mesa/anv/native_gpu_memory.adb | Grant borrow/return FFI |
| native_gpu_query.o | userspace/mesa/anv/native_gpu_query.adb | Device and budget query FFI |

Do NOT include the test main, `mesa_discovery_slot`, `mesa_probe_log`,
`native-init-trace`, its linker `--wrap` options, `mesa_triangle_surface`, or
`native_gpu_presenter`. Desktop supplies its own entry point, logging, launch
slot, composition and presentation. The CPU-readback test presenter is not a
production cross-process image-sharing mechanism.

Compile service-device using the configured `anv_kmd_backend.c` compilation
entry, removing its input/output/dependency-generation arguments and adding
the owned ANV include directory. It includes ANV internal/generated headers;
compiling with just installed public Vulkan headers is insufficient. Compile
the two build-ID C files with the native compiler wrapper. Compile the three
Ada transport bodies against `userspace/runtime`, with the established
`-gnatA -gnat2022 -O2 -mno-red-zone -fno-pic` flags, in an isolated object
directory. Desktop's Ada binder must account for any Ada units integrated into
its project; do not import a second program binder or startup object.

## Final link ordering

The existing verified recipe uses `tests/mesa-anv/native-compiler.sh cpp`:

```text
native compiler wrapper, cpp mode
  --manifest <Desktop's own generated manifest.o>
  <Desktop binder and bound objects, assets, fonts, compositor adapters>
  <the six production objects above>
  -Wl,--build-id=sha1
  -Wl,--start-group
    -Wl,--whole-archive <build>/src/intel/vulkan/libvulkan_intel.a
    -Wl,--no-whole-archive
    <remaining native static archives from Meson's intro-targets.json>
    <optional native scene archive>
    userspace/runtime/adalib/libgnat-user.a
  -Wl,--end-group
  -o <private Desktop output>
```

Retain the aggregate Intel archive FIRST and whole. Generated dispatch tables
contain weak references: a successful ordinary archive link can silently omit
implementations. Do not replace whole-archive with an expanding per-command
undefined-symbol list. Deduplicate the aggregate from the remaining inventory.
Reject missing archives or paths outside the configured build. Do not insert
host Mesa or Linux libraries.

The wrapper supplies native musl headers, static C++/GCC runtime libraries,
CuBit CRT, `native_build_id.ld` plus `cubit.ld`, and non-PIE/no-red-zone flags.
Simply using Desktop's existing `cubit-c++` command omits this recipe's explicit
Mesa build-ID linker script. A production helper should move or reuse the
wrapper deliberately, not silently substitute a host Vulkan loader.
Set an explicit stack contract; Desktop's existing Mesa link uses
`CUBIT_STACK_SIZE=16777216`. This is a starting contract, not a stack-usage proof.

## Launch authority and accounting are separate prerequisites

### Image-sharing boundary

The current `Intel_GPU_Buffer_Views.Share` presentation path produces a
terminal-forwardable, read-only CPU grant. Its own contract explicitly does
not establish GPU completion or scanout readiness. `native_gpu_presenter.c`
forwards that CPU presentation reference; it does not import another process's
VkImage into Desktop's Vulkan device.

The current ADLN PPGTT encoder rejects `Read_Only` leaves, and VM_Buffer must
not widen such a request to `Read_Write`. Thus the existing CPU grant cannot
be used as sufficient authorization for a writable GPU alias. A future import
needs a separately authenticated backing-use contract, producer completion,
consumer retirement, layout agreement and VM isolation. A copied raw VkImage
or an application-supplied physical address provides none of those.

First Desktop integration remains same-process/same-device composition. Keep
the native external-memory FD/dma-buf/host and DRM-modifier extensions disabled
until their actual transport and lifetime semantics exist. The synthetic
native discovery fixture now checks eight forbidden extension advertisements,
three external handle-type buffer/image query rejections, and positive private
image format queries. It exercises real Mesa policy with synthetic device
metadata, not physical GPU import or hardware read-only enforcement.
This gate passed in native CuBit under four-CPU QEMU TCG:
`tests/mesa-anv/target/external-policy.7CVPj9/serial.log`, using the executable
from `native-instance-link.eefllz4z`. Discovery retained seven synthetic query
replies, returned one physical-device description, and destroyed its instance
without opening a logical device or leaving provider references. The staged
test executable was restored and byte-compared after the runner finished.

`userspace/services/desktop/manifest.ccl` currently requests display but NOT
render authority. Linking service-device does not change this.

The current native test manifests use `(request-render read-write render)`;
the compiler generates `CCL_Manifest_Bindings.Slot_Render`. The Desktop must
use its own generated binding, never the demo's numeric slot. In procmgr,
render admission additionally requires trusted system startup approval
(`systemStartup and approveRender`); a manifest is a request, not a grant.
An opt-in Desktop startup needs a deliberate manifest/configuration policy
and must preserve software startup when hardware is unavailable. Use the
existing render-startup optional/software policy rather than giving Desktop
raw driver inspection or device-owner authority. Do not change this policy
as a side effect of the link bundle.

`cubit_mesa_service_start(slot, &owner)` accepts a stable, already admitted
slot. Initialize owner to NULL. Non-NULL owner on failure means ownership was
accepted and must be retained through cleanup. There is exactly one static
owner and one persistent per-GPU `anv_memory_budget` per process. Query results
are observations, not memory reservations; the driver/supervisor still grants
actual allocations. Do not create separate budget records for different
compositor images or reset accounting on device destruction.

Borrow device/queue handles only while ready. Serialize all calls and queue
use. Retire every image consumer and GPU submission before close. Pending or
unsafe close retains the capability and storage; it does not authorize a new
start or endpoint-slot reuse. Start currently selects graphics queue family 0
and requires the owned WB-coherent memory contract.

## Required validation before handoff as a usable bundle

1. Record hashes of source inputs, copied/prepared ANV transport, all libraries,
   native runtime, generated objects, compiler configuration and final ELF.
   Reject stale prepared transport, and verify inputs did not change during
   the build. Do not reuse a prebuilt bundle against a different Ada runtime.
2. Link a small consumer of all four service entry points without test trace,
   presenter or discovery-slot objects. Verify no unresolved symbols and
   verify required common dispatch definitions in the final ELF, including
   properties2, framebuffer/pipeline-layout creation/destruction, QueueSubmit
   and CmdCopyImageToBuffer. This is link evidence, not GPU execution.
3. Link the opt-in Desktop through its own binder/manifest; preserve legacy
   and software paths. Compositor owner controls those source edits.
4. Verify startup admission denial/software behavior in QEMU. Physical NUC
   testing is required for ANV device initialization, actual composition and
   presentation. Same-device in-process images are the first integration;
   cross-process image/fence capabilities remain separate work.

The reusable builder and link/verifier gates above have passed. Desktop's own
binder/manifest link, startup admission and actual GPU composition remain
pending. Shared native builds and build-script edits require the shared build
lock; the compositor's ongoing input work must not be interrupted to obtain it.
