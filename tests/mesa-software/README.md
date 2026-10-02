# Mesa software rendering probes

## Native CuBit Gallium softpipe

The separate `native-softpipe.c` probe runs inside CuBit, without LLVM/JIT,
filesystem authority, OpenGL/Vulkan frontends or a display surface. It validates
1024 clear pixels and 992 non-edge shader-triangle pixels. This is software
rasterization, not Intel GPU acceleration.

Under `../mesa-anv/host-shell.nix`, holding `coordination/build.lock`, prepare
a new source copy from the source pinned by `../mesa-anv/source.nix`:

```sh
bash tests/mesa-software/prepare-cubit-source.sh "$MESA_SOURCE" "$CUBIT_MESA_SOURCE"
bash tests/mesa-software/configure-cubit-softpipe.sh "$CUBIT_MESA_SOURCE" "$CUBIT_MESA_BUILD"
bash tests/mesa-software/build-native-softpipe.sh "$CUBIT_MESA_SOURCE" "$CUBIT_MESA_BUILD"
```

Use absolute paths for these variables. Preparation refuses an existing
destination. The patch introduces explicit CuBit OS detection ahead of inherited
Linux compiler macros; POSIX utility selection is not full POSIX conformance.
The native probe asserts CuBit detection at compile time. File/allocation linker
wrappers report calls/failures without changing results or granting permissions.

Boot using the **repository** Nix environment, which also supplies boot tools:

```sh
flock --exclusive --nonblock coordination/build.lock nix develop -c env \
  SOFTPIPE_IMAGE="$CUBIT_MESA_BUILD/native-softpipe.app" \
  bash tests/headless/run.sh --test softpipe --accel kvm --timeout 30 --keep-logs
```

The runner uses 512MiB by default and installs the probe only into a temporary
disk. See `docs/mesa-software-native-boundary.md` for remaining integration work.

## Native OpenGL frontend probe

The additional native OpenGL probe is built with
`bash tests/mesa-software/build-native-opengl.sh SOURCE BUILD` under the same
Nix shell and build lock, then booted using `--test opengl` (override artifact
with `OPENGL_IMAGE`). It creates a surfaceless Mesa compatibility context and
private RGBA8 framebuffer, calls GL entrypoints through dispatch, and checks
clear and triangle pixels. There is no GLX/EGL window-system emulation here.
`compile-native-probe.py` reuses the generated Mesa state-tracker compile
configuration to preserve its internal header ABI instead of guessing feature
macros. This is a Mesa-version-specific test frontend, not a stable CuBit API.
Native QEMU readbacks pass (1024 clear and 992 triangle samples); Mesa reports
OpenGL3.3 compatibility. This is not a full conformance result. The CuBit patch
also preserves strong pthread attribute references for static linking; Mesa's
weak-reference workaround otherwise left recursive-mutex calls unresolved at
address zero even though libc supplies their implementations.
The depth follow-up checks4096 color/depth samples across both draw orders
with depth disabled and enabled. Enabled GL_LESS must preserve the nearer color
and depth0.25; the disabled control must retain the last color and depth1.0.

The same build script produces `native-buffer.app`; run it with `--test buffer`
(optional `BUFFER_IMAGE` override). This separate Gallium display-target probe
uses page-aligned caller-owned BGRA storage,48x24 pixels with256-byte pitch,
and checks original pixels plus untouched row padding/tail. It uses no readback
copy and does not submit anything to Desktop. The adapter supports only one
live target and no Linux handle import or display submission.

The first combined GL-then-buffer run failed a second context allocation
(262368 bytes) after GL cleanup. The isolated probe does not resolve that
multi-context memory-pressure issue; investigate before claiming continuous
multi-context application support.

`native-mesa-window.app` (produced by the same build script) is the next
presentation probe. Run `--test mesa-window` to start Desktop and attach an
immutable Mesa-rendered four-color buffer; the runner captures a screenshot
and checks every composed pixel using `check-window.py`. Override the executable
with `MESA_WINDOW_IMAGE` if needed. It uses only Desktop authority, no raw display
access, and retains storage until process termination. There is no extra
readback copy before attachment, but the existing CPU compositor still copies
pixels into its scene. The probe now uses a native GL frontend drawable and
draws four compatibility quads through GL dispatch; its former Gallium-only
clear path has been replaced. The repeated-presentation follow-up alternates
two disjoint buffers over nine frames, changes the color pattern each frame,
and verifies both the new pixels and unchanged currently-attached pixels.
It reuses an old buffer only after successful attachment replacement, never
on a present acknowledgement. The final frame is retained for the screenshot
oracle. This relies on the current synchronous CPU compositor; it is not a
GPU fence or a general multi-GPU swapchain.

The build also produces `native-mesa-cube.app`: the same native window and
two-buffer transport with a24-bit depth/stencil attachment, orthographic camera,
model rotations and six colored cube faces. Explicit GLSL120 vertex and
fragment shaders are compiled and linked inside CuBit, using compatibility
matrix input and explicit position/color attributes. Geometry uses one
interleaved vertex buffer (24 vertices) and one index buffer (36 indices),
uploaded once and rendered with indexed triangles. Upload errors or unexpected
buffer sizes are fatal. Compile/link failures are fatal and report an info log.
The fragment shader samples a4x4 RGBA8 texture using explicit UV attributes,
nearest filtering and clamp-to-edge. Distinct asymmetric texel levels allow
the independent geometric oracle to detect flips, swaps and constant samples.
The program belongs to this demo's single retained context, not a reusable
multi-context renderer. To test it, set
`MESA_WINDOW_IMAGE` to that executable's absolute path and
`MESA_WINDOW_SCENE=cube`, then run the same `--test mesa-window` command.
`check-cube-window.py` computes independent camera-ray/box intersections for
the final frame;194673 non-edge composed pixels pass, including all16 texels
on each of three visible faces. This is native CPU-rendered OpenGL3D, not Intel
GPU acceleration. This does not yet cover alpha blending or filtered font atlases.

## Native context-lifecycle diagnostic (known failure)

The build also emits `native-context-lifetime.app`. Set `OPENGL_IMAGE` to its
absolute path and run `tests/headless/run.sh --test opengl --accel kvm
--timeout 25 --keep-logs` under Nix and the shared build lock. It attempts
three complete context/render/teardown cycles, logging heap-break growth and
successful `munmap` byte totals through a test-only linker wrapper. The wrapper
does not change allocation or release behavior. This is currently expected to
fail on the second context with512MiB guest RAM; it is not a passing test.
CuBit libc's no-op `munmap` loses reusable storage from musl's large allocations.
Use this reproducer to validate real reclamation, not to justify larger heaps.

## Linux-hosted Vulkan baseline (separate path)

The Rust Vulkan harness is **Linux-hosted**, not a native CuBit port or hardware acceleration.
The Rust harness uses Ash to load the specified ICD directly; no global Vulkan
loader configuration or host GPU selection is used. It requires one CPU device
named llvmpipe, submits a 4096-byte fill, uses a transfer-to-host memory barrier
and a bounded fence wait, then checks every word. It then compiles a compute
pipeline from validated SPIR-V, dispatches a 32x32 coordinate-dependent pixel
pattern, and checks all 1024 words after a shader-to-host barrier and fence.
Finally it draws a red right triangle into a cleared 32x32 RGBA attachment,
copies that image into the readback buffer, and checks all 992 samples not
exactly on the diagonal edge. Both covered and uncovered regions must match.
This verifies transfer, synchronization, compute and vertex/fragment execution,
and offscreen triangle rasterization. It does not test presentation or 3D depth.

Verified baseline: Mesa 26.2.3, llvmpipe LLVM 21.1.8 (256 bits), direct ICD
entrypoint, 4096-byte transfer readback and 1024 exact compute-generated pixels
and 992 triangle samples PASS on the Linux host. No native CuBit result is
implied by this evidence.

Configure the pinned Mesa source from `../mesa-anv/source.nix` with the existing
`../mesa-anv/host-shell.nix` Nix environment:

```sh
meson setup tests/mesa-anv/build-software "$MESA_SOURCE" \
  --buildtype=release --wrap-mode=nofallback \
  -Dplatforms=[] -Dgallium-drivers=llvmpipe -Dvulkan-drivers=swrast \
  -Dglx=disabled -Degl=disabled -Dgbm=disabled -Dllvm=enabled \
  -Dvalgrind=disabled -Dlibunwind=disabled -Dlmsensors=disabled
ninja -C tests/mesa-anv/build-software -j2 \
  src/gallium/targets/lavapipe/libvulkan_lvp.so
```

Compile and validate the shader in `tests/mesa-anv/host-shell.nix` after the
Rust build has created the ignored target directory:

```sh
glslangValidator -V --target-env vulkan1.1 tests/mesa-software/pattern.comp \
  -o tests/mesa-software/target/pattern.spv
spirv-val --target-env vulkan1.1 tests/mesa-software/target/pattern.spv
glslangValidator -V --target-env vulkan1.1 tests/mesa-software/triangle.vert \
  -o tests/mesa-software/target/triangle.vert.spv
glslangValidator -V --target-env vulkan1.1 tests/mesa-software/triangle.frag \
  -o tests/mesa-software/target/triangle.frag.spv
spirv-val --target-env vulkan1.1 tests/mesa-software/target/triangle.vert.spv
spirv-val --target-env vulkan1.1 tests/mesa-software/target/triangle.frag.spv
```

Run from the repository root, under Nix:

```sh
nix develop -c cargo run --locked --manifest-path tests/mesa-software/Cargo.toml -- \
  tests/mesa-anv/build-software/src/gallium/targets/lavapipe/libvulkan_lvp.so \
  tests/mesa-software/target/pattern.spv
```

Use a workspace-local TMPDIR for large builds. The software build is separate
from the existing ANV build and does not modify native binaries or live images.
Follow-up: native runtime, memory,
threading and CuBit surface/fence integration. Linux DRI/DRM or file-descriptor
dependencies must not be mistaken for a native CuBit backend.
