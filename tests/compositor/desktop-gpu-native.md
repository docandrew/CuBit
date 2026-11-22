# Native Desktop GPU scene build

`build-desktop-gpu-scene-native.py` compiles the production scene owner,
image-reader leases, glyph residency, wallpaper owner and Vulkan lifecycle
policies against the actual CuBit Ada runtime. It binds the scene/backdrop
elaboration closure, compiles the existing Vulkan C interfaces with the pinned
CuBit musl compiler and existing Mesa headers, validates the existing SPIR-V
shaders, and creates a static archive. No Mesa library is rebuilt.

From the repository root, using the existing Mesa source tree:

```sh
nix-shell tests/compositor/vulkan-affine-shell.nix --run '
  cd kernel
  alr exec -- python3 ../tests/compositor/build-desktop-gpu-scene-native.py \
    --mesa-source ../userspace/mesa/build/source-aee5fe5697d39dd4
'
```

Each run retains a private directory under `tests/compositor/build/desktop-gpu-native-*`.
The directory contains the archive, copied sources/runtime/headers, generated
shaders, `inputs.json`, `result.json`, and symbol inventories. Source and snapshot
hashes are checked before and after the build. Outputs never use Nix's temporary
directory, which is removed when its shell exits. No shared runtime, Mesa output,
service staging, ISO or Git index is modified.

The initial complete run compiled 122 production Ada source files, produced
63 Ada/binder objects and 12 musl C objects, and checked 779 input files. Its
archive symbol audit resolved every `cubit_vulkan_*` reference internally.
Remaining external symbols were the four existing Mesa service functions,
the Rust font rasterizer, two wallpaper assets, memcpy/memset, and CuBit's Ada
last-chance handler. This audit checks symbol closure, not calling conventions
or runtime behavior; the native compiler and the existing C static assertions
provide separate representation checks.

The archive is a component-build artifact, not a bootable Desktop service.
Native linking must supply the existing Mesa service, fonts, assets and runtime
and establish correct elaboration for the consuming executable. The normal
Desktop build still uses its software backend. This gate does not establish
manifest authority, successful device admission, native GPU drawing, client
image import, Display sharing, scanout, tear-free behavior or performance.
The existing hosted Mesa pixel/fault tests and SPARK reports remain separate
evidence. Full native application integration and hardware tests are required.
