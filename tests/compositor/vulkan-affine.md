# Vulkan affine, alpha and coverage rendering

The new Vulkan renderer supports opaque or premultiplied BGRA color images and
R8 coverage masks tinted by a straight-alpha ARGB color. It applies per-output
scale, rotation and damage clipping from the existing SPARK geometry. Desktop
has not selected this renderer: image import, native queue ownership and target
transport still need integration. The software fallback is unchanged.

## Allocation and recording boundary

`Vulkan_Affine_Binding.Draw_Output` reuses `Compositor_Affine.Plan` / `Clip` and
`Compositor_Transform.Build`. Its foreign call receives exact rational transform
coefficients, dimensions and scissor. The C adapter creates two reusable graphics
pipelines, one sampler, one descriptor layout and one pipeline layout. Creation
has no pixel allocation or GPU submission. Partial creation failure destroys
created objects before returning; this is trusted Vulkan resource setup, not a
proved asynchronous retirement policy.

A draw borrows an active compatible render pass, command buffer and source
descriptor. It binds a pipeline and descriptor, sets viewport/scissor, pushes
68 bytes of parameters and records a fixed six-vertex draw. It does not allocate
per-draw descriptors, buffers, vertex storage or pixels; it does not upload,
read back, submit, wait, or retire anything. Color sources require premultiplied
pixels. Coverage sources are R8; there is no intermediate BGRA mask expansion.

The caller must provide authorized, nonaliasing images, validated dimensions
(including source dimensions in 1..65535), compatible formats/pass/subpass,
correct layouts, synchronization and live descriptor/storage references. All
referenced resources must outlive GPU execution. Destroying a pipeline engine
requires confirmed quiescence; returning from `Record_Draw` is not such evidence.
The context pointer and Vulkan handles are a private foreign boundary, not IPC
capabilities. Nothing here converts a CPU pointer or read-only grant into a
Vulkan image.

## Exact nearest sampling

The initial floating-coordinate shader failed the independent oracle at 125%
scale: a mathematically exact texel edge rounded to the preceding texel. The
replacement preserves SPARK's rational coefficients as pairs of 32-bit words.
It evaluates pixel-center coordinates and source indices using exact integer
comparisons. A floating quotient estimate is only a candidate: the shader
checks the defining inequalities before accepting it, corrects by one where
possible, and otherwise performs at most 16 integer-search steps per axis.

The arithmetic uses core 32-bit operations, including `umulExtended` to obtain
both product words; no shaderInt64 feature is requested. See the Khronos
[GLSL built-in function specification](https://docs.vulkan.org/glsl/latest/chapters/builtinfunctions.html).
The actual shader is not SPARK-proved. Integer bounds, ABI conversion, GLSL,
SPIR-V generation, Vulkan and Mesa remain audited/tested foreign boundaries.
The shader's cost on i915 is unmeasured; this is a correctness baseline for later
GPU profiling, not a 240 Hz performance result.

## Evidence

Run the hosted oracle with:

```sh
nix-shell tests/compositor/vulkan-affine-shell.nix --run 'bash tests/compositor/test-vulkan-affine.sh'
```

The script validates SPIR-V for Vulkan 1.0 and runs pinned Mesa lavapipe with
synchronization validation. It executes the real Ada binding, C adapter and
shaders. The independent C reference inverse-maps pixel centers using integer
arithmetic from the original screen/surface/damage inputs, rather than using the
renderer coefficients or scissor.

Each of the normal and forced-integer-fallback variants passes:

- 232 frame cases: 212 draws, 20 empty, 178,176 target pixels checked.
- 100%, 125%, 150%, 200%, 300% and two-thirds scaling; four rotations;
  opaque, alpha-over and tinted-mask modes; three output origins.
- Sixteen additional extreme-coordinate cases, including logical spans of
  2^31 and denominator arithmetic above 32 bits.
- Exact opaque pixels and undamaged sentinels. Blended/mask channels permit
  one quantization unit against the independently rounded reference.
- Sixteen missing-dispatch failures, cleanup after second-shader and
  second-pipeline creation failures, and ten invalid recording contexts.
- Zero Vulkan validation errors, including at final device destruction.

The forced variant starts with an intentionally inaccurate quotient estimate,
exercising the exact integer fallback while demanding the same pixels. The
normal variant remains the production shader input. Test-only upload/readback
buffers inspect results; they are not part of the renderer. These are Linux CPU
Mesa results, not native CuBit GPU execution or display screenshots.

The existing CuBit Ada runtime and musl/Mesa configuration compile both the
binding and C adapter, including the final normal SPIR-V. This is native
compilation only; the shader has not run on the NUC through CuBit.

## Proof and remaining integration

The SPARK proof job covers the new binding and reused affine/transform bodies,
including the foreign-call precondition requiring valid geometry and exactly
matching transform coefficients. Session 40606 completed successfully with
155 analysis results: 15 flow and 140 prover, none unproved or justified. Shader
arithmetic and Vulkan object lifetimes are explicitly outside that proof.
Hosted/native logs, shader hashes, the final proof summary and source hashes
are saved in `/tmp/cubit-vulkan-affine-evidence/`.

Reproduce with:

```sh
nix develop -c bash -c 'cd kernel && alr exec -- gnatprove -P ../tests/compositor/vulkan_affine.gpr -u vulkan_affine_binding.adb compositor_affine.adb compositor_transform.adb --mode=all --level=4 --report=all --checks-as-errors=on -j1'
```

Next integration gates are bounded descriptor/command resources and their
SPARK lifecycle; actual source-image import and writable target leases; render
completion distinct from latch/scanout retirement; native CuBit fault/overload
and exact-pixel tests; then hardware profiling. The retained-front pool and
software transport remain available, but neither is automatically connected to
this renderer by these files.
