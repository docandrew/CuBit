# Desktop wallpaper GPU preparation

Desktop_Backdrop_Style is shared by the existing software wallpaper painter
and Desktop_GPU_Scene.Backdrop. Capture emits an opaque physical-output fill
and, for Wallpaper or Cubie, one Fill/Fit/Center image command. Missing sources
and command overflow invalidate the complete scene rather than presenting a
background-only prefix. Output DPI/origin do not alter physical wallpaper
placement. The existing software fallback remains the everyday Desktop path.

Desktop_Backdrop_Owner is a limited owner for one immutable embedded asset and
one caller-reserved general slot (128..135). Acquire admits at most one backing
allocation and one upload chunk. Poll observes one fence and admits at most one
next chunk. Resident acquisitions reuse the source without uploading pixels.
Full confirmed coverage is required for import. Close requires both explicit
retirement of CPU captures and a healthy idle renderer. Pending or uncertain
work retains storage; there is no eviction, unbounded queue, or internal wait.
Callers must reserve distinct slots and keep owners alive for all captured
source references. These are same-process immutable assets, not client imports.

Desktop_Backdrop_Upload borrows the existing staging mapping and submits one
chunk. Desktop_Backdrop_Pixels is the narrow, unproved pointer boundary: it
checks the plan, format, dimensions and capacity, then calls memcpy for each
row. Its caller supplies exclusive, nonaliasing writable memory of the stated
capacity. Embedded source arrays have fixed dimensions. The synchronous copy
retains no pointer and leaves padding untouched. This initial staging transfer
is not a per-frame copy or an extra premultiplied/shadow atlas. Mesa allocation
sizes remain charged through the existing Desktop device ledger; that ledger
is not total process or driver memory accounting.

Clean selected SPARK reports establish 5 checks for capture, 11 for upload and
31 for the owner, with zero unproved or justified checks. Earlier 588/589/600
reports included cached results from other units; do not count those totals as
new checks for these adapters. Proof covers stated state/initialization/bounds
contracts under existing imported contracts. It does not prove raw pointers,
memcpy, Mesa/Vulkan, CPU-reference retirement assertions or display scanout.

Validation completed in the private candidate:

- 384 software scaled/rotated/style cases; 1728 independent scalar wallpaper
  cases plus eight invalid-call guards.
- 288 capture cases, including rejection of partial scenes.
- 464 staging chunks across both complete atlases with padding and six no-write
  rejection cases; 72-chunk lifecycle and 720 pending observations.
- Owner tests for both assets (72/144 chunks), 1000 resident hits each without
  another upload, CPU/GPU retirement gates and uncertain-transfer retention.
- Real hosted Mesa llvmpipe: actual embedded assets, 384 style/DPI/rotation
  frames, 2359296 exactly matching pixels, 216 initial chunks, two retained
  images, complete cleanup and zero Vulkan validation errors.
- Native CuBit Desktop software candidate: full compile/bind/link with input
  hashes; primary selection, arrangements, 125/150% scaling, mixed-scale seams
  and cursor restoration all pass.

Build the hosted policy tests using desktop_backdrop.gpr, backdrop_pixels.gpr,
backdrop_upload_tests.gpr and backdrop_owner_tests.gpr. The upload and owner
executables take scenarios 0, 1 and 2. Their device is mocked; real staging
memory is used. The desktop_backdrop_real.gpr oracle uses real hosted Vulkan.
Run test-desktop-backdrop-real.sh in vulkan-affine-shell.nix, with
CUBIT_FONT_HOST_ARCHIVE pointing to the matching frozen host font library.
CUBIT_WALLPAPER_OBJECT and CUBIT_CUBIE_OBJECT optionally select frozen embedded
asset objects; otherwise the existing Desktop build objects are used.

This prepares the GPU path. Mainloop GPU routing, complete decoration/client
capture, physical display handoff, hardware tear-free operation and measured
240 Hz/input-to-photon latency remain unfinished.
