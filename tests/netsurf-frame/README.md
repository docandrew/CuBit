# NetSurf frame-binding boundary

Run inside the repository's Nix environment:

```sh
nix develop -c python3 tests/netsurf-frame/test-boundary.py
```

The test extracts the actual `cubit_netsurf_redraw` function from the production
adapter and compiles it with observable foreign-library mocks and ASan/UBSan.
It covers 140 cases: clipped damage with padded pitch, extreme/invalid extents,
null admission, extreme caret geometry, original surface/clip restoration,
and restoration even when the browser renderer reports failure. The renderer
mock also changes the clip so caret clipping cannot depend on residual state.

This is a C FFI boundary test, not a SPARK proof or a NetSurf rendering test.
Actual mapping validity and exclusive writable ownership remain caller
obligations. The library owns its original RAM surface; redraw borrows the
application pointer synchronously and restores the library surface afterward.
No pixel buffer or copy is introduced by this binding.

2026-10-01: all 140 cases pass in `/tmp/cubit-netsurf-frame-final.log`.
Production-flags syntax and object compilation with real NetSurf/libnsfb
headers pass. `make -C kernel netsurf-https-test` rebuilt the fixture and
restored the normal homepage archive/application. The native four-CPU TCG
`netsurf-https` regression passes its 120-second run, native shell marker,
real TLS 1.3 page-fetch fixture and final guest fault scan. Evidence:
`/tmp/cubit-netsurf-frame-native.log`,
`/tmp/cubit-netsurf-frame-native.serial`, and source/artifact hashes in
`/tmp/cubit-netsurf-frame-native-inputs.json`.

Reproduce the native check while holding the shared build lock:

```sh
flock --exclusive --nonblock coordination/build.lock nix develop -c bash -c '
  set -e
  make -C kernel netsurf-https-test
  bash tests/headless/run.sh --test netsurf-https --accel tcg,thread=multi \
    --cpus 4 --timeout 120 --keep-logs
'
```

The native fixture verifies startup/fetch and checks for reported guest faults;
it is not a pixel oracle, protected-buffer test, mixed-DPI test or performance
measurement. NetSurf still uses the legacy application path pending density-aware
rendering and protected-frame integration. Renderer return status is still
handled as before; the mocked failure case verifies restoration only.

## Signed content invalidation

`Client_Signed_Clip.Edge` is a pure SPARK function exported directly to the C
frontend. It translates each signed content edge by a scroll offset in widened
arithmetic, then clips to the viewport. Its contract proves the exact result
and bounds for every `Interfaces.C.int` input, including nonpositive limits.
The C frontend only marshals edges and forwards a nonempty rectangle; it does
not subtract potentially extreme signed coordinates before clipping.

```sh
nix develop -c gprbuild -p -P tests/compositor/signed_clip.gpr
nix develop -c tests/compositor/build/signed-clip/signed_clip_tests
nix develop -c gnatprove -P tests/compositor/signed_clip.gpr \
  -u client_signed_clip.adb --level=2 -j1
nix develop -c python3 tests/compositor/test-browser-invalidation.py
```

The policy test checks 583,164 cases against a piecewise interval oracle.
The C/Ada boundary test extracts production `embed_invalidate`, links the actual
Ada object through its C ABI, and runs 6,562 cases with ASan/UBSan on the C side.
It checks negative/extreme/inverted/offscreen rectangles, extreme scrolls,
empty suppression and the explicit whole-view invalidation sentinel.
The Ada object has assertions and overflow checks enabled. The SPARK report
contains four successful analysis results, zero unproved or justified checks.
These are hosted checks; validity of the supplied foreign pointers remains
outside the proof, as does NetSurf itself. This does not implement DPI scaling.

The integrated signed-clipping change also passes the complete native
`netsurf-https-test` build and 120-second four-CPU TCG HTTPS regression.
Logs: `/tmp/cubit-browser-clip-native.log` and `.serial`; input hashes:
`/tmp/cubit-browser-clip-inputs.json`. This verifies native linkage/startup/fetch
and reported guest faults, not rendered pixels or extreme-coordinate injection
inside CuBit (those cases are covered by the hosted policy and ABI tests).
