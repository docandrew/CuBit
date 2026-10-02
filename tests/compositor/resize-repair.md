# Resize-release footprint regression

A window's actual old rectangle and its previously presented resize outline
are different objects. When shrinking, replacing the old rectangle with the
outline drops the exposed part of the original window from repaint damage.
Desktop must retain old surface, new surface and previous outline footprints.

`Compositor_Transition.Cover` is a pure SPARK envelope policy. Its postcondition
proves containment of both surface rectangles and, when present, the old outline;
it also proves a valid result when either surface rectangle is valid. Level-2
GNATprove reports one functional contract and one termination result, zero
unproved. The policy adds no allocation, image storage, copies or queued work.

```sh
nix develop -c bash -c '
  gprbuild -q -p -P tests/compositor/transition.gpr &&
  tests/compositor/build/transition/transition_tests &&
  gnatprove -P tests/compositor/transition.gpr -u compositor_transition.ads \
    --level=2 --checks-as-errors=on -j1
'
nix develop -c python3 tests/compositor/test-transition-integration.py
```

The policy test covers 51,200 shrink/grow/preview cases and coordinate limits.
The integration harness compiles Desktop's actual release damage expression,
rectangle adapters and visual-margin expansion with the real pure policy.
It checks 6,868 combinations, including a previous outline outside both final
and old surface bounds. Main, coordinate conversion, display IPC and renderer
execution are not covered by the policy proof.

Under the shared build lock, after building/staging the intended Desktop:

```sh
nix develop -c python3 tests/compositor/run-resize-repair.py UNIQUE_TAG
```

The native observer shares the desktop-display regression VM after its normal
input sequence. It enlarges and shrinks Workbench three times, waits for the
outline to be displayed before release, parks the cursor elsewhere, and requires
exact restoration of 48,000 wallpaper pixels after each shrink. It first checks
that the enlarged window visibly changes that region. The standard runner
continues to enforce its final native fault scan. This is a functional pixel
regression under four-CPU TCG, not a performance or hardware scanout benchmark.

Before the fix, the actual release-code harness failed containment of the old
window, and the native oracle found 36,978 stale pixels on the first shrink:
`/tmp/cubit-transition-before-glue.log`, `/tmp/cubit-resize-before-run.log`,
`/tmp/cubit-resize-before.resize-shrunk-0.ppm`. Servo independently exposed the
same symptom in `/tmp/cubit-servo-tabs-horizontal.png`.

After the fix, session 42907 completed with exit 0: Desktop build/link/staging,
three native enlarge/shrink cycles with exact 48,000-pixel restoration each,
the 100-second desktop-display test and final fault scan all passed. Source and
staged Desktop hashes matched `/tmp/cubit-resize-after-inputs.sha256` at exit.
Logs: `/tmp/cubit-resize-after-retry.log`, `/tmp/cubit-resize-after-native.log`,
`/tmp/cubit-resize-after.serial`. Full captures are
`/tmp/cubit-resize-after.resize-{baseline,enlarged-0,shrunk-0,enlarged-1,shrunk-1,enlarged-2,shrunk-2}.ppm`;
convenience PNGs: `/tmp/cubit-resize-before.png`, `/tmp/cubit-resize-after.png`.
This native case uses the software direct-output path and vertical resizing;
both-axis envelope coverage is proved and exercised by hosted tests. The full
Servo scenario, Mesa native-density path and physical NUC display were not
rerun for this change. The default staged Desktop contains the fix.
