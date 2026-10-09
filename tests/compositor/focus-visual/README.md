# Native titlebar focus regression

Run in the pinned compositor Nix shell. `build.py --linked DIR
--runtime-archive FILE --output NEW_DIR` builds a two-window fixture using the
verified Desktop artifact's frozen runtime and recorded manifest/native compiler.
The archive must match that runtime. No production source, service or image is
modified. Alire selects the matching native GNAT toolchain.

Copy the resulting `focus.app` alongside explicit compatible boot seeds
(`cubit_kernel`, `initrd.img`, `display.svc`, `clock.svc`, `logstore.svc`) into a
new seed directory, then run `run.py LINKED SEED NEW_OUTPUT --approve-render`.
The approved-render variant expects denied GPU admission and a fresh software
child; omit the option to request software directly. Metrics must be off.
All disk images are disposable private outputs. Input hashes are recorded and
rechecked. The runner terminates only its own VM, including on failure.

The fixture creates two overlapping 300x200 windows via the native desktop
protocol and then sleeps. Creation schedules a full redraw. Six native Alt-Tab
inputs must alternate visibly and restore the initial full-redraw reference
exactly three times. The exposed old-title strip must be uniformly inactive per
row. The oracle excludes the diagnostic HUD and taskbar clock. The coordinates
are intentionally specific to the fixture's 1024x768, 100% DPI geometry.

This is QEMU software-fallback validation, not physical GPU/scanout validation,
latency measurement, per-output DPI coverage or a new SPARK proof. Existing
`test-focus-title-damage.py` separately checks the actual damage-routing source
and rejects a removed-old-focus-damage mutant. UI hit-map repair is independent.

Initial successful evidence: `/tmp/cubit-focus-visual-native-1/result.json`,
Desktop SHA f6fc080d3ac0c61ede35dd04294ccf053a95e5a3231acf25db9de272504d6f85.
