# Shared file picker / mocked workspace checks

Run all hosted model/widget tests plus the real Linux Workbench event loop:

```sh
nix develop -c bash tests/ccl-file-dialog/run-preview.sh
```

This never opens a visible window or reads/writes user documents. The mock
workspace exists only in memory. An SDL test driver captures frames in a fresh
`/tmp/cubit-file-dialog.*` directory and sends hover, Open, selection, Cancel,
Save, filename typing, and load events through the shared Workbench loop.
Pillow checks status-bar changes, selection/filename updates, and unchanged
source pixels after modal typing and save/load. PNGs are left for inspection.

The separate Ada test covers invalid/path names, non-1 string bounds, empty and
maximum-size source, invalid source bytes, duplicate-name rejection, capacity
failure without overwriting earlier files, modal keyboard/mouse interaction,
drag cancellation, scrolling a full list, and clipped dialog drawing.

Focused proof (not the whole dialog or filesystem):

```sh
nix develop -c sh -c 'cd kernel && alr exec -- gnatprove -P ../tests/ccl-file-dialog/dialog_tests.gpr -u cubit-file_selection.adb --level=2 --report=all --checks-as-errors=on -j2'
```

Native IPC end-to-end, using a temporary guest disk:

```sh
nix develop -c make -C kernel ccl-workbench
nix develop -c tests/headless/run.sh --test ccl-workspace --accel kvm --timeout 40
```

The native test checks actual persisted `ccl-0001.ccl`, `ccl-0002.ccl`, and
`clock.ccl` contents and rejects an attempted overwrite. The Linux mock does
not emulate IPC wire encoding, policy checks, grants, or storage failure modes.
