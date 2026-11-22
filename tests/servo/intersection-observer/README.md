# IntersectionObserver on CuBit

Penny enables the pinned Servo implementation after applying lifecycle, percentage-margin and cross-origin geometry repairs in `userspace/servo/intersection_observer.py`. The native test checks visible/outside transitions, disconnect/reobserve, last-unobserve/reobserve, percentage margins against a nonsquare root, and null root bounds in a sandboxed cross-origin iframe.

Run in Nix in a private build workspace (never against the user disk):

```sh
python3 tests/servo/intersection-observer/test-native.py --seed /absolute/native-fixture-directory --app /absolute/cubitshell.app --desktop /absolute/desktop.svc --kernel /absolute/cubit_kernel
```

The seed supplies init.ccl, desktop.img and boot.iso. The runner creates disposable copies, verifies injected binaries, boots CuBit in QEMU TCG, and retains logs/screenshots. Native acceptance on 2026-10-03: lifecycle baseline failed after disconnect; patched lifecycle and expanded privacy/margin fixture passed. YouTube also rendered and survived 120 seconds with 12 alternating resizes, menu open/Escape and body clicks without abort or input resynchronization. This is not proof of full IntersectionObserver conformance, arbitrary-site isolation, leak freedom or video playback. Nonlocal top-document intersection geometry remains limited in upstream Servo; no synthetic viewport geometry is exposed.

Disposable VM images and copied binaries are removed automatically on exit, including failed tests. Logs, screenshots, result files and SHA-256 artifact manifests are retained. Use `--keep-images` only when a specific failure requires examining the disk or binaries, and remove them after diagnosis.
