# Production startup render denial

This native CuBit/QEMU regression uses the production procmgr and devmgr.
QEMU supplies no Intel render provider. Two copies of a sentinel executable
declare `(request-render read-write render)`:

* `render-denied.app` has no startup approval and must not submit admission.
* `render-unavailable.app` is explicitly approved. It must submit admission,
  but cannot resume without an admitted rendering session.

The application emits `TEST: FAIL` if its entry point executes. The runner
requires exactly two denied launches, exactly one submitted request, two
successful child-stop requests, both named startup failures, and a subsequent
Devices inventory/window. It rejects kernel kill-authority denial; its normal
fault scan rejects the sentinel and cleanup failures. Stop acceptance is not
proof that remote GPU resources have retired: uncertain broker state remains
retained separately.

Run from the repository root, with no other shared build running:

```sh
flock --exclusive --nonblock coordination/build.lock nix develop -c bash -c '
  make -C kernel procmgr devmgr devices render-launch-policy &&
  bash tests/headless/run.sh --test render-launch-policy \
    --accel tcg,thread=multi --timeout 70 --disk /path/to/test-base.img'
```

The runner copies the base disk before installing test images. Neither this
fixture nor its applications are included in production startup profiles.
This is negative admission coverage, not successful production admission,
Mesa device creation, or Intel hardware execution. Existing reciprocal
admission IPC fixtures separately exercise successful synthetic GPU admission.
