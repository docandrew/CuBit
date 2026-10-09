# Backend admission and recovery policy

Run in the repository Nix environment:

```sh
gprbuild -p -P tests/compositor/backend-selection/test.gpr
tests/compositor/backend-selection/build/check
gnatprove -P tests/compositor/backend-selection/test.gpr -u compositor_backend_selection.adb --level=2 --report=all
```

Set BACKEND_SELECTION_OBJECT_DIR to an absolute private directory for concurrent
runs. The tests cover all 128 startup readiness combinations, 16384 reselections,
early-output software selection, all 64 drain-evidence combinations, stale keys,
uncertainty, duplicate recovery and rejected GPU reactivation.

The pure SPARK policy admits GPU only when all seven readiness observations are
true. Selection is one-shot. An explicit recovery records the exact output,
epoch, frame and buffer identity; capture is blocked while draining. Software
recovery requires renderer, sources, readback and output-writer retirement plus
queued full repaint. Uncertainty quarantines; stale observations cannot switch.

Proof establishes policy transitions conditional on supplied facts. It does not
authenticate a capability, validate a Vulkan handle, prove GPU/Display retirement,
or prove the Main/FFI adapters. Full repaint means queued invalidation, not a
completed frame. Native integration and physical hardware remain separate gates.
Publishing this policy alone does not switch the shared Desktop build to Vulkan.
