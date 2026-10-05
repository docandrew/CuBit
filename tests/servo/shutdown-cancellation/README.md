# Shutdown cancellation

Native fixtures deliberately keep JavaScript running when File → Close is requested. The window fixture interrupts a custom-element constructor; the worker fixture interrupts an imported script. The window also verifies an ordinary InvalidCharacterError before entering the loop.

Run `tests/servo/run-shutdown-cancellation.py` inside the Nix shell with `--mode window` or `--mode worker`, `--seed` pointing to a disposable desktop seed directory (desktop.img, boot.iso, init.ccl), and explicit `--app`, `--desktop`, `--kernel`, `--accel kvm --cpu Broadwell --hda --no-profile --directory`. Use the private-workspace runner for a private seed. It copies the images and verifies payloads, requires the loop-start and mode-specific cancellation markers, exercises resize, closes through the menu, and requires process/scope reclamation without panic. Images are cleaned by NativeArtifacts.

The staged pre-fix binary fails the window case at error.rs:91, JS_IsExceptionPending(cx), after shutdown interrupts the constructor. The fix retains that assertion for live globals and preserves uncatchable cancellation only for closing Window or WorkerGlobalScope. This does not establish that every shutdown/error path is correct.

The worker control uses the existing ImportScripts closing path, which returns without JSFailed. It checks that this behavior remains intact; it does not exercise the new exception-handler worker branch.
