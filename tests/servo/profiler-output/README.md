# Profiler shutdown regression

Run `test-native.py` in Nix within a private workspace with `--seed`, `--app`, `--desktop`, and `--kernel` absolute paths, as for the IntersectionObserver native fixture. The seed directory provides init.ccl, desktop.img and boot.iso and must omit Bookmarks for the negative case.

The default case enables timing output with its directory absent: Penny must close normally, log the output failure, and avoid panic/abort. Repeat with `--directory` to create the directory in the disposable image, then require an extracted timing TSV. Both cases also exercise an outward window resize. The original Servo profiler panicked on the missing directory. `userspace/servo/profiler_output.py` makes creation and writes fallible for file and stdout destinations while preserving profiler shutdown acknowledgment.

Native CuBit tests cover file creation failure and successful report writing. They do not simulate every filesystem error or prove general crash freedom.

Disposable VM images and copied binaries are removed automatically on exit, including failed tests. Logs, screenshots, result files and SHA-256 artifact manifests are retained. Use `--keep-images` only when a specific failure requires examining the disk or binaries, and remove them after diagnosis.

Use `--directory --host youtube.com` for a network-page shutdown profile. The runner waits up to 60 seconds for the identified Penny process to be reclaimed, then checks its network-scope retirement before extracting the timing TSV. Reported close duration is host wall time under emulation.

With `/servo/profile-check`, Penny enables Servo's existing script-event profiler and logs `PENNY-JS` classic-script compilation and execution durations in microseconds. Compilation logs include original UTF-8 source size and URL; execution logs identify the document. These are elapsed spans, including any nested work and waits, not thread CPU time. They do not cover all module or callback execution. Event categories and nested compile/execution spans must not be added together. This instrumentation does not read the clock or emit diagnostics during normal browsing.

The default accelerator is explicit TCG. Use `--accel kvm` with host device access for hardware-assisted virtualization; it uses `-cpu host` and does not silently fall back. `execution.json` records the actual command and mode. Neither mode alone establishes a fair cross-browser benchmark.

The `--no-profile` control uses the same navigation, resize, close, process-reclaim,
and network-scope-retirement gates, but removes `/servo/profile-check` from the
disposable disk and skips TSV expectations. It retains the lightweight existing
load/frame/input diagnostics. Use it to distinguish a profiling-path failure from
normal operation; a single successful navigation is not a stability guarantee.

The opt-in shell stall probe reports a phase only when its iteration sequence has
not advanced between one-second samples. Phase 7 covers `WebView::paint()`. Paint
breadcrumbs further bracket query collection, renderer update, and draw. These
are localization diagnostics, not accurate performance measurements: logging and
the sampling thread affect scheduling. The probe is joined on normal shutdown.
