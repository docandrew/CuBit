# Workbench input fairness

The common Workbench event loop uses `Client_Input_Budget`: at most 32 poll
attempts and a one-millisecond admission window per batch. One initial poll is
always admitted. The count bound applies even with a frozen clock; backward
clock movement stops the batch after its first poll. Each batch then returns to
rendering, periodic work and VM service. Existing immediate-mode pointer barriers
may end a batch earlier. If the queue has not been observed empty, the platform
yields to ready peers without a timed sleep before the next batch.

This is an admission bound, not a one-millisecond execution guarantee: an
individual handler, FFI call, VM action or paint can run longer, and the clock
has millisecond resolution. Native scheduler behavior remains outside this
SPARK policy's proof. It preserves event order and does not discard/coalesce
text, key or pointer edges.

## Checks

Run inside the repository's pinned Nix shell:

```sh
gprbuild -p -P tests/compositor/input_budget.gpr
tests/compositor/build/input-budget/input_budget_tests
gnatprove -P tests/compositor/input_budget.gpr -u client_input_budget.adb --level=2 -j1
```

The proof has eight successful analysis results, zero unproved or justified
checks. The policy test covers 1,188 admission cases including extreme clock
values, rollback and wraparound, then 10,000 sustained-input batches.

Build the actual hosted Workbench under the shared build lock:

```sh
flock --exclusive --nonblock coordination/build.lock nix develop -c bash -c '
  set -e
  make -C kernel ui-fonts-host
  gprbuild -p -P userspace/ccl/ccl_ui_preview.gpr
  bash tests/ccl-input-budget/run.sh
'
```

`flood.c` interposes only on the test process's SDL input, clock, presentation,
yield and delay boundaries. The real Workbench loop, editor, renderer and
platform adapter execute. Input is always ready, and a guard fails if more than
32 events are consumed before painting. Frozen-clock mode requires four batches
of exactly 32 text events; advancing-clock mode requires four batches of one.
Both require scheduler yields and reject timed sleeps while input remains.
The interposer quits the preview after four presentations; `timeout` bounds a
failed run. Normal application code has no test hook or automatic redraw timer.

Native regression (also under the shared lock):

```sh
make -C kernel ccl-workbench
bash tests/headless/run.sh --test ccl-workspace --accel tcg,thread=multi \
  --cpus 4 --timeout 120 --keep-logs
```

The native workspace fixture exercises protected publication, editing, file
save/open, live-label operation and REPL behavior. The continuous queue oracle
is hosted; this does not measure native saturated-input latency, hardware
presentation, 240 Hz operation or physical keypress-to-photon time.

2026-10-01 validation: all policy/proof checks and both actual-loop flood cases
pass. Native build and the full 120-second `ccl-workspace` run pass. Logs:
`/tmp/cubit-input-budget-policy.log`, `/tmp/cubit-input-budget-verified.log`,
`/tmp/cubit-input-budget-native.serial`; source, proof and binary hashes:
`/tmp/cubit-input-budget-inputs.json`.
