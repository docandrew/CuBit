Hosted I2C interrupt owner, planner-demand binding and channel-turn regression suite. Uses actual owner/controller/turn, planner/configuration/reset/byte adapters and runtime ring bodies with modeled MMIO/FIFO/clock and message syscall declarations. Includes 24 startup/wait failure scenarios, binding/vector/message matrix, original transfer/register/configuration/LPSS checks and composed channel lifecycle. No AML changes or ACPICA oracle are involved; these are transport tests.

Run with TMPDIR=/home/doc/cubit-build-tmp in the Nix development environment:

```sh
bash tests/i2c-interrupt-wait/run.sh
```

`--validate-only` checks pins and classifications without compilation; `--source-root DIR` selects a source snapshot. The runner uses fresh private outputs, O2 assertion/overflow checks and O3 suppressed checks, strict warnings, resource limits, source revalidation, and exact test counts. Modeled interfaces are explicitly listed in sources.json. Native source-context pins are not native execution. No proof claim is renewed by this suite.
