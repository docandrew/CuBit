Hosted execution of the unchanged actual native I2C Main with modeled controller facade, channel service, startup handles, logging and syscall boundaries. Actual binding decoder, control-event decoder and launch types are used. 23 cases cover MSI finite waits, polling fallback, startup refusal/retention, IPC wake/reply, Stop dispatch, bounded event batches, idle deadlines and malformed notice envelopes. This suite does not execute the actual controller/service bodies; tests/i2c-interrupt-wait covers that separate composition.

Run with TMPDIR=/home/doc/cubit-build-tmp in the Nix development environment:

```sh
bash tests/i2c-msi-entry/run.sh
```

`--validate-only` checks pins and classifications without compilation; `--source-root DIR` selects a source snapshot. The runner uses fresh private outputs, O2 assertion/overflow checks and O3 suppressed checks, strict warnings, resource limits, source revalidation, and exact test counts. Modeled interfaces are explicitly listed in sources.json. Native source-context pins are not native execution. No proof claim is renewed by this suite.
