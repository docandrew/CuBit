# HID driver hosted regressions

Run from the repository's Nix development environment, with a private output
root selected before entering Nix:

```sh
export TMPDIR=/home/doc/cubit-build-tmp
nix develop --command bash tests/hid-i2c-native/run.sh
```

The runner works independently of its caller's working directory. Optional
`--source-root DIR` selects a different complete source tree. `--validate-only`
checks source hashes and generated fixtures without compiling or running Ada;
its result explicitly does not claim functional testing.

At a proof milestone, `run.sh --prove-only` runs SPARK flow analysis and level1
proofs for the actual `HID_Startup_Readiness` unit. The project includes only
that policy and its pure wire/type dependencies; no boundary model participates
in the proof. It checks one-shot selection, clock/deadline/stop precedence,
first-receipt preservation and the readiness/stop/deadline query contracts.
The gate requires all analyzed entities in SPARK, no skipped checks or Assume
statements and valid prover results. Its machine-readable dependency audit
rejects external claim references. This is a policy proof, not proof of native
delivery, caller ownership, Main, the driver or AML interpretation. Callers must
honor the Delivery precondition and keep one serialized policy state.

The default runs five mains in checked and release profiles, with a separate
startup-cutoff scenario in the pipeline main:

- `readiness_tests`: one-shot startup reporting, clock/deadline/stop precedence,
  first-delivery preservation and exact native reporting frames/deadlines using
  modeled transport.
- `system_event_tests`: full publisher lifetimes, header/length validation,
  channel/grant/control routing, logstore-peer filtering and native clock/queue
  adapters with modeled kernel operations.
- `calibration_tests`: actual descriptor parsing and slot compilation derive
  matching logical ranges. Single/multiple slots, inconsistent and empty axes,
  early parser refusal of reversed limits, signed/unsigned full-width bounds,
  budget failure and invalidation are covered. These are logical coordinates,
  not physical-unit normalization or hardware observations.
- `touchpad_publication_tests`: actual HID descriptor/report parsing, buffered
  I2C lifecycle, reset/serving GPIO composition, pointer publication, queue pump,
  wait adapter and generated launch-data decoding. Shutdown regression retains
  the zero-button recovery report across buffer close, an absent consumer and
  refused publication; native `Drained` stays false until output is drained.
  The startup-cutoff scenario queues a GPIO arm reply and an early pin event:
  expiry of the overall startup budget stops dispatch before a new input read,
  while the timely arm result is consumed and its lease released separately.

`sources.json` names exact relative source inputs, including their hashes.
Production code comes from its normal service/lib locations. `host/` contains
explicit boundary models; generated parameter fixtures are unchanged compiler
outputs. Their manifest/schema hashes, descriptor shape/SHA-256 and decoder
identity are checked before execution. Regenerate and revalidate these fixtures
when the manifest/schema changes. Native startup/launch-block access is not
executed, and the driver Main is not run by these tests.

Each invocation creates unique output directories under TMPDIR. Builds are
serial at nice19 with a180-second limit; each test has30 seconds,64MiB stack,
no core dump and a1MiB log limit. Source drift fails the run. `COMPLETE.json`,
`commands.json`, input hashes and bounded logs record the scope/results. Release
`-gnatp` is for hosted functionality comparison; this does not authorize checks
suppression for unproved production units.

The default functional tests are Linux-hosted with modeled grants/queues/
transport. They provide no native driver execution or physical hardware
validation; the optional proof mode has the narrow policy scope above.
Native semantic/declaration gates, kernel/image builds and hardware acceptance
are separate. AML code is unchanged by this suite; its ACPICA and proof gates
remain separate obligations.

- `hid_i2c_interop_tests`: actual buffered HID client joined to the actual DesignWare request handler, controller owner and native completion encoder. Thirteen cases cover combined/repeated-START, read-only/write-only, refused/uncertain input/output copies, unauthorized caller, lost reply and hardware abort; wrong token/peer, delayed retirement, payload suppression and no replay are checked. MMIO/grants/transport remain explicit models. Abort must preserve faulted/potentially-enabled state; successful completion is separately disabled. This is hosted interoperability, not native execution or an electrical/hardware test.

The actual-handler interoperability fixture also fills its combined 16-byte
transfer capacity with write-only, read-only and 2-byte-write/14-byte-read
transactions. The receive FIFO model has only two slots, so the latter two
cases exercise repeated receive-credit replenishment. Exact command count,
final STOP, repeated START for combined transfers, copied bytes, grant
retirement and no replay are checked. This is a bounded hosted instance,
not a claim about real FIFO depth or physical controller timing.
