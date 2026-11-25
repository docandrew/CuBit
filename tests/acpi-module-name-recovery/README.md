# Module name-recovery regressions

The T580's first `UNKNOWN_NAME` was the rooted `\XBAS` operand in an
OperationRegion address expression in SSDT **SgPch**, resolved from
`\_SB_.PCI0.RP01`. Its definition was a field in a later SSDT in the recorded
replay order. That order has not been established as the firmware's XSDT order.
ACPICA reported the missing name, removed the incomplete region, retained
uninitialized dependent fields, and continued loading. It did not successfully
resolve that early reference or heal the failed region after the later definition.

These regressions use only our own ASL. They contain no captured tables,
laptop addresses, OEM values, or laptop probe lists. The synthetic `BASE`
provider appears before, after, or outside the consumer. Explicit rooted-name
controls cover the same lookup shape. Completed prefix effects execute once;
remaining terms execute in order, matching ACPICA's observed recovery behavior.
Skipping the entire expression, retrying it, or deferring it past following
statements fails these guards.

Run in the pinned development environment with fresh output directories:

```sh
export TMPDIR=/home/doc/cubit-build-tmp
timeout --kill-after=3s 650s nix develop -c python3 tests/acpi-module-name-recovery/run.py --profile shipping --output /tmp/acpi-name-shipping
timeout --kill-after=3s 650s nix develop -c python3 tests/acpi-module-name-recovery/run.py --profile checked --fixtures /tmp/acpi-name-shipping/fixtures --output /tmp/acpi-name-checked
```

The suite has 26 programs per profile, compiled from 20 own ASL units with
normal IASL `-oa`. The checked run can reuse those exact compiler outputs;
source and AML hashes must match. Ada builds use the canonical profile flags,
warnings as errors, all warnings on the test main, j2 and a 240-second cap.
Every program retains the 12-second wall, 24-GiB address-space and 128-MiB
stack limits. Namespace, method, startup fuel and target limits match the
qualification harness; tests do not reduce them to obtain a pass.

The purpose-built owner admits only the four synthetic memory bases used by
the fixtures. Each target is an in-process cell authenticated by the test's
issued owner identity. It never dereferences, maps, reads or writes an address,
opens a device, or sends IPC. Delays and clocks are unavailable. No device or
region initialization policy is simulated beyond the field owner needed here.

Checks cover module completion, the following declaration, prefix/tail write
counts and order, recovery count and typed diagnostic, invalid-field reads,
`CondRefOf`, opaque `RefOf` arguments, writes, and DWordAcc metadata. Three
programs deliberately trip a Fatal guard; two expect Uninitialized and one
expects Missing_Operand. Those are expected outcomes, not positive AML passes.
Invalid-field reads and inspection fail without provider I/O; names and opaque
references still exist, and writes report the missing destination operand.

The engine adds `Missing_Operand`; startup diagnostic code 44 is appended while
codes 0–43 retain their values. The contract ledger lists the two existing write
Post outcome domains widened to admit this real failure. Preconditions and
frame/cursor guarantees remain intact.

`RESULT.json`, `COMMANDS.json`, `INPUTS.json`, compiler logs and per-case logs
record commands, counts, flags, hashes and first failures. A failed expectation
stops the run. These are hosted synthetic regressions, not native hardware,
the full ACPICA suite, performance parity, or SPARK proof coverage. The private
candidate still stops later in the T580's same table with Uninitialized; full
T580 initialization is not established by this packet.
