# HID typed-channel contract proof

This suite checks the pure conversion from copied ACPI HID/I2C connection facts
into typed I2C target and GPIO interrupt descriptions. These are data, not endpoint
or hardware authority. The native-sized 256-byte copied-span instance is selected.

The strengthened contract establishes successful preparation iff the supported
resource roles, controller initiation, address/speed, pin and trigger/polarity
conditions hold. Successful output preserves the exact I2C/GPIO projection and
all copied metadata. Refusal-code precedence remains regression-tested rather
than included in the functional theorem.

Run from the repository root with the project Nix toolchain:

```sh
export TMPDIR=/home/doc/cubit-build-tmp
nice -n 19 nix develop -c bash tests/acpi-hid-channel-proof/run.sh
bash tests/acpi-hid-channel-proof/run.sh --validate-only
```

The validation-only command needs Node on PATH and rechecks sources, recipes,
original command selection and cached reports; it does not execute proofs/tests.
A fresh run requires cached/Nix GNATprove, cvc5 and gprbuild. The runner uses private
TMPDIR output, one worker, 180-second compiler/proof and 30-second runtime limits,
a five-second solver limit, 64MiB stack and disabled core dumps.

At each of levels 1 and 2 the selected Prepare instance has 23 proved checks and
2 flow checks. Its two Valid expression-function dependencies are separately
analyzed, each with a termination check and no remaining assumptions. The audit
checks this dependency closure explicitly and rejects 24 altered evidence cases.
The existing channel-policy/badge-roundtrip fixture passes 325 checks per checked
and release profile with identical output. No pragma Assume is added.

Only the instantiated function is analyzed. The generic-definition warning is
expected and does not replace the required nonempty instance report. Cached
COMPLETE.json records exact commands, source-copy and recipe hashes and tools.
The original implementation body changes only by removing a redundant use-type
clause made visible by the strengthened specification.

This does not prove every generic capacity, ACPI parsing/discovery, resource
freshness, backend support, authority, native launch/IO, the full AML interpreter
or complete ACPI service. No ACPICA invocation is needed for this pure data
adapter; interpreter/reference and hardware validation remain separate work.
This suite is opt-in and is not yet registered in the global test groups.
