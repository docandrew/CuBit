# Scoped native delay policy proof

Run `bash tests/acpi-native-delay-proof/run.sh` in pinned Nix, exporting
TMPDIR=/home/doc/cubit-build-tmp before entering it. The default runs fresh
SPARK levels1/2 with cvc5,5-second solver limits,nice19,j1 and180seconds per
level. `--validate-only` verifies current sources, cached reports and24negative
controls without running a prover. No whole-Core proof is selected.

The concrete1000us-slice policy instance selects6entities and discharges23checks
per level (7flow+16prover), with no Assume or skipped/unproved checks. The
proof models arbitrary typed asynchronous clock/wake inputs. It covers selected
flow,bounds, loop termination and completion metadata. The raw callback boundary
must return normally in bounded time and avoid reentry. Physical time accuracy,
CPU relinquishment, actual scheduler behavior, the stateful native facade and
all enclosing service/interpreter correctness are outside this proof.

The proof instance uses the real ACPI_Delay_Waits sources. The external-input
model is a proof fixture, never part of the native program. Source/tool/report
fingerprints preserve the distinction between fresh proof and cached evidence.
