# Copied LoadTable during controlled module Field startup

In the pinned repository Nix environment, export TMPDIR=/home/doc/cubit-build-tmp
and run `bash tests/acpi-copied-module-fields/run.sh`. `--validate-only` checks
source/fixture/tool expectations without compiling or invoking ACPICA.

The actual Core and interpreter stage a DSDT and retain an owned copied SSDT.
The caller buffer is overwritten before startup. The DSDT writes a named modeled
EC register, then LoadTable executes the copied SSDT's two register writes through
the same controlled Field context. Success checks all three effects, caller-width
True, duplicate-load refusal without extra I/O/result roots, and one-shot startup.
An uncertain second write preserves the exact receipt and completed effects;
denied writes and the default hardware-free executor perform no I/O.

Four cases run at both AML widths in checked/release profiles:152 checks in8
processes,81 compiled Ada units/profile. Provider identity, permissions, hardware,
transport and completion are modeled. This is not a native EC controller test,
physical ownership/lifetime validation or a new SPARK proof.

Five normally compiled iASL fixtures must match pinned AML bytes. A separate normal
ACPICA reference confirms AE_ALREADY_EXISTS for an already-loaded table and an
unchanged marker. Four outstanding cache allocations are retained and qualified;
no clean teardown or reference equivalence for hardware/startup is claimed.
The full table-loading, definition lifetime and interpreter objective remains open.

Fresh private output, one compiler job, nice19,64MiB stack,core0,180seconds/build,
30seconds/runtime and1MiB logs. Checked uses assertions/overflow checks; release
uses -gnatp. Each source/tool/runner/fixture and compiled profile is checked.
