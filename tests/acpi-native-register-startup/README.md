# Hosted ACPI startup and query regression

This suite runs actual AML Core, inventory/provider ownership, startup selection,
module execution and query callbacks with modeled IPC and hardware. It does not
run a native CuBit service or prove the interpreter.

Export TMPDIR=/home/doc/cubit-build-tmp **before** entering nix develop, and keep
it exported inside the development command. Run `run.sh` from the Nix environment
with gprbuild available. Provide `IASL_BIN` and `ACPIEXEC_BIN` if ACPICA is absent
from PATH; executables must resolve to the Nix store. Set `NODE_BIN` if necessary.
`ACPI_TEST_OUTPUT` selects an existing private parent directory; each run creates
its own output directory. Stack is 64 MiB, core dumps are disabled, compiler
commands use one worker with a 180-second limit, executions have 30-second limits,
and each command's output is bounded to 1 MiB. Run with niceness 19 when sharing
build resources.

The runner verifies sources.json, makes regular private source copies and compiles
only those copies. It checks original and copied hashes before/after commands and
refuses directory inventory changes. Checked builds use assertions/overflow
checks; release function testing uses -gnatp. A passing run reports 1,530 assertions
in 30 scenario processes across both profiles. Scenarios cover missing endpoints,
finite deadlines and clock failures, one-shot module startup, refused selection,
observer scalar/Buffer queries, register-backed argument calls, uncertain field
completion, and permanent closure. Pending I/O is retained without replay.

Fresh iasl builds must byte-match both embedded AML images (399 and 59 bytes).
acpiexec comparisons cover the observer integer/Buffer and root/_SB _INI values.
ACPICA's automatic device initialization differs from the hosted test's deliberate
refusal of an uninstalled region handler; no equivalence is claimed for that
barrier. ACPICA 20260408 reports four outstanding cache allocations at shutdown;
the report preserves this diagnostic and does not claim a clean shutdown.

The suite does not install region handlers, issue _REG notifications, grant raw
hardware access, drive an I2C controller, deliver native interrupts, establish
retained-value lifetime correctness, or constitute a new SPARK proof. Those
remain separate integration and verification work.
