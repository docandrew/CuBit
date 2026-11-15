Hosted native serving-loop regressions

In the Nix environment run `bash tests/acpi-native-server-loop/run.sh`.
`--mode checked` enables assertions/overflow checks on the loop and test bodies;
Core/Namespace code generation stays -gnatp because of the known compiler issue.
`--validate-only` checks hashes, exact directory inventories and source mirrors.

The suite exercises real observer/serving code with modeled messages, clocks
and logging: idle and finite waits, spurious wakes, fairness, stop ordering,
maintenance refusal and retained queues. It proves no hardware or whole-service
quiescence. The separate prove.sh remains an optional selected policy proof;
refreshing its project closure does not extend historical proof evidence.

Outputs go to fresh TMPDIR directories, not shared source directories. Builds
use one worker, 180s timeout; execution30s; address space24GiB; output1MiB;
stack64MiB, no core dumps. Exact ALI unit inventory and per-unit flags are checked.
Compiler warnings remain errors. No kernel/image/QEMU/native execution.
