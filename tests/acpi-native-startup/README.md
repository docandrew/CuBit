# Direct native ACPI owner lifecycle

Run `bash tests/acpi-native-startup/run.sh --mode checked` or `--mode release`
inside pinned Nix with TMPDIR=/home/doc/cubit-build-tmp. The default is release;
`--validate-only` checks pinned inputs, closed directory inventory and mirrors.

This replaces the old provider/configure startup API suite. The actual new
Native_Instance.Start executes against actual Requests/Core/AML and generic local
snapshot startup. Runtime entry, clock, registration, snapshot source, serve loop
and logger delivery are MODELS here. Boundary source/declarations are exact
mirrors. The separate native-entry suite checks actual runtime declarations;
the separate native-server-loop suite executes the actual loop with modeled IPC.

52 fresh processes cover13scenarios at32/64bits with successful/throwing Critical
logging: normal startup, module/root/bus/device Fatal, ordinary initializer error,
unconfigured region, unavailable/overflowing clock, snapshot/copy refusal,
registration refusal and unexpected loader exception. Assertions check one load,
finite microsecond deadline, no registration before completion, serve ordering,
one exact Critical diagnostic attempt, and no effect on a second Start. A logging
exception must preserve the load outcome; no reset of the process-lifetime latch
is exposed by the fixture. The model's native-copy routines fail if invoked.

Build180s, each process30s, stack64MiB, logs1MiB, nice19/j1. Compiler tool, source,
mirror, profile and artifact hashes are audited. This is no native/kernel boot,
stamp or capability authentication, real logger delivery, native OOM test, stack
proof, SPARK proof or hardware acceptance claim. Native allocator exits on OOM.
