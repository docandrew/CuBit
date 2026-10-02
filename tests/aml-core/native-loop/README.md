# Native ACPI IPC loop tests

ACPI_Native_Server.Run is the serialized native service runner. The launcher
supplies trusted authority configuration and provider capability slot, and owns
the service state and bulk adapter for the process lifetime. Run never resets
these objects, including when it returns a fatal status with cleanup pending.

The runner deliberately consumes all IPC classes through Poll_Any_Ipc. It has
no event subscriptions yet; unknown/untrusted messages go through endpoint
rejection. It issues no asynchronous requests, so an unexpected completion is a
fatal condition instead of a permanently ready queue causing a spin. Missing
wait support and exhausted clock values likewise return explicit stop reasons.
Reply failure does not repeat dispatch. The kernel's reply-capability checks
control delivery; sender PID does not authenticate the request.

Pending grant returns retry after 100 milliseconds using the GETTIME epoch that
matches Wait_For_Activity_Until. Deadlines saturate below the indefinite-wait
sentinel without wrapping. Requests can still be processed between attempts.
Successful cleanup restores indefinite idle waiting. The runner is tested native
boundary code, not itself SPARK-proved, and has not yet been launched on CuBit.

prepare.py extracts the actual runtime message/completion layouts, activity enum,
capability-slot subtype and time syscall number. The IPC script and grant mock
then exercise the real runner, native endpoint, block importer and service core.
These mocks do not establish kernel authentication or live IPC behavior.

```sh
nix develop -c bash -c 'set -e; ulimit -S -s 65536; python3 tests/aml-core/native-loop/prepare.py; cd kernel; alr exec -- gprbuild -p -P ../tests/aml-core/native-loop/acpi_loop.gpr; ../tests/aml-core/build/native-loop/loop_tests'
```

The standard AML runner includes this target. 2026-10-01: 23 checks passed,
covering invalid configuration, idle waiting, unexpected completion, unavailable
clock, full bulk snapshot, failed replies, delayed return, pending cleanup across
fatal returns/restart and deadline saturation. Private native static-library
compilation passed at /tmp/cubit-acpi-loop-native-c67e_8zh; all 406 recorded inputs
matched afterward. No executable link, boot, CCL interaction, hardware access or
whole-call-chain stack bound is claimed. Logs: /tmp/cubit-acpi-loop-integrated.log
and /tmp/cubit-acpi-loop-native.log.


The authenticated bootstrap extension raises the hosted count to 39 checks.
It covers forged stamps, malformed configuration, invalid/reserved slots and
tags, unavailable startup wait, failed acknowledgment and rejected reconfiguration.
`ACPI_Launch.Decode` has eight proved checks plus one termination analysis, with
zero unproved/justified checks. `run.sh --prove` includes it. The native loop and
runtime transport remain outside that SPARK proof. The service README documents
the linked executable and the still-unimplemented trusted launcher/provider wiring.
