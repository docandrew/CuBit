# ACPI startup completion regressions

Export TMPDIR=/home/doc/cubit-build-tmp before entering the pinned Nix environment,
then run `bash tests/acpi-startup-completion/run.sh`. `--validate-only` checks the
closed input catalog without compiling. All outputs are unique below TMPDIR.

The suite checks the actual production policy, report adapter and 0253 client:

* 1,024 exhaustive phase/module-error/setup combinations plus 100 receipt/count retention combinations per checked/release profile.
* 58 transport scenarios per checked/release profile, including every
  receipt, malformed replies, lost receipts, immutable bounded confirmation,
  high 64-bit IDs, finite original deadlines and exact three-word launch-count receipts and retention after uncertain/positive dispatch.
* 2,666 actual report checks against the hosted Core and real report specifications.
  Discovery bodies fail closed and must never be invoked. The full Core uses
  -O0 -gnatp; this does not claim an assertion-enabled Core run.

Endpoint/IPC/GETTIME boundaries are modeled. Every compiler command has a finite
180-second limit, each test has 30 seconds, one compiler job, a 24-GiB address-space
limit, no core dumps and a 64-MiB stack. Original and snapshotted inputs are rehashed
around commands. The native-entry suite separately checks actual Native_Instance
and runtime/Startup selection. Neither suite establishes live native ordering,
hardware behavior or SPARK proof. No full build, kernel or QEMU invocation occurs.

A known Received receipt with Released=0 clears only the completion declaration's
retention obligation, allowing normal stop/retirement when other owners are quiet.
Positive full-width counts, unknown replies and non-Received receipts after dispatch
retain the owner until child quiescence exists. Two-word replies and inconsistent
nonzero counts are rejected. Other HID/physical/retained-result obligations remain
independent. Completion follows Event_Domain.Start/Configure; runtime GPIO/AML
register arbitration remains a separate integration obligation.
