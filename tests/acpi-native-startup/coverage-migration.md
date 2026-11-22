# Previous diagnostic/startup coverage

The native provider bootstrap and packet-Finish API are removed. The previous
acpi-native-startup compile selections and acpi-native-diagnostics suite are
superseded by these active checks, not carried as current native validation:

| Prior behavior | Current coverage |
| --- | --- |
| Owner, main, clock, log and server API typing | acpi-native-entry (actual API semantic checks; runtime rebase pending) |
| Upload/configure authorization and forged/malformed requests | acpi-native-server-loop711checks: observer-only, upload never accepted |
| Module/root/bus/device Fatal32/64bits | acpi-snapshot-startup142checks: actual local Finish and copied diagnostic after owner release |
| Exactly one Critical record and logger exception preservation | acpi-native-startup52fresh processes/profile: actual Start and formatter; delivery modeled |
| Failed/repeated Finish does not replay AML or logging | No wire Finish authority; actual Start second-call latch, tested after every success/failure/log exception |
| Lost reply does not repeat dispatch | acpi-native-server-loop actual runner, modeled lost reply |
| Unconfigured region/nonfatal initializer errors | Both local diagnostic and actual-Start scenario matrices |

No test claims real logstore delivery, kernel stamp authenticity, native OOM,
registration or native boot from these hosted models. Proof obligations and real
hardware/controller integration remain open. Full cumulative metadata migration
is still required before shared publication.
