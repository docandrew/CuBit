# Native stop-drain regression

Run in the repository Nix development environment:

```sh
bash tests/acpi-native-stop/run.sh --mode checked
bash tests/acpi-native-stop/run.sh --mode release
```

`--validate-only` checks the frozen input manifest and exact helper extraction
without compiling. The runner checks the real native Main source hash, extracts
Drain_Bridges and requires exactly one matching occurrence in the hosted test
after three identifier substitutions: Bridge_Loop to Scheduler, Bridges to
Manager, and Owner.all to Owner. Main itself is context, not a hosted compilation
input. It is never represented as fully executed by this test.

The manifest names 274 production inputs, 36 helper files and one Main context
input. Sources are copied to a unique TMPDIR directory, with no checkout build
outputs. Commands, logs, tool/source hashes and ALI profile checks are retained
in RESULT.json. The project is generated privately, not checked into the tree.

Each profile executes 1,473 assertions in five fresh processes: normal drain
(1,055), unknown completion (104), unavailable clock (104), unconfirmed original
grant retirement (104), and a late wake crossing the original stop deadline
(106). Both AML integer widths run. Normal mode covers nine lifecycles per width.
Checks cover request admission remaining closed, observer requests still served
when the loop processes IPC, no new AML evaluation, acknowledgement-only late
events, retirement of original channel/grant references and no deadline renewal.

The exact extracted stop helper, actual Native_Server.With_Dispatch, bridge
manager/scheduler, Requests/Core and Native_Endpoint execute. Explicit helpers
model SCI acknowledgement, downstream power/passive gates, logging, transport,
grants, heap and clock. Actual SCI execution has its own acpi-native-sci suite.
Checked mode enables assertions and overflow checks for the test main, endpoint,
Serving_State and both selected server units; inherited AML/Namespace units use
-gnatp. Release suppresses checks throughout. This is hosted regression evidence,
not full native Main startup/shutdown, hardware validation or a new SPARK proof.
