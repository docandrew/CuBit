# Direct ACPI observer runner

Run `bash tests/acpi-native-server-loop/run.sh --mode checked` or `--mode release`
inside the pinned Nix shell with TMPDIR=/home/doc/cubit-build-tmp. The default is
release. `--validate-only` checks inputs, closed Ada directory inventory and exact
production/API mirrors without compiling.

This replaces the obsolete provider-configure/grant-loop suite. It runs actual
ACPI Requests/Core and the exact production observer endpoint/native adapter and
server, using modeled IPC/log transport and actual bounded Refusal_Notes logic.
711 checks cover known/unknown/zero badges, denial of upload/receipt mutations,
canonical replies, all metrics pages, malformed envelopes, payload/PID claims,
repeated/refused logging, a full64-bit process identity, lost replies, spurious
wakes, explicit Wait_Forever and termination on unexpected completion.

96 strict source ALIs/profile; checked gnata+gnato vs release gnatp. Build180s,
runtime30s, stack64MiB, logs1MiB, nice19/j1. Exact input/mirror/artifact hashes are
recorded. These tests do not authenticate real kernel stamps or exercise native
transport, logger delivery, service registration, hardware or a SPARK proof.

The optional prove.sh targets only the pure observer policy wrapper. Its first
bounded level2 attempt stopped during Global-contract generation on a GNAT
front-end assertion in the existing AML namespace dependency, before producing
flow/proof results. No new observer proof is claimed. Do not automatically retry
or investigate that compiler failure; revisit after a relevant compiler/source
change or explicit user direction. The regular hosted test runner does not run
the prover. The AML core --prove hook surfaces this pending proof as a failure.
