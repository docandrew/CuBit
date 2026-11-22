# Native completion routing (hosted)

Run in the pinned Nix environment, with TMPDIR exported before Nix:

```sh
export TMPDIR=/home/doc/cubit-build-tmp
nix develop -c bash tests/acpi-native-completion-routing/run.sh --mode checked
nix develop -c bash tests/acpi-native-completion-routing/run.sh --mode release
```

The current native server loop and passive-query composition are exact guarded mirrors. The endpoint, events, passive-query backend and kernel queues are modeled. Core/requests/AML sources are real. The original812 observer checks are unchanged;107 added checks cover full-width copied completion fields, matching/duplicate/refusal routing, bounded fairness, finite deadlines, stop/owner refusal and callback exceptions.

Checked mode enables assertions and overflow checks for the changed server and both test mains. Namespace/Core dependencies use -gnatp because their full assertion-enabled compiler family is stopped; this is not a whole-service assertion-enabled test or a proof. The runner checks ALI inventories and flags. Native semantic checks are separate private evidence. No native execution, hardware, BAR placement or host-bridge transport integration is claimed.
