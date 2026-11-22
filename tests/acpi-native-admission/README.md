# Native endpoint admission regression

Run in the repository Nix development environment:

```sh
bash tests/acpi-native-admission/run.sh --mode checked
bash tests/acpi-native-admission/run.sh --mode release
```

`--validate-only` checks the explicit production and helper hashes without compiling.
The runner freezes only the listed inputs into a unique TMPDIR directory. It generates
its hosted project there and retains commands, logs, source/tool hashes and ALI profile
checks in RESULT.json. It does not write build outputs into the checkout.

Each profile executes 240 cases and 2,620 assertions in three fresh processes:
unselected (873), runtime (873) and observer (874). Cases cross admission open/closed,
diagnostic quiet/busy, runtime blocked/unblocked, downstream handler selection and
authorized/foreign sender. Checks cover dispatch order, short circuits, full-width
sender identity, observer availability, reply ownership and refusal logging.

The actual pending Native_Endpoint, Requests/Core, Serving_State and observer code
executes. Eighteen explicit helper units model downstream power/passive/event-domain
handlers, transport, heap and logging boundaries. They test routing and admission,
not the implementations of those modeled systems. Assertions and overflow checks
are enabled for Native_Endpoint, Serving_State and the test main in checked mode;
inherited AML/Namespace dependencies use -gnatp. Release suppresses checks throughout.
This is hosted regression evidence, not a new SPARK proof, full Main execution or
native/hardware validation. No AML language behavior is added by this suite, so it
does not claim additional ACPICA differential coverage.
