# Native SCI deferred-event regression

Run in the repository Nix development environment:

```sh
bash tests/acpi-native-sci/run.sh --mode checked
bash tests/acpi-native-sci/run.sh --mode release
```

`--validate-only` checks the explicit input hashes without compiling. The runner
copies only the listed 18 production and 21 helper files into a unique TMPDIR
directory, generates a hosted project there, and retains logs, commands, source
and tool hashes, and ALI profile checks in RESULT.json. No checkout build outputs
or checked-in project files are needed.

Each profile runs the original 74 assertions against actual pending SCI_Events,
FADT decoding, SCI_Registers and SCI_Quiesce. They cover admission, authenticated
kernel delivery versus a full-width foreign sender, twelve coalesced deliveries,
acknowledgement without register access or consumer/log work while deferred,
later single processing of a latched power event, no replay/double acknowledgement,
acknowledgement refusal, normal dispatch, failed disable and retry, copied-log
admission, preservation of latched status and late acknowledgement after disarm.

Helpers model firmware table copying, I/O ports with write-one-to-clear status,
kernel interrupt acknowledgements, clock, logging, message and event boundaries.
The synthetic FADT describes a legacy fixed PM1 block and SCI9. GPE blocks and
physical interrupt delivery are outside this fixture's coverage. Checked mode
enables assertions and overflow checks for the test main and four selected actual
units; release uses -gnatp. This is hosted regression evidence, not full Main
execution, a new SPARK proof, native or hardware validation. It adds no AML language
behavior and does not claim additional ACPICA differential coverage.
