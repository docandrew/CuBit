# Native ACPI event-domain hosted regression

Run in the Nix environment with TMPDIR exported before Nix:

    export TMPDIR=/home/doc/cubit-build-tmp
    nix develop -c bash tests/acpi-native-event-domain/run.sh

Run --validate-only for inventory/hash checks without compilation. --source-root selects a guarded candidate tree. The final JSON prints the unique output directory; run the companion ACPICA check in the same environment:

    node tests/acpi-native-event-domain/oracle.mjs /absolute/output/RESULT.json

Actual code: concrete native event domain, EC source/selector/shared command engine, receiver/methods, four-family consumers, AML/Core/runtime and guarded receipt pump. Models: native Fields/ports/clock/SCI/logger boundaries. All six scenarios run at AML32 and AML64: normal, startup _INI Notify, stop with Notify, no EC backing, no SCI and retained uncertain hardware. The actual _REG(3,0) method is invoked on the same Core during shutdown to check route lifetime; this harness does not replace separate region-registry tests. Fixed callbacks copy only; logger model rejects active/retained Field ownership. Observer receipt mutations are separately covered by the predecessor four-family suite.

Whole Core uses -O0 -gnatp, with explicit Check failures. Compile limit180s, test30s, address space24GiB, stack64MiB, core dumps0, one compiler job, nice19. ACPICA5s/128MiB/2MiB stack. No native/image/kernel builds or hardware execution. Semantic SDK checks and SPARK proof are separate; these tests prove neither full interpreter correctness nor physical interrupt/port behavior.

Notification-interest follow-up: this suite now drives the ACTUAL ACPI_Snapshot_Startup.With_Driver allocation/copy/completion path. It performs no manual Configure_Notifications(True). The native adapter must request capture before Finish_Snapshot and the event domain rejects a deliberately disabled policy before source enable. Hosted aligned allocation supplies the CuAlloc ABI; it is not native allocator validation. Seven modes run at each AML width.

Battery recovery follow-up: the battery-recovery mode uses actual AML methods and
captured Notify(0x80/0x81) on the same Core. Malformed _BST and _BIX data must
publish explicit failed acquisitions without the previous capacity; subsequent
valid notifications must publish recovered capacity. No hardware is exercised.

EC absence follow-up: three more modes at each width distinguish a complete namespace scan without any EC (with and without early backing), a described EC without backing, and an invalid identity result. Only the successful no-match scan certifies absence. All degraded cases retain fixed-button handling and refuse to invent live family samples.

The image19 port adds explicit absent-domain test boundaries for the power-service entry owner, saved-reply button watches and PNP0C0C discovery. This fixture sends no power IPC, has no FADT fixed-power source and no PNP0C0C device. Its real event-domain fixed callback still copies/logs the original press counts. These cases validate EC/lid/battery dispatch, not power-watch delivery, shutdown or button discovery; those owners have separate suites. Original battery assertions are retained.
