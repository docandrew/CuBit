# PCI bridge context regression

Run from the pinned development shell with `TMPDIR=/home/doc/cubit-build-tmp` exported before entering Nix:

```
python3 tests/acpi-pci-bridge-context/run.py --mode release
python3 tests/acpi-pci-bridge-context/run.py --mode checked
```

`--validate-only` checks the source and tool hashes without building. Each run copies the guarded dependency closure into a fresh directory under TMPDIR and writes its commands, inputs, logs and result there. It never accesses PCI hardware.

The 26 configurations comprise the exhaustive pure byte-probe regression, fifteen namespace/VM/provider topology fixtures, and ten provider/fuel/cache controls. The model checks each metadata offset, BDF and final class-code read. Fixtures cover nonzero segment and root bus, two bridges, multifunction and CardBus headers, nonbridge devices, a lexical container without `_ADR`, `_CID` root discovery, invalid addresses, request limits, and temporary regions recreated between invocations. Pending and uncertain operations must not be replayed.

These synthetic tables are maintained here; the original laptop DSDT is not distributed. Its separate replay receipt is an additional diagnostic, not a hardware acceptance test.

At introduction, release passes all 26 configurations. GNAT 15.3 crashes compiling the checked hosted Core at the existing `Define_Fields` postcondition (`Tree'Old.Values`), including on the unchanged base. The failure is retained as a toolchain admission and the checked command remains enabled for the follow-up repair. Native SDK semantic checks pass both profiles. This suite does not establish a proof of the complete namespace or service.
