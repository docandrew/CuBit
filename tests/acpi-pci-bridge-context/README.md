# PCI bridge context regression

Run from the pinned development shell with `TMPDIR=/home/doc/cubit-build-tmp` exported before entering Nix:

```
python3 tests/acpi-pci-bridge-context/run.py --mode release
python3 tests/acpi-pci-bridge-context/run.py --mode checked
```

`--validate-only` checks the source and tool hashes without building. Each run copies the guarded dependency closure into a fresh directory under TMPDIR and writes its commands, inputs, logs and result there. It never accesses PCI hardware.

The 26 configurations comprise the exhaustive pure byte-probe regression, fifteen namespace/VM/provider topology fixtures, and ten provider/fuel/cache controls. The model checks each metadata offset, BDF and final class-code read. Fixtures cover nonzero segment and root bus, two bridges, multifunction and CardBus headers, nonbridge devices, a lexical container without `_ADR`, `_CID` root discovery, invalid addresses, request limits, and temporary regions recreated between invocations. Pending and uncertain operations must not be replayed.

These synthetic tables are maintained here; the original laptop DSDT is not distributed. Its separate replay receipt is an additional diagnostic, not a hardware acceptance test.

The checked profile passes all 26 configurations. The repaired source also passes the 25 original-DSDT control and synthetic-topology release configurations; unchanged pure-probe checked/release evidence is reused. The namespace uses equivalent named ghost predicates for its value-preservation frames: GNAT 15.3 otherwise crashes lowering `Tree'Old.Values` and `Tree'Loop_Entry.Values` in the checked hosted Core. Contracts and overflow checks remain enabled in the checked profile. The original compiler failures are retained with the repair receipts.

The separate 509992-byte original firmware replay exceeds a 30-second checked staging budget before AML execution; that diagnostic is not included in this synthetic suite's pass count. Native SDK semantic checks pass both profiles. A separate SPARK front-end crash in the existing frame-roots dependency prevents analysis of the new ghost predicates through a concrete namespace instantiation. This suite does not establish a proof of the complete namespace or service.
