# Region declaration stability

Inside the pinned Nix shell:

```
python3 tests/acpi-region-declaration/run.py --output /absolute/new-private-directory
```

The test-only generic child exercises the actual private region predicate.
270 checks cover identical records, an unknown PCI route becoming known, all
256 address spaces, and refusals for a cleared/changed known route, each BDF
component, address, length, integer width, syntax, and original definition.
Runtime, assertion and overflow checks stay enabled. Build300s, run30s,
address space24GiB, stack128MiB. No native output, image, device access or lock.

Pair this gate with the full saved-firmware device-initialization replay in
tests/aml-laptop-replay for every ACPI/driver packet. These predicate checks do
not replace the real initialization walker and exact EC0._REG/SANV/LIDS replay.
