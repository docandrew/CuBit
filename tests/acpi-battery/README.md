# Hosted battery observations

Run in the CuBit Nix development environment with
`TMPDIR=/home/doc/cubit-build-tmp bash tests/acpi-battery/run.sh`.
`--validate-only` checks the closed source inventory and pinned inputs without
compilation or oracle execution. Results go to a unique local results directory.

Both checked (`-gnata -gnato`) and release (`-gnatp`) profiles exercise the pure
_BST integer decoder (14237 checks) and real Core AML observations (330 checks),
including PNP0C0A identity, _STA battery/device presence and retained-result
ownership. The ACPICA20260408 fixture returns matching values with repair
 disabled. Its exit0 shutdown allocation diagnostic is retained explicitly.

The Core observer has hosted testing, not proof or native battery validation.
The pure value decoder's scoped proof is in `tests/acpi-leaf-policy-proof`.
The trusted owner
must establish device identity, initialization/regions, _BIX units/technology,
notification/cache policy and native IO before publishing battery state.
Missing/error observations remain unknown, not battery absence.


The controlled module-startup milestone is recorded in MODULE-STARTUP-EVIDENCE.json.
Its current-source input/closure refresh does not renew historical proof results.
Fresh hosted checks cover both profiles; no native execution or hardware
authority/region connection follows from these tests. The native API gate
selects 59 units and produces 56 ALIs with zero objects per profile.
