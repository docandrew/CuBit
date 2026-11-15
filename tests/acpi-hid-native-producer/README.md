# acpi-hid-native-producer

Actual native producer/transport/allocator and canonical payload/protocols; Core/collection/runtime window/admission/physical boundary/IPC/grants/clocks explicitly modeled. No whole native instance execution.

Run from the pinned Nix environment after exporting `TMPDIR=/home/doc/cubit-build-tmp`:

```sh
bash tests/acpi-hid-native-producer/run.sh
bash tests/acpi-hid-native-producer/run.sh --validate-only
```

No native build, image or QEMU run. Source catalogs are closed; every production and helper input is hash checked. Temporary GPR files are hosted-test artifacts only. Checked and release profiles are compared where selected; whole Core remains O0/gnatp and does not establish assertion-enabled or proof coverage. Resource/fuel limits are test policy, not ACPI limits. Models are test-only and carry no native authority.
