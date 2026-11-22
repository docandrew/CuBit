# acpi-pre-device-provenance

Actual pre-device owner, GPIO transport and delay policy; Core/discovery/provider/IPC/grant/clock boundaries modeled. Exact immutable submitted GPIO facts, descriptor and receipt capture; no native/hardware execution.

Run from the pinned Nix environment after exporting `TMPDIR=/home/doc/cubit-build-tmp`:

```sh
bash tests/acpi-pre-device-provenance/run.sh
bash tests/acpi-pre-device-provenance/run.sh --validate-only
```

No native build, image or QEMU run. Source catalogs are closed; every production and helper input is hash checked. Temporary GPR files are hosted-test artifacts only. Checked and release profiles are compared where selected; whole Core remains O0/gnatp and does not establish assertion-enabled or proof coverage. Resource/fuel limits are test policy, not ACPI limits. Models are test-only and carry no native authority.
