# Native floating-format control

This is a control for Penny's KVM-only freeze in SWGL's completed-draw diagnostic
`snprintf("%.3f", ...)`. It checks six known decimal results, then three pthreads
perform 10,000 checked conversions each, yielding before every conversion. PASS
requires the identified process to be reclaimed after all worker joins.

Build in the repository Nix shell, with all outputs in a disposable directory:

```sh
mkdir -p /tmp/penny-float-probe
userspace/ccl/build/manifest/ccl-manifest userspace/ccl/catalogs/native-runtime-services.ccl tests/servo/float-format/manifest.ccl > /tmp/penny-float-probe/manifest.S
as --64 /tmp/penny-float-probe/manifest.S -o /tmp/penny-float-probe/manifest.o
userspace/libc/cubit-cc -O2 -g -pthread -o /tmp/penny-float-probe/probe.app tests/servo/float-format/probe.c --manifest /tmp/penny-float-probe/manifest.o
```

Run `test-native.py` through the private workspace runner in Nix with `--seed`,
`--app`, `--desktop`, `--kernel` absolute paths and `--accel kvm` (host KVM access)
or `--accel tcg`. It reuses the browser desktop seed but replaces the disposable
copy's browser executable with the probe; it does not change the staged browser.
The probe requests no filesystem or network capabilities. Large images and copied
ELFs are automatically removed; logs and hash manifests remain.

2026-10-03 KVM: both the sequential baseline and the 30,000-call concurrent control
passed. This does not reproduce the browser-specific failure, rule out FPU state
corruption, or establish a generic libc fix. Penny's integer timing formatter is
a verified workaround; rendering-time floating-point state remains to investigate.
