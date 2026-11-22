# Real ACPI consumer-refusal shutdown regression

This is an opt-in variant of `--test acpi-shutdown`. It uses the same real
acpi.svc, power.svc, q35/edu table and temporary disk. The additional SSDT
contains seventeen ordinary batteries, exceeding the complete-inventory bound
of sixteen, plus a lid and AC source. No field IO or retained-result failure is
injected. The driver must suspend the incomplete family, retain receipt routing,
keep other copied facts and process the subsequent power-button SCI. QEMU must
power off with status zero through the ordinary S5 path.

Use the repository Nix environment and the installed, previously pinned ACPICA
20260408 compiler. Create a unique output directory under TMPDIR first:

```sh
export TMPDIR=/home/doc/cubit-build-tmp
acpi_case_dir=$(mktemp -d "$TMPDIR/acpi-consumer-refusal.XXXXXX")
nix develop -c env TMPDIR="$TMPDIR" \
  /nix/store/72fphm0m9k2f8zd066ayxkqi44ia9ql5-acpica-tools-20260408/bin/iasl \
  -p "$acpi_case_dir/refusal" tests/headless/acpi/acpi-consumer-refusal.asl
flock --exclusive --nonblock /home/doc/git/cubit/coordination/build.lock \
  timeout --kill-after=3s 900s nix develop -c env TMPDIR="$TMPDIR" \
  ACPI_SHUTDOWN_REFUSAL_TABLE="$acpi_case_dir/refusal.aml" \
  bash tests/headless/run.sh --test acpi-shutdown --accel kvm --timeout 300 \
  --keep-logs --serial "$acpi_case_dir/serial.log"
```

Build the current native programs before this command using the coordinator's
ordinary locked gate. These instructions neither build an ISO nor select a real
disk. The run uses the harness's private temporary disk as usual. Never supply a
real device as a test disk. A busy build lock defers the run.

The injector waits for the isolated-family diagnostic and lid/AC facts before
pressing QEMU's power button. The extra reader rejects a shared-service failure,
missing/refused guard, or SCI/preparation before isolation. The normal shutdown
reader still requires QEMU exit0, actual-service startup, preparation, app/service
stop and an ordered storage/driver tail. Native disk consistency and driver-stop
checks remain enabled. A compiled fixture and hosted serial-reader controls are
not evidence that this native scenario has run; record that separately.
