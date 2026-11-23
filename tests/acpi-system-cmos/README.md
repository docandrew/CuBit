# Hosted SystemCMOS gate

Export `TMPDIR=/home/doc/cubit-build-tmp` before entering the pinned Nix shell, then run `bash tests/acpi-system-cmos/run.sh --mode checked` and `--mode release`. `--validate-only` checks the exact source/tool catalog. Outputs use disjoint temporary directories and report 39 runtime cases per profile, exact commands, hashes, logs and check counts.

The suite runs actual index/profile/transaction policies, the boot reservation ledger and profile codec, the serialized select/data port template, the optional CMOS mux and provider, Core/Field_IO, and Boot_RTC. Port capabilities, physical registers, monotonic time, waits and firmware-lock callbacks are modeled. Boot_RTC has an isolated modeled runtime interface; the original RTC regression is retained with its top-level procedure renamed.

Both AML integer widths are tested. SystemCMOS permits ByteAcc only (ACPI 6.6 Table 19.15). Ten non-byte declarations must be rejected by IASL with6100; separately recorded flag-byte edits and checksum repairs produce explicit malformed-field probes that CuBit must reject before any I/O. These are never represented as accepted ASL. Positive ACPICA references substitute simulated SystemMemory and validate only returned AML values.

Tests cover every logical CMOS byte/profile/NMI setting, RTC/century write refusal, exact nondelegable port ranges, port revocation, select/data failures, reentry and unknown completion without replay. The optional handler can be absent while ordinary/EC lanes remain available. The boot policy reserves aliases against all claimants while the service gets only the actual pair(s); immutable profile facts override configuration fields.

No real device, native executable or whole-service proof runs here. Proof receipts are separate: unchanged packages are reused, and one minor planner count equality is currently admitted as a follow-up. Kernel capability affine leases remain CAP-014; the boot handover currently validates only the legacy64-byte profile, without guessing extended-enable state.

After the coordinator builds/stages the candidate native services, run the real startup gate on Chungus:

```sh
export TMPDIR=/home/doc/cubit-build-tmp
timeout --kill-after=5s 1500s nix develop -c env TMPDIR=/home/doc/cubit-build-tmp bash tests/acpi-system-cmos/run-headless.sh kvm
```

The wrapper takes the shared build lock nonblocking, runs the existing `acpi-one-service` test with4CPUs, a unique copied-disk/serial-log location and120s guest timeout, then checks the actual Boot_RTC→ledger transfer→procmgr receipt→guarded StatusD read trace. It prints its receipt directory. It does not build/stage services itself. A busy lock exits75 for a later retry. The parser's9/9 self-tests use synthetic log text and are not headless/native evidence. Existing power-sources/gpio-launch/acpi-shutdown gates remain coordinator controls.
