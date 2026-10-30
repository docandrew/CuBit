# Hosted CuBit register transport checks

Use the pinned Nix environment with TMPDIR set according to coordination/acpi.md.
Run each selection/profile separately and inspect its result:

```sh
nice -n19 python3 tests/acpi-register-transport/run.py --mode release --selection register_transport_api
nice -n19 python3 tests/acpi-register-transport/run.py --mode release --selection register_transport_tests
nice -n19 python3 tests/acpi-register-transport/run.py --mode checked --selection register_transport_api
nice -n19 python3 tests/acpi-register-transport/run.py --mode checked --selection register_transport_tests
```

`register_transport_api` compiles only the adapter body against exact mirrored
runtime declarations, checked against their originals. It does not link or run.
`register_transport_tests` runs 235 checks per profile using the actual adapter,
client and Region_IO/Policy with explicit IPC, clock and hardware models.

Coverage includes the separately returned tag, full flag byte, full-width values
and deadlines, invalid local requests, clock advancement before dispatch, unknown
hardware completion, lost/error replies after effects, canonical-reply rejection,
receipt confirmation/retirement and no replay. Tests use no physical I/O or live
kernel IPC. Real authority, reply provenance, backing retention, terminal evidence
and syscall deadline enforcement remain integration assumptions.

The saved runner pins source inventories, runtime/tool hashes, strict fatal Ada
warnings and ALI dependencies. Builds use one worker/180 seconds, runtimes30
seconds, 64 MiB stack and bounded logs; use at most two heavy workers. Historical
exact-source receipts are in EVIDENCE.json. There is no executable fallback to
their original output directories. No new proof or native-driver acceptance is
claimed. `--validate-only` checks registration without compiling.
