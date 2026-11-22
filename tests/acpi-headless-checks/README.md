# Hosted ACPI headless evidence checks

Run `python3 -I tests/acpi-headless-checks/test-checks.py` in the project
Nix shell. `--captures DIR` additionally replays the three coordinator native
serial files `g-acpi-pci-config.serial`, `g-gpio-launch.serial` and
`g-acpi-shutdown.serial`. This is serial-evidence replay, not a fresh boot.
The headless runner uses `check-launch.py` for PCI and GPIO identity checks;
its existing `check-acpi-shutdown.sh` checks the real-service shutdown tail.

PCI comparison accepts a complete devmgr enumeration body even when kernel
loader output precedes it. A torn loader PID requires the named kernel
write-protection record as independent identity evidence; contradictory
identities refuse. The 24 slot bits match CuBit.Process_IDs.Slot_Bits
(KERN-003). The same checker slot must fault at the requested read-only
mapping. IDs, fault kind/address and unique attempt remain mandatory.

GPIO checks the same full child identity in held/released narration and the
admitted sink owner, with admission before any HID parameter output. A
procmgr release line may be mirrored after child output: queuing a diagnostic
before resume does not guarantee its serial arrival. Missing/duplicate
markers, wrong owners and premature output still refuse.

Logstore remains alive through shutdown, detaching its storage before the
filesystem commit. Shutdown accepts an ordered truncated final console tail
only when QEMU exited successfully; it refuses a logstore Stop or gaps and
reordering in the observed tail. The native runner separately verifies the
filesystem image and driver stops. None of these hosted checks establishes
native execution, disk consistency or hardware behavior by itself.

The checker also validates the opt-in real-service consumer-refusal scenario in
[consumer-refusal.md](../headless/acpi/consumer-refusal.md). Its synthetic controls
require an isolated battery refusal, surviving lid/AC facts and later SCI/S5
preparation, and reject a stopped shared owner. This does not replace QEMU exit0.

Preparation keeps power.svc's own request/reply/entry order. Its PREPARED record
may be mirrored after another producer starts stopping the first service.
Storage and driver tail ordering remains strict. GPIO's long devmgr records may
be clipped at the log mirror's text limit: require their complete badge prefixes
and the independent queue-position/client-parameter assertions instead of their
missing suffixes. The captured held/admitted/client/route identities still have
to agree. The optional capture replay now checks run.sh's full GPIO marker list
as well as the separate launch reader.
