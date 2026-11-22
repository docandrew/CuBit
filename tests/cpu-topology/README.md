# Firmware CPU IDs and SMP startup

Software CPU numbers index per-CPU stacks and scheduler state. They are not
hardware APIC destinations. `CPU_Topology` reserves logical index zero for the
boot CPU and appends enabled MADT local-APIC IDs, retaining order and suppressing
duplicates. Disabled/online-capable-only entries are not started. ID 255 is the
xAPIC broadcast destination and is rejected as an enabled CPU. This is an xAPIC
mapping, not x2APIC/hotplug support. The existing eight-started-CPU limit remains.

The BSP builds the table before starting APs; thereafter it is read-only by
convention. Both startup IPIs and scheduler reschedule IPIs translate through it.
This does not yet audit all device interrupt routing: drivers with hard-coded
MSI destination zero still need work before claiming general nonzero-BSP support.

## Tests and proof

Run in the Nix environment, from `kernel/`:

```sh
alr exec -- gprbuild -p -P ../tests/cpu-topology/topology.gpr
../tests/cpu-topology/build/main
alr exec -- gnatprove -P ../tests/cpu-topology/topology.gpr -u cpu_topology.adb --level=2 -j2 --report=all
```

Hosted assertions cover sparse IDs, nonzero boot CPU, duplicate insertion, and
all 255 usable destinations with four different boot IDs. Focused SPARK analysis
passes all 20 checks: bounds/initialization, termination, uniqueness and retention
of the boot CPU identity. These do not prove firmware truth, assembly, APIC MMIO,
cache ordering, scheduler concurrency, or complete hardware boot correctness.
Ghost predicates/contracts have no kernel runtime checks or code generation.

Native regression commands (hold the build lock, or use an isolated workspace):

```sh
python3 tests/usb-optical/run-live.py --uefi --sparse-apic-ids --pit-free-fixture
python3 tests/usb-optical/run-live.py --uefi --cpus 4 --pit-free-fixture --stall-ap-fixture
python3 tests/usb-optical/run-live.py --uefi --cpus 4
```

Sparse topology uses real QEMU APIC IDs 0,1,2,4,5,6 (two sockets, three cores
each), not forged firmware entries. The previous PIT-FREE 3 image stalls after
starting logical CPUs 1 and 2 on that topology. The new image starts all six and
passes desktop/DOOM/Workbench/Files live tests, with no PIT device.

The stalled-AP fixture uses GDB to redirect the first AP from Ada entry into the
assembly halt loop; it does not forge acknowledgment or timer counters. The BSP
reports `SMP FAILED cpu= 1 apic= 1 stage= 1` and stops rather than advancing to
the scheduler. The ordinary CPU clock metadata is independently supplied by the
existing PIT-free fixture; see `tests/boot-timer-rates/README.md`.

## Diagnostic stages

- 0: startup requested, no 64-bit trampoline acknowledgment yet.
- 1: reached 64-bit assembly trampoline.
- 2: CPU-local data initialized.
- 3: secondary stack/IDT ready and kernel address space installed.
- 4: local APIC setup complete, starting idle thread/CPU registration.
- 5: initialization complete, about to acknowledge BSP.

The acknowledgment and stage are aligned atomic 32-bit objects; assembly reads
the same width (the old code incorrectly read eight bytes). Startup waits have
both a two-second TSC budget and a finite poll cap; IPI delivery-pending polling
is also bounded. These are diagnostic budgets, not a hard-real-time guarantee.

N95 physical result is pending. Its original PIT delivery failure is separate;
this fixes a reproducible SMP bug but does not yet establish that it is the
remaining cause of that machine's boot stall.
