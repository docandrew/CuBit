# Kernel PCI classification and boot adapter

Run inside Nix with TMPDIR exported before entry:

```sh
bash tests/acpi-kernel-pci/run.sh
```

The source-only runner requires Node, GPRbuild and GNU timeout. It copies pinned
production sources and fixtures into a unique TMPDIR directory and records logs,
input hashes and results there. It needs no Git checkout, native build or private
workspace. Builds are sequential and bounded to 180 seconds; each executable is
bounded to 30 seconds. Captured output is bounded to 1 MiB per command.

The **actual unmodified Physical_Range_Map body** is compiled and executed.
Test doubles replace ACPI snapshot/copy operations, boot memory descriptions,
virtual-to-physical conversion and console output. The real classifier, MCFG
parser and admission policy run underneath it. The doubles do not touch hardware
or physical memory. Their capacity constants are checked against the pinned real
ACPI and Config specifications. These checks do not verify every mock interface.

Checked/overflow and release builds cover:

- Exact static MCFG capacity and one allocation below; one byte and one allocation
  over capacity must refuse before copying.
- Snapshot/descriptor/copy failures, corrupt table bytes and duplicate MCFG;
  an incomplete inventory yields no firmware root, including after a first
  allocation was stored before a later duplicate.
- A non-first MCFG descriptor, no MCFG, exact copy bounds and idempotent sealing.
- PCI root enumeration with NVS, control-page and RAM conflicts. `Left_Out`
  counts rejected whole-bus candidates once, including a preceding NVS region
  crossing two buses; NVS/control roots remain independently discoverable.

A release-only benchmark reports host wall time and exact enumeration-call counts
for synthetic 256-root, over-launch-capacity and maximum-allocation inventories.
Its timing is informational, not a native boot performance threshold. The launch
capacity selector reports whether each complete inventory fits.

This suite does not prove the adapter, run the real ACPI copy implementation,
map devices, or boot CuBit. The full authority proof target and the root-export
policy proof remain separate. Native QEMU and laptop validation are separate gates.
