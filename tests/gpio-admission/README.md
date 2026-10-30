# GPIO channel badge regressions

In the pinned Nix environment, export TMPDIR=/home/doc/cubit-build-tmp, then:

    bash tests/gpio-admission/run.sh tests /absolute/private/output
    bash tests/gpio-admission/run.sh proof /absolute/private/output

The runner snapshots exact production/test sources and verifies them after the
run. Builds use one job and finite time limits. Tests run both checked
(-gnata/-gnato/-O2) and release (-gnatp/-O3) profiles. The pure codec uses no
syscalls, memory mappings or simulated hardware.

Tests cover all 524,288 possible low payload encodings, all 65,535 real pin
numbers, five valid trigger/polarity combinations and every level/both-edge
encoder refusal. They check explicit wire vectors, no-pin and reserved polarity
refusals, every reserved bit and every unsupported version. There are 2,752,586
checks per profile. Successful encode/decode must preserve the exact pin,
trigger, polarity and original canonical word.

The level-2 proof target is GPIO_Channel_Badges only: runtime safety, termination,
no global state, successful decode exactly when Valid_Word, canonicality, field
extraction and encode/decode round-trip. The proof summary must have no unproved
or justified checks and no pragma Assume. A passing runner is evidence for
these declared contracts, never proof of runtime IPC, launcher admission,
GPIO_Pin_Leases, the controller, IRQ delivery, whole ACPI or AML.

The badge's high sixteen bits are 0x4750 (GP); version is bits47..44=1;
reserved bits43..19 must be zero; bit18 selects edge; bits17..16 select
high0/low1/both2, with3 reserved; bits15..0 contain the ACPI pin, with0xFFFF
refused. Both-edge polarity is refused for level mode. Pin numbers are ACPI
connection numbers, not the controller owner's internal registry slots.

Authority comes from a kernel-stamped badge on an admitted controller endpoint.
Decoding caller-provided words is never an authority substitute. The launcher
must validate ACPI resource facts, supported controller/pin geometry and policy
before minting the endpoint. This badge grants one interrupt input; it contains
no address and grants no output, register, wake or sharing permission. Lease IDs
and event delivery generations are separate. Native integration must leave the
laptop parent interrupt disabled until generic ONESHOT/epoch ACK support lands.
