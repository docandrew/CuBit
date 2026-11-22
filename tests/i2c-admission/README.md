# I2C wire admission regressions

In the pinned Nix environment, set TMPDIR=/home/doc/cubit-build-tmp and run:

    bash tests/i2c-admission/run.sh tests /absolute/private/output
    bash tests/i2c-admission/run.sh proof /absolute/private/output

The test runner snapshots exact sources and uses checked (-gnata/-gnato) and
release (-gnatp/-O3) profiles: badges28170, startup diagnostic replies8805, and
transfer request/completion/native-result/work-policy tests1508371, for1545346
checks per profile. The transfer fixture intentionally compiles the actual
native result adapter and pure work policy; it executes no native syscall or
hardware. Driver registration and IPC reply delivery are not modeled as proof.

Badges describe the target, addressing mode and speed ceiling. They become
scope only through kernel stamping on an admitted endpoint. Payloads never
supply authority. Request226/protocol1 uses exactly four words: canonical input
and output grant references (zero when unused), packed16bit write/read counts
with upper32bits zero, and an explicit absolute GETTIME-ms or Forever deadline.
Combined capacity is checked before grants or controller work.

Completion227/protocol1 carries explicit status and hardware-success/effects
flags, full64bit owner ticket, requested-write and delivered-read counts, and a
reservedzero word. Requested writes are not acknowledged-byte counts. Tests
cover every16bit count, canonical references, unknown status/reserved fields,
shifted array bounds, deadline boundaries and exact driver-result conversion.
Work-policy tests check scaling at slow/standard/fast/fast-plus model timings,
monotonicity and full-size transfers rather than assuming the fastest bus.

The retired two-word startup codec has been removed. Startup diagnostic data
remain useful for logging; they no longer define a configure RPC. proof.gpr now
targets badges, transfer wire and diagnostic replies. Historical proofs for the
superseded startup codec do not prove this changed source set. The current level-2 milestone passes134 scoped SPARK checks (99prover and35flow),
with zero unproved/justified checks or pragma Assume. This covers runtime safety
and the declared contracts of the three codecs and grant identity helper. It
does not prove unspecified transfer round-trip semantics, stateful ownership,
volatile/native adapters or the whole service; those remain separate work.
