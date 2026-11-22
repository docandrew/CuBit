# Cached shutdown hosted regression

Export TMPDIR=/home/doc/cubit-build-tmp before entering Nix, then run
`bash tests/acpi-cached-s5/run.sh`. `--validate-only` checks source/tool pins.

Actual code covers Core/Bootstrap/Requests stored `_S5` observation, native
preparation/final entry and the existing final named-register sequencer.
Kernel messages/saved replies, published clock, hardware readiness/Fields and
logger boundaries are models. Only a stored package is accepted: methods and
`_PTS`/`_TTS` execution are skipped. A context-bound cached mode accepts Idle or
logically Stopped AML only while independent hardware and ownership checks
remain ready; Fresh/Active, retained/uncertain effects and authority loss
refuse. Ordinary preparation still requires Idle.

The suite expects 192 static checks across 8 AML32/AML64 processes and 442
adapter/entry checks across 50 processes. It covers stopped success, normal
stopped refusal, physical readiness failures both before preparation and
between preparation/entry, finite clocks, saved-reply refusal, stop priority,
unknown writes and no replay. Two repair-disabled ACPICA static-value runs
compare the same package integers. ACPICA may report four outstanding cache
allocations at exit: logs retain this diagnostic; value agreement is not a
clean reference shutdown claim.

Core runs `-O0 -gnatp` with explicit checks, one compiler job, nice19,
180s compile/30s runtime, 24GiB address space, 64MiB stack, no cores.
ACPICA uses 5s/128MiB/2MiB. These tests do not prove SPARK properties, execute
the native service, validate real sleep sequencing or establish laptop
shutdown. Actual native-instance semantic checks and coordinator native
codegen/boot/hardware gates are separate.
