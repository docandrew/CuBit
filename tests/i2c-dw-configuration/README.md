# Hosted DesignWare initialization tests

In the pinned Nix shell, export TMPDIR=/home/doc/cubit-build-tmp, then:

    bash tests/i2c-dw-configuration/run.sh --output-root /absolute/private/output

The guarded runner copies four production source files and one test main into a
unique private directory, compiles strict checked/release profiles and verifies
4,931 checks per profile, four terminal commands and compiler/input/tool hashes.
All outputs remain private. No native build, controller access or ACPICA is
involved: this is a modeled register backend, not an AML execution test.

Coverage: standard/fast counter selection, optional hold and its version gate,
all seventeen normal access positions with unavailable/denied/unknown status,
clock loss/expiry/reversal before and after each access, explicit step quota with
a non-advancing clock, RX drain and its bound, invalid TX/RX state, disable/enable
waits, abort-source preservation before interrupt clear, no repeat after terminal
failure/success and possible enable effects after failed post-access clocks.
No configuration proof, timing/electrical validity or native acceptance is claimed.
