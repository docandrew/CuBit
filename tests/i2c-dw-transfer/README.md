# Hosted DesignWare I2C tests

In the Nix shell, export TMPDIR=/home/doc/cubit-build-tmp and run:

    bash tests/i2c-dw-transfer/run.sh --output-root /absolute/private/output

The runner copies and hashes the four exact production inputs and both test
mains to a unique private output. It builds checked and release profiles with
strict warnings, records terminal statuses/tool and artifact hashes, verifies
compiler flags and checks 61,056 planner plus 196,742 register checks per profile.
No native hardware access, CCL driver registration, ACPICA or VM is involved.
ACPICA does not emulate this controller's FIFO/MMIO transport. ACPI resource and
AML integration retain their separate ACPICA-based regression suites.

Backend callbacks are hosted register/clock models. All FIFO parameter byte pairs,
FIFO stalls/credit bounds, command encoding, abort evidence, invalid occupancy,
overflow/underflow, ticket reuse, access refusal/uncertainty, clock errors before
and after IO, and a planner/port combined transaction are covered. This suite
does not prove protocol, buffer ownership, callback termination or native safety.
See the controller README for the separate planner proof scope and assumptions.
