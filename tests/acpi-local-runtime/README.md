# Local ACPI runtime handoff

Run in the pinned Nix development environment with GNAT, GPRbuild, Node and ACPICA available:

    export TMPDIR=/home/doc/cubit-build-tmp
    nix develop -c bash tests/acpi-local-runtime/run.sh

The suite runs 13 scenarios for each AML integer width against the actual hosted Core and delay-domain policy, using modeled owner/clock callbacks. It checks one-time startup transfer, finite event admission, nested calls, stop during claim/evaluation, typed retained arguments/results, and refusal without replay. Builds use -gnatp because full assertion-enabled Core generation has a known toolchain failure. Each build is bounded to 180 seconds, execution to 30 seconds, and address space to 24 GiB.

The native child adapter is covered separately by the native SDK semantic fixture. These hosted checks do not execute CuBit, access physical devices, authenticate event sources or prove the complete service. Callback termination and serialization are assumptions. Stop closes admission while retaining backing and results; it is not permission to unmap storage.
