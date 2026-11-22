# Local ACPI runtime handoff

Run in the pinned Nix development environment with GNAT, GPRbuild, Node and ACPICA available:

    export TMPDIR=/home/doc/cubit-build-tmp
    nix develop -c bash tests/acpi-local-runtime/run.sh

The suite runs 21 scenarios for each AML integer width against the actual hosted Core and delay-domain policy, using modeled owner/clock callbacks. It checks one-time startup transfer, finite event admission, nested calls, stop during claim/evaluation, typed retained arguments/results, and refusal without replay. Builds use -gnatp because full assertion-enabled Core generation has a known toolchain failure. Each build is bounded to 180 seconds, execution to 30 seconds, and address space to 24 GiB.

The native child adapter is covered separately by the native SDK semantic fixture. These hosted checks do not execute CuBit, access physical devices, authenticate event sources or prove the complete service. Callback termination and serialization are assumptions. Stop closes admission while retaining backing and results; it is not permission to unmap storage.

Fixed consumer sample entry points preserve an ordinary bounded AML error only
with the same idle provider and no newly retained error result. The suite checks
an actual division-by-zero error followed by a usable owner, typed argument
invocation, and modeled indeterminate/module/charge/access/busy-provider and
newly-retained-error refusals. These unsafe controls still close the owner once;
retained storage is released only by its explicit test owner. This is regression
evidence, not a new proof or hardware acceptance.
