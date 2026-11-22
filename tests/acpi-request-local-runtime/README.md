# Request-owned local runtime windows

    export TMPDIR=/home/doc/cubit-build-tmp
    nix develop -c bash tests/acpi-request-local-runtime/run.sh

Requires the pinned GNAT/GPRbuild, Node and ACPICA tools on PATH. Builds are -O0 -gnatp, limited to 180 seconds and 24 GiB; runs to 30 seconds. Inputs are hash-checked and copied into private output directories before compilation.

The actual request/bootstrap/Core pipeline initializes synthetic AML and queues real _INI notifications. The test uses the local runtime policy with modeled provider ownership and delay callbacks. Fresh, receiving and region-blocked owners cannot claim/open; a complete foreign request cannot affect the bound runtime; matching requests enter once and begin/end finite windows. An unsuccessful event stops once, and lifecycle operations preserve revision, import and pending-notification observations. End delegates to the runtime's original-Core check without repeating readiness. Storage release at test end is fixture cleanup only, not native retirement evidence.

The new child generics select their parent's private Core and add no wire operation, pointer export, AML evaluation, receipt acknowledgement or hardware access. Native integration must bind callbacks from the exact same Root.Fields.Runtime child used by startup and event consumers. Serialization, bounded normal-return callbacks and trusted binding remain assumptions; there is no new SPARK proof or native/hardware execution claim.
