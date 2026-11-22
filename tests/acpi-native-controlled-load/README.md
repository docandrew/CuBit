# Native controlled snapshot facade

Export TMPDIR=/home/doc/cubit-build-tmp before entering Nix, then run:

    bash tests/acpi-native-controlled-load/run.sh checked
    bash tests/acpi-native-controlled-load/run.sh release

Each profile performs 214 checks in 24 isolated Ada processes, covering both
AML integer widths. The actual native read/load bodies and opt-in child facade
run against modeled kernel authority/table data, generated Startup accessor,
clock and EC/region barrier. No actual syscall, provider admission or hardware
is used. The real runtime declarations have a separate semantic API gate.

Cases cover two native copy chunks, catalog/descriptor/copy/clock failures,
early/late deadlines, module exceptions, repeated calls, shared-driver refusal
and nested calls that cannot clear the outer active guard. The default loader
still refuses hardware access. Diagnostics preserve the last actual copy call.

The runner verifies source hashes before and after execution and ALI profile
flags. One compiler worker has a 180-second target limit; each executable has
a 30-second limit. --validate-only does not compile. --resume RESULT_DIRECTORY
verifies a previous private result directory and every copied source before
reusing its cache. Historical records are preserved.

This establishes no new SPARK proof, real generated-startup authentication,
native EC authority/region connection/recovery lifetime, or QEMU/laptop result.
The production native Instance still selects the default loader.


Native controlled-loader integration evidence is recorded in
NATIVE-CONTROLLED-LOAD-EVIDENCE.json. The API gate now selects 60 units and emits
57 ALIs with zero objects per profile. Hosted kernel/startup/clock models are
not native authentication, provider admission or hardware validation.
