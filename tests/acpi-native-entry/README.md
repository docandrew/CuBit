# Native ACPI one-service API checks

Export TMPDIR=/home/doc/cubit-build-tmp before Nix. Run tests/acpi-native-entry/run.sh --validate-only or --mode checked|release. The default profile is release.

The runner pins and freezes actual component source directories and runtime SDK declarations, validates the typed component and regenerates Startup from the current one-service manifest/catalog/schema. It semantically compiles the selected units in selections.txt, including native Main, local root, pooled Fields/runtime, notification/event adapters including HID/I2C inventory events, EC/device and pure AML bodies. Strict warnings and checks follow each selected profile. No objects, executable linking, full build, boot, hardware execution or new SPARK proof are claimed. Compile limits are180 seconds/24GiB per selection, nice19, one worker; generated results live only under TMPDIR.

Source hashes, a closed Ada inventory, component-directory list, compiler/tool hashes, selected ALI inventory/profiles and generated Startup hashes are checked. CCL_MANIFEST_TOOL and CCL_BUILD_TOOL may point at an alternate location of the exact pinned tool binaries. Build those tools through the normal CCL tool workflow before testing if absent. A changed tool or source requires explicit catalog reconciliation.

The earlier fixtures/actual mirrors, reference files and GPR project files are historical captures and are not used by this runner. Their old API verdicts do not substitute for these current source checks. Cleanup of those captures is separate from this integration change. Whole-Core functional code generation is tested separately with -gnatp; both profiles here use -gnatc only.

Native event activation cohort: the selections list now includes the concrete event domain and four-family consumers on the same Core/runtime, deferred edge EC queries, quiet copied fact logging, and explicit event/SCI/region retirement. This remains SDK semantic checking only: no objects, native execution, hardware validation or new SPARK proof. The companion acpi-native-event-domain suite exercises the actual coordination code with modeled native boundaries.

Explicit runtime_selections may select named CuBit SDK units under userspace/runtime/gnat. Each must be a unique selected bare unit name already in the pinned input inventory. This does not add the runtime directory to component sources or permit arbitrary path fallback.

Startup completion selects the actual 0253 client, phase/retention policy,
report adapter, runtime declaration and modified Native_Instance. Runtime/CCL
tools are pinned to the coordinator snapshot. No retained shutdown execution.

Runtime-selected SDK declarations also receive -gnatg and -gnatyM120 to
check the runtime style rules while preserving each checked/release profile.

--mode native-object regenerates Startup and compiles the actual Native_Instance to an object with the native ccl-build command flags, including O2, no-red-zone and no-pic. It uses neither gnatc nor gnatp and checks the emitted object/ALI profile. Source/output paths are private substitutions of the image15 CCL step. It performs no link, full/kernel/image build or native execution. This is required alongside the checked/release semantic profiles before native integration publications.

The default semantic inventory contains 227 selections, retaining all 222 earlier selections and adding five startup bridge/event/delay declarations or bodies. For a bounded startup iteration, run `bash tests/acpi-native-entry/run.sh --mode release --selection startup` (or `--mode checked --selection startup`). This explicit named selection checks nine units and reports its own selection/artifact counts; it does not stand in for the default full selection. Unknown selection names and named native-object selections are refused. Native object/build/boot validation is coordinator-owned in the current ACPI workflow.

The ancestor-propagation change extends the explicit startup semantic selection to 11 units, including both EC discovery spec and body. The full default inventory now contains 228 selections; all earlier 227 remain. Native object generation and hardware execution remain coordinator-owned.

The Unload framing leaf adds its concrete body to the full semantic selection: 255 units, retaining all254 earlier selections. Its two source files are part of the closed live Ada inventory. The historical fixtures remain unchanged and unused. This remains compile-only SDK checking, with no native execution or new proof claim.

The closed source inventory also includes the four existing shared ACPI CMOS protocol/window specifications and bodies from the named base. These repair an earlier inventory omission; the full selection remains255 units and the original flags, tools and named selections remain unchanged. This source-inventory repair adds no production-code changes or proof claim.

The proved Unload statement window/fuel helper adds one concrete body selection, for256 total. Both helper sources are included in the full closed directory inventory. Existing flags, limits and historical fixtures remain unchanged.
