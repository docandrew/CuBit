ACPI component source semantic gate

Export TMPDIR=/home/doc/cubit-build-tmp before entering the pinned Nix environment. Set ulimit -s 65536 and ulimit -c 0. Run:

    node tests/acpi-component-sources/run.mjs --source-root /absolute/cubit --output /absolute/fresh/output --manifest-tool /absolute/cubit/userspace/ccl/build/manifest/ccl-manifest

The gate parses the two actual component.ccl source lists and default/main choice; libraries or unfamiliar source syntax are refused. It copies only their directly declared production/shared source directories plus each component's implicit own directory. Compiler search order is preserved, including the existing native-before-hosted acpi_service.ads shadow; other duplicate source names are refused. The real ccl-manifest tool generates SEPARATE Startup/Parameters declarations from each actual manifest. Current CuBit API specs and the runtime project's shared ABI spec directories supply declarations; host GNAT supplies its standard runtime. There is no flat ACPI fixture fallback.

Each main compiles semantically in checked/release profiles, with strict warnings, one compiler at a time, nice19,180second compile and30second generation limits,1MiB logs, ALI switch checks and zero object output. Inputs, commands, per-component selections/shadows, ALI hashes and failures are retained in RESULT.json. Existing outputs are refused. The invoked manifest binary is pinned, not rebuilt or represented as source-reproducibly built here.

This checks current source visibility and declaration compatibility. It is NOT native code generation/link/boot, target-runtime execution, a whole stack bound or SPARK proof. The independent original-backend negative fixture fails on missing acpi_target_storage.ads; current backend/frontend pass with the added root/provider source directories.
