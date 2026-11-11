# Hosted PCI fields

Export TMPDIR=/home/doc/cubit-build-tmp before entering the Nix development environment, then run `bash tests/acpi-pci-fields/run.sh`. Requires Node, GNAT/gprbuild, ACPICA tools and GNU timeout. All outputs and REPORT.json go to a fresh TMPDIR directory; the runner does not edit the checkout or require Git history/private workspace files.

The pinned source inventory selects the host Ada runtime and excludes the unused native ACPI service instance. It includes the coherent current Core, namespace, provider and protocol sources. Source and copied-input hashes are checked around commands. Compiler jobs are serial, builds have a 180-second limit, executions have a 30-second limit, captured output is limited to 1 MiB, and the stack limit is 64 MiB with core dumps disabled.

Pure protocol, geometry, policy, dispatch and provider tests run in strict checked/assertion/overflow and release profiles. Whole-Core startup uses only -O2 -gnatp functional code generation. It is not an assertion-enabled Core test or a SPARK proof.

Four actual AML startup fixtures cover bus zero, segment 7/bus 1, an absent bus-zero window, and a SystemMemory alias read before its bus is prepared. Each compares OUT0/ALV0 with ACPICA; reference shutdown allocation diagnostics are retained. Transport, clocks, heap refusal and scalar hardware values are modeled. Per-target named identities, deadlines, read-only policy, stale authority, uncertainty, explicit preparation and metadata-only installation are checked. No native execution, mapping, laptop acceptance or full-service verification is implied.
