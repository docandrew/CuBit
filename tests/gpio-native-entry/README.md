# GPIO native entry semantics

Run inside the pinned Nix environment with TMPDIR=/home/doc/cubit-build-tmp:

    bash tests/gpio-native-entry/run.sh

The closed source catalog validates the real driver component/manifest, regenerates
Startup using driver-services.ccl, then semantically checks all component bodies
and bodyless specs in checked/release profiles. It generates ALIs and no objects;
no full build, linking, boot, kernel/MMIO execution or SPARK proof occurs. Outputs
are unique under TMPDIR. --validate-only checks inputs/tools without compilation.

The GPIO_Kernel_Regions specification and body live in different component source
directories; both source selections produce one logical ALI. Exact distinct ALI
names and profile flags are checked. Inputs are snapshotted and original/copy
hashes rechecked around commands. Kernel behavior remains a separate native gate.

The parent-IRQ component has45 semantic selections and44 distinct ALIs per
profile. Its real manifest declares an optional platform interrupt at resource0,
and generated Startup exposes the optional parent used by production Main. The
removed polling loop is now only a hosted model and is outside this component.
No native execution or physical parent-IRQ delivery is established here.
