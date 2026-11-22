# Native Desktop and production Mesa link checkpoint

`tools/build_desktop_vulkan_link.py` builds a private Desktop executable with
its own Ada binder, the existing production Mesa service bundle and the new
Vulkan context owner/FFI. It leaves the normal manifest and software rendering
path intact. It does not call GPU startup, obtain render authority, activate a
GPU renderer or stage an image. Native software-startup evidence follows below.

Under `coordination/build.lock`, use the pinned
`tests/compositor/vulkan-affine-shell.nix` and provide a verified bundle, an
existing Mesa source directory, and a new output directory. The helper checks
shader tools before creating output, compiles and validates the existing SPIR-V,
uses the native musl compiler for C boundaries, and preserves Mesa's audited
whole-archive/group ordering and build-ID linker script. It verifies source
hashes and the bundle again after linking and rejects unresolved symbols or
missing ownership bridge exports. The output stack contract is 16 MiB.

The previous `service-bundle-production` correctly failed verification after
`libgnat-user.a` changed. A new bundle was built with the existing production
builder at `tests/compositor/build/desktop-service-bundle-r1`. The first Desktop
attempt compiled/bound Ada but lacked glslangValidator in the standard shell;
it remains incomplete. The Vulkan-shell retry passed:

- `build/desktop-vulkan-link-r2/result.json`: native link passed, GPU disabled,
  executable not run, required Ada/C lifecycle symbols present.
- ELF SHA256: `27944cd24748d42fd686fee5925e60392d0af6f73f747169ae7ef79efac719b8`.
- ELF size: 52,072,872 bytes (about 49.7 MiB on disk). This is not resident memory
  or GPU allocation usage; no memory-performance claim follows from link size.
- `build/desktop-vulkan-link-r2.log` contains compiler/link diagnostics;
  `inputs.json` retains root source and runtime hashes.

Before activation, Desktop still needs optional render admission in its manifest,
startup/retirement wiring, production child registration, target allocation,
whole-frame recording/fallback, and presentation integration. Procmgr already
supports optional rendering through request type 11 param0=1, including fresh
software-child fallback. At this checkpoint the CCL manifest compiler exposes
only required `request-render`; extending that spelling is a coordinated
prerequisite, not permission to make the normal Desktop hardware-dependent.

## Native software startup

`test-desktop-vulkan-boot.py LINKED_DIRECTORY SEED_DIRECTORY NEW_OUTPUT` runs
inside Nix with a copied kernel/initrd and service seed set. It builds a private
disk and ISO, boots the exact linked ELF, and operates the Apps menu using QEMU
keyboard input. It does not rebuild or stage shared services. Seed and runner
hashes are recorded and rechecked; these are prebuilt services, not a claim
that the current complete source tree builds.

`build/desktop-vulkan-boot-r2/result.json` passed with the ELF above, four TCG
vCPUs and 1 GiB guest RAM. Three menu-open cycles each changed 106,723 pixels.
After each Escape, the desktop above the clock/taskbar restored exactly. No
guest fault was detected. PNGs, serial output, private boot inputs and the
runner copy are retained alongside the result. This proves native elaboration,
software startup and keyboard-driven repaint with the linked production Mesa
libraries. It does not call Mesa device startup, exercise GPU rendering, prove
whole-frame GPU fallback or measure physical latency.

The initial current-tree headless attempt (`build/desktop-vulkan-boot-r1`)
failed before QEMU: Config's `ccl-objects-schemas.adb:123` omitted the new
`Default` component. Its kernel/initrd preparation also refreshed several
staged service binaries; it did not replace staged Desktop. That failure and
its input changes remain recorded. The successful retry used the previously
tested `tests/servo/build/focus-repair-gate-fismyob5/stage` seed, copied privately,
without modifying Config/CCL or substituting a source-build success claim.
