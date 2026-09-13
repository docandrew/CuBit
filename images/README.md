# CCL system-image profiles

These are **Linux-hosted image build inputs**, not scripts run with authority
inside CuBit. The selected `init.ccl` and `system.ccl` still run natively in
procmgr/devmgr during boot.

| Profile | Controls | Existing Make target |
| --- | --- | --- |
| `development-initrd.ccl` | Normal bootstrap archive | `initrd` / `iso` |
| `laptop-initrd.ccl` | All-in-initrd laptop fallback | `laptop-live-iso` |
| `laptop-usb.ccl` | Minimal bootstrap plus ISO9660 payload and GRUB menu | `usb-live-iso` |

The normal development ext2 disk contents and the fallback ISO's outer GRUB
staging still use existing Make recipes. This slice replaces archive membership
and the USB ISO contents; it is not yet a complete CCL build graph.

## Declarations

The header tag `v1` names a declaration-format version, represented internally
by the shared `CCL.Declarations.Format_Version` enum. It is not an image ID,
package release, integer expression, or quoted string. Unknown versions and the
old numeric header are rejected. Manifests, service catalogs, startup profiles,
system configuration, and image catalogs use the same convention.

`artifacts.ccl` names the available artifacts and bootstrap providers:

```lisp
(artifact "xhci" repository "kernel/isodir/boot/xhci.drv")
(bootstrap-requires "usb-optical" "xhci" "xhci.drv")
```

An image chooses a catalog, layout, provider, and placements:

```lisp
(system-image v1
  (catalog "cubit-native-v1")
  (layout optical)
  (provider "usb-optical")
  (kernel "kernel")
  (boot-menu "grub-usb-live")
  (settings "system-live")
  (startup "init-usb-live")
  # Additional required bootstrap files and payload follow:
  (file bootstrap "xhci" "xhci.drv")
  (file optical "ccl-workbench" "apps/ccl-workbench.app"))
```

The fragment above is deliberately incomplete; use the full profiles for a
bootable image. Fields accept pure CCL expressions, including string
concatenation. No arbitrary shell command or host call is a declaration form.

The catalog is a trusted repository build input. Its name is a compatibility
label, **not a signature or identity proof**. Required bootstrap artifacts must
be present at the exact names their consumers use. The CCL evaluator does not
hard-code Desktop, Filesystem, driver IDs, or service authority names.

`repository` sources resolve inside the repository. `supplied-file` and
`supplied-tree` sources name explicit caller-provided inputs such as the
Nix-selected DOOM WAD or third-party license notices. Missing or unused bindings
are errors; supplied trees cannot contain symlinks and are optical content only.

## Realization boundary

The bounded Ada/SPARK-mode frontend returns a typed plan. The Linux Python
adapter snapshots selected artifact bytes, validates the native CCL config
profiles, and verifies startup executable membership. Only then does it invoke
CPIO/GRUB using fixed argument lists. It checks the resulting archive and ISO
primary-tree bytes against the plan before replacing an existing image.

Path traversal, conflicting file/directory destinations, case aliases, unknown
artifacts/providers, and missing bootstrap dependencies are rejected. The
generated `boot/initrd.img` path is reserved.
The ISO audit follows CuBit's terminal-dot/version normalization and compares
declared payload bytes; it allows GRUB's additional generated loader files.

Each input document is bounded to 8192 bytes; catalogs, dependency lists, and
placements each allow 64 entries. Paths are bounded to 192 ASCII bytes. The
realizer limits individual input files to 128 MiB, resolved input bytes to
512 MiB, and files/tree entries to bounded collections of at most 512.

An adjacent `.plan.json` records exact profile/catalog/input hashes, destination
paths, private-input markers, and the final output hash. JSON is an inspection
report here, not a CuBit runtime protocol or a replacement configuration
language. Source builds/toolchains are still controlled by Make/Nix: these hashes
do **not** establish hermeticity, signing, reproducible ISO bytes, or provenance.
The image and sidecar are individually replaced atomically, not as one
transaction; the output hash allows detection of a stale/mismatched sidecar.

Image inclusion neither approves execution nor grants authority. Existing ELF
manifests, launch approval, and kernel/service enforcement remain separate.
This work does not alter procmgr's current minting powers.

No GNATprove result is claimed for the new image frontend yet. Its source uses
SPARK mode without Assume or SPARK-Off additions; the Linux realization adapter
and filesystem/tool interactions are outside that proof boundary.

## Build and inspect

```sh
nix develop -c make -C kernel test-ccl-images
nix develop -c make -C kernel usb-live-iso
nix develop -c make -C kernel laptop-live-iso
nix develop -c make -C kernel iso
nix develop -c python3 tests/ccl-images/test-fallback-boot.py
```

The fallback boot test uses the existing laptop profile's HDA device and no
writable disk. Missing-HDA startup is a separately tracked boot-manager defect.

To inspect a checked placement plan without realizing an image:

```sh
nix develop -c userspace/ccl/build/image/ccl-image images/artifacts.ccl images/laptop-usb.ccl
```

The realizer's `--check-only` additionally resolves/snapshots actual inputs and
prints their hashes, without running archive/ISO tools. Run
`nix develop -c python3 userspace/ccl/tools/ccl-image/realize.py --help`
for explicit input binding options.

### Private cartridges

Ordinary profiles include only the original SameBoy test cartridge. A personal
USB build can explicitly add local cartridges:

```sh
SAMEBOY_ROMS_DIR=/absolute/path/to/local-roms nix develop -c make -C kernel usb-live-iso
```

The adapter preserves the existing `00.gb` test ROM and assigns `01.gb` through
`15.gb` to sorted local inputs. It never downloads cartridges or modifies
originals. Private source filenames are not included in the report.
`--release` rejects the explicit private-cartridge option; it is not a general
license/private-data classifier. The standard catalog and explicit input
bindings still need review. Do not distribute a personal image with private
cartridges.

## Next slices

- Derive source-build dependencies from package artifacts rather than Make's
  current build-target supersets.
- Move development ext2 payload/directory creation into the same plan.
- Add artifact identities, toolchain/store references, signing and installation
  policy without conflating them with image membership.
- Model service readiness/order beyond the existing sequential startup plan.
