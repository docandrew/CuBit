# Workbench examples

The USB live ISO includes all `.ccl` examples in this directory under `/samples/ccl/` and editable
copies in the live workspace, `@mem:0/work`. The initrd-only laptop image also
includes the workspace copies. No extra filesystem authority is needed.

Open CCL Workbench, press **Ctrl+O**, select a file and press **Enter**:

| Sample | Try it |
| --- | --- |
| `button-clock.ccl` | F5 registers a Refresh button; click it for uptime. Stop closes it. |
| `button-output.ccl` | F5 registers Print uptime; each click adds a line to Output. |
| `enums.ccl` | F5 interprets a typed Color value; F8 shows its BASIC declaration. This sample also uses functions and strings, which remain interpreter-only. |
| `variants.ccl` | Scalar-payload variants and exhaustive matching, in both Interpret and Compile / Run VM / Step. F8 switches to BASIC. |
| `clock-label.ccl` | F5 updates the label once; F7 watches it periodically. |
| `function-clock-label.ccl` | The same label, using typed function definitions. |
| `monotonic-clock.ccl` | F5 returns elapsed time as `hh:mm:ss`, not calendar time. |

F8 switches between Lisp and BASIC. Opening another file with unsaved edits
offers Save / Discard / Cancel. Save opens the existing save-as-new-file picker,
then continues to Open only after saving succeeds. Discard allows opening
without saving; canceling that picker still keeps your edits. Ctrl+S also saves
under a new name. Live-workspace changes are lost at reboot.
Workbench currently selects its NVMe workspace first if one is available; the
USB-only live profile uses the RAM workspace. The picker does not yet browse
the CD's read-only originals or switch volumes.

Rebuild the USB LiveCD with `nix develop -c make -C kernel usb-live-iso`;
the image is `kernel/cubit_laptop_usb.iso`. New examples are automatically
seeded into the RAM workspace. Add their explicit optical entries to
`images/artifacts.ccl` and `images/laptop-usb.ccl` too: `test-ccl-images`
checks the entire samples directory against the optical plan and compares
the workspace copies byte-for-byte.

To boot the USB LiveCD in QEMU, use `nix develop -c make -C kernel run-usb-live`.
After building once, `run-usb-live-fast` reuses that ISO without rebuilding.
Both use the same GTK/X11 window backend as `run-desktop`; the normal desktop
target uses a separate NVMe workspace rather than this live RAM workspace.
