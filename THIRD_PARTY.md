# Third-party code, data and inspiration

CuBit is GPL-3.0-or-later ([LICENSE](LICENSE)). This file lists everything in
it that came from someone else, where its licence and notices live, and
whose work taught us how things should be done.

Corrections are welcome. If you see something of yours that isn't credited
properly, please open an issue.

## In this repository

| What | Where | Licence | Notices |
|---|---|---|---|
| GNAT run-time library (copied, then adapted for CuBit) | `kernel/runtime/gnat/`, `userspace/runtime/gnat/` | GPL-3.0-or-later with the GCC Runtime Library Exception 3.1 | Per file (Free Software Foundation, AdaCore, Universidad Politécnica de Madrid, European Space Agency) |
| ACPICA ASL test suite (ASLTS) sources, and `iasl`/`acpiexec` reference output | `tests/aml-*/reference/` | Intel ACPICA licence, taken under its BSD-3-Clause option | Per file (Intel Corporation) |
| Intel GPU context and workaround sequences, adapted from Linux v6.16 i915 (`intel_lrc.c`, `gen8_engine_cs.c`, `intel_workarounds.c`) | `userspace/services/intel-gpu/intel_gpu_adln_*.adb` | MIT | Full notice in `intel_gpu_adln_lrc_template.adb`; the others point to it |
| Realtek RTL8852B register tables and constants, from Linux's rtw89 driver | `userspace/services/rtw89/` | Realtek's GPL-2.0 OR BSD-3-Clause, taken under BSD-3-Clause | Per file; full text in `spark/rtw89_tables_8852b.ads` |
| Teapot control points (freeglut) | `tests/mesa-teapot/teapot-control-points.h` | MIT | In the file |
| Intel GuC firmware | `firmware/i915/` | Intel redistribution licence | `firmware/LICENSE.i915` |
| Realtek RTL8852B firmware | `firmware/rtw89/` | Realtek redistribution licence | `firmware/LICENCE.rtlwifi_firmware.txt` |
| Bluecurve icons and cursors (interim), as generated atlases | `userspace/services/desktop/desktop_icons.ads`, `desktop_window_icons.ads`, `desktop_cursors.ads` | GPL-3.0 (source repository); Fedora ships them as GPL-2.0-or-later | `userspace/lib/ui/ASSET_LICENSES.md`, `userspace/lib/ui/licenses/BLUECURVE_GPL-3.0.txt` |
| Patches to Mesa, GStreamer and GCC | `tests/mesa-anv/`, `userspace/servo/media/`, `userspace/ports/gcc/` | The patched project's licence (MIT, LGPL-2.1-or-later, GPL-3.0-or-later) | Upstream's |

Test fixtures under `tests/acpi-lenovo-hid/fixture/` include short excerpts
of the IdeaPad Slim 3i's ACPI tables, decompiled with `iasl`. They are used
only to test interoperability. We plan to replace them with hand-written
equivalents.

## Fetched when CuBit is built (not in this repository)

These are pinned by commit or content hash in `flake.nix`, `flake.lock` and
the build scripts. Images that contain them carry each one's licence; see
`images/`.

| Project | Used for | Licence |
|---|---|---|
| [doomgeneric](https://github.com/ozkl/doomgeneric) (id Software, Simon Howard) | the DOOM port | GPL-2.0-or-later |
| [SameBoy](https://github.com/LIJI32/SameBoy) (Lior Halphon), including its own boot ROMs | the Game Boy emulator port | Expat (MIT) |
| [Mesa](https://mesa3d.org/) | Vulkan (ANV) and software rendering | MIT, plus per-file licences; images carry Mesa's own notice bundle (`userspace/mesa/NOTICE.md`) |
| [Servo](https://servo.org/) and SpiderMonkey (`mozjs`), as the [penny-servo](https://github.com/docandrew/penny-servo) and [penny-mozjs](https://github.com/docandrew/penny-mozjs) forks | the Penny browser | MPL-2.0, with the dependencies' own licences |
| GStreamer, GLib, Opus | Penny's media playback | LGPL-2.1-or-later (GStreamer, GLib), BSD-3-Clause (Opus) |
| GCC, GNAT and binutils (with musl for the cross toolchain), from nixpkgs | the compilers, both on Linux and on CuBit | GPL-3.0-or-later (GCC Runtime Library Exception for the run-time libraries); musl is MIT |
| Linux 6.12 and BusyBox, from nixpkgs | the test guest for the VMM | GPL-2.0-only; the corresponding source is described in `tests/vm-linux/LICENCE.guest.txt` |
| [SPARKNaCl](https://github.com/rod-chapman/SPARKNaCl) (Rod Chapman) | cryptography | BSD-3-Clause |
| [libkeccak](https://github.com/damaki/libkeccak) (Daniel King) | SHA-3 / Keccak | BSD-3-Clause |
| [sparktls](https://github.com/docandrew/sparktls), [sparktlscrypto](https://github.com/docandrew/sparktlscrypto), [sparkx509](https://github.com/docandrew/sparkx509), [sparkentropy](https://github.com/docandrew/sparkentropy), [sparkmlkem](https://github.com/docandrew/sparkmlkem) | TLS, cryptography, certificates, entropy, ML-KEM | BSD-3-Clause; sparkmlkem is Apache-2.0 |
| Turso | `config-storage`'s database | MIT |
| IBM Plex, Literata, EB Garamond and Source Serif fonts, from nixpkgs | the UI and the text apps | SIL Open Font License 1.1; notices in `userspace/lib/ui/licenses/` |
| [CBOR for Ada](https://github.com/b-erdem/cbor_ada) (b-erdem) | ccl-remote's wire format | Apache-2.0; notice in `userspace/ccl/remote/CBOR_LICENSE.txt` |
| The Rust standard library and the crates in each `Cargo.lock` (font rasterizing: `ab_glyph`, `ttf-parser` and others) | Rust programs | MIT / Apache-2.0, per crate; font crates' notices in `userspace/rust/fonts/licenses/` |
| Bible translations from eBible.org | the Bible app's texts | Per translation: public domain or the licence recorded in `assets/bible/sources.json`, copied into the package's notice |

## What we learned from

No code from these projects is in CuBit unless it is listed above. We read
them, we owe them a lot, and we cite them in comments where we rely on what
they showed us.

- **Linux**, the source of hardware facts that no datasheet gives:
  - i915 for Intel graphics, rtw89 for the Realtek Wi-Fi chip;
  - pinctrl for Intel GPIO, xHCI, ACPI quirks;
  - KVM for what a virtual CPU must look like.

  When Linux code is under GPL-2.0-only, we take facts from it, such as
  register numbers, sequences and quirks, never its code.
- **coreboot**, for Intel chipset layout: sideband ports, GPIO communities.
- **ACPICA**, for the ACPI behaviour CuBit's AML interpreter is checked
  against.
- **QEMU**, as the machine almost every CuBit test runs on.
- **Con Kolivas**, whose BFS (2009) and MuQSS schedulers showed that a
  small, simple rule keeps a desktop responsive under load: run the thread
  with the earliest virtual deadline. CuBit's scheduler is built on his
  ideas:
  - `kernel/src/virtual_deadlines.ads` follows MuQSS's earliest-deadline
    rule, and BFS's rule that a sleeper keeps its deadline but gets no more
    than the rest of its slice;
  - `kernel/src/realtime_admission.ads` caps admitted real time at a share of
    each CPU, as his SCHED_ISO did.

  He put the desktop and its users first, and he measured what they feel,
  at a time when mainline Linux would not. His work never got the welcome
  in mainline it deserved, and CuBit owes it a great deal. No code of his is
  in CuBit; the ideas are his.
- **seL4 and the capability-system tradition**, for capability handles and
  CSpaces.
- **BeOS and Haiku**, for the desktop's look and its Deskbar.
- **Nix**, for reproducible, content-addressed builds, which ccl-build
  follows.
- **Kubernetes Secrets**, for the shape of the secrets service.
- The **XPFO** discussion on the Linux kernel mailing list and the
  **ret2dir** paper (Kemerlis, Polychronakis and Keromytis, USENIX Security
  2014), for why process memory leaves the kernel's map.
- The **Intel, AMD, ACPI, xHCI, NVMe, UFS, PCI and HID** specifications,
  which come first wherever they say enough.
