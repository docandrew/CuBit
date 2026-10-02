# Native Mesa software cube

Build from the repository Nix environment, holding the shared build lock:

```sh
flock --exclusive --nonblock coordination/build.lock nix develop -c make -C kernel mesa-cube
```

This resolves the pinned Mesa source, prepares a writable copy with the CuBit
platform patch, configures native softpipe, builds the CuBit ELF and stages
`kernel/isodir/boot/mesa-cube.app`. The source/configuration cache is keyed by
the pin, patch and configuration inputs. Earlier cache directories are retained;
the script never modifies the Nix store. Target objects use the CuBit libc;
host build tools run in the Mesa Nix environment.

The build also prints the path of a fresh upstream notice bundle under
`userspace/mesa/build/notices.*`. It preserves the source pin, platform patch,
upstream license overview, version and complete license-text directory.
The normal build includes a linker map with archive extraction reasons and
symbol cross-references, plus the ELF hash. This connects the packaging audit
to the actual linked binary, including the non-Mesa runtime dependencies.
`LINKED-SOURCES.json` resolves Mesa archive members through compilation records
and hashes their source files, separating generated sources and unresolved
runtime members. It fails on missing Mesa mappings. It does not inventory
included headers, generators or direct link inputs.
The complete adapted Mesa source tree is also retained as `MESA-SOURCE.tar.gz`,
preserving per-file notices even for headers and generator inputs. This adds
about 106 MiB at the current pin. Separate runtime dependencies remain outside
that archive; the bundle is not a general distribution-license audit.
`build/notice-path` identifies the successful build's bundle for image tooling.

Check notice integrity and rejection behavior with:

```sh
nix develop -c bash tests/mesa-software/test-notices.sh /path/to/prepared-mesa-source
```

The demo renders nine frames of a textured, depth-tested OpenGL cube, then
retains the final image. Space resumes/pauses continuous rotation; Escape closes
it, including during animation. Each replacement retires the previous CPU
attachment before that buffer is rendered again. Present is not a retirement
fence. Only Desktop authority is requested.
It is software rendering, not Intel GPU acceleration. LLVM/JIT, EGL and a
general public app GL API are not supplied by this target yet.

The implementation still shares the validated frontend/winsys sources with
`tests/mesa-software`. The live USB image packages the app and its source/notice
bundle under `licenses/mesa`. Config supplies the ninth Apps entry, **Mesa Cube
(software)**. `usb-live-iso` builds the app dependency; the image audit checks
the packaged ELF against the bundled hash and requires the source/notices.

The UEFI live image passed a four-CPU native QEMU regression with optical Apps
launch, 194673 independently checked pixels, a complete animated rotation,
pause and Escape shutdown. Evidence: `tests/mesa-software/target/mesa-live-boot.log`.
Run this path with `tests/usb-optical/run-live.py --uefi --cpus 4 --mesa` inside
Nix under the shared build lock. This remains CPU softpipe rendering; it does
not validate Intel reset, firmware execution or GPU command submission.

The fresh-cache target and its staged ELF were validated with native QEMU:
`tests/mesa-software/target/mesa-staged-run.log` records 194673 independently
checked cube pixels plus Escape shutdown. This is not a NUC hardware test.

Set `MESA_WINDOW_ANIMATION=1` for the headless `mesa-window` test to inject
Space after the deterministic screenshot, require 36 more frames with buffer
isolation checks, pause, then close. Keep `MESA_WINDOW_SCENE=cube` and point
`MESA_WINDOW_IMAGE` at the staged app. This is a functional test, not a frame
rate benchmark; rendering is deliberately paced.
