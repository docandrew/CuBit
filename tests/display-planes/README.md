# Display planes: planner, codecs and QEMU cursor evidence

Design and results: [docs/display-planes.md](../../docs/display-planes.md).

## Hosted tests and proofs (Nix shell, from `kernel/`)

```sh
bash ../tests/display-planes/prepare.sh
alr exec -- gprbuild -p -P ../tests/display-planes/planes.gpr
../tests/display-planes/build/plane_tests
alr exec -- gnatprove -P ../tests/display-planes/planes.gpr \
  -u cubit-display_planes.adb cubit-display_plane_protocol.adb \
     cubit-gpu_plane_protocol.adb --level=2 --report=fail --checks-as-errors=on -j8
```

`plane_tests` covers concrete plans for two to four cursors on one or two
outputs (priority, ties, oversize, straddling, offscreen, absent outputs,
frame gating), a 4500-plan sweep that re-checks the proved contracts at run
time, clipping at every edge, and codec round trips/bit flips. Assertions
are enabled (`-gnata`), so every planner postcondition is also executed.

## QEMU evidence (under `coordination/build.lock`)

`qemu/qemu-evidence.sh` is a `QEMU_BIN` wrapper. It adds QEMU's
`virtio_gpu_update_cursor` trace, a monitor for screendumps and a VNC
socket from which `qemu/vnc_cursor.py` reads the cursor QEMU draws on the
host (RichCursor). The guest is unchanged.

```sh
E=$TMPDIR/planes-evidence
CUBIT_REAL_QEMU=$(command -v qemu-system-x86_64) CUBIT_PLANES_EVIDENCE=$E \
QEMU_BIN=$PWD/tests/display-planes/qemu/qemu-evidence.sh \
  bash tests/headless/run.sh --test display-grants-virtio-vga --accel kvm --timeout 120 --keep-logs
python3 tests/display-planes/qemu/analyze.py $E

# Desktop pointer: CUBIT_PLANES_DESKTOP=1 moves the PS/2 mouse from the host.
CUBIT_PLANES_DESKTOP=1 ... --test desktop-virtio-vga ...
python3 tests/display-planes/qemu/analyze_desktop.py $E
```

QEMU's screendump is the guest scanout and does not include a host-drawn
cursor; that absence is what the analyzers check, together with the trace
and the VNC cursor image. `*-host.png` composites the published cursor at
the traced position to show what a host window displays.
