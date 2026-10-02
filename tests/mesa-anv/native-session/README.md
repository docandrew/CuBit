# Admitted native GPU session check

This test-only CuBit application exercises the production render capability
and the Ada FFI used by the Mesa port. It checks session health and the memory
contract, creates a 4 KiB buffer, obtains a writable grant, acquires a CPU
borrow through the render capability, writes/reads the first and last words,
returns the borrow, and requests mapping retirement. It then binds, unbinds
and rebinds the BO offline at GPU VA 4 GiB, prepares the private VM and registers
the context. Session close retains the bound BO until retirement; no early
buffer-name close or physical reclamation is assumed.

Before allocation, `mesa-query-decoders` calls the actual Mesa-port discovery
decoder and native Ada query transport using the same manifest capability.
Status0 means identity/topology, CS clock, private VM, memory policy and budget
all passed their existing protocol validation; failures1..5 identify those
checks respectively. This does not construct a Mesa physical device or prove
the coherent-memory requirement needed to expose a Vulkan device.

It submits no application GPU commands and does not prove CPU/GPU coherence.
Context registration runs the driver's initialization marker and requires its
completion and scheduling-disable acknowledgment. The BO's CPU test data is
never submitted for execution. Policy1
(explicit maintenance) is accepted for these CPU-only accesses. Pending mapping
retirement is reported and left to session retirement, not treated as safe
reclamation. Close is attempted once, including on error; uncertainty never
causes a retry of allocation, mapping or session close.

Build from the checkout root in the Nix environment under the shared lock:

```sh
flock --exclusive --nonblock coordination/build.lock \
  nix develop -c make -C kernel render-session-test
```

The target generates the manifest and its named slot binding, compiles the
actual FFI sources, and stages `render-session.app`. It does not change a live
image or any startup profile. A hardware test profile must explicitly include
`(start "render-session.app" (priority 3) (render approve-declared))` after the
Intel backend becomes available. Public app launch is deliberately insufficient
to grant render authority. No MMIO, GPU broker or process-control capability is
requested by the app.

The opt-in profile is `tests/hardware/init-render-session.ccl`; packaging must
include the staged app and explicitly select that profile. It is not selected
by the normal image. Diagnostics also publish through a manifest-scoped
logstore endpoint, for the desktop Boot Logs viewer. Publication waits are
bounded, with serial fallback; missing log completions never authorize reuse
of the publisher's page. The app retains that storage until logger disconnect
confirms retirement, which can keep the test process alive after its result.

`images/render-session.ccl` preserves the normal live image membership and
adds this app with the opt-in startup profile. After rebuilding the kernel,
procmgr, devmgr, Intel driver and test, package under the shared build lock:

```sh
nix develop -c bash -c 'bash tests/usb-optical/build-live.sh "$DOOM_WAD" --uefi --render-session'
```

The separate output is `kernel/cubit_live_render_session.img`; the normal
`cubit_live_uefi.img` is not overwritten. The adjacent plan records hashes
of packaged artifacts, including pre-existing staged applications. Rebuilding
selected targets does not imply that all other bundled applications were rebuilt.

Expected diagnostics start with `RENDER-SESSION admitted app entered`
and end with `RENDER-SESSION PASS private context initialized (NO application batch)`.
Every intermediate operation prints its numeric status. `TEST: FAIL` means
the roundtrip or a required cleanup request failed. A PASS does not establish
completed session retirement, reclaimed backing, Mesa rendering or GPU work.
No entry marker means admission/startup must be investigated before the FFI.

The earlier CPU-only image reports `PASS CPU buffer roundtrip (NO GPU submission)`;
it does not contain these subsequent discovery/context checks. Do not apply the
new expected markers retroactively to that offered image.

CPU-only target compile/link/staging passed on 2026-10-01. Native execution on an Intel
GPU remains unverified; ordinary QEMU has no matching Intel render backend.
