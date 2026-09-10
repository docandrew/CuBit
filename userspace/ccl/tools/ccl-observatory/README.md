# CuBit Observatory — native guest, browser frontend

This is the browser frontend, **not the native CuBit desktop Workbench**.
It renders actual bindings reported by the control app, invokes its clock
endpoint, and submits expressions to the CCL interpreter **inside CuBit**.
No Linux evaluator, API relay, synthetic objects, or fabricated telemetry.

## Try it

Install pinned frontend assets once:

```sh
nix develop -c npm --prefix userspace/ccl/tools/ccl-observatory ci --ignore-scripts --no-audit --no-fund
```

In one terminal, build and boot the isolated headless guest (one-hour limit):

```sh
nix develop -c make -C kernel ccl-remote-lab
```

This explicitly uses KVM (no silent software-emulation fallback). If KVM is
unavailable, use `CCL_LAB_ACCEL=tcg,thread=multi` on the make command; that is
useful for correctness tests, not performance measurements.

This uses the existing `kernel/nvme_disk.img` as a base, stages current app and
clock images into a **temporary copy**, and refreshes the kernel/initrd.
It requires the base system to have been built with `make -C kernel world`.
The original disk is not modified. This command does not launch a desktop.

In a second terminal:

```sh
nix develop -c make -C kernel ccl-web-preview
```

Open **http://127.0.0.1:8787/** (exact origin) and click **Connect to CuBit**.
Try `(+ 20 22)`, `(concat "Hello, " "CuBit!")`, or `(+ true 1)` to see a
type error. Select the clock node and invoke its endpoint for monotonic
milliseconds, not calendar time. Ctrl+Enter submits the expression.

Each evaluation is independent, with 4096 fuel and at most 1024 ASCII source
bytes. `(clock.monotonic-ms)` now invokes the native clock endpoint from inside
an expression. The complete formatter is in `userspace/ccl/samples/monotonic-clock.ccl`:
it samples once and formats elapsed time as HH:MM:SS (hours do not wrap at 24).
Each Evaluate takes a fresh sample. To keep it running, click **Run editor as
widget** in the **Live label** panel below the editor. The native host retains
that source and evaluates it once per second after completion. Editing the
editor does not change the loaded program. **Stop widget** stops future runs.

This is one lab-owned periodic label, not the general CCL desktop UI protocol.
The browser renders native typed results and observes the run count; it does
not run a timer that evaluates CCL or fabricate clock updates. Closing the
browser or pausing observations leaves the native widget running. Reconnecting
shows its current source/result/state. Rebooting the guest clears it.

The reusable SPARK `CCL.Periodic_Programs` record owns lifecycle, fuel/interval
configuration, retained results, and generation/sequence checks. The native
adapter owns evaluation and the existing Clock-only grant set. No NEEDS syntax,
authenticated discovery, general UI authority, or additional service grants
are implemented by this slice. A generation is stale-action protection, not
a remote authority token: the plaintext endpoint still has one shared lab slot.

Native `accept` waits end at the next timer deadline (using the four-word
network IPC timeout encoding); missed ticks coalesce, and failures stop the
program. A slow HTTP request can still delay it up to the request deadline,
and a blocking host IPC call has no interpreter-enforced wall-clock limit.
This is not a hard-real-time or general asynchronous host implementation.

The generic interpreter host bridge uses the same exact descriptor/contract
grant lookup as VM linking. The native adapter currently installs only its
manifest-authorized clock binding, not arbitrary service access. Schema details
are bundled Clock v1 metadata, **not authenticated live discovery**.

The static preview server binds loopback, serves an explicit asset allowlist
(including Three.js and its MIT license), and has no API or execution path.
The browser sends CBOR directly to `127.0.0.1:18445/ccl`; QEMU forwards that
loopback port to the guest's explicitly approved TCP listener,
`10.0.2.15:8080`. No relay or frontend CDN is involved.

Without a guest, the layout and local scratch editor still work. No requests
are sent until Connect. Failed requests disable execution and mark observations
stale. Polling pauses in hidden tabs or with Pause updates; observation age is
visible. Reload starts disconnected. Restart the static server after asset
edits, since files are loaded at startup. Other preview ports are supported for
offline layout only; the native lab adapter admits exactly the origin above.

## Security boundary

**Development plaintext only, not remote management authentication.** Do not
expose this on a LAN, public/cloud interface, or a machine with untrusted local
clients. Host/Origin checks and CORS restrict browser access but are not
credentials; local non-browser clients can forge them. Binding to loopback is
part of this lab's containment, not proof of peer identity.

The manifest requests only an approved listen scope and the clock endpoint,
not outbound connections, files, secrets, process administration, or desktop
authority. Fresh per-request evaluation state conveys no additional authority.
HTTP and CBOR have strict shapes, fixed limits, and a five-second whole-request
read deadline. Invalid framing is rejected before evaluation.

The graph is an adapter-binding view, **not a full system inventory, signed
provenance report, or security-posture verdict**. TLS, authenticated session
admission, full native TCP hardening, and owner-death/reply-slot cleanup remain
prerequisites for broader exposure (including SEC-016 in the hardening backlog).

## Checks

```sh
nix develop -c make -C kernel test-ccl-remote test-ccl-web-preview
nix develop -c make -C kernel prove-ccl-remote
# While ccl-remote-lab is running (pause browser updates during this test):
nix develop -c node tests/ccl-remote/smoke.mjs
```

The native smoke test covers actual bindings, CORS preflight, fragmented CBOR,
integer/string evaluation, diagnostics, clock IPC, bad-origin/malformed-CBOR
rejection, a stalled request deadline, and a successful request afterwards.
The focused proof target covers the pure boundary packages, not the complete
HTTP/TCP/IPC implementation or browser code.

See `userspace/ccl/remote/README.md` for the wire profile and
`docs/network-inbound-implementation.md` for native network limitations.
