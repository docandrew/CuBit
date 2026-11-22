# USB boot diagnostics

The N95 can reach Desktop with unusable input. Diagnostics must therefore be
visible without opening a menu, typing a command, or owning the display directly.

The private `boot-debug-ts3jrkxp` diagnostic image adds:

- xHCI startup capture: at most 128 ASCII lines of 96 characters, bounded memory;
  serial output remains available. This is a bring-up trace, not an audit log.
- After logstore registers, xHCI requests a publisher endpoint from devmgr.
  Devmgr accepts this request only from its boot-created xHCI PID and derives
  the collector from the service registry, never caller-supplied destination data.
  Diagnostic budget 15/issuance 1 is reserved in this private image.
- One asynchronous log publication at a time, paced at 100 ms. No synchronous
  collector wait in the input path. Failed publications count as loss, not retry
  storms. Driver-owned grant storage remains process-lived.
- A startup-launched `boot-logs.app` has Desktop and log-observer endpoints,
  no publisher, filesystem or network request. It shows retained records and
  automatically cycles 25-line pages every eight seconds.
- The diagnostic collector retains 128 records, rather than the normal 16.

The early capture is necessary because xHCI may provide the storage used to
load logstore itself. Sending requests to devmgr before its bootstrap handshake
finishes exposed a startup-message ordering bug during testing; the publisher
must not request its endpoint before logstore exists. This does not prove the
general bootstrap mailbox protocol sound and deserves separate hardening.

Limitations: this is isolated diagnostic-image integration, not a persistent
logging deployment. Lines longer than 96 characters are clipped; storage and
viewer history are bounded. A driver which fails before a usable live-system
storage path exists still needs the boot panel. Runtime disconnect recovery,
general reader filtering, and durable retention are separate work. The adapter
and UI are regression-tested, not newly SPARK-proved.

## Validation / diagnostic image

Native QEMU UEFI/4CPU/USB-flash tests, PS/2 disabled:

- `tqx_v7zu`: four-port hub, automatic viewer, retained xHCI startup records;
  capture summary reports zero overflow/publication losses. Screenshot inspected.
- `mlvn83o6`: eight-port QEMU descriptor-rejection fixture; USB input unavailable,
  but Desktop and viewer open and show the rejection via logstore.

Private artifact: `kernel/cubit_n95_usb_logs_v17.img` under the snapshot.
SHA256: `ee9d0df086805d2884108f94014d709cc4e766ad1c3cc93f65e96c12b9d60e30`.
The snapshot excludes the networking agent's later work. This is for physical
diagnosis, not evidence that the NUC's input issue is fixed.

## v18: reconnect timing experiment

Physical v17 logs discovered the VIA 2109:0812 SuperSpeed hub and USB storage,
but not the 2109:2812 USB2 companion carrying the keyboard and mouse. The
SuperSpeed hub returned unsupported-interface status 0F. This is not evidence
that the keyboard's own HID descriptor was rejected.

The private v18 driver waits a bounded three seconds after starting the
controller, rather than ending its discovery wait at the first connected port.
It then records every root port's PORTSC, including disconnected ports. This
adds boot delay intentionally to test a reconnect race; it is not a substitute
for runtime hotplug handling. Power/routing remains another possible cause.
The root-port summary now prints full decimal values (16 previously appeared
as 6 because the diagnostic used only the last digit).

QEMU delayed-attachment fixture `aqjj9gtg` attached a four-port hub 600 ms into
this window. With PS/2 disabled, downstream mouse buttons and keyboard
transitions reached Desktop. This validates the delayed-arrival test case,
not the timing or transaction-translator behavior of the physical VIA hub.

Final-image viewer fixture `j5cdipqc` also passed with late hub attachment:
89 retained records, no capture overflow or publication losses, screenshot
inspected. Artifact under the private snapshot:
`kernel/cubit_n95_usb_discovery_v18.img`.
SHA256: `d13e5357ceffabcaa5721a64d2c1821d016e00d3da2c9e9f08134189c5aa8a77`.

Physical outcome: the user confirmed both keyboard and mouse work through the
VIA hub with v18. Logs identify 2109:2812, downstream mouse 045e:0823 on port 1
and keyboard 24f0:0140 on port 4. This supports delayed reconnection as the
failure cause; it does not establish a universal three-second timing bound.

## Hardening after physical success

Private capture and viewer capacity are increased to 512. The collector remains
at 128 retained records; the viewer consumes the paced live publication rather
than requiring the entire history to fit in the collector. Capture overflow,
collector gaps, and viewer evictions are distinct counters. A full viewer keeps
the newest record (including the final capture summary), evicts its oldest,
and explicitly reports the eviction. A late subscriber can still miss history.

During the bounded startup window the controller's sole event-ring consumer
drains at most one ring's worth of events per iteration. This prevents startup
port notifications accumulating untouched while waiting. The final PORTSC scan
still determines enumeration; this is not runtime hotplug support.

### Removing the startup delay safely

Do not replace the window with an arbitrary short quiet period: a USB3 device
can be ready while its USB2 companion has not yet appeared. The next step is a
controller-owned port lifecycle that continues after startup:

1. Record port-change notifications through the existing single event owner;
   coalesce by port, then read fresh PORTSC rather than trusting a stale event.
2. Move enumeration out of Initialize's nested procedure into explicit state
   with bounded reset/address/configuration deadlines. Already-running HID and
   storage requests must continue while another device is being enumerated.
3. Track each slot's parent/path and lifetime. Disconnect cancels work, releases
   pressed keys/buttons, and disables the device before reclaiming DMA. Do not
   reuse a slot while old completions can be confused with its new lifetime.
4. Give hub status endpoints the same lifecycle, including removal of children.
   Do not claim root-port hotplug automatically handles downstream hub changes.
5. Separate boot-media readiness from input readiness. Storage discovery may
   have a bounded boot deadline; a late keyboard must not require rebooting.

Regression cases must include delayed root and downstream devices, removal
during enumeration/transfers, reconnect/slot reuse, and simultaneous storage
and input. Pure lifecycle decisions are candidates for SPARK contracts;
DMA ownership, MMIO ordering and physical hardware behavior remain explicit
adapter obligations. No new proof claim is made for the native changes here.

v19 validation: QEMU UEFI/4CPU/USB-flash, PS/2 disabled. Thirty-root-port
fixture `lesionkp` retained 133 records with no capture/publication/service/
viewer losses (screenshot inspected). Delayed-hub HID fixture `njj2oo95` passed
mouse-button and keyboard delivery. The greater-than-512 viewer eviction path
was not exercised by these native fixtures.

Private artifact: `kernel/cubit_n95_usb_hardened_v19.img` under the same snapshot.
SHA256: `e20bcf653aa0233ab3ff14050749c41f9ffaa6ffdb5db77b494cd4516b4513f6`.
v18 remains the physically confirmed baseline; v19 has only QEMU validation.
