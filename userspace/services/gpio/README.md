# GPIO driver

This is the INTC1057 Alder Lake-N GPIO driver source, supporting the ACPI I2C-HID
bring-up path. The production entry point is `main.adb`, with portable policy and
state owners in `lib/` and native adapters in `native/`. Shared client framing,
channel badges and lease identities live in `userspace/lib/gpio-admission`.

Startup validates the launcher identity, maps only launcher-admitted community
resources, parks observed host-owned GPIOs, freezes geometry, registers the GPIO
role and reports readiness once. The bounded event loop drains client/admin
requests and advances parking/admission/retirement. After parking, it validates
the optional manifest-named parent interrupt against the launcher description
and the current readable single-line capability. A missing parent is a typed
degradation: event ARM/ACK requests report Unsupported_Channel, while cleanup
and the service remain available. Inconsistent descriptions/capabilities and
uncertain acknowledgements retain resources for supervision.

The event loop uses the kernel interrupt doorbell, with bounded progress wakes
for operations already in flight. There is no periodic pin polling fallback.
An authenticated parent notice holds client/admin request consumption in the
same turn; Stop and kernel lifetime/channel events can still progress. The
sweep completes previously accepted controls, reads admitted pin status and
confirms event masking/status clearing before acknowledging the parent line.
The client's exact-ticket ACK rearms its own pin later. A backpressured client
event therefore does not keep the shared parent line masked.

Coalesced notices keep the original finite sweep deadline. Unknown hardware
effects, failed ACKs and invalid/reversed/exhausted clocks do not become source
clearance and are not retried as successful operations. Startup makes one
quiet-source parent acknowledgement after confirmed community parking and a
sealed registry, so a pre-registration doorbell cannot strand the line.

The launcher supplies exact pin/mode facts through authenticated dynamic
registration. Capacity is a policy limit, not a fabricated startup pin list.
Clients arm/release a lease and acknowledge the exact event ticket. Retirement
retains state until pending events/controls drain, the sink is closed, and the
channel/lease is detached. Unknown register effects or cleanup outcomes retain
ownership without replay. Firmware-owned and absent community resources are
reported separately; physical HVCMOS pads outside the GPIO namespace are untouched.

Logging uses the existing logger: bounded stage lines and a final summary,
including observed counts and typed failures. Mapping is not an authority to
access arbitrary firmware addresses; kernel/launcher-issued device handles and
exact metadata determine admitted resources.

`tests/gpio-native` supplies hosted execution of the actual Main with modeled
kernel boundaries, dynamic registration/retirement, controller reuse, full GPIO
pin-domain parking checks, and scoped SPARK proofs. See its README for the exact
claims and trusted assumptions. These tests are not native or laptop validation.

The program is declared by component.ccl and manifest.ccl as gpio.drv, matched
by ACPI ID INTC1057. It requires four device-memory capabilities in admitted
_CRS order; the native startup checks the complete ACPI launch configuration
and all handle descriptions before mapping or probing any community. Timing,
work and capacity limits are named compiled policy in GPIO_Native_Policy;
devmgr supplies neither typed parameters nor a process-control capability.
The existing Startup.Ready endpoint identifies the launcher for admin IPC.

tests/gpio-native-entry regenerates real Startup from the driver catalog and
semantically checks every component unit against the native SDK in checked
and release profiles. It does not link, boot or validate hardware. Activation
requires the coordinator's ACPI admission/0253 deferred-launch support and
image integration. Only completed startup AML may release GPIO parking.
