# GPIO driver

This is the INTC1057 Alder Lake-N GPIO driver source, supporting the ACPI I2C-HID
bring-up path. The production entry point is `main.adb`, with portable policy and
state owners in `lib/` and native adapters in `native/`. Shared client framing,
channel badges and lease identities live in `userspace/lib/gpio-admission`.

Startup validates the launcher identity, maps only launcher-admitted community
resources, parks observed host-owned GPIOs, freezes geometry, registers the GPIO
role and reports readiness once. The bounded event loop drains client/admin
requests, advances parking/admission/retirement and polls admitted pins. Each
iteration has explicit burst/work limits and finite deadlines. Parent interrupt
binding and enablement remain disabled pending generic ONESHOT/epoch-ACK support.

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

The executable is not yet registered with the main build. Draft CCL declarations
are in `tests/gpio-native/integration-draft`: activation requires coordinator-owned
`acpi-memory`/process request support, actual generated Startup, readiness-route
agreement, and the pending runtime Close(Carried) overload. The draft is not an
accepted manifest and must not be substituted for those integration steps.
