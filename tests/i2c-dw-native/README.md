# I2C native adapter and request regressions

Use the pinned Nix shell with TMPDIR=/home/doc/cubit-build-tmp:

    bash tests/i2c-dw-native/run.sh tests /absolute/private/output
    bash tests/i2c-dw-native/run.sh semantic /absolute/private/output

Hosted tests run seven groups in checked and release profiles: named register
window (110), native clock/startup composition (5204), serialized requests
(1109), exact-owner grant copying (255), generic startup/client loop (769), and
kernel-event classification (1082), local logger shutdown (12): 8541 checks per profile. Startup and loop
cases get fresh processes because mapping/initialization attempts are one-shot.
The event policy is bound into Main; the actual loop tests cover idle stop, active drain and faulted drain, including callback reentry.

Aligned host RAM models MMIO. Syscall/capability fixtures are explicit test
models; actual opaque Process_ID and generation-bearing grant identity units
are used where applicable. Native startup uses finite work budgets and samples
no GETTIME, including when that callback is unavailable. Runtime clock tests
retain unavailable/reversed-clock coverage. There is no configure handshake,
startup wall deadline or saved configure reply.

Coverage includes wrong/stale authority, rights, BAR, alignment and range;
exactly one mapping attempt, no cached admission borrowed by another generic
instance; one retained client reply, competing channel scopes, ten-bit rejection,
read/write/capacity boundaries; copied grant slices acquired with exact expected
owner, release uncertainty, and no replay after uncertain hardware or reply
failure. The actual generic loop and volatile native register adapter run over
modeled RAM. The event classifier accepts lifecycle/channel actions only for a
kernel publisher, preserving full-width publisher identity comparisons.

Semantic mode checks eleven selected generic controller/request/grant/loop/wire
units per profile with -gnatc and zero objects. It instantiates actual native
grant adapters and reply types in a fixture that refuses authority/delivery.
It does not compile or execute Main. Separate entry-point validation must use
generated Startup bindings and the current runtime; it must not substitute this
fixture for the program's policy.

sources.json pins production and fixture inputs. The runner copies them into
private outputs, uses j1/nice19, bounds each compiler command to180s and each test
to30s, records statuses and verifies inputs again on completion. Hosted GPRs are
private test artifacts, not production build rules. No new stateful proof,
native execution, electrical timing, MSI/EOI, PWR lifecycle acknowledgements, GPIO/HID or laptop
functionality is established by these tests. Kernel metadata authenticity,
grant mapping/lifetime/exclusivity and bounded non-raising adapters remain
explicit trust assumptions.
