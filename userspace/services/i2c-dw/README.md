# DesignWare I2C service

`main.adb` is the native entry point. `component.ccl` declares a driver; devmgr
matches the Intel Alder Lake-N I2C0/I2C1 PCI IDs and supplies an immutable
`Generic_PCI_Configuration`, the named register grant and readiness endpoint.
Only one register page is mapped after the native adapter checks the grant's
kind, rights, range and correspondence with the launch BAR. This driver does
not receive PCI configuration-port authority.

Startup uses the owner's finite reset/configuration/work budgets. It leaves the
controller disabled and idle, without selecting a peripheral address or speed.
The loop reports Ready or Not_Present through the existing generic devmgr
protocol; it serves clients only after a Ready acknowledgement. Optional role
registration is for the primary instance. Other controller instances use
endpoints minted by the launcher. There is no configure RPC or separate I2C
launch kind.

Incoming requests use `Poll_Service_Request`; a successful `Save_Reply` retains
the exact request's reply authority while the controller progresses. Sender
identity stays an opaque `Process_ID`. The kernel-stamped channel badge supplies
the target/address mode and speed ceiling. No payload field can replace them.
The four-word transfer codec validates operation, protocol version, canonical
generation-bearing input/output grant references, 16-bit counts, combined
capacity and an explicit absolute-millisecond or Forever deadline. Each copy
acquires the requested slice with that exact sender as expected owner and
returns the acquisition before continuing.

The native loop serializes admission, disabled configuration, transfer and
quiescence. Main advances a bounded work burst between request polls and yields
between active bursts. `i2c_work_policy` scales the finite work allowance with
the selected timing counters. This is a resource policy, not a guarantee of
success under unlimited clock stretching; caller deadlines remain independent.
Completion227 carries a status, full-width ticket, hardware-success and
effects-possible flags, requested-write count and delivered-read count. A write
count is not evidence of bytes acknowledged on the bus. Uncertain accesses or
grant retirement stop the loop rather than replaying work.

Stage, failure and periodic health lines use the existing logger and serial
output. Safe register evidence is logged once on startup failure when a sample
exists. FIFO payload bytes are never logged. Main polls at most eight events
per turn alongside service requests and bounded hardware work. It routes kernel
channel wakes through the runtime registry. A grant lifetime notice disconnects
the opaque logger publisher before further writes; serial diagnostics remain.
The shared logger is unchanged.

A kernel Stop notice closes admission immediately. The existing request drains
under its original work allowance/deadline and keeps its original saved reply.
An idle controller can stop immediately without MMIO. A successful stopped state
requires independent disabled/idle evidence; a fault during drain remains a
fault with possible hardware effects. Main does not replay or reset uncertain
hardware to manufacture a clean stop. The power-service lifecycle acknowledgement
protocol and interrupts/MSI/EOI remain future integration work. Process exit by
itself must not be treated as proof that hardware is quiescent.

The ownership boundaries are `transfer/` (planner/register protocol), `setup/`
(disabled configuration), `lpss/` (Intel reset/timing profile), `owner/`
(controller lifecycle), `requests/` (serialized client/grant ownership), and
`native/` (kernel/runtime adapters and loop). Shared pure wire codecs are in
`userspace/lib/i2c-admission` because HID and other clients need them.

Validation scope: hosted planner/owner/loop and wire tests, plus semantic-only
compilation against the current runtime. These do not establish native boot,
electrical timing or laptop functionality. The 133MHz root-clock and electrical
timing profile still require hardware validation. Stateful driver proofs, native
execution, catalog publication and
HID/GPIO integration remain incomplete. No touchpad address, GPIO pin, polarity
or HID descriptor register is guessed from a synthetic fixture.
