The native entry is main.adb. Its component.ccl declares an Intel Alder Lake-N
I2C0/I2C1 PCI driver. Devmgr supplies immutable Generic_PCI_Configuration, the
named register authority, readiness endpoint and an optional one-vector MSI
controller authority. The native mapping adapter checks kind, rights, range
and the launch BAR before mapping one register page.

Startup runs finite reset/configuration/work budgets and leaves the controller
disabled and idle. MSI startup additionally reads back a zero source mask under
a finite clock deadline before reporting Ready. Missing MSI selects the named
1 ms polling fallback; inconsistent delivery/vector/authority bindings refuse
startup before mapping. Client service begins after the Ready acknowledgement.
Each instance owns one physical controller; optional role registration names
the primary instance.

Clients open the persistent com.cubit-os.i2c-transfer request channel. The
kernel-stamped admitted badge fixes the target/address mode and speed ceiling.
The finite absolute deadline, write/read counts and reserved fields are checked
in the channel header. Payload views come from the established channel mapping.
One active transfer borrows those views through the same controller owner until
independent quiet evidence permits returning them. No client field supplies a
physical address, grant identity, target or speed. Full Process_ID identities
remain opaque. Ownership uncertainty retains the mappings and publishes no
completion that would permit recycling them; there is no automatic replay.

Main serializes request admission, bounded channel turns and hardware progress.
It polls at most eight events per turn. A successful hardware No_Action sample
ends that turn. In MSI mode the owner derives TX-empty/RX-full/STOP/error sources
from the actual transfer planner, writes and reads back the mask, and Main waits
on the kernel event lane using the original finite request deadline. An armed
owner masks and verifies readback before the next FIFO work. Software progress
continues in bounded turns. The 1 ms sleep applies only to active polled mode;
idle service uses a finite health deadline. Failed masking, uncertain access,
clock/deadline failure or an unavailable kernel wait retains the physical owner
and borrowed mappings for supervision.

Kernel IRQ notices require canonical envelope fields and the admitted vector's
pending bit. Main also validates the common lifecycle-notice envelope before
forwarding control/grant events. Stop closes admission and drains the existing
request under its original work/deadline limits. Mapping retirement and a clean
stopped state require actual controller quiescence. A drain fault remains
retained. Process exit alone is not evidence of hardware quiescence. The power
service lifecycle acknowledgement remains future integration work.

Stage, failure and periodic health lines use the existing logger and serial
output. Startup identifies the hardware and msi-channel or polled-channel mode;
MSI readiness logs msi-masked-ready, and polling logs its interval. FIFO payload
bytes are never logged. Grant lifetime notices disconnect the existing opaque
logger publisher according to its established lifetime rules.

The source boundaries are transfer/ (planner/register protocol), setup/
(disabled configuration), lpss/ (Intel reset/timing profile), owner/ (controller
lifecycle and borrowed byte views), native/ (channel service, IRQ binding and
kernel/runtime adapters), and requests/ (existing serialized request helpers).
Shared wire/admission codecs used by HID clients live in userspace/lib/i2c-admission.

Run tests/i2c-interrupt-wait for the actual owner/controller/channel-turn,
planner-demand and binding regressions. tests/i2c-msi-entry executes the actual
Main with explicitly modeled controller/service/syscall boundaries; it covers
startup refusal, finite waits, polling fallback, IPC wake/reply, Stop, retention,
bounded event batches and malformed notices. Existing stack, native-helper and
GPIO/HID interoperation suites remain separate checks. These hosted results and
native semantic/object compilation do not establish native execution, electrical
timing, working MSI delivery or laptop touchpad behavior. New stateful and IRQ
changes are tested, without a renewed SPARK proof claim. The 133 MHz root clock,
electrical profile, native bring-up, timing instrumentation and formal milestone
proofs still need validation. Laptop touchpad metadata comes from firmware;
synthetic fixtures do not establish its address, GPIO pin/polarity or HID register.
