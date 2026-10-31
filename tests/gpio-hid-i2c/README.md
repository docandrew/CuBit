# GPIO / HID / I2C owner interoperability

This hosted gate joins the actual GPIO owners and HID reset/device/input/report
pipeline to DW_I2C_Requests, DW_I2C_Owner, the transfer/register engine, the shared
request codec and I2C_Native_Results. I2C success/status/counts/tickets now come
from the real request/controller owners rather than canned HID completions.

```sh
export TMPDIR=/home/doc/cubit-build-tmp
nix develop --command bash tests/gpio-hid-i2c/run.sh
```

`--validate-only` checks160 shared production/hosted inputs and three local test/
fixture units without executing. `--source-root DIR` selects a checkout. Inputs
are hash-pinned; no private source fallback. The existing GPIO server fixture is
reused from tests/gpio-hid-input. The local I2C_HID_Server_Model composes real
server owners with explicit transport, grant-copy, register and device FIFO
models. Neither fixture belongs in a production component.

Checked (-gnata/-gnato) and release (-gnatp) profiles audit compiler ALI switches.
Limits remain nice19/j1,180sbuild,30sruntime per sequence,64MiBstack,core0,1MiBlogs.
Source pins, logs, terminal command statuses and completion summaries remain in
unique TMPDIR directories.

Three sequences pass14,289checks/profile: ordered6,511; early-event6,514;
input-abort1,264. There are eight build/runtime commands across both profiles.
Counts include repeated model assertions and retained predecessor calibration/
scheduler cases; they are not independent hardware cases or a coverage percentage.

The success paths cover reset, descriptor fetch, GPIO input events, actual I2C
command/FIFO transfer, grant retirement, report parsing, pointer publication,
exact-ticket ACK/rearm, early event buffering and final cleanup. The model checks
exact HID write bytes, repeated START, final STOP, two-byte receive FIFO credit,
actual transfer counts and positive controller-disable confirmation. Before each
read the destination page is filled with a canary; bytes must pass through the
real server output-copy path before HID consumes them. Every submitted I2C request
must have one real server completion. GPIO requests/replies/events are queued,
with no synchronous peer reentry.

The abort path raises a modeled hardware abort during the first input transfer.
The real I2C owner remains Faulted with Controller_May_Be_Enabled, and HID stops
reading, drains its grants/output and confirms a single GPIO lease release.
Further HID steps submit no new I/O. HID/GPIO drain is deliberately separate
from the I2C controller's retained fault; no hardware quiescence is invented.

Kernel sender/badge/reply authority, launcher admission, grant acquisition and
retirement, memory allocation, clock, MMIO semantics and the target device FIFO
remain models. Isolated predecessor calibration/client timing cases retain their
own fake callbacks outside the joined live path. This does not run native process
Main loops, kernel IPC, actual ACPI/AML discovery, parent IRQs or Lenovo hardware.
No production fix, new SPARK proof, ACPICA oracle or native touchpad acceptance is
claimed. The separate GPIO/HID and HID/I2C focused failure suites remain useful.
