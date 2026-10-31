# GPIO events through the HID input pipeline

This hosted integration joins the real GPIO dispatcher/channel/lease/controller
and IRQ owner with HID reset, HID_I2C_Device, GPIO input control, report parsing,
pointer publication and the HID native envelope adapter. GPIO replies, leases
and event tickets come from the server owners, rather than a fake successful
completion. The kernel, MMIO and I2C bus/grants remain models.

```sh
export TMPDIR=/home/doc/cubit-build-tmp
nix develop --command bash tests/gpio-hid-input/run.sh
```

`--validate-only` checks158 shared input files and three local fixture/test files
without executing. `--source-root DIR` selects a checkout explicitly. Shared
production and existing HID hosted fixtures are pinned in sources.json; local
GPIO_HID_Server_Model composes the production server with bounded nonreentrant
transport queues and a one-pin register/W1C model. It is not production code.
No private workspace source fallback is used.

Both profiles audit ALI switches: checked -gnata/-gnato and release -gnatp.
Limits: nice19/j1,180sbuild,30sruntime per scenario,64MiBstack,core0,1MiBlogs.
Outputs, source pins, command results and explicit completion summaries are
retained in unique TMPDIR subdirectories.

Two scenarios pass3,171 explicit checks/profile (1,584 ordered and1,587 early
event), six build/runtime commands total. Each main joined lifecycle performs
two real GPIO arms, two releases and six exact-ticket ACK/rearms. It checks:

- Reset interrupt and initial report discovery through the actual HID owners.
- GPIO masks before event delivery; input reads and modeled grant retirement
  finish before an ACK request, which rearms the actual controller.
- Ordered delivery and a new real GPIO event delivered before the preceding ACK
  reply reaches the HID client. The next report read starts exactly once from
  the retained early event. The controller ticket continues across reset/input.
- Relative pointer reports, coalescing/backpressure, consumer changes and reset
  recovery through the existing publication pipeline.
- Device-reset failure drains its GPIO lease; confirmed parked/masked server
  state and HID/device/output obligations are checked separately.

Counts also include retained predecessor calibration/scheduler/client-timing
checks; they are not independent hardware cases or a coverage percentage. The
predecessor's synthetic startup-cutoff cases are not part of this joined gate;
they remain in tests/hid-i2c-native. Its isolated alternate-device/client tests
retain their explicit fake callbacks, outside the joined live lifecycle.

I2C transfer results, grant creation/retirement, allocation, kernel transport
metadata and launcher admission remain modeled. This does not execute the I2C
server, actual native IPC/MMIO, AML/ACPI discovery, parent IRQs or a real laptop.
No new SPARK proof, ACPICA oracle or end-to-end native touchpad acceptance is
claimed. No production change was needed for these passing sequences.
