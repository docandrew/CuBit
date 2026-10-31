# HID reset / GPIO service interoperability

This hosted test joins the actual HID_GPIO_Reset bridge and GPIO_Client_Controls
to GPIO_Request_Dispatch, GPIO_Channel_Owner, GPIO_Pin_Leases and the
Intel_GPIO_Controller/IRQ owner. The server generates its real completion status,
lease and event ticket; the test does not synthesize successful server replies.

```sh
export TMPDIR=/home/doc/cubit-build-tmp
nix develop --command bash tests/gpio-hid-interop/run.sh
```

`--validate-only` checks the30production input hashes and test hash without
execution. `--source-root DIR` selects a checkout explicitly. Both checked
(-gnata/-gnato) and release (-gnatp) profiles run, with compiler ALI switch audits.
Each build is bounded to180seconds and each scenario to30seconds, nice19/j1,
64MiBstack,core0,1MiBlogs. Outputs and command statuses remain in unique TMPDIR
subdirectories. This gate adds no proofs and makes no new ACPICA claim.

Seven scenarios pass724checks/profile:

| Scenario | Expected joined behavior |
| --- | --- |
| happy | Confirmed arm, masked event, exact lease/ticket and confirmed release |
| backpressure | Same masked event retained until sink accepts; no repeated control/register work |
| late-arm | Client deadline retains drain obligation; late success returns actual lease for fresh cleanup |
| lost-arm | Armed server and uncertain client remain owned; independent supervisor stop confirms server drain |
| lost-release | Confirmed hardware masking does not establish client completion when reply delivery is lost |
| unknown-arm | A modeled unmask store with indeterminate result faults the real controller; client retains uncertainty; no replay |
| server-expired | Request arriving after its deadline refuses without arm register writes |

Submit, saved replies and events use explicit bounded queues. Server dispatch and
completion delivery run after client Submit returns; neither side synchronously
calls its peer. Opaque process IDs, tokens and leases exceed32bits. Clock samples
are cached and shared by client/server turns; selected-pin W1C, neighbor enable
preservation, event masking and at most one register access per controller step
are checked. Release uses the HID bridge's fresh cleanup window despite an
expired original HID deadline.

The fixture models launcher admission/configuration of one profile pin, kernel
sender/badge and saved-reply authority, transport delivery/failure, MMIO registers
and chipset W1C. These remain assumptions, not kernel/native/hardware validation.
It does not exercise dynamic admin registration, I2C transfers, the full HID
Device/input/report pipeline, parent IRQs or real touchpad reset. Those have
separate partial tests/integration work; passing this gate is not end-to-end
native touchpad acceptance or whole ACPI/AML proof.
