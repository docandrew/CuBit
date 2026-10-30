# DesignWare I2C controller foundation

These units form the hosted controller foundation for i2c-dw.drv. There is no
native main, component registration, LPSS initialization or live I2C access yet.

DW_I2C_Transfer plans one bounded fixed-length write/read or combined transfer.
Submit receives explicit byte quota, FIFO depths and an absolute deadline.
Poll emits one ticketed FIFO action, and Finish_Access accounts its known or
uncertain result once. RESTART is set on the first read after a write; STOP on
the final command. Outstanding read credits never exceed the supplied RX depth.
Completion requires STOP and complete receive draining. Abort/deadline/clock,
invalid observations and uncertain accesses are terminal. The internal owner
must quiesce/disable/drain hardware before retirement. Ticket generations survive
retirement and refuse wrap. Payload buffers belong to the exclusive request
owner, and action indices select its retained spans.

DW_I2C_Registers.Port binds static clock/read/write adapters to one authorized
controller BAR. Named register selectors map internally to offsets; clients do
not supply physical addresses. Probe checks the component signature and decodes
the two 8-bit FIFO fields plus one. Capture copies raw IRQ/abort/FIFO evidence
without reading destructive CLR_* registers or DATA_CMD. FIFO over/underflow
and out-of-range occupancy are refused before narrowing. An abort source is
copied before any clearing. Execute permits one FIFO access for each increasing
ticket in a limited Access_Owner, including consuming tickets refused before IO.
If a successful access is followed by a failed clock check, its physical result
remains Succeeded for accounting; the caller must fail/quiesce the request and
must not retry that action. Diagnostic clock errors never roll back IO.

Trusted native obligations: exclusive matching planner/Access_Owner lifetimes,
authentic BAR/IRQ grants, correct endian interpretation and detected FIFO depth,
fresh samples, stale IRQ clearing before Submit, bounded/non-reentrant callbacks
that do not raise, precise one-use access, disjoint retained payload buffers,
barriers, LPSS clock/reset and bus timing, and bounded interrupt/service waits.
Quiescence is asserted only by this internal owner, never by a wire client.
No guessed touchpad address, speed, GPIO or descriptor register is embedded.

Tests: tests/i2c-dw-transfer runs 61,056 planner and 196,742 register/combined
checks per profile (checked and release). All 65,536 FIFO encodings are covered;
fault injection covers deadlines, clock loss/reversal, refusal/uncertainty,
FIFO faults, STOP draining, abort diagnostics and duplicate action suppression.
The planner alone has an accepted level-2 state/bounds milestone: 36 checks
(8 runtime, 15 functional, 5 initialization, 8 termination), zero unproved or
justified. The register adapter is regression-tested, not newly proved.
Neither result proves full I2C protocol/progress, IPC, native or hardware behavior.

Primary register references: Linux v6.18 drivers/i2c/busses/
[i2c-designware-master.c](https://github.com/torvalds/linux/blob/v6.18/drivers/i2c/busses/i2c-designware-master.c),
[i2c-designware-common.c](https://github.com/torvalds/linux/blob/v6.18/drivers/i2c/busses/i2c-designware-common.c),
and i2c-designware-core.h. The Ada implementation uses the documented register
semantics and is not a translation of the Linux implementation.

Remaining: LPSS clock/reset and controller setup, explicit timing from admitted
resource facts, generated Startup bindings, async submit/completion service owner,
GPIO interrupt integration, then the existing HID descriptor/reset/input path.
The coordinator owns native builds/images/headless and shared launcher changes.
