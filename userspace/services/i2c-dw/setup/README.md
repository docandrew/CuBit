# Bounded DesignWare configuration owner

This is a real register sequence driven one primitive access per event-loop
step, not a native driver entry point. It is intended to follow independently
validated LPSS reset/clock setup and the existing component/FIFO probe. The
trusted owner supplies the target, mode, clock/electrical timing-derived counts,
FIFO depths, optional version-supported SDA hold field, deadline and step quota.
No guesses or defaults substitute for those launch/device facts.

The sequence masks interrupts, disables the controller and observes disable and
idle status before changing configuration. A nonempty TX FIFO is refused. Stale
RX bytes are drained one access per step, bounded by detected RX depth. An
impossible RX level or continued refill at the drain bound is refused. It writes
the selected standard/fast timing counters, optional hold field, master/restart/
slave-disable control, target and FIFO thresholds. It copies TX_ABRT_SOURCE
before the single destructive interrupt-clear read. It then enables the
controller and observes enabled status. Interrupts remain masked: the later
transfer/interrupt owner must install and arm the actual interrupt path before
submitting data. Ready means this sequence completed, not that native input,
interrupt delivery, LPSS, address authorization or physical bus health is proved.

All writes and destructive reads either advance or enter terminal failure. Only
bounded status polls revisit an access. The limited owner cannot be restarted by
Start after success or failure. Clock availability/reversal and one unchanged
absolute deadline are checked before and after each access. A separate explicit
step budget guarantees termination of polling even if the clock never advances.
Successful writes and drains are counted before a failed post-access clock check,
and possible enable effects remain visible for cleanup. Unknown accesses remain
terminal; there is no automatic replay, rollback, reset or recovery attempt.

This is fresh initialization after a trusted LPSS reset, not active/held-bus
recovery. Linux's disable path may need an abort handshake for an active master;
the reset prerequisite avoids claiming that a blind disable is adequate there.
The actual LPSS implementation, reset/readback validation and held/stuck bus
recovery remain required work. The native adapter must preserve exclusivity,
access semantics, ordering, bounds, endian interpretation, callback termination
and non-reentry and must not let exceptions escape. A denied/unavailable access
must have no effect; an access whose effect is uncertain must report that status.
The reset prerequisite and all configuration values are internal trusted owner
inputs, not client-provided hardware authority or arbitrary physical addresses.

Reference semantics were checked against pinned Linux v6.18
i2c-designware-master.c (master/FIFO initialization and transfer target setup),
i2c-designware-common.c (disable-status handshake), and i2c-designware-core.h
(register offsets, control/status bits). Original pinned files are retained in
the preceding engine/register workspaces. The implementation is independently
written Ada/SPARK-mode code. No new proof or native/hardware result is claimed
for this configuration unit. Hosted evidence and its scope are recorded below.


Hosted evidence for this source: 4,931 checks/profile (checked and release).
The required controller-version input rejects SDA-hold programming below
Minimum_Hold_Version before any access. The checked version boundary is the
DesignWare v1.11* signature from the upstream register definitions. An explicit
Program_Hold=False requests no hold write; it is not an automatic fallback.
There is no new proof milestone for configuration, nor native/hardware result.
See tests/i2c-dw-configuration for the portable guarded regression runner.
