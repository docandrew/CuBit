# Composed I2C controller owner

DW_I2C_Owner composes LPSS reset, timing, DesignWare probe/configuration,
transfer planner and register port for one exclusive controller. It is an
internal polling owner; native Main, service registration, interrupt/EOI loop,
CCL component, logging and hardware acceptance remain outstanding.

Startup contains controller PCI identity, BAR span, an absolute deadline and
three explicit work quotas. Start validates the controller profile, resets,
probes, masks interrupts and verifies disabled/idle/empty FIFOs, preserving
abort diagnostics before clearing status. It enters Available without selecting
a target or programming timing. No dummy target or default bus speed is used.

Submit takes the admitted target and speed ceiling for this request, copies
write bytes into owned bounded storage and retains one monotonic request ticket.
The authenticated request layer derives target/speed from a kernel-stamped
canonical channel badge. The generic owner itself is not an IPC authenticator.
A busy or uncollected request cannot be replaced by a different channel.

Advance derives/programs the scoped timing and target only after disabled/idle
verification, enables, performs bounded FIFO work, then verifies STOP/all bytes,
disable-status/idle/TX/RX, copied abort and status cleanup before retiring both
configuration and planner state. Only successful cleanup permits another target.
Every step uses the explicit absolute deadline and independent step budget.
Callbacks never wait on a peer or retry an uncertain access. Clock units must
be GETTIME scheduler milliseconds throughout the native composition.

A short collection retains the exact result without hardware replay. Collection
zeroes unused output/owned buffers and consumes completion once. A failed or
unknown access leaves the entire controller Faulted even after result collection;
no automatic reset, retarget, retry or rollback is attempted. Possible effects,
last phase/register/value/status and conservative enabled state remain explicit.
DW_I2C_Configuration.Retire is pure state retirement and is not evidence about
hardware. This owner calls it only after the external quiescence checks above.

Hosted owner tests: 4022 checks per checked/release profile, including alternating
targets and speeds, copied input, RESTART/STOP, retained completion, contention,
quota/clock failures and every exercised configuration/cleanup access failing
with Denied, Unknown_Effect or post-access clock loss. Tests use actual production
bodies over modeled registers. Addresses 0x15/0x2A/0x33 are fixtures, not laptop
facts. Whole-stack tests and actual native binding semantic checks are separate.

The stateful owner/configuration/request composition is tested, not newly proved.
Existing scoped planner/timing proofs do not prove these new transitions or
callbacks. Trusted obligations include kernel-stamped admission, BAR authority,
exclusive ordered register access, callback termination/non-reentry/no exceptions,
precise access/effect reporting and the actual hardware clock/electrical profile.
No native execution, real interrupt routing or hardware validation is claimed.

Startup diagnostics now retain cumulative effects and the last available raw
adapter value across reset/probe/mask/quiescence. Earlier successful or unknown
writes survive later clock/deadline/quota failure. A probe wrapper records each
actual read before inner clock checks can replace local diagnostics. Denied
accesses do not replace the last retained value. Startup_Value_Available is false
when no adapter value was obtained; native logging must not label an absent zero
as a sampled hardware value. The wire carries zero when there is no value.

Post-work clock failure is attributed to the phase that actually performed the
step, including the final status-clear step that had moved to Available. The
controller remains Faulted. This fixes a misleading failure-stage transition;
it does not perform rollback or retry. Owner8053checks/profile now includes all
exercised startup IO faults, all startup clock sample failures/deadlines and every
owner quota boundary, actual saved reply delivery/refusal/reentry, last-value
retention and client-admission gating. No new stateful proof or hardware claim.
