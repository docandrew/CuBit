# HID over I2C driver

This driver owns one launcher-admitted I2C channel and one GPIO interrupt-input
channel for a PNP0C50 device. Firmware discovery remains with the ACPI service;
controller access remains with the I2C and GPIO drivers. It has no raw physical
memory or register mapping interface.

`main.adb` consumes generated `Startup` and `Parameters` bindings from
`manifest.ccl`. Descriptor register, gain/direction, button/confidence policy
and startup/control/read/cleanup/backoff durations are required launch values.
Logical coordinate bounds come from the compiled report's finger slots, so
the launcher does not supply guessed X/Y limits. Inconsistent or empty bounds
are refused before pointer readiness. Gain must still be admissible for those
bounds; observed values and scale refusal are included in bring-up logs.
Compiled capacities are implementation ceilings, not observed hardware facts.

The loop uses asynchronous controller requests, authenticated event/completion
routing, bounded queue turns, one cached clock sample and bounded pointer
publication. The overall startup cutoff is checked before queue dispatch, so
completion cannot start new input work when the sampled time has expired.
Startup readiness is
reported once. A timeout does not establish
cancellation or grant retirement; shutdown retains uncertain ownership and
requires device/GPIO quiescence, buffer closure, drained pointer recovery and
logger detachment before exit. Backpressure retains the final release report;
an absent consumer is checked again using the launch backoff during shutdown.
The current GPIO transport is polled; parent interrupts stay disabled.

The existing logstore API emits typed bring-up stages and a summary, mirrored
to serial. Grant lifetime notices from unrelated I2C peers do not disable
logging. Matching logstore-peer notices conservatively disconnect it.

Implementation lives in `lib/` and `native/`; shared input/wire units remain in
their existing locations named by `component.ccl`. Generated startup/parameter
files and hosted mocks are not production sources.

Current evidence is hosted policy/pipeline testing, scoped SPARK contracts for
the one-shot startup-readiness policy and native API semantic checking. Native
delivery and caller-state ownership are outside that policy proof. Native
linking, execution, laptop behavior and whole-driver SPARK
proof remain unfinished. Launcher/catalog integration and image registration
are pending. No AML coverage or full-interpreter proof follows from this
HID driver checkpoint.
