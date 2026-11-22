# Triangle lifecycle timings

`native-triangle-probe.h` reports `MESA-TIMING cpu-us` for the first and every
32nd invocation, plus failures. Measurements use `CLOCK_MONOTONIC`, not GPU
timestamps. Clock failure or backward motion invalidates the sample explicitly.

Stages: resource setup, graphics pipeline creation, command setup/recording,
queue submission, completion wait, CPU readback/validation and optional consumer,
then resource destruction. Setup includes image/buffer allocation and shader
module creation. Existing diagnostic calls and scheduling delays fall within
these elapsed intervals. The timing report itself is outside the intervals.
Discovery, device creation and final service retirement are outside this probe.
Failure samples end at the failure point plus cleanup; zero unvisited phases
must not be interpreted as measurements of successful execution.

These are lifecycle latency samples, not a warmed-up renderer benchmark or FPS.
The 256-cycle fixture deliberately recreates resources/pipelines each iteration.
No sleep, submission, synchronization, or retirement semantics were changed.

Unit test in the pinned development shell:

```sh
nix develop -c bash -c 'cc -std=c11 -Wall -Wextra -Werror tests/mesa-anv/probe-timing-test.c -o /tmp/cubit-probe-timing-test && /tmp/cubit-probe-timing-test'
```

Native NUC latency remains unmeasured until a separately built image containing
this change is tested. The existing v47 image is unchanged.
