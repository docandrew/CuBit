# Graphics baseline and event-driven GPU wakeup — 2026-09-21

Native CuBit, hosted by QEMU 11.1/KVM, one vCPU, unpinned, on an AMD Ryzen
7 5800X Linux workstation. Display 1024x768. Nix optimized service builds.
One run per configuration: exploratory results, not a statistical guarantee.
Commands and measurement boundaries are in [README](README.md#graphics-copy-baseline).
Raw serial logs and validated JSON are in the ignored local directory
`results/graphics-baseline/`. No Linux graphics workload was benchmarked.

## Change isolated

Replaced virtio-gpu's idle `SLEEP(10)` polling with the existing atomic
`Wait_For_Activity_Until` primitive. Queued service requests and latched IRQs
wake it; a one-second deadline permits diagnostic reporting. Queue contents
are still drained through their typed receive paths. No new authority, mapping,
grant forwarding or GPU command-completion mechanism was introduced.

Both before and after have identical new copy counters. This comparison does
not measure the overhead of instrumentation versus an uninstrumented build.
The underlying GPU command submission still polls the used ring, occasionally
sleeping; changing that safely is separate work.

## Repainting input measurements

2,048 normalized input publications, each preceded by a 320x200 damage request.
Times measure publication to focused-app receipt, **not frame completion or
photons**. Percentiles are histogram upper bounds; maxima are observed samples.

| Backend / change | CPU peer | p50 upper bound | p99 upper bound | Maximum | Samples >= 1 ms |
|---|---|---:|---:|---:|---:|
| Boot framebuffer baseline | None | 0.966 ms | 0.966 ms | 4.884 ms | 8/2048 |
| Virtio polling baseline | None | 0.968 ms | 1.659 ms | 6.572 ms | 344/2048 |
| Virtio activity wait | None | 1.104 ms | 1.381 ms | 4.938 ms | 550/2048 |
| Virtio polling baseline | Busy peer | 0.966 ms | 1.104 ms | 6.422 ms | 204/2048 |
| Virtio activity wait | Busy peer | 1.382 ms | 1.382 ms | 6.576 ms | 2029/2048 |

All five runs passed input integrity, independent PIT calibration comparison,
and graphics-record validation. Both loaded runs passed full load-coverage
validation. The activity-wait result **does not meet the 1 ms input target**;
median input latency and loaded misses regressed, despite the better idle p99.
Do not interpret the unexpectedly better loaded baseline p99 as a load benefit:
these are individual runs with coalescing and host-scheduling variation.

## Presentation progress and copies

In steady repaint diagnostic intervals, the polling driver processed about
93 presents per approximately one-second reporting interval, versus 976–978
after the change. With the busy peer: 91–93 versus 803–812. These are display
service operations, **not monitor FPS**, and are not synchronized frame-rate
measurements. The old path coalesced many more requests while asleep.

Observed cumulative idle virtio upload-request counts increased from 181 to
2,043; staging regions increased from 177 to 2,039. More updates reaching the
backend means more copy work. The counters separately confirm staging, backend
and previous-buffer repair copies remain. Their cumulative snapshots include
startup and miss different tails, so bytes cannot be compared as equal-work
bandwidth figures. Legacy GPU-copy counters stayed zero in these fixtures.

This removes a concrete service-wakeup throttle, not all graphics bottlenecks.
The input benchmark can no longer get relatively cheap replies while most
presentation requests are coalesced behind a sleeping driver. This is why
tracking only input p99, or only copied bytes, would be misleading.

## Verification and next steps

The counter increment/overflow contract was SPARK-proved. Hosted counter tests
and all 22 Python performance-report tests passed. This does not prove the
driver, placement of instrumentation, or timing bounds. The wakeup change uses
the existing kernel queue-enrollment mechanism; its native use is regression
tested, not a new SPARK proof of the whole GPU service.

The native `desktop-doom` KVM regression also passed: the harness verified
actual game pixels and a responsive Apps menu after the change. That check
does not certify audio quality or tear-free physical scanout.

Next: integrate the already-modelled bounded grant-forwarding lifetimes into
kernel mappings/teardown, then use safely shared presentation buffers to remove
staging/backend copies. This has **not** been integrated by this change.
Add phase-aligned frame-sequence/fence measurements, CPU residency, and an
independently paced workload before claiming sustained rendering throughput.
Repeat measurements after each change, preserving both delivered-update counts
and input latency. A Linux comparison needs matched completion semantics,
hardware, resolution/refresh, damage workload and buffering; beating Linux is
the target, not a result established here.

## Parent forwarding-hold hardening follow-up

The next kernel change separates a kernel forwarding hold from ordinary user
acquisitions and preserves that hold across receiver teardown. This does not
enable forwarding or remove any pixel copies. Details and proof boundaries:
[grant lifetimes](../grant-loans/README.md#parent-hold-implementation-2026-09-21).

The final compact-layout kernel was measured with the same one-vCPU KVM virtio
fixture, one busy peer, and 75-second timeout. All 6,144 input transitions passed
integrity; load coverage, reference clock and graphics records validated. In the
repainting phase, p50 was <=1.382 ms, p99 <=2.212 ms, maximum 7.758 ms, with
1,788/2,048 samples >=1 ms. This is still above target and the p99 bound is worse
than the preceding 1.382 ms loaded wakeup run. These individual runs do not
establish the cause of that difference or an improvement. No graphics speedup
is attributed to this lifetime work. Raw logs/JSON are kept separately under
the ignored `results/forwarding-holds/` directory; the earlier intermediate
layout run is labelled separately from the final-layout measurement.

## Native per-output broker/driver follow-up

The broker now banks leases, source grants, sessions and damage per output;
virtio supports two 1024x768 double-buffered heads. Native QMP pixel tests pass
on one and four vCPUs, including releasing output zero while output one keeps
updating. This is not yet two-monitor Desktop composition or asynchronous
backend stall isolation. Copies and synchronous GPU command waits remain.

After that change, the same **single-output** loaded input fixture passed all
6,144 events, load coverage, reference-clock and graphics-record validation.
Repainting: p50 <=1.105 ms, p99 <=1.381 ms, maximum 6.686 ms; 2,015/2,048 samples
were >=1 ms. Steady reporting intervals contained 799 and 817 backend presents
(not physical FPS). This one run is near the earlier activity-wait result, but
does not establish either a speedup or a statistical non-regression guarantee.
It still misses the 1 ms target and does not measure keypress-to-photon latency.
Logs/JSON are in ignored `results/multi-output/`.

## Native Desktop output splitting

Desktop now splits scene damage into separate output-local transfer buffers and
tracks independent in-flight sessions. The native two-output pixel/input fixture
passes on one and four vCPUs (spanning drag, per-monitor maximize, primary-only
taskbar, exact secondary wallpaper/cursor cleanup). The existing single-output
Desktop protocol regression also passes.

The same **single-output** one-vCPU KVM fixture, one busy peer and 75-second
timeout delivered all 6,144 transitions. Input integrity, load coverage,
reference clock and graphics records validate. Repainting p50 <=1.104 ms,
p99 <=1.381 ms (rounded up), maximum 6.526 ms; 2,036/2,048 samples were >=1 ms.
This is the same p99 histogram bucket as the preceding per-output broker run,
not evidence of a speedup or statistical non-regression. It still misses the
1 ms target. It measures publication-to-application handling, not photons or
two-output throughput. GPU waits and staging/backend/repair copies remain.
Raw evidence is in ignored `results/desktop-output-split/`.

## Nonblocking broker/GPU session presentation

The broker no longer waits synchronously for session-frame GPU replies. The
driver progresses per-head transfer/set-scanout/flush chains from fenced used-ring
completions and IRQ activity, rather than spinning for each runtime command.
One frame per output is bounded; uncertain work is quarantined, not recycled.
The native delayed-head fixture demonstrates head 1 and broker query progress
while head 0 is held. This is a responsiveness/lifetime architecture change,
**not a demonstrated rendering-speed improvement**.

The matching single-output, one-vCPU KVM loaded run passed all 6,144 input
transitions, full busy-peer coverage, independent reference-clock calibration
and graphics-record validation. Repainting p50 <=1.105 ms, p99 <=1.933 ms,
maximum 6.825 ms; 1,496/2,048 samples were >=1 ms. The prior output-split run
had p99 <=1.381 ms and 2,036 misses. Fewer threshold misses but a worse tail is
not an overall latency win; the 1 ms target remains unmet.

Steady diagnostic intervals reported 382/386 broker submissions versus the
previous run's roughly 800 operations. These are not monitor FPS or matched-work
throughput measurements. Async pending frames coalesce subsequent damage, and
the new command chain introduces scheduler wakeups where synchronous IPC could
directly hand execution to its peer. That is a performance follow-up hypothesis,
not an isolated causal measurement. No scheduler policy was changed here.
Fenced device completion is retained; do not remove lifetime evidence merely to
make a latency number smaller.

The broker's `present_ms` now measures CPU staging/submission for session
frames, excluding deferred GPU waiting; do not compare that counter as if its
old end-to-end scope were unchanged. Copy stages remain and no zero-copy claim
is made. Raw serial and validated JSON: ignored `results/nonblocking-gpu/`.

## Bounded pre-paint input dispatch — 2026-09-22

The compositor now drains input again after handling service requests and before
painting. A synchronous service reply can run a client which publishes input;
previously that input waited behind painting until the next loop. Both drains
share one 64-event budget per pass, preserving bounded opportunities for service
requests and presentation. No scheduler, buffer-lifetime or GPU changes were
made for this comparison.

Controlled A/B/B/A runs used the same single-output, one-vCPU KVM loaded fixture
above. A is the preserved original Desktop binary; B is the new binary. All four
runs passed all 6,144 input deliveries, reference-clock, full busy-peer coverage
and graphics-record validation. Each repainting phase contains 2,048 samples.

| Run | p50 upper bound | p99 upper bound | Maximum | Samples >= 1 ms |
|---|---:|---:|---:|---:|
| A1 original | 1.105 ms | 1.381 ms | 6.184 ms | 1072 |
| B1 pre-paint drain | 0.139 ms | 0.277 ms | 1.198 ms | 3 |
| B2 pre-paint drain | 0.139 ms | 0.277 ms | 1.056 ms | 2 |
| A2 original restored | 1.105 ms | 1.381 ms | 6.130 ms | 930 |

The p99 bucket is five times lower in both B runs, and both meet the observed
1 ms input target. Maxima still exceed 1 ms. This is publication-to-focused-app
receipt in a closed-loop diagnostic, **not keypress-to-photon**, a hard timing
guarantee, or a comparison with Linux. Two runs per variant are encouraging
repeatability evidence, not a broad statistical characterization.

Rendering was not simply suppressed to improve input: B1 recorded 861 and 852
logical frame submissions in steady diagnostic intervals, with 432 and 427
backend submissions. Coalescing remains; these are not monitor FPS. Both A and
B continued making busy-peer progress. Copy counters still show staging,
backend and repair copies; this change removes input queuing behind those
operations, not the operations themselves. It is portable compositor ordering,
not a virtio-specific trick.

Raw logs and validated reports: ignored `results/prepaint-dispatch/`. Commands:
`tests/headless/run.sh --test bench-input --input-backend virtio --load
--accel kvm --cpus 1 --timeout 75 --serial <log> --keep-logs`, in Nix;
`tests/performance/report.py <log> --require-load --require-input-integrity
--require-reference-clock --require-graphics` (also `--require-input-target`
for B). The production staged binary was restored to B after A2.

### Combined shared-library / mixed-resolution follow-up

After extracting the shared display models, adding pointer confinement, native
per-head dimensions and the Settings display view, the same loaded single-output
fixture still passed all 6,144 events and all validation gates, including
`--require-input-target`. Repainting p50 <=0.139 ms, p99 <=0.277 ms, observed
maximum 3.926 ms; 2/2,048 samples were >=1 ms. This is another observed p99 pass,
**not elimination of long-tail stalls**. The closed-loop and non-photon limits
above still apply. Raw evidence: ignored `results/graphics-final/`.

Separate functional regressions passed for native mixed-resolution scanouts,
GTK's undersized-EDID fallback, the read-only Settings Displays page,
single-vCPU dual outputs, firmware framebuffer presentation, oversized-mode
discovery, DOOM/multi-app behavior, and explicitly built delayed-head progress.
These functional tests do not establish mixed-output rendering throughput.
The final `run-delayed-display.sh` regression also passed: input progressed
during the delayed read while the transfer-buffer fingerprint remained stable.
Production Desktop/display/GPU artifacts were restored and checked against their
staged counterparts afterward.
