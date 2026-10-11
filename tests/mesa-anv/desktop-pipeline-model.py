#!/usr/bin/env python3
"""GPU-001 step 3 follow-up: why the NUC's Desktop got jerkier, in numbers.

A discrete-event model of Desktop's frame pipeline over the intel-gpu
session queue, with the timing rules taken from the code:

- Desktop runs a frame's GPU stages strictly one after another (the NUC's
  `queue-peak=1`; Complete_Output returns Pending until the previous stage's
  fence completes, and the next stage is submitted on a later loop turn).
- The driver: idle, a kick wakes it at once; busy, it takes descriptors and
  observes completions only on its turns, every POLL microseconds
  (Submission_Poll_Us), and publishes the status line on that turn.
- Desktop learns a stage completed:
    sync   (step 2): the 0x0A27 reply, the moment the driver observes it;
    timer  (step 3 as landed): its 1 ms fallback, on the millisecond clock
           (Wait_For_Activity_Until (now + 1) ends at the next tick);
    event  (the fix): the driver's OP_GPU_WAKE answer, the moment it observes.
- Each loop turn and IPC costs TURN_US.

It is a model, not a measurement: GPU times per stage are inputs. It shows
how the per-stage cost scales with the number of stages, which grows with
the number of stale client surfaces (the Logs window).
"""
import math

TURN_US = 30          # a Desktop loop turn / an IPC hand-off


def frame_us(stages, gpu_us, poll_us, mode):
    """Time from the first stage's submit to Desktop seeing the last done."""
    t = 0.0
    for gpu in gpu_us[:stages]:
        # Submit: the driver was idle (previous stage done), a kick wakes it.
        start = t + TURN_US
        done = start + gpu
        # The driver observes on its next busy turn after `done`.
        turns = max(1, math.ceil(gpu / poll_us))
        observed = start + turns * poll_us
        if mode in ('sync', 'event'):
            seen = observed + TURN_US
        else:  # timer: Desktop wakes at the next millisecond tick after
            # it went to sleep, then every millisecond, until observed.
            seen = math.floor(start / 1000.0 + 1) * 1000.0
            while seen < observed:
                seen += 1000.0
            seen += TURN_US
        t = seen
    return t


def main():
    small = 60.0          # an upload or readback of a damaged band (us)
    render = 1500.0       # the frame's render pass (us)
    print('stages  per-frame time (ms)                    frames/s')
    print('        step2-sync  step3-timer  fix(event,250us)  step2  step3  fix')
    for stale in (0, 2, 4, 8, 12, 16):
        # Per stale surface an upload; then the render and the readback.
        gpu = [small] * stale + [render, small]
        n = len(gpu)
        sync = frame_us(n, gpu, 1000, 'sync')
        timer = frame_us(n, gpu, 1000, 'timer')
        fixed = frame_us(n, gpu, 250, 'event')
        print(f'{n:4d}    {sync / 1000:8.2f}   {timer / 1000:9.2f}   {fixed / 1000:12.2f}'
              f'      {1e6 / sync:5.0f}  {1e6 / timer:5.0f}  {1e6 / fixed:5.0f}')


if __name__ == '__main__':
    main()
