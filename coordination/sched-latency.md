# Scheduler latency benchmark agent

Updated: 2026-09-29 09:30. Status: done, idle (no jobs, no lock held).

Scope: an interbench-style benchmark (`bench-latency` headless case) and a
baseline on the current (strict-priority + interim idle placement) scheduler.

## Owned

- `tests/sched-latency/` (new: app source, build script, init profile,
  manifest, Linux reference, README with results)

## Shared files I edit

- `tests/headless/run.sh`: additive only (a `bench-latency` case), edited
  while holding `coordination/build.lock`.

## Not touched

`kernel/src` (scheduler work by another agent), `userspace/services/filesystem`,
`userspace/libc/overlay/src/cubit/file.c`, sparktls, sparkentropy.

## Native runs

run.sh rebuilds the kernel/initrd from the working tree each run, under the
build lock: `tests/headless/run.sh --test bench-latency --accel kvm`.

## Results / requests

Baseline recorded in `tests/sched-latency/README.md`. Of the 15 native runs
that reached the `spam` load, 11 panicked in `Process.noteContextStarted`
("Dispatch of executing or retiring process"). Suspected cause: the interim
idle-CPU placement queueing a still-executing thread on another CPU. For the
scheduler agent. Not bisected.
