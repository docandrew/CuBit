# Desktop latency observer

`desktop-latency-observer.app` prints, every 5 s, metricsvc's cumulative
summaries of three Desktop series (metrics-enabled Desktop, which is the
session default):

| Key | Name | Measures |
| --- | --- | --- |
| 13 | `desktop.loop_turn` | one event-loop pass that did work, housekeeping included |
| 14 | `desktop.input_to_present` | oldest unpresented input's intake until the frame showing it is submitted |
| 15 | `desktop.input_source_age` | pointer report's driver capture until desktop's intake (ms clock) |

Each line: `DESKTOP-LATENCY: <name> count= p50_upper_us= p99_upper_us= max_us=
producer_dropped=`. A periodic stall shows as p99/max growth. Negative control
(2026-10-10, QEMU KVM, timing build, 500 Hz pointer flood): a Desktop that
writes its whole periodic report at the period boundary on the synchronous
console showed loop_turn p99 81,920 us and input_source_age p99 57,344 us;
the shipped Desktop shows 448 us and 1,024 us.

Build (repository root, Nix, under coordination/build.lock), then start it
after desktop.svc in an init profile:

```sh
cd kernel
mkdir -p ../tests/compositor/latency-observer/build/generated
../userspace/ccl/build/manifest/ccl-manifest ../userspace/ccl/catalogs/native-runtime-services.ccl \
  ../tests/compositor/latency-observer/manifest.ccl \
  --ada-output ../tests/compositor/latency-observer/build/generated/ccl_manifest_bindings.ads \
  > ../tests/compositor/latency-observer/build/manifest.S
alr exec -- gcc -c ../tests/compositor/latency-observer/build/manifest.S \
  -o ../tests/compositor/latency-observer/build/manifest.o
alr exec -- gprbuild -p -P ../tests/compositor/latency-observer/observer.gpr
```
