# DOOM short-write regression

```sh
nix develop -c make -C kernel test-doom-audio
```

Linux-hosted test of the **production** `CuBit.Doom_Sound` source, staged
unchanged into `build/src`. The fixture substitutes only the audio sink and
debug messages; no freestanding GNAT runtime or privileged syscalls run here.
Host assertions/overflow checks are enabled; native assertion policy is unchanged.

The sink returns controllable accepted-frame counts, including zero. Tests
compare the complete captured stereo PCM with a full-write reference for short
effects, exact batch boundaries, multiple batches and 11,025 Hz resampling.
They also check a one-frame pending tail, prolonged full-ring backpressure,
reuse of a channel while its old PCM is pending, and shutdown/reinitialization.
An update makes at most two write attempts and never waits for space.

The original implementation failed the first short-effect frame-count
assertion: it marked the effect inactive and discarded the unwritten PCM.
The fixed implementation retains a suffix of the existing 1536-frame output
buffer, drains it before mixing again, and drains even with no active channels.
No additional PCM buffer or heap allocation is required.

This verifies partial-write preservation, not resampler fidelity, SPARK
properties, physical output quality, or scheduling deadlines. Already mixed
audio cannot be selectively unmixed when a channel stops or is reused, just
as frames already published to the stream cannot be recalled. Shutdown
intentionally discards pending PCM instead of blocking until hardware drains.

Native smoke test after building with `nix develop -c make -C kernel doom`:

```sh
nix develop -c tests/headless/run.sh --test desktop-doom --accel kvm --cpus 4 \
  --timeout 40 --keep-logs --serial /tmp/doom-audio.serial
```

That checks integration/startup/input; listening and further underrun
measurements are still needed. Game-loop-driven refill and other scheduling
limits are unchanged by this fix.
