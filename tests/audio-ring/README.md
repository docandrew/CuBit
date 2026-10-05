# Audio ring counter rollover

Run `nix develop -c bash tests/audio-ring/run.sh --negative`.

The hosted test exercises the production ring geometry at every stereo-frame
start spanning one ring before and after U32 rollover, with 4-byte, 1024-byte,
and full-ring writes. It models the client's contiguous reservation spans and
the mixer's modular counter indexing independently, then compares every byte.
It also checks occupancy across rollover and that the allocation fits the data.
The negative test substitutes the old 8128-byte capacity and removes the
power-of-two assertion: the sample comparison must fail.

At 48 kHz stereo S16LE, a U32 byte counter wraps after about 6.214 hours.
The 8192-byte data ring divides the counter modulus, keeping offsets continuous.
The 64-byte header requires a third page; Mixer preallocates eight rings, adding
32 KiB total. The IPC open reply still supplies actual header and data sizes.
Clients should be rebuilt with the new runtime; it rejects non-power-of-two
write rings rather than allowing rollover corruption.

Native verification on 2026-10-03 used the existing isolated audio harness in
`.build-workspaces/penny-demand-nq9qtuvx`. A test-only Mixer change initialized
both counters to `16#FFFF_F000#` instead of zero. All other ring operations used
the production source. Artifact `tmp/penny-audio-b8s88p5o` contains the native
run and exact capture report: 105600 signal frames match sample-for-sample
across two simultaneous sources, source removal, and nine simultaneous sources.
This is accelerated boundary coverage in CuBit/QEMU, not a six-hour soak or
physical hardware test. Browser audio integration remains separate.
