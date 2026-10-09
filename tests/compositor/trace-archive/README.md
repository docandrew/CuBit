# Compositor capture archive

The archive preserves real Desktop trace events for later analysis, without
putting filesystem calls in Desktop's rendering or publication path. These
units provide a bounded binary format and a checked reader. On-device capture
controls and a CCL timeline/flame-graph viewer remain to be implemented.

## Reproduce the checks

From the repository root, use the pinned Nix environment and kernel Alire
toolchain. Set an absolute output directory to keep concurrent runs separate:

```sh
nix develop
export TRACE_ARCHIVE_OUTPUT=/absolute/path/to/private/archive-build
cd kernel
alr exec -- gprbuild -p -P ../tests/compositor/trace-archive/trace_archive.gpr
"$TRACE_ARCHIVE_OUTPUT/archive_check"
"$TRACE_ARCHIVE_OUTPUT/framing_check"
alr exec -- gnatprove -P ../tests/compositor/trace-archive/trace_archive.gpr -u compositor_trace_archive.adb compositor_trace_framing.adb --level=2 --timeout=30 -j2 --report=all
cd ..
python3 tests/compositor/trace-archive/check-reader.py --reader "$TRACE_ARCHIVE_OUTPUT/archive_reader" --output "$TRACE_ARCHIVE_OUTPUT/reader-faults"
"$TRACE_ARCHIVE_OUTPUT/archive_reader" /path/to/capture.cubittrace
```

The chunk test checks exact round trips, full-width identities, all single-bit
mutations across 1,000 events, invalid identities and reserved fields. The
framing test exercises the 4,096-event limit, sequence/endpoint rejection,
footer integrity, trailing bytes and incomplete files. The Python reader test
independently checks every chunk checksum in the retained native fixture,
then checks valid decoding and seven truncated/corrupt/reordered/replayed
variants. This is reader fault coverage, not native filesystem fault injection.

## Version 1 byte layout

Every chunk is 256 bytes: 32 unsigned 64-bit words in little-endian order.
Word 31 is FNV-1a64 over the preceding 248 bytes. FNV detects accidental
corruption; it does not authenticate data. Word 1 is always 256. All unused
words must be zero. Magic values include the version.

| Chunk | Word 0 magic | Remaining fields |
|---|---|---|
| Header | `0x4354414800000001` | 2 capture ID; 3 observer incarnation; 4 start microseconds; 5 event budget; 6 clock descriptor (1); 7–30 zero |
| Event | `0x4354415200000001` | 2 consecutive archive sequence; 3 observer incarnation; 4 PID; 5 issued publisher tag; 6 batch; 7 first raw-history sequence; 8 producer drops; 9 batch gaps; 10–25 full trace packet; 26–30 zero |
| Footer | `0x4354414600000001` | 2 capture ID; 3 observer incarnation; 4 event count; 5 end microseconds; 6 stop reason; 7 rolling chain; 8 skipped rows; 9 rejected rows; 10 abandoned events; 11 emitted events; 12 endpoint mismatches; 13–30 zero |

Stop reasons are 0 requested stop, 1 budget reached, 2 observer failed.
The chain starts at the header checksum and incorporates each accepted event
checksum as `(chain xor checksum) * 0x100000001b3` modulo 2^64. A reader must
check the entire file through EOF; a footer followed by more data is rejected.
The format budget is at most 4,096 events (1,049,088 bytes with header/footer).
The native test collector uses 256 events and one 4 KiB output page.

Clock descriptor 1 means boot-local monotonic microseconds with **unknown boot
identity**. A capture ID is not a globally unique boot ID. Do not join clocks
across captures. Match events with observer incarnation, PID and issued
publisher tag, then the complete source, writer or frame identity. Missing
records remain gaps; retained source age is not input latency.

## Proof and native evidence boundaries

`Compositor_Trace_Archive` and `Compositor_Trace_Framing` are pure SPARK,
including their bodies. Their contracts cover encoding round trips, decoded
validity, bounded framing transitions and EOF completion. Filesystem I/O,
byte adapters, clocks, grant handling and the hosted reader are outside these
proof boundaries. Checksums and successful format validation do not prove a
successful storage flush or authenticate the original publisher.

The retained fixture contains 78 real Desktop events from native CuBit running
in QEMU with the Mesa software backend. The private collector used exclusive
create, exact positioned-write counts, explicit flush, close and confirmed
grant retirement before announcing success. The file was retrieved from the
stopped VM's private NVMe image. Native negative tests separately verified
existing-file preservation, RAM flush refusal and full-volume write refusal.
The native adapter is an opt-in integration fixture; see
[`../trace-native/README.md`](../trace-native/README.md) for normal save and
collector-exit tests. The default raw-observer fixture keeps its existing
authority.
See `evidence.json` for hashes, runs and explicit scope.

Completion durations in this fixture include software work and Desktop
completion collection under uncontrolled host load. They do not measure
hardware latch, physical keypress-to-photon, or demonstrated 240 Hz operation.
A collector exit after its first acknowledged write is tested by the native
fixture. Interruption inside an in-flight write, power loss, and observer
restart remain open.
