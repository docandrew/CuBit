# Native audio sink failure tests

Run under the repository Nix shell and shared build lock:

```
flock --exclusive --nonblock coordination/build.lock nix develop -c python3 tests/servo/audio-output/run.py
```

`--kernel PATH` selects a built CuBit kernel; default: `kernel/cubit_kernel`.
`--metadata PATH` reuses the JSON produced by the pinned media environment.
Otherwise the runner builds that environment with Nix. Each run has an isolated
ISO and output directory; no user disk is attached. The supervisor starts the
test as an ordinary process without audio authority. Its injected transport
exercises GStreamer and the production sink on CuBit, not real HDA hardware.

The suite also registers and creates the statically linked Opus decoder,
converter, resampler and mixer. This checks native loading, not codec fidelity.

Cases cover exact partial writes, failed playback queries, drain and write
stalls, invalid write acknowledgements, cancellation during writing/draining,
and rejected opens, followed by another successful pipeline. Failures must
produce a resource error originating at the sink. Cancellation must produce no
error and teardown must finish within one second. The two-second no-progress
limit is an output failure policy, not an end-to-end media latency guarantee.

The sink is wired into Penny when its media feature is enabled. Native HDA
fidelity and browser integration are separate tests. Flush/reopen failure has an explicit error path but is not
yet exercised by this suite.

Use `--suite hub` for shared-output lifecycle coverage: nine simultaneous inputs,
three late joins, cancellation while a producer is blocked, output-failure
propagation, and independent completion at exact sample boundaries. These tests
use a simulated device backlog. Actual HDA integration was separately verified
in the private workspace: `tmp/penny-audio-8pr8fdvd` captured 2400 exact signal
frames, short-source DMA completion while a longer source remained pending,
and successful reuse after every previous input had ended.

The shared owner must serialize control operations and poll for output errors.
A producer retains its input until any concurrent push has returned. End-of-
stream is serialized with that input's pushes; completion is scoped to its last
sample, rather than the entire output becoming empty. Device completion covers
CuBit ring/DMA accounting, not downstream emulator codec or amplifier latency.
The hub selects GStreamer's NOW start time: ZERO can rewind the forced-live
silence timeline when the first input arrives. Output timestamps and sample
offsets must form a continuous timeline until a flush/reopen resets the sink.
Browser A/V synchronization and long-duration playback remain integration work.
This isolated suite grants no browser audio authority.

Use `--suite player` for the registered per-player sink: nine/one/nine independent
pipelines sharing one output, flushing seek replay, paused preroll, cancellation
of scheduled data, factory binding lifetime, and missing-device failure. Session
creation/poll/destruction before playback never opens the device or starts the
mixer. The first player starts the shared hub. Removing its last input destroys the
hub and closes the output, even while the browser session remains alive. The
next player lazily recreates the output from the retained transport callbacks.
Paused players retain their inputs; this does not suspend paused sessions. Poll the session from its owner to propagate
output errors. Transport context must outlive every retained player.

This adapter uses a provisional 40ms scheduling lead. A/V latency reporting and
synchronization remain unverified; browser integration is enabled in source. Tests
of exact captured Opus and WebAudio output run separately against real CuBit
Mixer/HDA in QEMU. They do not establish physical-device shutdown fidelity.

The player regression uses distinct power-of-two sample values and checks all
960 frames from each input independently, so losses cannot hide in a total sum.
The shared output retains clock synchronization even without active inputs:
unpaced forced-live mixing can run its sample timeline ahead of the wall clock.
A 20ms early-write offset provides a bounded device reserve. This setting passed
native multi-player tests and eight sequential Opus captures with WebAudio;
long-duration playback and end-to-end A/V latency remain separate checks.

`webaudio.html` tests sixteen JavaScript AudioContext lifecycles: a scheduled
4096-frame stereo buffer at 48kHz, `onended`, then `close()`. Channels contain
+0.125/-0.125 float samples. `/servo/perf-check` enables title progress markers.
Check its native QEMU capture with:

```
python3 tests/servo/audio-output/check-webaudio-capture.py CAPTURE_DIRECTORY 16
```

The checker reads `output.wav`, validates the PCM format, and requires sixteen
contiguous signals. It permits one sample unit of conversion dither, including
background silence. It reads the physical PCM payload because QEMU WAV size
fields may stay zero. This verifies page-generated audio, not A/V sync or
physical hardware. Sampled owned mapped memory stayed approximately flat from
contexts eight to sixteen; this is neither an RSS measurement nor leak proof.

The pinned GstPlay recipe fixes unchanged-stream-selection success reporting.
Native Servo reproduced `SetTrackFailed` when selecting the current audio track
before the fix. Eight clips passed repeated selection afterward, and invalid
index 999 still failed. A second patch stores a valid audio-track choice while
that track type is disabled, avoiding an empty stream-selection failure that
otherwise prevents Servo from reaching the enable operation. Eight native
players passed disable, repeated disable, and re-enable; invalid index 999 was
rejected both enabled and disabled. All eight decoded clips and two WebAudio
signals matched the capture reference within one sample unit.

These immediate toggles do not establish sustained silence while disabled,
multi-track switching, A/V sync, or absence of intermittent audio gaps. Those
remain separate checks. Rebuild the browser to consume these dependency fixes.


Sustained disable testing found a second issue: the static build omitted the
volume plugin, so playbin mute did not silence the stream. The build now links
and registers that plugin. Servo retains requested mute separately from track
enablement and applies their combined mute state without changing volume.
A native four-second disabled clip was silent after a 648-frame buffered
prefix (13.5 ms); a separate explicitly muted clip stayed silent through track
re-enable. A 50% volume clip and five full-volume clips matched their reference
within one sample unit, followed by two correct WebAudio signals. The checker
allows at most 100 ms of contiguous initial reference samples and requires at
least three seconds of silence afterward; that is a regression bound, not a
promised worst-case latency. This is backend/QEMU evidence, not a rebuilt full
browser, physical hardware, or elimination of the known intermittent gaps.


The full Penny HTML test (`mute-volume.html`) exposed a 648-frame startup burst
at full volume when an element's volume was set before its backend existed.
Player creation now applies the stored volume alongside mute. The corrected
browser test played muted, 50%, and 100% clips: capture contained exactly the
half/full reference clips (error at most one sample unit), with no other
nonzero samples. Check a recording with:

```
python3 tests/servo/audio-output/check-mute-volume-capture.py CAPTURE_DIRECTORY REFERENCE_S16
```

The reference is the independently decoded 48 kHz stereo signed-16 PCM for
the embedded generated Opus clip. This verifies the full browser in QEMU;
physical hardware, A/V synchronization, and intermittent gaps remain separate.

The player suite also interrupts a single two-second buffer after output starts.
It requires PAUSED to complete, output to settle, exact remaining samples on
resume, prompt NULL cancellation, and a flushing seek while paused that emits
only the replacement buffer. These use the injected native transport, not HDA.

Audio output now reports its bounded ring and DMA capacity through the owner-scoped
playback query. The runtime rejects impossible capacities/backlogs; run
`bash tests/audio-playback/run.sh` inside Nix for boundary coverage. Rebuild HDA,
mixer, the user runtime and Penny together: playback reply word 2 now carries
device capacity and old zero-filled replies are rejected.

HDA uses 32 periods (32 KiB of PCM-only shared DMA). Penny includes transport
capacity, its 40 ms submission lead and the mixer's output period in GStreamer
render-delay negotiation. Private TCG tests measured video-frame delivery versus
audio progress at 12–20 ms offset, previously 260–271 ms. This is not hardware
or on-screen/speaker timing. Clean 360p continuous and pause tests preserved all
reference samples but still inserted short silence gaps (about 5–27 ms); glitch-free
playback is not yet established. No test probes are included in production sources.

The live mixer now allows 40 ms for late input before mixing past it, and reports
that allowance in each player's render delay. Previously, native mixer QoS
reports and capture mismatches identified input being dropped before reaching
CuBit's audio transport. Eight consecutive 12-second 360p clips preserved all
reference samples with this allowance; the first contained 13.33 ms of inserted
silence and the remaining seven had no gaps. Pause/resume preserved all samples
with only the expected pause gap. These TCG runs do not prove glitch-free playback
or absence of leaks.

An instrumented run measured video appsink delivery 5–18 ms behind the estimated
audio playback position across eleven samples, with all reference audio preserved.
This measures CuBit's period-granular backlog, not physical screen/speaker timing.
Native hub/player regressions passed with the probes removed, including blocked
producer cancellation, exact multi-player contributions, in-flight pause/resume,
paused flushing seek, and factory lifetime. The final clean browser capture also
preserved all reference audio. Detailed run evidence is recorded in
`build/mixer-allowance-publication.json`; staged binaries require a rebuild.


`seek.html` is a generated-media template for full-browser seeking. Generate a
12-second VP8/Opus fixture under Nix with:

```
python3 tests/servo/audio-output/generate-seek-fixture.py --ffmpeg /path/to/ffmpeg --output /tmp/penny-seek
```

Use the generated `pages` as `/servo/pages`, enable `/servo/perf-check`, and
record the QEMU HDA output at 48 kHz stereo signed-16 PCM. The page checks a
paused seek to six seconds (clock stays still), a playing backward seek to two
seconds, a playing forward seek to ten seconds, and end-of-stream. Require all
four `CuBitBrowserPerfSeek*` title markers and `CuBitBrowserPerfAudioEnded`.
Then run `python3 tests/servo/audio-output/check-seek-capture.py CAPTURE_DIR`.
The checker requires audio bands for seconds 0–1, 6–7, 2–3, and 10–11 in order;
each second has a distinct tone at `300 + 100 * second` Hz. It uses 100 ms
spectral windows and limits ambiguous transition windows. It does not establish
sample-exact seek boundaries, absence of gaps, or screen/speaker synchronization.

The clean private browser passed both DOM checks and the recorded segment-order
check (`penny-audio-vxrexpyq`). An independently decoded, uninterrupted copy of
the source was rejected by the capture checker. This is native CuBit/QEMU
integration evidence; the fixture is local data, not an HTTP range-seek test.


For network-backed seeking, run the loopback-only server in the Nix shell:

```
python3 tests/servo/audio-output/serve-seek-fixture.py --media /tmp/penny-seek/seek.webm --log /tmp/penny-http.json
```

It prints host and QEMU user-network URLs. Put the QEMU URL in `/servo/pages`
and enable `/servo/perf-check`. Media responses support byte ranges and are
paced at approximately 120 KiB/s. Require the seek/EOS markers and captured
audio sequence described above, plus nonzero 206 range requests in the HTTP
log. Add `--recovery` to truncate the initial resource: require
`CuBitBrowserPerfNetworkErrorHandled`, then successful seeks/EOS on a replacement
source in the same element. Stop the test server with Ctrl+C.

Native CuBit/QEMU runs passed normal HTTP playback (`penny-audio-mm5wf0bh`)
and recovery after incomplete media/header responses (`penny-audio-9ezbgt2i`).
Both recordings reproduced the requested audio segment order. These cover plain
HTTP through the netstack, not TLS, adaptive streaming, sustained bandwidth loss,
mid-playback disconnects, or malformed-media security isolation.


The server also accepts `--cert CERT.pem --key KEY.pem` and optional
`--server-name HOST` (default `tls-test.cubit.internal`). It then requires TLS
1.2 or later and records the negotiated protocol for range requests. For the
existing test PKI, copy its `ca.der` to `/tls/roots.der` and a hosts entry
`10.0.2.2 tls-test.cubit.internal` to `/servo/hosts` **only in the disposable test
image**. Keep certificate verification enabled; no host trust-store changes are
needed. The printed QEMU URL uses the certificate hostname. The printed loopback
host URL identifies the listener, but verifying clients must use the certificate
hostname for SNI and name checking.

Native HTTPS playback/seek (`penny-audio-b0p0boza`) negotiated TLS 1.3 and passed
the recorded audio segment-order check. A separate media request with a trusted
certificate for the wrong hostname failed name validation before HTTP; the same
video element then played the valid HTTPS source and passed all seeks plus the
audio check (`penny-audio-1lbxsj4i`). This tests certificate rejection for media
subresources, not just top-level navigation. Both runs used a test CA scoped to
the disposable image. It does not establish adaptive streaming or hostile-code
isolation. Host fixture logs can show TLS EOF when QEMU closes keep-alive sockets
at teardown; this is separate from the explicitly checked rejection handshake.


### Video retirement and idle memory

Generate a page using a supplied 640x360 WebM (the native regression uses a
12-second VP8/Opus clip):

```
python3 tests/servo/audio-output/generate-retirement-page.py clip.webm --output /tmp/penny-retirement
```

Overlay the generated `pages` as `/servo/pages` in a disposable desktop disk,
enable `/servo/perf-check`, and launch Penny with its normal media capabilities.
The page plays and removes eight video elements, navigates away with
`location.replace`, and waits 60 seconds on a blank page. It uses normal cleanup;
it does not request a full GC. The nested document is independently base64
encoded so its closing script tag cannot terminate the outer fixture.

After the `CuBitBrowserPerfAudioEnded` marker, summarize the serial trace:

```
python3 tests/servo/audio-output/summarize-retirement-memory.py serial.log
```

The summary requires all eight cycles in order, one completed idle phase,
at least 50 seconds between its first and last memory samples, one window,
monotonic sample times, and no recognized crash/script-error markers. It
reports kernel process-owned mapped bytes, not RSS or live allocator bytes.
Retained mappings alone do not prove a leak, and a flat interval does not
prove that there are none. The generated provenance records the media hash.

Native release b70a260c completed this fixture on the current desktop snapshot:
36 samples, peak 496787456 bytes; 12 idle samples spanning 55017 ms fell from
345665536 to 278323200 bytes. This is a single natural-cleanup observation,
not a memory budget or performance comparison with another browser.

The player suite checks nine/one/nine groups across output close/reopen cycles,
with exact per-input samples and balanced opens/closes. After every group it
requires no output writes during a 100 ms idle interval. It also keeps a started
player alive across factory unregister/session release, then verifies that its
stop closes the output. These use an injected transport; native browser capture
and Mixer/HDA idle observation are separate integration checks.
