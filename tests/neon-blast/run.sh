#!/usr/bin/env bash
# Linux-hosted Neon Blast checks (APP-008). The game's portable units built
# for the host; nothing here runs on CuBit. Run inside nix develop with
# TMPDIR outside /tmp:
#   tests/neon-blast/run.sh             keys, audio and replay determinism
#   tests/neon-blast/run.sh --shots DIR also write replay screenshots (PNG)
# The Vulkan path's hosted run (lavapipe) is tests/neon-blast/run-vulkan.sh.
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/../.." && pwd)"
shots=""
if [ "${1:-}" = "--shots" ]; then shots="$2"; fi
work="$here/build/run"
mkdir -p "$work/a" "$work/b" "$work/c"
(cd "$root/kernel" &&
   alr exec -- gprbuild -p -q -j8 -P "$here/neon_blast_host.gpr" &&
   alr exec -- gprbuild -p -q -j8 -XNEON_HOST_BUILD=release -P "$here/neon_blast_host.gpr")

"$here/build/neon_keys_tests"

# The synthesizer renders the same samples however the caller cuts blocks.
one=$("$here/build/neon_audio_host" "$work/blocks-1024.wav" 12 7 1024 | sed -n 's/.*hash *//p')
two=$("$here/build/neon_audio_host" "$work/blocks-37.wav" 12 7 37 | sed -n 's/.*hash *//p')
if [ "$one" != "$two" ]; then
  echo "NEON-AUDIO: FAIL block size changes the samples ($one vs $two)"; exit 1
fi
echo "NEON-AUDIO: PASS block-size independent (hash $one)"
python3 -I "$here/check_wav.py" "$work/blocks-1024.wav"

# A seed and the autopilot replay exactly: twice, and with the native
# build's flags (-O3, no checks) against the checked build.
"$here/build/neon_replay_host" "$work/a" 15 7 640 400 0 > "$work/a.txt"
"$here/build/neon_replay_host" "$work/b" 15 7 640 400 0 > "$work/b.txt"
"$here/build/release/neon_replay_host" "$work/c" 15 7 640 400 0 > "$work/c.txt"
for run in b c; do
  if ! diff <(grep -E 'hash|score' "$work/a.txt") <(grep -E 'hash|score' "$work/$run.txt") >/dev/null; then
    echo "NEON-REPLAY: FAIL run $run differs"; cat "$work/a.txt" "$work/$run.txt"; exit 1
  fi
done
grep -E 'hash|score|render' "$work/c.txt"
echo "NEON-REPLAY: PASS deterministic (checked twice, release once)"

# The style space: seeds reach every region and mode, tempos span their
# range, the hot stages keep their floors, few seeds repeat.
"$here/build/release/neon_styles_host" 1 2000

# Beat-to-sample: kicks sound where the composer put them, measured on the
# soundtrack alone (the game's explosions share the kick's band), a minute
# of the replay seed's first stage (one kit: the detector's lag depends on
# the kick's timbre). Baseline 13.5 ms = the 7 ms tape stage + the
# detector's lag for that kit; a slip of a 4 ms simulation tick or an audio
# block moves the median past 2 ms. The mutation shifts the kick times by
# one tick, both ways: the check must then fail.
"$here/build/release/neon_audio_host" "$work/sync.wav" 60 7 1024 7 > /dev/null
python3 -I "$here/check_sync.py" "$work/sync.wav" "$work/sync.wav.kicks.txt" 13.5 2.0 3.0
for slip in 200 -200; do
  awk -v d=$slip '{print $1+d}' "$work/sync.wav.kicks.txt" > "$work/sync-slip.txt"
  if python3 -I "$here/check_sync.py" "$work/sync.wav" "$work/sync-slip.txt" 13.5 2.0 3.0 > /dev/null; then
    echo "NEON-SYNC: FAIL the check did not catch a one-tick slip ($slip samples)"; exit 1
  fi
done
echo "NEON-SYNC: PASS a one-tick slip either way is caught"

# One continuous piece: 150 s through a stage's whole form (the boss
# falls after 8 peak bars) into the next stage, no gaps, no fades between
# sections; the mutation silences a second of it and must fail.
"$here/build/release/neon_audio_host" "$work/flow.wav" 150 7 1024 9
python3 -I "$here/check_wav.py" "$work/flow.wav"
python3 -I "$here/check_flow.py" "$work/flow.wav" "$work/flow.wav.sections.txt"
python3 -I "$here/mute_wav.py" "$work/flow.wav" "$work/flow-gap.wav" 60 1
if python3 -I "$here/check_flow.py" "$work/flow-gap.wav" "$work/flow.wav.sections.txt" > /dev/null; then
  echo "NEON-FLOW: FAIL the check did not catch a one-second gap"; exit 1
fi
echo "NEON-FLOW: PASS a one-second gap is caught"

if [ -n "$shots" ]; then
  mkdir -p "$shots"
  "$here/build/release/neon_replay_host" "$work/c" 40 7 1280 800 300 > /dev/null
  python3 -I "$root/tests/text-editor/ppm_to_png.py" "$work/c"/*.ppm --into "$shots" > /dev/null
  cp "$work/c/replay.wav" "$shots/replay.wav"
  echo "screenshots: $shots"
fi
echo "NEON-HOST: ALL PASS"
