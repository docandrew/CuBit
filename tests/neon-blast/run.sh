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

# Beat-to-sample: kicks sound where the composer put them, measured on the
# soundtrack alone (the game's explosions share the kick's band). Baseline
# 13.5 ms = the 7 ms tape stage + the detector's lag; a slip of a 4 ms
# simulation tick or an audio block moves the median past 2 ms.
python3 -I "$here/check_sync.py" "$work/blocks-1024.wav" "$work/blocks-1024.wav.kicks.txt" 13.5 2.0 2.0

if [ -n "$shots" ]; then
  mkdir -p "$shots"
  "$here/build/release/neon_replay_host" "$work/c" 40 7 1280 800 300 > /dev/null
  python3 -I "$root/tests/text-editor/ppm_to_png.py" "$work/c"/*.ppm --into "$shots" > /dev/null
  cp "$work/c/replay.wav" "$shots/replay.wav"
  echo "screenshots: $shots"
fi
echo "NEON-HOST: ALL PASS"
