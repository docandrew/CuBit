#!/usr/bin/env bash
set -eu
root=$(cd "$(dirname "$0")/../.." && pwd)
mkdir -p "$root/tests/doom-audio/build/src" "$root/tests/doom-audio/build/obj"
cp "$root/userspace/runtime/gnat/cubit.ads" \
   "$root/userspace/ports/doom/cubit-doom_sound.ads" \
   "$root/userspace/ports/doom/cubit-doom_sound.adb" \
   "$root/userspace/ports/doom/cubit-doom_mixer.ads" \
   "$root/userspace/ports/doom/cubit-doom_mixer.adb" "$root/tests/doom-audio/build/src/"
cd "$root/kernel"
alr exec -- gprbuild -P ../tests/doom-audio/audio.gpr
../tests/doom-audio/build/main
