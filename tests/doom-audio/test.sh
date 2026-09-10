#!/usr/bin/env bash
set -eu
root=$(cd "$(dirname "$0")/../.." && pwd)
mkdir -p "$root/tests/doom-audio/build/src" "$root/tests/doom-audio/build/obj"
cp "$root/userspace/runtime/gnat/cubit.ads" \
   "$root/userspace/c/cubit-doom_sound.ads" \
   "$root/userspace/c/cubit-doom_sound.adb" "$root/tests/doom-audio/build/src/"
cd "$root/kernel"
alr exec -- gprbuild -P ../tests/doom-audio/audio.gpr
../tests/doom-audio/build/main
