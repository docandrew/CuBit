#!/usr/bin/env bash
# Mutation check for Desktop_GPU_Wake (Linux-hosted): each mutation breaks one
# rule in a copy of the unit; gpu_wake_tests must then fail.
# Run inside the Nix shell: bash tests/compositor/gpu-wake/test-gpu-wake-mutations.sh
set -uo pipefail
cd "$(dirname "$0")/../../../kernel"
SRC=../userspace/services/desktop
WORK=$(mktemp -d "${TMPDIR:-/tmp}/gpu-wake-mutations.XXXXXX")
trap 'rm -rf "$WORK"' EXIT
caught=0; survived=0; stale=0
mutate() {
    local name=$1 from=$2 to=$3
    rm -rf "$WORK/src" "$WORK/obj"; mkdir -p "$WORK/src"
    cp "$SRC"/desktop_gpu_wake.ad? "$WORK/src/"
    if ! python3 - "$WORK/src/desktop_gpu_wake.adb" "$from" "$to" <<'PY'
import sys
path, old, new = sys.argv[1:4]
text = open(path).read()
if text.count(old) != 1: sys.exit(1)
open(path, 'w').write(text.replace(old, new))
PY
    then echo "STALE    $name"; stale=$((stale + 1)); return; fi
    if ! DESKTOP_SRC="$WORK/src" GPU_WAKE_OBJ="$WORK/obj" \
         alr exec -- gprbuild -p -q -P ../tests/compositor/gpu-wake/gpu_wake.gpr >"$WORK/build.log" 2>&1; then
        echo "NO-BUILD $name"; stale=$((stale + 1))
    elif "$WORK/obj/gpu_wake_tests" >"$WORK/run.log" 2>&1; then
        echo "SURVIVED $name"; survived=$((survived + 1))
    else
        echo "caught   $name ($(grep -m1 FAIL "$WORK/run.log" || echo assertion))"; caught=$((caught + 1))
    fi
}
mutate "looks again for ever (spin)" "            Looked_Again := not Looked_Again;" "            Looked_Again := False;"
mutate "arms again while armed" "      if Armed_Token /= 0 then
         Mode := Event;
         return;
      end if;" ""
mutate "a foreign completion disarms" "      if not Matches (C.token) then
         return;
      end if;" ""
mutate "no event: sleeps on a missing event" "         when Hook.Unavailable =>
            Period.Timer := Period.Timer + 1;" "         when Hook.Unavailable =>
            Mode := Event;
            Period.Timer := Period.Timer + 1;"
mutate "reports every turn" "      if Now_Ms - Period_Start_Ms < Report_Period_Ms then" "      if False then"
echo "mutations: caught=$caught survived=$survived stale=$stale"
[ "$survived" -eq 0 ] && [ "$stale" -eq 0 ]
