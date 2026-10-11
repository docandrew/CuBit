#!/usr/bin/env bash
# Mutation check for GPU-001 step 3's Mesa timeline logic (Linux-hosted).
# Each mutation breaks one rule in a copy of the Mesa glue's Ada sources;
# gpu_timeline_tests must then fail (a check or a runtime-checked contract).
# A mutation whose text is not found, or that does not compile, is stale.
# Run inside the Nix shell: bash tests/mesa-anv/test-gpu-timeline-mutations.sh
set -uo pipefail
cd "$(dirname "$0")/../../kernel"
SRC=../userspace/mesa/anv
WORK=$(mktemp -d "${TMPDIR:-/tmp}/gpu-timeline-mutations.XXXXXX")
trap 'rm -rf "$WORK"' EXIT

caught=0
survived=0
stale=0

mutate() {
    local name=$1 file=$2 from=$3 to=$4
    rm -rf "$WORK/src" "$WORK/obj"
    mkdir -p "$WORK/src"
    cp "$SRC"/native_gpu_*.ad? "$WORK/src/"
    if ! python3 - "$WORK/src/$file" "$from" "$to" <<'PY'
import sys
path, old, new = sys.argv[1:4]
text = open(path).read()
if text.count(old) != 1:
    sys.exit(1)
open(path, 'w').write(text.replace(old, new))
PY
    then
        echo "STALE    $name"
        stale=$((stale + 1))
        return
    fi
    if ! MESA_SRC="$WORK/src" TIMELINE_OBJ="$WORK/obj" \
         alr exec -- gprbuild -p -q -P ../tests/mesa-anv/gpu-timeline/gpu_timeline.gpr >"$WORK/build.log" 2>&1; then
        echo "NO-BUILD $name"
        stale=$((stale + 1))
    elif "$WORK/obj/gpu_timeline_tests" >"$WORK/run.log" 2>&1; then
        echo "SURVIVED $name"
        survived=$((survived + 1))
    else
        echo "caught   $name ($(grep -m1 'FAIL\|raised' "$WORK/run.log" || echo 'assertion'))"
        caught=$((caught + 1))
    fi
}

mutate "a point counts as reached one value early" native_gpu_timeline.ads \
    "(P.GPU <= Completed (P.Context));" "(P.GPU <= Completed (P.Context) + 1);"
mutate "resolve takes the lowest reached point" native_gpu_timeline.adb \
    "for I in reverse 1 .. T.Count loop" "for I in 1 .. T.Count loop"
mutate "resolve keeps resolved points" native_gpu_timeline.adb \
    "      T.Value := Old.Points (Highest).Value;
      Drop_First (T, Highest);" "      T.Value := Old.Points (Highest).Value;"
mutate "a CPU signal keeps the points it covers" native_gpu_timeline.adb \
    "      Drop_First (T, Covered);
      T.Value := V;" "      T.Value := V;"
mutate "a CPU signal may go backwards" native_gpu_timeline.adb \
    "      OK := V >= T.Value;" "      OK := True;"
mutate "a point may repeat a value" native_gpu_timeline.adb \
    "      if V <= Last_Value (T) then" "      if V < Last_Value (T) then"
mutate "find answers the point below the wait" native_gpu_timeline.adb \
    "         if T.Points (I).Value >= W then" "         if T.Points (I).Value + 1 >= W then"
mutate "same-context waits kept" native_gpu_timeline.adb \
    "      if P.Context /= Job and then P.GPU > S (P.Context) then" \
    "      if P.GPU > S (P.Context) then"
mutate "merge keeps the lower value" native_gpu_timeline.adb \
    "      if P.Context /= Job and then P.GPU > S (P.Context) then" \
    "      if P.Context /= Job and then (S (P.Context) = 0 or else P.GPU < S (P.Context)) then"
mutate "select drops the second wait" native_gpu_timeline.adb \
    "               Second := (Context => C, Target => S (C));" "               null;"
mutate "move leaves the source signalled" native_gpu_timeline.adb \
    "      Source.Count := 0;" "      null;"
mutate "move reuses the source's last value" native_gpu_timeline.adb \
    "      New_Source_Next := Last + 1;" "      New_Source_Next := Last;"
mutate "a misaligned batch is valid" native_gpu_job_rules.ads \
    "      GPU mod Batch_Alignment = 0 and then Bytes /= 0" "      Bytes /= 0"
mutate "a call deadline rounds down" native_gpu_job_rules.ads \
    "                (if Remaining_Ns mod Nanoseconds_Per_Millisecond = 0 then 0 else 1);" \
    "                0;"
mutate "a faulted context is healthy" native_gpu_job_rules.ads \
    "      State = GQ.Context_State'Enum_Rep (GQ.Unused));" \
    "      State = GQ.Context_State'Enum_Rep (GQ.Unused) or else State = GQ.Context_State'Enum_Rep (GQ.Faulted));"

echo "mutations: caught=$caught survived=$survived stale=$stale"
[ "$survived" -eq 0 ] && [ "$stale" -eq 0 ]
