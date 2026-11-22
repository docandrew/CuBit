#!/usr/bin/env bash
# Concurrency of the Storehouse (docs/ccl-build.md, "The Storehouse"): real
# processes at once on one store.
#
#   race.sh step DRIVER SCRATCH N
#       N processes run the same three-output step (store_driver, which
#       sleeps first so they overlap): exactly one runs it, the others wait
#       for it and are answered by its action record; one entry per output.
#   race.sh tool TOOL SCRATCH N -- ARGS...
#       N ccl-build processes run the same command (in ARGS, @STORE@ is the
#       shared store and @OUT@ each process's own export directory): all
#       succeed, each step runs once in all, and the exports are identical.
#
# Afterwards the store is consistent (store_driver check) and holds no work,
# claims or step locks. Run inside `nix develop` with TMPDIR set.
set -euo pipefail

mode=$1; program=$2; scratch=$3; count=$4; shift 4
[[ ${1:-} == -- ]] && shift
ARGS=("$@")
here=$(cd "$(dirname "$0")" && pwd)
driver=$here/build/store_driver
chmod -R u+w "$scratch" 2>/dev/null || true; rm -rf "$scratch"
mkdir -p "$scratch"
store=$scratch/store

sources=$scratch/sources
mkdir -p "$sources"
printf '#!/bin/sh\n/bin/sleep "$1"\n/bin/cat in.txt > a.txt\n/bin/cat in.txt in.txt > b.txt\necho done > c.txt\n' >"$sources/three.sh"
chmod +x "$sources/three.sh"
echo "raced input" >"$sources/in.txt"

failures=0
fail() { failures=$((failures + 1)); echo "FAIL $*"; }

pids=()
for i in $(seq 1 "$count"); do
    case $mode in
        step) "$program" step "$store" "$sources" 2 >"$scratch/$i.out" 2>"$scratch/$i.err" & ;;
        tool)
            args=("${ARGS[@]//@STORE@/$store}")
            "$program" "${args[@]//@OUT@/$scratch/out-$i}" >"$scratch/$i.out" 2>"$scratch/$i.err" & ;;
    esac
    pids+=($!)
done
for i in $(seq 1 "$count"); do
    wait "${pids[$((i - 1))]}" || fail "process $i failed: $(tail -2 "$scratch/$i.err" | tr '\n' ' ')"
done

case $mode in
    step)
        ran=$(grep -c '^ran ' "$scratch"/*.out | awk -F: '{s += $2} END {print s}')
        [[ $ran == 1 ]] || fail "the step ran $ran times, not once"
        [[ $(cut -d' ' -f2- "$scratch"/*.out | sort -u | wc -l) == 1 ]] || fail "the processes saw different outputs"
        # One entry per distinct content: the input and the three outputs
        # (a.txt is a copy of the input, so the same entry).
        expected=$( (sha256sum "$sources/in.txt" | cut -d' ' -f1; cut -d' ' -f2- "$scratch/1.out" | tr ' ' '\n') | sort -u | wc -l)
        [[ $(ls "$store/files" | wc -l) == "$expected" ]] || fail "$(ls "$store/files" | wc -l) file entries, not $expected"
        ;;
    tool)
        # Every step ran in exactly one process.
        dup=$(grep -h '^ran ' "$scratch"/*.out | awk '{print $2}' | sort | uniq -d)
        [[ -z $dup ]] || fail "steps ran more than once: $(echo $dup | head -c 200)"
        for i in $(seq 2 "$count"); do
            diff -r "$scratch/out-1" "$scratch/out-$i" >/dev/null || fail "exports 1 and $i differ"
        done
        ;;
esac
waited=$(grep -l 'waiting for' "$scratch"/*.err 2>/dev/null | wc -l)
"$driver" check "$store" complete >"$scratch/check.out" 2>&1 || fail "inconsistent: $(head -3 "$scratch/check.out")"
left=$( (ls -A "$store/work"; ls -A "$store/locks" | grep -vx collection) 2>/dev/null || true)
[[ -z $left ]] || fail "left in work/ or locks/: $(echo $left)"
echo "race.sh $mode: $count processes ($waited waited for another), $failures failed"
[[ $failures == 0 ]]
