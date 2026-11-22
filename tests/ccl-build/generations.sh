#!/usr/bin/env bash
# Generations and garbage collection (docs/ccl-build.md, "Generations and
# garbage collection"), hosted, with real processes:
#   - generations: numbered, immutable, an unchanged build adds none; switch,
#     rollback (history kept), diff by identity, refusal of a generation that
#     does not exist and of malformed profile names and numbers;
#   - gc: --dry-run deletes nothing; gc deletes exactly what no root reaches;
#     --keep-generations; a removed checkout's roots die; damaged/ emptied;
#   - the lock protocol: gc waits while a build runs and keeps what it made;
#     builds and gc in a loop at once never lose an entry a generation names;
#   - crashes: a kill at every system call of gc and of switch leaves a
#     consistent store (crash.sh).
# The builds are store_driver's three-output step; the commands are
# ccl-build's. Run inside `nix develop` with TMPDIR set:
#   bash tests/ccl-build/generations.sh [TOOL]
set -euo pipefail

here=$(cd "$(dirname "$0")" && pwd)
repo=$(cd "$here/../.." && pwd)
tool=${1:-$repo/userspace/ccl/build/ccl-build/ccl-build}
driver=$here/build/store_driver
: "${TMPDIR:?set TMPDIR outside /tmp}"
scratch=$(mktemp -d "$TMPDIR/ccl-build-generations.XXXXXX")
pids=()
cleanup() {
    for p in "${pids[@]}"; do kill "$p" 2>/dev/null || true; done
    chmod -R u+w "$scratch" 2>/dev/null || true; rm -rf "$scratch"
}
trap cleanup EXIT

passed=0; failed=0
check() {  # check NAME COMMAND...
    local name=$1; shift
    if "$@"; then passed=$((passed + 1)); echo "PASS $name"
    else failed=$((failed + 1)); echo "FAIL $name"; fi
}
fails() { ! "$@"; }
store=$scratch/store
sources=$scratch/sources
mkdir -p "$sources"
printf '#!/bin/sh\n/bin/sleep "$1"\n/bin/cat in.txt > a.txt\n/bin/cat in.txt in.txt > b.txt\necho done > c.txt\n' >"$sources/three.sh"
chmod +x "$sources/three.sh"
gen() { echo "$1" >"$sources/in.txt"; "$driver" generation "$store" "$sources" sys "${2:-0}" | tail -1; }
cb() { "$tool" "$@" --store "$store"; }
current() { cb generations --profile sys | awk '$1 == "*" {print $2}'; }
entries() { ls "$store/files" | wc -l; }
consistent() { "$driver" check "$store" >"$scratch/check.out" 2>&1 || { cat "$scratch/check.out"; false; }; }
sha() { printf '%s' "$1" | sha256sum | cut -d' ' -f1; }
out_of() { sha "$1"$'\n'; }   # the a.txt output of input text $1 is the text and a newline

echo "== generations"
check "first generation" test "$(gen one)" = "generation 1"
check "an unchanged build adds no generation" test "$(gen one)" = "unchanged 1"
check "second generation" test "$(gen two)" = "generation 2"
check "third generation" test "$(gen three)" = "generation 3"
cb generations --profile sys | tee "$scratch/list"
check "three generations listed" test "$(grep -c ' components' "$scratch/list")" = 3
check "the newest is current" test "$(current)" = 3
check "rollback" cb rollback --profile sys
check "rollback goes to 2" test "$(current)" = 2
check "rollback again" cb rollback --profile sys
check "... to 1" test "$(current)" = 1
check "nothing before 1" fails cb rollback --profile sys
check "history kept: switch forward to 3" cb switch --profile sys 3
check "switched to 3" test "$(current)" = 3
check "a missing generation is refused" fails cb switch --profile sys 9
check "a malformed number is refused" fails cb switch --profile sys 03
check "a malformed profile name is refused" fails cb generations --profile ../etc
check "a profile name with a slash is refused" fails cb generations --profile a/b
cb diff --profile sys 1 3 | tee "$scratch/diff"
check "diff names the changed parameter" grep -q '~ in.txt' "$scratch/diff"
check "diff names the changed output" grep -q "~ work/a.txt  $(out_of one | cut -c1-16) -> $(out_of three | cut -c1-16)" "$scratch/diff"
check "diff: c.txt is unchanged" fails grep -q 'c.txt' "$scratch/diff"
check "the store is consistent" consistent

echo "== garbage collection"
echo garbage >"$sources/in.txt"
"$driver" step "$store" "$sources" 0 >/dev/null     # outputs no root keeps
before=$(entries)
cb gc --dry-run | tee "$scratch/dry"
check "dry run deletes nothing" test "$(entries)" = "$before"
check "dry run names the garbage outputs" grep -q "would delete REGULAR_FILE $(sha $'garbage\ngarbage\n')" "$scratch/dry"
check "dry run keeps generation outputs" fails grep -q "$(out_of one)" "$scratch/dry"
cb gc | tee "$scratch/gc"
check "gc deletes what the dry run named" test "$(grep -c '^delete ' "$scratch/gc")" = "$(grep -c '^would delete ' "$scratch/dry")"
check "gc freed the garbage" test ! -e "$store/files/$(sha $'garbage\ngarbage\n')"
check "every generation is still whole" consistent
for n in 1 2 3; do check "switch to $n after gc" cb switch --profile sys "$n"; done
check "gc --keep-generations 1" cb gc --keep-generations 1
check "only the current generation is left" test "$(cb generations --profile sys | grep -c ' components')" = 1
check "generation 1's outputs are gone" test ! -e "$store/files/$(out_of one)"
check "the current generation's outputs stay" test -e "$store/files/$(out_of three)"
check "a deleted generation cannot be switched to" fails cb switch --profile sys 1
check "consistent after keep" consistent

echo "== checkout roots die with their checkout"
checkout=$scratch/checkout
mkdir -p "$checkout"
echo rooted >"$sources/in.txt"
"$driver" root "$store" "$sources" "$checkout" >/dev/null
cb gc >/dev/null
check "a live checkout's root keeps its outputs" test -e "$store/files/$(out_of rooted)"
rmdir "$checkout"
cb gc | tee "$scratch/gc-dead"
check "a removed checkout's outputs are deleted" test ! -e "$store/files/$(out_of rooted)"
check "and its root directory" test -z "$(ls "$store/roots/checkouts")"

echo "== damaged entries"
echo damaged >"$sources/in.txt"
gen damaged >/dev/null
victim=$store/files/$(sha $'damaged\ndamaged\n')   # b.txt: not also an input
chmod u+w "$victim"; echo tampered >"$victim"
check "the tampered entry is found by verify" fails "$tool" verify --store "$store"
"$driver" generation "$store" "$sources" sys 0 >"$scratch/heal.out" 2>&1
check "using it moves it to damaged/ and makes it again" grep -q 'damaged: .* does not match its identity' "$scratch/heal.out"
check "the entry is whole again" test "$(sha256sum <"$victim" | cut -d' ' -f1)" = "$(sha $'damaged\ndamaged\n')"
check "the step ran again" grep -q '^ran ' "$scratch/heal.out"
check "verify passes" "$tool" verify --store "$store"
check "gc empties damaged/" bash -c "$(printf '%q ' "$tool") gc --store '$store' | grep -q '1 damaged entries removed'"

echo "== gc refuses while an older ccl-build (no collection lock) uses the store"
sleep 60 &
older=$!; pids+=("$older")
mkdir "$store/work/$older-1"          # its staging, as an older ccl-build names it
check "gc refuses" fails cb gc
check "gc names the process" bash -c "$(printf '%q ' "$tool") gc --store '$store' 2>&1 | grep -q 'older ccl-builds) are using the store: $older'"
kill "$older"; wait "$older" 2>/dev/null || true
check "gc runs once it is gone (and removes its work)" cb gc
check "the dead process's work is gone" test ! -e "$store/work/$older-1"

echo "== gc waits for a running build and keeps what it made"
echo slow >"$sources/in.txt"
"$driver" generation "$store" "$sources" sys 3 >"$scratch/slow.out" 2>&1 &
slow=$!; pids+=("$slow")
sleep 1
cb gc >"$scratch/gc-wait.out" 2>&1 &
collector=$!; pids+=("$collector")
slow_status=0; wait "$slow" || slow_status=$?
gc_status=0; wait "$collector" || gc_status=$?
check "the build succeeded" test "$slow_status" = 0
check "gc succeeded" test "$gc_status" = 0
check "gc said it waited" grep -q 'waiting for the builds' "$scratch/gc-wait.out"
check "the build's generation is whole" consistent
check "its outputs are there" test -e "$store/files/$(out_of slow)"

echo "== builds and gc at once, in a loop"
race=$scratch/race
mkdir -p "$race"
(
    for i in $(seq 1 20); do
        mkdir -p "$race/$i"; cp "$sources/three.sh" "$race/$i/"; echo "race $i" >"$race/$i/in.txt"
        "$driver" generation "$store" "$race/$i" sys 0 >>"$race/builds.out" 2>&1 || echo "BUILD FAILED $i" >>"$race/builds.out"
    done
) &
builder=$!; pids+=("$builder")
(
    for i in $(seq 1 20); do
        "$tool" gc --store "$store" --keep-generations 2 >>"$race/gc.out" 2>&1 || echo "GC FAILED $i" >>"$race/gc.out"
    done
) &
collector=$!; pids+=("$collector")
wait "$builder"; wait "$collector"
check "no build failed" fails grep -q 'FAILED\|not in the store' "$race/builds.out"
check "no gc failed" fails grep -q 'FAILED' "$race/gc.out"
check "consistent after the race" consistent
check "the newest generation is current" test "$(current)" = "$(cb generations --profile sys | tail -1 | awk '{print $2}')"

echo "== crashes in gc and in switch (a kill at every system call)"
setup_cmd="cd $(printf '%q' "$sources") && for t in one two three; do echo \$t > in.txt; $(printf '%q' "$driver") generation \"\$STORE\" . sys 0; done; echo junk > in.txt; $(printf '%q' "$driver") step \"\$STORE\" . 0"
check "gc --keep-generations 1, killed anywhere" env CRASH_CHECK=collected CRASH_SETUP="$setup_cmd" \
    bash "$here/crash.sh" tool "$tool" "$scratch/crash-gc" -- gc --store @STORE@ --keep-generations 1
check "switch, killed anywhere" env CRASH_SETUP="$setup_cmd" \
    bash "$here/crash.sh" tool "$tool" "$scratch/crash-switch" -- switch --store @STORE@ --profile sys 1

echo "generations tests: $passed passed, $failed failed"
[[ $failed == 0 ]]
