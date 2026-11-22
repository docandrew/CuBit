#!/usr/bin/env bash
# Crash consistency of the Storehouse (docs/ccl-build.md, "The Storehouse"):
# kill a process at EVERY system call that changes the store, then check that
# the store holds whole entries or nothing, that recovery cleans up after the
# dead process, and that the work then completes with the same results.
#
# The kill is real: strace delivers SIGKILL on entry to the Nth call of one
# system call (`-e inject=CALL:signal=KILL:when=N`), so the process dies with
# the call not made and everything before it done. Every call that changes
# the store is a kill point (claim, stage, write, fsync, chmod, rename,
# unlink, ...); together they cover every state a crash can leave.
#
#   crash.sh step  DRIVER SCRATCH             a step with three outputs (store_driver)
#   crash.sh tool  TOOL SCRATCH -- ARGS...    any ccl-build command; in ARGS, @STORE@ is
#                                             the store and @OUT@ an export directory,
#                                             compared with the reference run's
#
# Optional: CRASH_SETUP is a bash command run before each run, with STORE set
# to the store (to fill it first); CRASH_CALLS="rename renameat2" limits the
# kill points to those calls; CRASH_SAMPLE=N kills at about N points spread evenly
# over a call that is made more often (the first and last included). Run inside `nix develop` with TMPDIR set (strace is the host's).
# Prints one line per kill point that fails, then a summary; exits 1 if any
# failed.
set -euo pipefail

mode=$1; program=$2; scratch=$3; shift 3
[[ ${1:-} == -- ]] && shift
ARGS=("$@")
command -v strace >/dev/null || { echo "crash.sh: strace is needed"; exit 1; }
here=$(cd "$(dirname "$0")" && pwd)
driver=$here/build/store_driver
mkdir -p "$scratch"

# The system calls that change a store.
mutating='^(openat|open|creat|write|pwrite64|fsync|fdatasync|chmod|fchmod|fchmodat|utimensat|link|linkat|symlink|symlinkat|rename|renameat|renameat2|unlink|unlinkat|mkdir|mkdirat|rmdir|flock|ftruncate)$'

sources=$scratch/sources
mkdir -p "$sources"
printf '#!/bin/sh\n/bin/sleep "$1"\n/bin/cat in.txt > a.txt\n/bin/cat in.txt in.txt > b.txt\necho done > c.txt\n' >"$sources/three.sh"
chmod +x "$sources/three.sh"
echo "the step's input" >"$sources/in.txt"

work() {  # work STORE [strace args...]: the command line under test
    local store=$1; shift
    case $mode in
        step) "$@" "$program" step "$store" "$sources" 0 ;;
        tool)
            local args=("${ARGS[@]//@STORE@/$store}")
            "$@" "$program" "${args[@]//@OUT@/$store.out}" ;;
    esac
}
results() { grep -E '^(ran|hit) ' "$1" | sed -E 's/^(ran|hit) /done /' || true; }
wipe() { chmod -R u+w "$1" "$1.out" 2>/dev/null || true; rm -rf "$1" "$1.out"; }
entries() { (cd "$1" && ls files executables trees 2>/dev/null) || true; }
setup() { if [[ -n ${CRASH_SETUP:-} ]]; then STORE=$1 bash -c "$CRASH_SETUP" >/dev/null; fi; }

# The reference: one run without a crash, traced to find the kill points.
# It runs in the store path every crashed run uses: the store's path reaches
# step directories, which tools record (.ali files), so another path would
# give other step identities downstream.
reference=$scratch/reference-store
wipe "$reference"; wipe "$scratch/store"; setup "$scratch/store"
work "$scratch/store" strace -qq -o "$scratch/trace" -e trace=all >"$scratch/reference.out"
mv "$scratch/store" "$reference"
if [[ -d $scratch/store.out ]]; then mv "$scratch/store.out" "$reference.out"; fi
results "$scratch/reference.out" >"$scratch/reference.result"
calls=$(sed -E 's/^([a-z0-9_]+)\(.*/\1/' "$scratch/trace" | grep -E "$mutating" | sort | uniq -c | awk '{print $2 ":" $1}')
if [[ -n ${CRASH_CALLS:-} ]]; then
    calls=$(for entry in $calls; do for c in $CRASH_CALLS; do if [[ ${entry%%:*} == "$c" ]]; then echo "$entry"; fi; done; done)
fi
[[ -n $calls ]] || { echo "crash.sh: no kill points found"; exit 1; }
echo "kill points: $(echo $calls)"

points=0; failures=0
fail() { failures=$((failures + 1)); echo "FAIL $*"; }
for entry in $calls; do
    call=${entry%%:*}; count=${entry##*:}
    points_of() {  # every call, or with CRASH_SAMPLE=N about N spread evenly, the last included
        if [[ -z ${CRASH_SAMPLE:-} || $count -le $CRASH_SAMPLE ]]; then seq 1 "$count"
        else (seq 1 $(( (count + CRASH_SAMPLE - 1) / CRASH_SAMPLE )) "$count"; echo "$count") | sort -nu; fi
    }
    for n in $(points_of); do
        points=$((points + 1))
        store=$scratch/store
        wipe "$store"; setup "$store"
        status=0
        work "$store" strace -qq -o /dev/null -e trace="$call" -e inject="$call":signal=KILL:when="$n" \
            >"$scratch/killed.out" 2>&1 || status=$?
        # Whatever the kill left: whole entries or nothing, no record naming
        # a missing or damaged output.
        if [[ -d $store ]] && ! "$driver" check "$store" ${CRASH_CHECK-complete} >"$scratch/check.out" 2>&1; then
            fail "$call #$n: after the kill: $(head -3 "$scratch/check.out" | tr '\n' ' ')"
            continue
        fi
        # Recovery: the work completes, with the reference's results.
        if ! work "$store" >"$scratch/again.out" 2>&1; then
            fail "$call #$n (exit $status): the run after the crash failed: $(tail -3 "$scratch/again.out" | tr '\n' ' ')"
            continue
        fi
        results "$scratch/again.out" >"$scratch/again.result"
        if ! cmp -s "$scratch/again.result" "$scratch/reference.result"; then
            fail "$call #$n: results differ from the reference: $(diff "$scratch/reference.result" "$scratch/again.result" | head -3 | tr '\n' ' ')"
            continue
        fi
        if [[ $(entries "$store") != $(entries "$reference") ]]; then
            fail "$call #$n: the store's entries differ from the reference run's"
            continue
        fi
        if [[ -d $reference.out ]] && ! diff -r "$reference.out" "$store.out" >/dev/null; then
            fail "$call #$n: exported outputs differ from the reference"
            continue
        fi
        if ! "$driver" check "$store" ${CRASH_CHECK-complete} >"$scratch/check.out" 2>&1; then
            fail "$call #$n: after recovery: $(head -3 "$scratch/check.out" | tr '\n' ' ')"
            continue
        fi
        # The dead process's staging and claims are gone: the recovering
        # run removed them and its own.
        left=$( (ls -A "$store/work"; ls -A "$store/locks" | grep -vx collection) 2>/dev/null || true)
        if [[ -n $left ]]; then
            fail "$call #$n: left after recovery: $(echo $left)"
        fi
    done
done
wipe "$scratch/store"
echo "crash.sh $mode: $points kill points, $failures failed"
[[ $failures == 0 ]]
