#!/usr/bin/env bash
# Mutation check for the thread-slot proofs: each mutant must FAIL to prove.
# Run from the Nix shell: tests/thread-slots/mutations.sh
set -u
here=$(cd "$(dirname "$0")" && pwd)
ksrc=${1:-$here/../../kernel/src}
shared=$here/../../shared
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
# alr needs the kernel's workspace.
cd "$here/../../kernel"

mutate() {  # name file sed-expression
    local name=$1 file=$2 expr=$3 dir="$work/$1"
    mkdir -p "$dir/src"
    cp "$ksrc"/thread_slots.ad? "$here"/slot_protocol.ad? "$here"/main.adb \
       "$shared"/publications/readiness_publication.ads "$shared"/time/clock_publication.ad? "$dir/src/"
    sed -i "$expr" "$dir/src/$file"
    if cmp -s "$dir/src/$file" "$ksrc/$file" 2>/dev/null || cmp -s "$dir/src/$file" "$here/$file" 2>/dev/null; then
        echo "MUTANT $name: NOT APPLIED"; return 1
    fi
    sed "s|for Object_Dir use \"build\"|for Object_Dir use \"$dir/build\"|; s|for Exec_Dir use \"build\"|for Exec_Dir use \"$dir/build\"|; s|(\".\", Kernel_Src, Shared_Src & \"/publications\", Shared_Src & \"/time\")|(\"$dir/src\")|" \
        "$here/thread_slots_tests.gpr" > "$dir/t.gpr"
    if alr exec -- gnatprove -P"$dir/t.gpr" -u thread_slots.adb slot_protocol.adb \
         --level=2 --timeout=30 -j2 --report=fail >"$dir/log" 2>&1 \
       && ! grep -q "medium\|high" "$dir/log"; then
        echo "MUTANT $name: SURVIVED (proof still passes)"; return 1
    fi
    if ! grep -q "medium\|high" "$dir/log"; then
        echo "MUTANT $name: ERROR (no proof failure reported)"; tail -5 "$dir/log"; return 1
    fi
    echo "MUTANT $name: killed -- $(grep -m1 'medium\|high' "$dir/log")"
}

fail=0
# Take hands out a slot without marking it taken.
mutate take-unmarked thread_slots.adb 's/                Set (T) := True;/                null;/' || fail=1
# Take hands out the last slot even when it is taken.
mutate take-taken thread_slots.adb 's/            if not Set (T) then/            if not Set (T) or else T = Slot'"'"'Last then/' || fail=1
# Give_Back frees nothing: the exiting thread's slot stays taken forever.
mutate give-back-nothing thread_slots.adb 's/        Set (S) := False;/        null;/' || fail=1
# A new thread recorded in the main thread's slot instead of the one it took.
mutate create-main-slot slot_protocol.adb 's/         S.Owner (T) := Slot;/         S.Owner (T) := RP.Main_Thread_Slot;/' || fail=1
# An exit that frees the main thread's slot instead of its own.
mutate exit-wrong-slot slot_protocol.adb 's/      Thread_Slots.Give_Back (S.Slots, S.Owner (T));/      Thread_Slots.Give_Back (S.Slots, S.Owner (Main));/' || fail=1
exit $fail
