#!/usr/bin/env bash
# Mutation check for the notification proofs: each mutant must FAIL to prove.
# Run from the Nix shell: tests/notifications/mutations.sh [kernel-src-dir]
set -u
here=$(cd "$(dirname "$0")" && pwd)
ksrc=${1:-$here/../../kernel/src}
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
cd "$here/../../kernel"   # alr needs the kernel's workspace

mutate() {  # name file sed-expression
    local name=$1 file=$2 expr=$3 dir="$work/$1"
    mkdir -p "$dir/src"
    cp "$ksrc"/notification_objects.ad? "$here"/fan_in_model.ad? "$here"/main.adb "$dir/src/"
    sed -i "$expr" "$dir/src/$file"
    if cmp -s "$dir/src/$file" "$ksrc/$file" 2>/dev/null || cmp -s "$dir/src/$file" "$here/$file" 2>/dev/null; then
        echo "MUTANT $name: NOT APPLIED"; return 1
    fi
    sed "s|(\".\", Kernel_Src)|(\"$dir/src\")|; s|for Object_Dir use \"build\"|for Object_Dir use \"$dir/build\"|; s|for Exec_Dir use \"build\"|for Exec_Dir use \"$dir/build\"|" \
        "$here/notifications_tests.gpr" > "$dir/t.gpr"
    if alr exec -- gnatprove -P"$dir/t.gpr" -u notification_objects.adb fan_in_model.adb \
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
# Wake overwrites the bits with the badge instead of ORing it in.
mutate no-or notification_objects.adb 's/N.Bits := N.Bits or Badge;/N.Bits := Badge;/' || fail=1
# A wait that takes the bits but does not clear them.
mutate no-clear notification_objects.adb '/Outcome := Got;/,/N.Bits := 0;/ s/N.Bits := 0;/null;/' || fail=1
# A second waiter replaces the first (no Busy).
mutate steal-waiter notification_objects.adb 's/elsif N.Waiter \/= No_Thread or else N.Root.Id \/= No_Object then/elsif N.Root.Id \/= No_Object then/' || fail=1
# A bound leaf may have a blocking waiter.
mutate leaf-waiter notification_objects.adb 's/elsif N.Waiter \/= No_Thread or else N.Root.Id \/= No_Object then/elsif N.Waiter \/= No_Thread then/' || fail=1
# The second level forwards again (a chain longer than two).
mutate second-forwards notification_objects.adb 's/if At_Level = First and then N.Root.Id \/= No_Object then/if N.Root.Id \/= No_Object then/' || fail=1
# A root may become a leaf (three levels, cycles).
mutate root-as-leaf notification_objects.adb 's/if L_Id = R_Link.Id or else Badge = 0 or else L.Is_Root or else Is_Leaf (R) then/if L_Id = R_Link.Id or else Badge = 0 or else Is_Leaf (R) then/' || fail=1
# A leaf may become a root.
mutate leaf-as-root notification_objects.adb 's/if L_Id = R_Link.Id or else Badge = 0 or else L.Is_Root or else Is_Leaf (R) then/if L_Id = R_Link.Id or else Badge = 0 or else L.Is_Root then/' || fail=1
# Binding does not mark the root (a later bind could make it a leaf).
mutate unmarked-root notification_objects.adb '/procedure Bind_Leaf/,/end Bind_Leaf;/ s/R.Is_Root := True;/null;/' || fail=1
# Bits pending at binding are not forwarded (a lost wake).
mutate bind-drops-bits notification_objects.adb 's/Signal (R, Badge, Second, Told);/null;/' || fail=1
# A wake with a waiter rings the bound thread instead of waking the waiter.
mutate bound-before-waiter notification_objects.adb 's/elsif N.Waiter \/= No_Thread then/elsif False then/' || fail=1
# The model's end does not advance the generation (a stale link revives).
mutate no-generation fan_in_model.adb 's/S.G (I) := S.G (I) + 1;/null;/' || fail=1
# A leaf may be bound to a thread.
mutate leaf-to-thread notification_objects.adb '/procedure Bind_Thread/,/end Bind_Thread;/ s/if Is_Leaf (R) then/if False then/' || fail=1
# Revoke that does not advance the generation (an old capability still works).
mutate revoke-keeps-generation fan_in_model.adb '/procedure Revoke (S/,/end Revoke;/ s/S.G (I) := S.G (I) + 1;/null;/' || fail=1
# A Wake that does not check the capability's generation.
mutate wake-ignores-generation fan_in_model.adb '/procedure Wake_With/,/end Wake_With;/ s/Done := Authorizes (S, I, Gen);/Done := S.O (I).Live;/' || fail=1
# Revoke that leaves the stale waiter waiting.
mutate revoke-keeps-waiter notification_objects.adb '/procedure Revoke (N/,/end Revoke;/ s/N.Waiter := No_Thread;/null;/' || fail=1
# A leaf link names the root's life, which Revoke keeps (review 4: the
# revoke bypass; an old Wake right's binding outlives the revocation).
mutate link-names-life fan_in_model.adb 's/(Id => R, Gen => S.G (R))/(Id => R, Gen => S.L (R))/' || fail=1
# A Bind_Leaf that does not check the root right's generation.
mutate bind-ignores-generation fan_in_model.adb '/procedure Bind_With/,/end Bind_With;/ s/Done := Authorizes (S, R, Gen);/Done := S.O (R).Live;/' || fail=1
# A wake of a leaf whose root was revoked or ended leaves it bound.
mutate wake-keeps-stale-root fan_in_model.adb '/The root was revoked or ended: the leaf is unbound/,+1 s/Unbind_Leaf (S.O (I));/null;/' || fail=1
exit $fail
