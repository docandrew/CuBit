#!/usr/bin/env bash
#  Mutation checks for the channel core: each mutation of a production unit
#  must make the hosted differential test fail. Run in the Nix shell.
set -uo pipefail
here=$(cd "$(dirname "$0")" && pwd)
root="$here/../.."
rt="$root/userspace/runtime/gnat"
work=$(mktemp -d "${TMPDIR:-/home/doc/cubit-build-tmp}/channel-core-mutations.XXXXXX")
trap 'rm -rf "$work"' EXIT
mutate() {
    local name=$1 file=$2 from=$3 to=$4
    rm -rf "$work/src" "$work/build"; mkdir -p "$work/src"
    #  Every source the hosted test builds (channel_core.gpr's list).
    for f in $(python3 -c 'import re,sys; t=open(sys.argv[1]).read(); print(" ".join(re.findall(r"\"([a-z_.-]+\.ad[sb])\"", t.split("Source_Files")[1].split(";")[0])))' "$here/channel_core.gpr"); do
        if [[ -f "$here/$f" ]]; then cp "$here/$f" "$work/src/"; else cp "$rt/$f" "$work/src/"; fi
    done
    python3 - "$work/src/$file" "$from" "$to" <<'PY' || { echo "MUTATION $name: pattern not found"; return 2; }
import sys
p, a, b = sys.argv[1:]
s = open(p).read()
if a not in s: sys.exit(1)
open(p, 'w').write(s.replace(a, b, 1))
PY
    cat > "$work/m.gpr" <<GPR
project M is
   for Source_Dirs use ("src");
   for Object_Dir use "build/obj"; for Exec_Dir use "build"; for Main use ("main.adb");
   package Compiler is for Default_Switches ("Ada") use ("-O1", "-gnat2022", "-gnata", "-gnato"); end Compiler;
end M;
GPR
    (cd "$root/kernel" && alr exec -- gprbuild -p -q -P "$work/m.gpr") >/dev/null 2>&1 || { echo "MUTATION $name: build failed (caught by compiler)"; return 0; }
    if "$work/build/main" > "$work/out" 2>&1; then
        echo "MUTATION $name: NOT CAUGHT"; return 1
    fi
    echo "MUTATION $name: caught ($(tail -1 "$work/out" | cut -c1-110))"
}
fail=0
mutate admit-no-per-client cubit-channel_admission.adb "         or else not Within (S.Declared (Offer).Per_Client, S.Clients (Found).Counts (Offer) + 1))" "         or else False)" || fail=1
mutate admit-no-total cubit-channel_admission.adb "        or else not Within (S.Declared (Offer).Total, S.Totals (Offer) + 1)" "        or else False" || fail=1
mutate release-keeps-total cubit-channel_admission.adb "         S.Totals (Offer) := S.Totals (Offer) - 1;" "         null;" || fail=1
mutate schedule-lowest-first cubit-channel_schedule.adb "      for C in Class loop" "      for C in reverse Class loop" || fail=1
mutate schedule-ignores-served cubit-channel_schedule.ads "(S.Active (I) and then S.Ready (I) and then not S.Served (I))" "(S.Active (I) and then S.Ready (I))" || fail=1
mutate tags-reuse cubit-request_tags.adb "            T.Next_Tag := T.Next_Tag + 1;" "            null;" || fail=1
mutate tags-duplicate-accepted cubit-request_tags.adb "            if T.Entries (I).State = Waiting then" "            if True then" || fail=1
mutate tags-fail-misses cubit-request_tags.adb "            T.Entries (I).State := Ended;" "            null;" || fail=1
mutate bells-take-any cubit-bell_sets.adb "            Was_Rung := Was_Rung or else S (B) (I).Rung;" "            Was_Rung := True;" || fail=1
mutate bells-note-other cubit-bell_sets.adb "         if S (B) (I).Used and then S (B) (I).Ref = Ref then
            S (B) (I).Rung := True;" "         if S (B) (I).Used then
            S (B) (I).Rung := True;" || fail=1
mutate rc-never-rings cubit-request_channels.adb "      Ring := Armed /= 0 and then Armed /= Last;" "      Ring := False;" || fail=1
mutate rc-arm-no-recheck cubit-request_channels.adb "      if Can_Take (S) then
         Disarm_For_Requests (S);
         return False;" "      if False then
         Disarm_For_Requests (S);
         return False;" || fail=1
mutate rc-take-without-slot cubit-request_channels.adb "      return Queues.Can_Take (S.Ring);" "      return S.Ring.Requests.Available > 0;" || fail=1
exit $fail
