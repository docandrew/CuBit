#!/usr/bin/env bash
# Mutation check for intel-gpu session and GuC context recycling
# (Linux-hosted). Each mutation breaks one rule in a copy of the driver
# sources; the recycling tests must then fail. A mutation whose text is not
# found, or that does not compile, counts as stale (the check is outdated).
# Run from anywhere inside the Nix shell: bash tests/intel-gpu-recycling/mutations.sh
set -uo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here"
SRC=../../userspace/services/intel-gpu
WORK=$(mktemp -d "${TMPDIR:-/tmp}/gpu-recycling-mutations.XXXXXX")
trap 'rm -rf "$WORK"' EXIT
caught=0; survived=0; stale=0
mutate() {
    local name=$1 file=$2 from=$3 to=$4
    rm -rf "$WORK/src" "$WORK/obj"
    mkdir -p "$WORK/src"
    cp "$SRC"/*.ad? "$WORK/src/"
    if ! python3 - "$WORK/src/$file" "$from" "$to" <<'PY'
import sys
path, old, new = sys.argv[1:4]
text = open(path).read()
if text.count(old) != 1:
    sys.exit(1)
open(path, 'w').write(text.replace(old, new))
PY
    then
        echo "STALE    $name"; stale=$((stale + 1)); return
    fi
    if ! RECYCLING_SRC="$WORK/src" RECYCLING_OBJ="$WORK/obj" \
         gprbuild -p -q -P recycling.gpr >"$WORK/build.log" 2>&1; then
        echo "NO-BUILD $name"; stale=$((stale + 1)); return
    fi
    local failed=0
    for t in session_lifecycle_tests context_recycling_tests slot_storage_tests \
             client_account_tests render_sessions_tests render_control_tests; do
        if ! "$WORK/obj/$t" >"$WORK/run-$t.log" 2>&1; then failed=1; break; fi
    done
    if [[ $failed -eq 0 ]]; then
        t=test-recycle-glue.py
        if ! RECYCLING_SRC="$WORK/src" python3 test-recycle-glue.py >"$WORK/run-glue.log" 2>&1; then
            failed=1
        fi
    fi
    if [[ $failed -eq 0 ]]; then
        echo "SURVIVED $name"; survived=$((survived + 1))
    else
        echo "caught   $name ($t)"; caught=$((caught + 1))
    fi
}
R=intel_gpu_render_sessions.adb
mutate "recycle ignores what the session still holds" $R \
    "        Object.Items (Index).Held /= No_Holdings
      then return; end if;
      Object.Items (Index) := (others => <>);" \
    "        False
      then return; end if;
      Object.Items (Index) := (others => <>);"
mutate "recycle frees a session that is still admitted" $R \
    "not Live (Object, Index) or else Object.Items (Index).State /= Closed or else" \
    "not Live (Object, Index) or else"
mutate "recycle ignores the release facts" $R \
    "if Object.Failed or else not All_Released (Facts) or else" \
    "if Object.Failed or else"
mutate "recycle while quarantined" $R \
    "if Object.Failed or else not All_Released (Facts) or else" \
    "if not All_Released (Facts) or else"
mutate "image release also drops the GuC context hold" $R \
    "         Object.Items (Index).Held (Context_Image) := False;" \
    "         Object.Items (Index).Held := No_Holdings;"
mutate "reservation takes a slot in use" $R \
    "         if Object.Items (Index).State = Free then return Index; end if;" \
    "         if Object.Items (Index).State /= Active then return Index; end if;"
mutate "tags repeat after recycling" $R \
    "      Object.Issued := Object.Issued + 1;" \
    "      Object.Issued := 1;"
mutate "a recycled tag still resolves" $R \
    "         Object.Items (Tag_Slot (Tag)).Tag = Tag" \
    "         True"
C=intel_gpu_context_table.adb
mutate "context ID released before DEREGISTER_DONE" $C \
    "      Driver.State (Object.Items (Index (ID)).Item) in Life.Deregistered | Life.Ready | Life.Fresh);" \
    "      Driver.State (Object.Items (Index (ID)).Item) in Life.Deregistered | Life.Ready | Life.Fresh | Life.Disabled | Life.Deregister_Pending);"
mutate "release of an unretired context" $C \
    "      Object.Items (Index (ID)).Retired and then not Object.Items (Index (ID)).Work_Held and then" \
    "      not Object.Items (Index (ID)).Work_Held and then"
mutate "MODE_SET sent without reply credit" $C \
    "         if Action in Life.Enable | Life.Disable and then
           Credits.Available (Object.Credits) < Answer_Words" \
    "         if False and then
           Credits.Available (Object.Credits) < Answer_Words"
mutate "credit accounting skipped" $C \
    "      Credits.Exchange (Object.Credits, Item.Charged, Held, Accepted);" \
    "      Accepted := True;"
mutate "a reused ID keeps the old context's state" $C \
    "      if Accepted then Initialize (Object, Index (ID)); end if;" \
    "      if Accepted then Object.Items (Index (ID)).Opened := False; end if;"
B=intel_gpu_client_budgets.adb
mutate "a charged account is forgotten" $B \
    "      if not Node.Closed or else Node.Charged /= 0 then return; end if;" \
    "      if not Node.Closed then return; end if;"
mutate "forget leaves the last record behind" $B \
    "      if Freed /= Object.Count then" \
    "      if False then"
S=intel_gpu_session_store.adb
mutate "new slots keep leftover bytes" $S \
    "      Memory := [others => 0];
      declare" \
    "      declare"
mutate "new slots lack their defaults" $S \
    "         Fresh : Element with Address => Where;" \
    "         Fresh : Element with Import, Address => Where;"
mutate "growth counted though the commit failed" $S \
    "      Storage.Commit_Through (Region, Elements * Unsigned_64 (Element_Bytes), OK);
      if not OK then return; end if;" \
    "      Storage.Commit_Through (Region, Elements * Unsigned_64 (Element_Bytes), OK);
      OK := True;"
K=intel_gpu_render_control.adb
mutate "close-own of a retired session is denied" $K \
    "        (Resolve_Retired (Object, Sender, Stamped_Tag) /= 0 or else" \
    "        (False or else"
mutate "anyone may close a recycled session's tag" $K \
    "     (Sessions.Was_Issued (Object.Sessions, Stamped_Tag) and then" \
    "     (True and then"
M=main.adb
mutate "recycling assumes deregistration without the GuC's answer" $M \
    "      if Context_Retirement (Session) = Context_Drain.Deregistered then
         Intel_GPU_Render_Control.Acknowledge_Deregistration (Render_Admission, S);" \
    "      if True then
         Intel_GPU_Render_Control.Acknowledge_Deregistration (Render_Admission, S);"
mutate "recycling ignores the image's GGTT claim" $M \
    "      if Image_Retirement_Results (S) = Image_Retirement.Address_Released then
         Intel_GPU_Render_Control.Release_Image (Render_Admission, S);" \
    "      if True then
         Intel_GPU_Render_Control.Release_Image (Render_Admission, S);"
mutate "recycling ignores outstanding backing" $M \
    "        (Backing_Released => not Private_Contexts (S).Parent.Ready and then
           (not Usage.Known or else Usage.Charged = 0)," \
    "        (Backing_Released => True,"
mutate "a dead client's session is never retired" $M \
    "            End_Client_Session (S, \"queue grant ended\");" \
    "            null;"
mutate "recycling releases before the session table frees the slot" $M \
    "      Intel_GPU_Render_Control.Recycle (Render_Admission, S, Facts, Accepted);
      if not Accepted then" \
    "      Accepted := True;
      if not Accepted then"
echo "Recycling mutations: $caught caught, $survived survived, $stale stale"
[[ $survived -eq 0 && $stale -eq 0 ]]
