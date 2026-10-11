#!/usr/bin/env bash
# DISP-000a mutation checks: each mutant of the flip rule must fail the
# hosted register-model test, both with contracts checked (-gnata) and
# with contracts off (the model alone: writes_to_live, lost frames).
set -uo pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$HERE/../.." && pwd)"
SRC="$ROOT/userspace/services/intel-gpu"
WORK="$(mktemp -d "${TMPDIR:-/tmp}/primary-flip-mut.XXXXXX")"
trap 'rm -rf "$WORK"' EXIT
mutate() { # name file python-replace-old python-replace-new
  local name=$1 file=$2 old=$3 new=$4
  for mode in contracts model; do
    rm -rf "$WORK/src" "$WORK/obj"; mkdir -p "$WORK/src" "$WORK/obj"
    cp "$SRC"/intel_gpu_flip_state.ad? "$SRC"/intel_gpu_vblank_clock.ad? \
       "$SRC"/intel_gpu_primary_flip.ad? "$HERE/primary_flip_tests.adb" "$WORK/src/"
    OLD="$old" NEW="$new" python3 - "$WORK/src/$file" <<'PY'
import os,sys
p=sys.argv[1]; s=open(p).read(); o=os.environ['OLD']; n=os.environ['NEW']
assert s.count(o)>=1, 'mutation anchor missing: '+o
open(p,'w').write(s.replace(o,n,1))
PY
    flags="-gnat2022 -O1"; [ $mode = contracts ] && flags="$flags -gnata"
    if (cd "$WORK/obj" && gnatmake -q $flags -I"$WORK/src" "$WORK/src/primary_flip_tests.adb" -o t >/dev/null 2>&1 && ./t >/dev/null 2>&1); then
      echo "SURVIVED $name ($mode)"; status=1
    else echo "killed $name ($mode)"; fi
  done
}
status=0
# Baseline must pass in both modes, or every kill is meaningless.
mutate baseline intel_gpu_flip_state.ads "package Intel_GPU_Flip_State" "package Intel_GPU_Flip_State" | sed 's/killed/BASELINE FAILED/; s/SURVIVED/baseline ok/'
mutate ignore-maybe intel_gpu_flip_state.ads \
  "(S.Armed = B or else S.Seen = B or else S.Maybe (B));" "(S.Armed = B or else S.Seen = B);"
mutate no-writable-check intel_gpu_primary_flip.adb \
  "      if not FS.Writable (S, To_Buffer (Item)) then" "      if False then"
mutate latch-on-any-change intel_gpu_flip_state.adb \
  "Latched := S.Owed and then Live = S.Armed;" "Latched := S.Owed and then Live /= S.Seen;"
mutate drop-supersede intel_gpu_flip_state.adb \
  "      Superseded := S.Owed;
      if S.Armed in Buffer and then S.Armed /= S.Seen then
         S.Maybe (S.Armed) := True;
      end if;
      S.Armed := B;" "      Superseded := False;
      if S.Armed in Buffer and then S.Armed /= S.Seen then
         S.Maybe (S.Armed) := True;
      end if;
      S.Armed := B;"
mutate clear-fills-live intel_gpu_primary_flip.adb \
  "      if not FS.Writable (S, To_Buffer (First)) then
         if not" "      if False then
         if not"
mutate zero-surface-sentinel intel_gpu_primary_flip.adb \
  "      Problem := Ready_To_Flip;
      if (Firmware_Surface" "      Problem := (if Firmware_Surface = 0 then Not_Called else Ready_To_Flip);
      if (Firmware_Surface"
exit $status
