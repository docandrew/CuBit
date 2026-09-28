#!/usr/bin/env bash
# Mutation check for the locator proofs: each mutant is a plausible bug;
# gnatprove must fail to prove it (a mutant that proves means a weak
# specification).
set -uo pipefail
here=$(cd "$(dirname "$0")" && pwd)
src=$here/../../userspace/runtime/gnat
mkdir -p "$here/build"
work=$(mktemp -d "$here/build/mutations.XXXXXX")
export TMPDIR="$work/tmp"
mkdir -p "$TMPDIR"
trap 'rm -rf "$work"' EXIT
units="cubit.ads cubit-net_address.ads cubit-locators.ads cubit-locators.adb cubit-net_locator.ads cubit-net_locator.adb"
killed=0; total=0
mutant() {   # name file old new
    total=$((total + 1))
    rm -rf "$work/src" && mkdir -p "$work/src"
    for u in $units; do cp "$src/$u" "$work/src/"; done
    python3 - "$work/src/$2" "$3" "$4" <<'PY'
import sys
p, old, new = sys.argv[1:]
s = open(p).read()
assert s.count(old) == 1, (p, old)
open(p, 'w').write(s.replace(old, new))
PY
    [ $? -eq 0 ] || { echo "BADTEXT  $1"; return; }
    if [ -n "${DRY:-}" ]; then echo "applies  $1"; return; fi
    local out=$work/prove.out
    if (cd "$here" && gnatprove -P locators_tests.gpr -XLOC_SRC="$work/src" \
          -XLOC_OBJ="$work/obj-$total" -j0 --checks-as-errors=on >"$out" 2>&1); then
        echo "SURVIVED $1"
    elif grep -qE "(medium|high): " "$out"; then
        echo "killed   $1: $(grep -m1 -oE '(medium|high): [^[]*' "$out")"
        killed=$((killed + 1))
    else
        echo "ERROR    $1:"; tail -5 "$out"
    fi
}
mutant "no-op (control: must survive)" cubit-locators.adb "   use CuBit.Net_Address;" "   use CuBit.Net_Address;"
mutant "port digits not limited" cubit-locators.adb \
    "      if Field'Length not in 1 .. 5 then" "      if Field'Length < 1 then"
mutant "port 0 accepted" cubit-locators.adb \
    "      if Value in Port_Number then" "      if Value <= 65_535 then"
mutant "octet digits not limited" cubit-locators.adb \
    "         elsif Is_Digit (Field (I)) and then Count < 3 and then" \
    "         elsif Is_Digit (Field (I)) and then"
mutant "field runs past a ':'" cubit-locators.adb \
    "      while J <= Text'Last and then Text (J) /= ':' loop" \
    "      while J <= Text'Last and then Text (J) /= ']' loop"
mutant "authority takes any character" cubit-locators.adb \
    "        Is_Word (Text (I))" "        Text (I) /= ':'"
mutant "IPv6 group count not bounded" cubit-locators.adb \
    "         Room := Head_N + Tail_N < 8;" "         Room := True;"
mutant "name host not checked" cubit-net_locator.adb \
    "      elsif Valid_Name (Text (First .. Last)) then" "      elsif Last >= First then"
echo "mutants killed: $killed/$((total - 1)) (plus one control that must survive)"
