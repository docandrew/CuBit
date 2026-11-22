#!/usr/bin/env bash
# Mutation check for the address-space tag proofs: each mutant must FAIL to
# prove. Run from the Nix shell: tests/address-space-tags/mutations.sh
set -u
here=$(cd "$(dirname "$0")" && pwd)
ksrc=${1:-$here/../../kernel/src}
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
# alr needs the kernel's workspace.
cd "$here/../../kernel"

mutate() {  # name file sed-expression
    local name=$1 file=$2 expr=$3 dir="$work/$1"
    mkdir -p "$dir/src"
    cp "$ksrc"/address_space_tags.ad? "$here"/tag_protocol.ad? "$here"/main.adb "$dir/src/"
    sed -i "$expr" "$dir/src/$file"
    if cmp -s "$dir/src/$file" "$ksrc/$file" 2>/dev/null || cmp -s "$dir/src/$file" "$here/$file" 2>/dev/null; then
        echo "MUTANT $name: NOT APPLIED"; return 1
    fi
    sed "s|for Object_Dir use \"build\"|for Object_Dir use \"$dir/build\"|; s|for Exec_Dir use \"build\"|for Exec_Dir use \"$dir/build\"|; s|(\".\", Kernel_Src)|(\"$dir/src\")|" \
        "$here/address_space_tags_tests.gpr" > "$dir/t.gpr"
    if alr exec -- gnatprove -P"$dir/t.gpr" -u address_space_tags.adb tag_protocol.adb \
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
# Keep translations cached for an older life of the tag.
mutate keep-older-life address_space_tags.ads 's/(Current \/= No_Life and then Loaded = Current)/(Current \/= No_Life and then Loaded <= Current)/' || fail=1
# Keep whatever the CPU last loaded the tag for.
mutate keep-any-life address_space_tags.ads 's/(Current \/= No_Life and then Loaded = Current)/(Current \/= No_Life)/' || fail=1
# A new address space that does not advance the tag's life.
mutate life-not-advanced address_space_tags.ads 's/return Life is (Current + 1)/return Life is (Current)/' || fail=1
# A CR3 value without the tag.
mutate cr3-without-tag address_space_tags.adb 's/Value : constant Unsigned_64 := Root or Unsigned_64 (T);/Value : constant Unsigned_64 := Root;/' || fail=1
# A load that does not remember which life it loaded.
mutate load-forgets-life tag_protocol.adb 's/        S.Loaded (C, T) := S.Lives (T);/        null;/' || fail=1
# A load that keeps nothing in name only: the dropped translations stay.
mutate load-keeps-dropped tag_protocol.adb '/procedure Load (S : in out State; C : CPU; T : Slot; Kept : out Boolean) is/,/end Load;/ s/            S.Cached (C, T) := No_Life;/            null;/' || fail=1
# Flush_All as a CR3 reload: only the tag the CPU runs loses its translations.
mutate flush-current-tag-only tag_protocol.adb '/procedure Flush_All (S : in out State; C : CPU) is/,/end Flush_All;/ { s/            S.Cached (C, T) := No_Life;/            if S.Active (C) = T then S.Cached (C, T) := No_Life; end if;/; s/            S.Removed (C, T) := False;/            if S.Active (C) = T then S.Removed (C, T) := False; end if;/ }' || fail=1
# A new life while a CPU still runs the slot's old address space.
mutate life-while-running tag_protocol.ads 's/Pre  => Inv (S) and then Runs_Nowhere (S, T) and then/Pre  => Inv (S) and then/' || fail=1
exit $fail
