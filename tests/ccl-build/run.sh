#!/usr/bin/env bash
# CCL build, first slice (docs/ccl-build.md), hosted on Linux:
#   1. core and store unit tests (hosted Ada);
#   2. every component.ccl type-checks, the negative cases are refused; sleep.app and the multi-unit
#      boot-timeline.app build with GNAT 15.3;
#   3. cache hits, mtime-only changes, another checkout sharing the store,
#      a fresh store giving the same bytes, early cutoff after a comment
#      edit, a real code edit (the byte comparison against make's gprbuild
#      outputs is tests/ccl-build/compare.sh, run when a program migrates);
#   4. embedded blobs: a vendored firmware blob linked into a program, its
#      bytes checked in the ELF against the pinned digest, its licence
#      exported; a wrong digest refused; a Fetch blob from a local HTTP
#      server (wrong digest refused and not stored, then stored, then a
#      rebuild with the server stopped answered from the store);
#   5. a real crash: SIGKILL while a tree is copied in, then recovery;
#   6. with --prove: gnatprove on the Storehouse core (level 1, memory-capped).
# Run inside `nix develop` with TMPDIR outside /tmp:
#   TMPDIR=/home/doc/cubit-build-tmp nix develop -c bash tests/ccl-build/run.sh [--prove]
set -euo pipefail

here=$(cd "$(dirname "$0")" && pwd)
repo=$(cd "$here/../.." && pwd)
prove=no
[[ ${1:-} == --prove ]] && prove=yes
: "${TMPDIR:?set TMPDIR outside /tmp}"
scratch=$(mktemp -d "$TMPDIR/ccl-build-tests.XXXXXX")
server=
cleanup() {
    if [[ -n $server ]]; then kill "$server" 2>/dev/null || true; fi
    chmod -R u+w "$scratch" 2>/dev/null || true; rm -rf "$scratch"
}
trap cleanup EXIT

passed=0
failed=0
check() {  # check NAME COMMAND...
    local name=$1; shift
    if "$@"; then passed=$((passed + 1)); echo "PASS $name"
    else failed=$((failed + 1)); echo "FAIL $name"; fi
}
ran() { grep -Eo '^ccl-build: [0-9]+ ran' "$1" | grep -Eo '[0-9]+'; }
step_ran() { grep -q "^ran  [0-9a-f]*  $2\$" "$1"; }
step_hit() { grep -q "^hit  [0-9a-f]*  $2\$" "$1"; }

echo "== building ccl-build, the tests and the user runtime"
(cd "$repo/kernel" &&
    alr exec -- gprbuild -q -p -P ../userspace/ccl/ccl_build.gpr &&
    alr exec -- gprbuild -q -p -P ../tests/ccl-build/ccl_build_tests.gpr &&
    make -s user_runtime ccl-manifest >/dev/null)
tool=$repo/userspace/ccl/build/ccl-build/ccl-build
: "${CUBIT_GNAT:?run inside nix develop}" "${CUBIT_BINUTILS:?run inside nix develop}"
imports=(--import "gnat-15.3.0-x86_64-linux-musl=$CUBIT_GNAT" --import "binutils-2.46=$CUBIT_BINUTILS")
declaration=userspace/apps/sleep/component.ccl
multi=userspace/apps/boot-timeline/component.ccl
program=userspace/apps/sleep/build/sleep.app

echo "== unit tests"
check "core tests" "$here/build/core_tests"
mkdir -p "$scratch/unit"
check "store tests" "$here/build/store_tests" "$scratch/unit"

echo "== declarations: every component.ccl type-checks against build-params.ccl and build-catalog.ccl"
check "all declarations type-check" "$tool" check --source-root "$repo" \
    $(cd "$repo" && ls userspace/apps/*/component.ccl userspace/services/*/component.ccl userspace/ccl/services/*/component.ccl \
      tests/ccl-build/fixtures/*/component.ccl)

echo "== negative declarations: each is refused with its diagnostic"
for case in "$here"/negative/*/; do
    file=$(ls "$case"*.ccl)
    expected=$(cat "$case/expect")
    if output=$("$tool" check --source-root "$repo" "$file" 2>&1); then
        check "refused: $(basename "$case")" false
    else
        check "refused: $(basename "$case")" grep -qF -- "$expected" <<<"$output"
    fi
done

echo "== sleep.app and boot-timeline.app (multi-unit) with GNAT 15.3"
store=$scratch/store
build() {  # build ROOT LOG DECLARATION [extra args]
    local root=$1 log=$2 file=$3; shift 3
    "$tool" build --store "$store" --source-root "$root" "${imports[@]}" "$@" "$root/$file" >"$log" 2>&1
}
check "first build" build "$repo" "$scratch/first.log" "$declaration" --out "$scratch/out"
cat "$scratch/first.log"
check "first build ran all 6 steps" test "$(ran "$scratch/first.log")" = 6
check "multi-unit build" build "$repo" "$scratch/multi.log" "$multi" --out "$scratch/out"
cat "$scratch/multi.log"
check "multi-unit: units found through .ali W lines" test "$(grep -c '^ran  [0-9a-f]*  compile ' "$scratch/multi.log")" -ge 4

echo "== cache"
check "second build" build "$repo" "$scratch/second.log" "$declaration"
check "second build ran nothing" test "$(ran "$scratch/second.log")" = 0

# Another checkout (a copy of what the declaration reads) shares the store.
root=$scratch/root
copy() { mkdir -p "$root/$(dirname "$1")"; cp -a "$repo/$1" "$root/$1"; }
for p in userspace/apps/sleep/main.adb userspace/apps/sleep/manifest.ccl "$declaration" build-params.ccl build-catalog.ccl \
         userspace/runtime/ada_source_path userspace/runtime/ada_object_path \
         userspace/runtime/gnat userspace/runtime/adalib \
         shared/time shared/interrupts shared/launch shared/publications shared/capabilities shared/boot \
         userspace/c/link.ld userspace/c/build/crt0.o userspace/ccl/build/manifest/ccl-manifest \
         userspace/ccl/catalogs/native-runtime-services.ccl userspace/ccl/catalogs/driver-services.ccl \
         userspace/ccl/interfaces/executable-manifest.ccl userspace/ccl/interfaces/build.ccl \
         userspace/ccl/interfaces/setting-catalog.ccl userspace/ccl/interfaces/settings; do
    copy "$p"
done
check "other checkout" build "$root" "$scratch/other.log" "$declaration"
check "other checkout is answered by the cache" test "$(ran "$scratch/other.log")" = 0

# Reproducible from scratch: a fresh store in the other checkout gives the
# same bytes as the first build.
fresh=$scratch/fresh-store
check "fresh store, other checkout" "$tool" build --store "$fresh" --source-root "$root" "${imports[@]}" \
    --out "$scratch/out-fresh" "$root/$declaration"
check "fresh build is byte-identical" cmp -s "$scratch/out-fresh/$program" "$scratch/out/$program"

touch "$root/userspace/apps/sleep/main.adb"
check "mtime-only change" build "$root" "$scratch/touch.log" "$declaration"
check "an mtime-only change runs nothing" test "$(ran "$scratch/touch.log")" = 0

# Early cutoff: a comment edit on one line recompiles main.adb to the same
# object, so nothing after it runs.
sed -i 's/^--  Sleep test app$/--  Sleep test app (comment edited)/' "$root/userspace/apps/sleep/main.adb"
check "comment edit" build "$root" "$scratch/comment.log" "$declaration"
cat "$scratch/comment.log"
check "comment edit recompiles main.adb" step_ran "$scratch/comment.log" "compile main.adb"
check "comment edit: same object, bind answered" step_hit "$scratch/comment.log" "bind main.adb"
check "comment edit: link answered" step_hit "$scratch/comment.log" "link userspace/apps/sleep/build/sleep.app"
check "comment edit ran one step" test "$(ran "$scratch/comment.log")" = 1

sed -i 's/sleep: starting/sleep: started!/' "$root/userspace/apps/sleep/main.adb"
check "code edit" build "$root" "$scratch/code.log" "$declaration" --out "$scratch/out2"
cat "$scratch/code.log"
check "code edit recompiles and relinks" step_ran "$scratch/code.log" "link userspace/apps/sleep/build/sleep.app"
check "code edit changes sleep.app" bash -c "! cmp -s '$scratch/out2/$program' '$scratch/out/$program'"

check "store verifies" "$tool" verify --store "$store"

echo "== embedded blobs (docs/device-matching.md, \"Firmware\")"
guc=firmware/i915/tgl_guc_70.bin
guc_sha256=2f1f57a1b23d186f2592318d1e07a1365968932841ccb3e7177c516ba006e2f6
embedded=tests/ccl-build/fixtures/embedded
check "embedded fixture builds" build "$repo" "$scratch/embed.log" "$embedded/component.ccl" --out "$scratch/out-embed"
cat "$scratch/embed.log"
check "the blob has its own step" step_ran "$scratch/embed.log" "embed guc-firmware"
check "the program's blob bytes hash to the pinned digest" python3 "$here/check-embedded.py" \
    "$scratch/out-embed/$embedded/build/embed-check.app" cubit_blob_guc_firmware "$guc_sha256"
check "the licence is exported with the program" \
    cmp -s "$scratch/out-embed/$embedded/build/licenses/LICENSE.i915" "$repo/firmware/LICENSE.i915"

# A checkout copy for declarations the repository does not hold.
blob_root=$scratch/blob-root
for p in "$guc" firmware/LICENSE.i915 "$embedded" build-params.ccl build-catalog.ccl \
         userspace/runtime/ada_source_path userspace/runtime/ada_object_path \
         userspace/runtime/gnat userspace/runtime/adalib \
         shared/time shared/interrupts shared/launch shared/publications shared/capabilities shared/boot \
         userspace/c/link.ld userspace/c/build/crt0.o userspace/ccl/build/manifest/ccl-manifest \
         userspace/ccl/catalogs/native-runtime-services.ccl userspace/ccl/catalogs/driver-services.ccl \
         userspace/ccl/interfaces/executable-manifest.ccl userspace/ccl/interfaces/build.ccl \
         userspace/ccl/interfaces/setting-catalog.ccl userspace/ccl/interfaces/settings; do
    mkdir -p "$blob_root/$(dirname "$p")"; cp -a "$repo/$p" "$blob_root/$p"
done
# The pinned digest's last word, and a wrong one.
right_word=0xa006e2f6
wrong_word=0xa006e2f7
sed -i "s/$right_word/$wrong_word/" "$blob_root/$embedded/component.ccl"
refused() {  # refused LOG EXPECTED DECLARATION [store]: the build fails, saying EXPECTED
    local log=$1 expected=$2 file=$3 at=${4:-$store}
    ! "$tool" build --store "$at" --source-root "$blob_root" "${imports[@]}" "$blob_root/$file" >"$log" 2>&1 &&
        grep -qF -- "$expected" "$log"
}
check "a vendored blob with a wrong digest is refused" \
    refused "$scratch/embed-wrong.log" "SHA-256 is $guc_sha256" "$embedded/component.ccl"

serve=$scratch/serve
mkdir -p "$serve"
cp "$repo/$guc" "$serve/"
port=$(python3 -c 'import socket; s=socket.socket(); s.bind(("127.0.0.1", 0)); print(s.getsockname()[1])')
python3 -m http.server --bind 127.0.0.1 --directory "$serve" "$port" >"$scratch/http.log" 2>&1 &
server=$!
for _ in $(seq 1 50); do curl -sf -o /dev/null "http://127.0.0.1:$port/" && break; sleep 0.1; done
fetched=tests/ccl-build/fixtures/fetched
mkdir -p "$blob_root/$fetched"
cp "$repo/$embedded/main.adb" "$repo/$embedded/manifest.ccl" "$blob_root/$fetched/"
sed -e 's/"embed-check"/"fetch-check"/' \
    -e "s|(vendored \"$guc\"|(fetch \"http://127.0.0.1:$port/tgl_guc_70.bin\"|" \
    "$blob_root/$embedded/component.ccl" >"$blob_root/$fetched/component.ccl"
fetch_store=$scratch/fetch-store
check "a fetched blob with a wrong digest is refused" \
    refused "$scratch/fetch-wrong.log" "SHA-256 is $guc_sha256" "$fetched/component.ccl" "$fetch_store"
check "a refused fetch stores nothing" test ! -e "$fetch_store/files/$guc_sha256"
sed -i "s/$wrong_word/$right_word/" "$blob_root/$fetched/component.ccl"
check "a fetched blob with the pinned digest builds" "$tool" build --store "$fetch_store" --source-root "$blob_root" \
    "${imports[@]}" --out "$scratch/out-fetch" "$blob_root/$fetched/component.ccl" >"$scratch/fetch.log" 2>&1
cat "$scratch/fetch.log"
check "the fetch ran" step_ran "$scratch/fetch.log" "fetch guc-firmware"
check "fetched bytes hash to the pinned digest" python3 "$here/check-embedded.py" \
    "$scratch/out-fetch/$fetched/build/fetch-check.app" cubit_blob_guc_firmware "$guc_sha256"
kill "$server"
wait "$server" 2>/dev/null || true
server=
check "with the server stopped, the store answers" "$tool" build --store "$fetch_store" --source-root "$blob_root" \
    "${imports[@]}" "$blob_root/$fetched/component.ccl" >"$scratch/fetch-offline.log" 2>&1
check "offline: the fetch is a hit" step_hit "$scratch/fetch-offline.log" "fetch guc-firmware"

echo "== crash: SIGKILL while copying a tree into the store"
crash_store=$scratch/crash-store
victim_tree=$scratch/victim-tree
# A tree large enough to take seconds: copies of the runtime's library.
for i in $(seq 1 40); do mkdir -p "$victim_tree/$i"; cp -a "$repo/userspace/runtime/adalib/." "$victim_tree/$i/"; done
"$tool" import --store "$crash_store" "$victim_tree" >"$scratch/crash.log" 2>&1 &
victim=$!
sleep 1
kill -KILL "$victim"
wait "$victim" 2>/dev/null || true
check "killed mid-copy: no tree entry" test -z "$(ls -A "$crash_store/trees")"
check "killed mid-copy: staging left in work/" test -n "$(ls -A "$crash_store/work")"
check "killed store verifies" "$tool" verify --store "$crash_store"
check "import after the crash completes" "$tool" import --store "$crash_store" "$victim_tree"
check "dead process's staging removed" test -z "$(ls -A "$crash_store/work")"
check "recovered store verifies" "$tool" verify --store "$crash_store"

if [[ $prove == yes ]]; then
    echo "== proof: Storehouse core, level 1"
    check "gnatprove storehouse" systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0 --quiet \
        bash -c "cd '$repo/kernel' && alr exec -- gnatprove -P ../tests/ccl-build/storehouse_proof.gpr \
            -u storehouse-hex.adb -u storehouse-canonical.adb -u storehouse-hashing.adb -u storehouse-paths.ads \
            --level=1 -j2 --report=statistics"
fi

echo "ccl-build tests: $passed passed, $failed failed"
[[ $failed == 0 ]]
