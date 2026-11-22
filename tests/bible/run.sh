#!/usr/bin/env bash
# Hosted Bible core tests (APP-005). Run inside nix develop with TMPDIR
# outside /tmp:
#   tests/bible/run.sh            build the images, check versification and
#                                 the reading faces' characters, run the
#                                 checked tests
#   tests/bible/run.sh --seed N   the same, fuzz and damage seeded with N
#   tests/bible/run.sh --bench    -O3 -gnatp timings (quiet host please)
#   tests/bible/run.sh --prove    SPARK proof of the core, level 2, memory-capped
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/../.." && pwd)"
images="$here/build/bible"
mkdir -p "$images"
if [ ! -s "$images/vul.bible" ] || [ "$root/tools/build_bible.py" -nt "$images/vul.bible" ] ||
   [ "$root/assets/bible/sources.json" -nt "$images/vul.bible" ]; then
  python3 -I "$root/tools/build_bible.py" "$images"
fi
build() {  # mode, mains...
  local mode=$1; shift
  (cd "$root/kernel" && alr exec -- gprbuild -p -q -j8 -XBIBLE_MODE="$mode" -P "$here/bible_tests.gpr" "$@")
}
case "${1:-}" in
  --prove)
    cd "$root/kernel"
    systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0 \
      alr exec -- gnatprove -P "$here/bible_proof.gpr" -j2 --level=2 --report=fail \
        --checks-as-errors=on "${@:2}"
    ;;
  --bench)
    build release bible_bench.adb
    cd "$here" && BIBLE_DIR="$images" build/bible_bench-release
    ;;
  *)
    python3 -I "$here/test_build_bible.py"
    python3 -I "$here/test_faces.py"
    python3 -I "$here/check_versification.py" "$images"
    build checked bible_tests.adb
    if [ "${1:-}" = "--seed" ]; then
      cd "$here" && BIBLE_DIR="$images" build/bible_tests-checked "$2"
    else
      cd "$here" && BIBLE_DIR="$images" build/bible_tests-checked
    fi
    ;;
esac
