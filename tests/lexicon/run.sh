#!/usr/bin/env bash
# Hosted lexicon core tests (docs/lexicon.md). Run inside nix develop with
# TMPDIR outside /tmp:
#   tests/lexicon/run.sh            build the image, run the checked tests
#   tests/lexicon/run.sh --seed N   the same, damage tests seeded with N
#   tests/lexicon/run.sh --bench    -O3 -gnatp timings (quiet host please)
#   tests/lexicon/run.sh --prove    SPARK proof of the core, level 2, memory-capped
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/../.." && pwd)"
image="$here/build/english.lexicon"
mkdir -p "$here/build"
if [ ! -s "$image" ] || [ "$root/tools/build_lexicon.py" -nt "$image" ]; then
  python3 -I "$root/tools/build_lexicon.py" "$image" --notice "$here/build/LEXICON-NOTICE"
fi
build() {  # mode, mains...
  local mode=$1; shift
  (cd "$root/kernel" && alr exec -- gprbuild -p -q -j8 -XLEXICON_MODE="$mode" -P "$here/lexicon_tests.gpr" "$@")
}
case "${1:-}" in
  --prove)
    cd "$root/kernel"
    systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0 \
      alr exec -- gnatprove -P "$here/lexicon_proof.gpr" -j2 --level=2 --report=fail \
        --checks-as-errors=on "${@:2}"
    ;;
  --bench)
    build release lexicon_bench.adb
    cd "$here" && LEXICON_IMAGE="$image" build/lexicon_bench-release
    ;;
  --seed)
    build checked lexicon_tests.adb
    cd "$here" && LEXICON_IMAGE="$image" build/lexicon_tests-checked "$2"
    ;;
  *)
    build checked lexicon_tests.adb
    cd "$here" && LEXICON_IMAGE="$image" build/lexicon_tests-checked
    ;;
esac
