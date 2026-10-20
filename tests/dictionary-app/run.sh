#!/usr/bin/env bash
# Hosted tests of the Dictionary/Thesaurus view (docs/lexicon.md). Run inside
# nix develop with TMPDIR outside /tmp. Screenshots: build/*.ppm and PNGs in
# DICTIONARY_SCREENSHOTS (default /home/doc/cubit-build-tmp/app-dict).
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/../.." && pwd)"
image="$root/tests/lexicon/build/english.lexicon"
shots="${DICTIONARY_SCREENSHOTS:-/home/doc/cubit-build-tmp/app-dict}"
mkdir -p "$root/tests/lexicon/build" "$here/build/source" "$shots"
if [ ! -s "$image" ] || [ "$root/tools/build_lexicon.py" -nt "$image" ]; then
  python3 -I "$root/tools/build_lexicon.py" "$image" --notice "$root/tests/lexicon/build/LEXICON-NOTICE"
fi
for unit in cubit.ads; do
  cmp -s "$root/userspace/runtime/gnat/$unit" "$here/build/source/$unit" ||
    cp "$root/userspace/runtime/gnat/$unit" "$here/build/source/"
done
(cd "$root/kernel" && alr exec -- gprbuild -p -q -j8 -P "$here/dictionary_app_tests.gpr")
cd "$here" && LEXICON_IMAGE="$image" build/dictionary_app_tests "$@"
for f in "$here"/build/*.ppm; do
  python3 -c "import sys; from PIL import Image; Image.open(sys.argv[1]).save(sys.argv[2])" \
    "$f" "$shots/hosted-$(basename "${f%.ppm}").png"
done
