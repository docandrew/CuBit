#!/usr/bin/env bash
# Hosted tests of the Bible app's view (APP-005). Run inside nix develop with
# TMPDIR outside /tmp. The reading faces are the pinned Literata, Source
# Serif 4 and EB Garamond the app embeds (fetched once into the CCL build
# store or this tool's cache, by tools/build_bible.py --font); BIBLE_FACES
# overrides them.
#   tests/bible-app/run.sh             the view's tests, PNG screenshots
#                                      (John 3 and Psalm 23 in each face)
# Screenshots: build/*.ppm and PNGs in BIBLE_SCREENSHOTS
# (default /home/doc/cubit-build-tmp/bible-app).
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/../.." && pwd)"
images="$root/tests/bible/build/bible"
shots="${BIBLE_SCREENSHOTS:-/home/doc/cubit-build-tmp/bible-app}"
mkdir -p "$images" "$here/build/source" "$shots"
if [ ! -s "$images/vul.bible" ] || [ "$root/tools/build_bible.py" -nt "$images/vul.bible" ]; then
  python3 -I "$root/tools/build_bible.py" "$images"
fi
for unit in cubit.ads; do
  cmp -s "$root/userspace/runtime/gnat/$unit" "$here/build/source/$unit" ||
    cp "$root/userspace/runtime/gnat/$unit" "$here/build/source/"
done
make -C "$root/userspace/rust" fonts-host >/dev/null
(cd "$root/kernel" && alr exec -- gprbuild -p -q -j8 -P "$here/bible_app_tests.gpr")
font() {  # the pinned face NAME, by the same fetch the app's build uses
  python3 -I "$root/tools/build_bible.py" --font "$1"
}
rm -f "$here"/build/*.ppm
cd "$here"
faces="Literata=$(font literata)|Source Serif 4=$(font source-serif-4)|EB Garamond=$(font eb-garamond)"
BIBLE_DIR="$images" BIBLE_FACES="${BIBLE_FACES:-$faces}" build/bible_app_tests "$@"
for f in "$here"/build/*.ppm; do
  python3 -c "import sys; from PIL import Image; Image.open(sys.argv[1]).save(sys.argv[2])" \
    "$f" "$shots/hosted-$(basename "${f%.ppm}").png"
done
