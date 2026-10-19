#!/usr/bin/env bash
# Text rendering quality (backlog UI-026), Linux-hosted. Run inside nix
# develop with TMPDIR outside /tmp:
#   tests/ui-fonts/quality/run.sh            render the samples through the
#       toolkit (and, at densities 1, 5/4, 3/2 and 2, through the Desktop's
#       CPU compositor text and the toolkit's density masks), render the
#       FreeType reference at each size, write side-by-side 1:1 and 4x crops
#       and the distance metrics to $FONT_QUALITY_OUT
#   tests/ui-fonts/quality/run.sh --bench    4K UI-text draw/measure timing,
#       toolkit and Desktop CPU text (three trials; compare the fastest)
#   tests/ui-fonts/quality/run.sh --kerning  layout tables vs HarfBuzz
# The reference tools (FreeType, HarfBuzz) come from the flake's pinned
# nixpkgs; none of them is part of CuBit.
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/../../.." && pwd)"
out="${FONT_QUALITY_OUT:-/home/doc/cubit-build-tmp/fonts/quality}"
mkdir -p "$here/build" "$out"
py="$here/build/python"
if [ ! -x "$py/bin/python3" ]; then
  # The nixpkgs revision flake.lock pins, by its locked reference.
  pinned=$(python3 -I -c 'import json,sys; l=json.load(open(sys.argv[1])); r=l["nodes"]["root"]["inputs"]["nixpkgs"]; n=l["nodes"][r if isinstance(r,str) else r[0]]["locked"]; print("github:%s/%s/%s?narHash=%s" % (n["owner"], n["repo"], n["rev"], n["narHash"]))' "$root/flake.lock")
  nix build --impure -o "$py" --expr "with (builtins.getFlake \"$pinned\").legacyPackages.x86_64-linux; python3.withPackages (p: [p.freetype-py p.pillow p.numpy p.uharfbuzz])"
fi
P=("$py/bin/python3" -I)
make -s -C "$root/userspace/rust" fonts-host
semibold="${IBM_PLEX_SANS_SEMIBOLD_FONT:-$(dirname "$IBM_PLEX_SANS_FONT")/IBMPlexSans-SemiBold.ttf}"
export IBM_PLEX_SANS_SEMIBOLD_FONT="$semibold"
case "${1:-}" in
  --kerning)
    (cd "$root/userspace/rust" && cargo run -q --locked --offline --release \
       --target x86_64-unknown-linux-gnu -p cubit-fonts --example layout_dump) |
      "${P[@]}" "$here/check_kerning.py" "$IBM_PLEX_SANS_FONT" "$IBM_PLEX_MONO_FONT" "$semibold"
    exit ;;
esac
(cd "$root/kernel" && alr exec -- gprbuild -q -p -P "$here/quality.gpr")
if [ "${1:-}" = "--bench" ]; then
  "$here/build/quality_sample" bench
  exit
fi
"${P[@]}" - "$here" "$here/build/samples.txt" <<'PY'
import sys; sys.path.insert(0, sys.argv[1]); from spec import *
with open(sys.argv[2], "w") as f:
    f.write(f"{WIDTH} {ROW} {TOP} {LEFT} {len(SAMPLES)}\n")
    for face, fg, bg, text in SAMPLES:
        f.write(f"{face} {fg:06X} {bg:06X} {text}\n")
PY
"$here/build/quality_sample" "$here/build/samples.txt" "$out/cubit.ppm"
"${P[@]}" "$here/reference.py" "$IBM_PLEX_SANS_FONT" "$IBM_PLEX_MONO_FONT" "$semibold" "$out/reference.png" --darken
"${P[@]}" "$here/compare.py" "$out/reference.png" "$out/cubit.ppm" "$out/compare" 240
"${P[@]}" "$here/metric.py" "$out/reference.png" "$out/cubit.ppm"
"${P[@]}" "$here/rows.py" "$out/reference.png" "$out/cubit.ppm"
# The Desktop compositor's CPU text path (Compositor_Software_Text, as
# drawUIText batches it) and the toolkit at non-unit densities, each against
# FreeType at the same pixel size.
for scale in 1/1 5/4 3/2 2/1; do
  n=${scale%/*}; d=${scale#*/}; tag="${n}x${d}"
  export FONT_QUALITY_SCALE=$scale
  "${P[@]}" "$here/reference.py" "$IBM_PLEX_SANS_FONT" "$IBM_PLEX_MONO_FONT" "$semibold" "$out/reference-$tag.png" --darken > /dev/null
  for mode in desktop toolkit; do
    [ "$mode/$scale" = toolkit/1/1 ] && continue
    "$here/build/quality_sample" "$here/build/samples.txt" "$out/$mode-$tag.ppm" $mode $n $d
    "${P[@]}" "$here/compare.py" "$out/reference-$tag.png" "$out/$mode-$tag.ppm" "$out/compare-$mode-$tag" 320 > /dev/null
    "${P[@]}" "$here/metric.py" "$out/reference-$tag.png" "$out/$mode-$tag.ppm"
    "${P[@]}" "$here/rows.py" "$out/reference-$tag.png" "$out/$mode-$tag.ppm"
  done
done
unset FONT_QUALITY_SCALE
