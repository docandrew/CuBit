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
samples() {  # write the selected sample set (FONT_QUALITY_SET) as $1
  "${P[@]}" - "$here" "$1" <<'PY'
import sys; sys.path.insert(0, sys.argv[1]); from spec import *
with open(sys.argv[2], "w") as f:
    f.write(f"{WIDTH} {ROW} {TOP} {LEFT} {len(SAMPLES)}\n")
    for face, fg, bg, text in SAMPLES:
        f.write(f"{face} {fg:06X} {bg:06X} {text}\n")
PY
}
# Regression thresholds (UI-032): worst group's ink error, worst face's row
# profile error (%), per density. Measured 2026-11-05 at the 16 px UI em
# (CuBit.UI_Metrics; x-height fitted on its overshoot as FreeType does):
# 1x 23.7 / 7.6%, 5/4 28.2 / 9.7% (Mono's 12.5-pixel grid), 3/2 25.1 /
# 11.4%, 2x 17.4 / 4.9%, status panel at 1x 20.3 / 8.2%. At the former
# 13 px em: 1x 22.0 / 7.9%, 5/4 23.8 / 7.7%, 3/2 35.7 / 16.6%, 2x 22.3 /
# 8.7%; the grid-fitted 1x that UI-032 removed measured 38.1 / 16.6%.
limits() {
  case $1 in
    1/1) echo "--limit=27 --limit=10" ;;
    5/4) echo "--limit=31 --limit=11" ;;
    3/2) echo "--limit=28 --limit=13" ;;
    *) echo "--limit=26 --limit=10" ;;
  esac
}
check() {  # REF IMG SCALE
  local l; read -r -a l <<< "$(limits "$3")"
  "${P[@]}" "$here/metric.py" "$1" "$2" "${l[0]}" || status=1
  "${P[@]}" "$here/rows.py" "$1" "$2" "${l[1]}" || status=1
}
status=0
samples "$here/build/samples.txt"
"$here/build/quality_sample" "$here/build/samples.txt" "$out/cubit.ppm"
"${P[@]}" "$here/reference.py" "$IBM_PLEX_SANS_FONT" "$IBM_PLEX_MONO_FONT" "$semibold" "$out/reference.png" --darken
"${P[@]}" "$here/compare.py" "$out/reference.png" "$out/cubit.ppm" "$out/compare" 240
check "$out/reference.png" "$out/cubit.ppm" 1/1
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
    check "$out/reference-$tag.png" "$out/$mode-$tag.ppm" $scale
  done
done
unset FONT_QUALITY_SCALE
# The status panel's own strings and colours at 1x (UI-032), Desktop CPU
# text, against FreeType; plus the browser-like (sRGB-blended) and LCD
# FreeType variants for side-by-side viewing (not scored).
export FONT_QUALITY_SET=panel
samples "$here/build/panel.txt"
"$here/build/quality_sample" "$here/build/panel.txt" "$out/panel.ppm" desktop 1 1
"${P[@]}" "$here/reference.py" "$IBM_PLEX_SANS_FONT" "$IBM_PLEX_MONO_FONT" "$semibold" "$out/panel-reference.png" --darken > /dev/null
"${P[@]}" "$here/reference.py" "$IBM_PLEX_SANS_FONT" "$IBM_PLEX_MONO_FONT" "$semibold" "$out/panel-srgb.png" --blend=srgb > /dev/null
"${P[@]}" "$here/reference.py" "$IBM_PLEX_SANS_FONT" "$IBM_PLEX_MONO_FONT" "$semibold" "$out/panel-lcd.png" --darken --lcd > /dev/null
"${P[@]}" "$here/compare.py" "$out/panel-reference.png" "$out/panel.ppm" "$out/compare-panel" 320 > /dev/null
check "$out/panel-reference.png" "$out/panel.ppm" 1/1
unset FONT_QUALITY_SET
[ $status = 0 ] && echo "text quality: within the regression thresholds" || echo "text quality: REGRESSION (see FAIL lines)"
exit $status
