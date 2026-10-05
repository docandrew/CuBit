#!/usr/bin/env bash
# Run inside nix develop: Client_Raster's proof, then hosted equivalence tests
# against a naive per-pixel reference.
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/ui_raster.gpr"
"$here/build/main"
if [[ "${1:-}" != "--no-prove" ]]; then
  alr exec -- gnatprove -P "$here/ui_raster.gpr" -u client_raster.adb -j4
  # Proof gate: every drawing unit compiled with pragma Suppress (All_Checks)
  # must prove with no unproved check, or the suppression is not safe.
  for pair in client_canvas:client_canvas_geometry.adb client_blend:client_glyph_blend.adb \
              glyph_cache:compositor_glyph_cache.adb glyph_layout:compositor_glyph_layout.adb \
              client_glyphs:client_glyphs.adb client_glyphs:compositor_glyph_software.adb; do
    alr exec -- gnatprove -P "$here/../compositor/${pair%%:*}.gpr" -u "${pair##*:}" \
      --level=2 --report=fail --checks-as-errors=on -j4
  done
  suppressed=$(grep -l "pragma Suppress (All_Checks)" ../userspace/lib/ui/*.adb ../userspace/lib/compositor/*.adb | wc -l)
  [[ "$suppressed" -eq 7 ]] || { echo "FAIL: $suppressed units suppress checks; add the new one to this gate"; exit 1; }
  echo "PASS: proof gate, 7 check-suppressed drawing units proved"
fi
