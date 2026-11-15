#!/usr/bin/env bash
#  PERF-011: the programs that link the UI toolkit's font archive use the
#  runtime's memcpy, memmove, memset and memcmp (CuBit.String), not the weak
#  copies Rust's core brings (compiler_builtins: three string instructions a
#  copy). Checks the archive and every staged program that draws text.
#  Run after make -C kernel world (or ui-fonts-native and the programs).
set -euo pipefail
root=$(cd "$(dirname "$0")/../.." && pwd)
archive=$root/userspace/rust/build/font-native/libcubit_fonts.a
boot=$root/kernel/isodir/boot
routines='memcpy|memmove|memset|memcmp'
failed=0
[ -f "$archive" ] || { echo "runtime-string linked: no $archive (make -C kernel ui-fonts-native)"; exit 1; }
if defined=$(nm -A --defined-only "$archive" 2>/dev/null | grep -E " [TW] ($routines)\$"); then
  echo "FAIL: the font archive defines its own routines:"
  printf '%s\n' "$defined"
  failed=1
fi
checked=0
for program in "$boot"/*.app "$boot"/*.svc; do
  [ -f "$program" ] || continue
  symbols=$(nm "$program" 2>/dev/null) || continue
  grep -qE ' T cubit_font_glyph$' <<<"$symbols" || continue
  checked=$((checked + 1))
  #  The runtime's are strong definitions; Rust's are weak.
  if weak=$(grep -E " W ($routines)\$" <<<"$symbols"); then
    echo "FAIL: $(basename "$program") links weak copies: $(awk '{print $3}' <<<"$weak" | tr '\n' ' ')"
    failed=1
  fi
done
[ "$checked" -gt 0 ] || { echo "runtime-string linked: no staged program draws text under $boot"; exit 1; }
if [ "$failed" = 0 ]; then
  echo "runtime-string linked: $checked programs use the runtime's $routines"
fi
exit "$failed"
