#!/usr/bin/env bash
set -euo pipefail
root=$(cd "$(dirname "$0")/../.." && pwd)
source_tree=${1:?prepared Mesa source required}
host_tree=${2:-$root/tests/mesa-anv/build-host}
out=$(mktemp -d "$root/tests/mesa-anv/target/offscreen-shader.XXXXXX")
# Header layout depends on packing/debug/platform feature macros. Reuse the
# actual library configuration rather than guessing a compatible subset.
mapfile -d '' -t defines < <(python3 -c '
import json, shlex, sys
entries = json.load(open(sys.argv[1]))
entry = next(e for e in entries if e["file"].endswith("/nir.c"))
for arg in shlex.split(entry["command"]):
    if arg.startswith("-D"):
        sys.stdout.buffer.write(arg.encode() + b"\0")
' "$host_tree/compile_commands.json")
test "${#defines[@]}" -gt 0
cc -std=c11 "${defines[@]}" -Wall -Wextra -Werror \
  -isystem "$source_tree/include" -isystem "$source_tree/src" \
  -isystem "$source_tree/src/intel" -isystem "$source_tree/src/compiler/nir" \
  -isystem "$host_tree/src" -isystem "$host_tree/include" \
  -isystem "$host_tree/src/compiler/nir" \
  -isystem "$host_tree/src/compiler" \
  -isystem "$host_tree/src/intel/compiler/gen" \
  -isystem "$host_tree/src/intel/genxml" \
  -isystem "$source_tree/src/intel/genxml" \
  -c "$root/tests/mesa-anv/offscreen-shader-test.c" -o "$out/probe.o"
c++ "$out/probe.o" -Wl,--gc-sections -Wl,--start-group \
  "$host_tree/src/intel/compiler/brw/libintel_compiler.a" \
  "$host_tree/src/intel/compiler/gen/libintel_compiler_gen.a" \
  "$host_tree/src/intel/compiler/libintel_compiler_nir.a" \
  "$host_tree/src/compiler/nir/libnir.a" "$host_tree/src/compiler/libcompiler.a" \
  "$host_tree"/src/intel/isl/*.a "$host_tree/src/intel/dev/libintel_dev.a" \
  "$host_tree/src/intel/mda/libmda.a" "$host_tree"/src/util/*.a \
  "$host_tree/src/intel/common/libintel_common.a" \
  "$host_tree/src/util/blake3/libblake3.a" \
  -Wl,--end-group -ldrm -lexpat -lzstd -lz -lm -ldl -pthread -o "$out/probe"
"$out/probe" "$out" | tee "$out/result.txt"
python3 "$root/tests/mesa-anv/check-probe-shaders.py" "$out"
sha256sum "$out/vertex.bin" "$out/fragment.bin"
echo "Retained hosted shader oracle: $out"
