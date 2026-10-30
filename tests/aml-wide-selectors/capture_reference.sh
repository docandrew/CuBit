#!/usr/bin/env bash
set -euo pipefail
cd -- "$(dirname -- "$0")/reference"
ulimit -s 65536
ulimit -f 2048
for name in region region32; do
 timeout --kill-after=2s 5s /nix/store/72fphm0m9k2f8zd066ayxkqi44ia9ql5-acpica-tools-20260408/bin/iasl -p "$name" "$name.asl" > "$name-compile.log" 2>&1
 printf 'compile\t0\n' > "$name-status.tsv"
 timeout --kill-after=2s 5s /nix/store/72fphm0m9k2f8zd066ayxkqi44ia9ql5-acpica-tools-20260408/bin/acpiexec -b 'execute INIT; execute BRED; execute INIT; execute BWRT; execute INIT; execute NRED; execute INIT; execute NWRT; execute INIT; execute WFUL; execute INIT; execute WONE; execute INIT; execute WZER' "$name.aml" > "$name-reference.log" 2>&1
 printf 'runtime\t0\n' >> "$name-status.tsv"
 sha256sum "$name.asl" > "$name-inputs.sha256"
 sha256sum "$name.aml" "$name-compile.log" "$name-reference.log" "$name-status.tsv" > "$name-evidence.sha256"
done
