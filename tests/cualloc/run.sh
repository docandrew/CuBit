#!/usr/bin/env bash
# CuAlloc's layer: the checked hosted tests (arithmetic and bounds checks
# on; threads, remote frees, exit and adoption included), the bounded model
# check of the remote-free protocol (model/inbox_model.py, under its own
# 24 GiB cap and a timeout, and its mutants, each of which must be caught),
# then the SPARK proof of the heap and the adapters' request arithmetic,
# then an audit that the proof covered every unit and used no assumption
# beyond the listed ones (check-proof.py). Hold
# coordination/build.lock; gnatprove needs a memory cap, e.g.
#   systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0 ...
# CUBIT_CUALLOC_PROOF_TIMEOUT: prover seconds per check (default 60).
set -euo pipefail
proof_timeout="${CUBIT_CUALLOC_PROOF_TIMEOUT:-60}"
if [[ ! "$proof_timeout" =~ ^[1-9][0-9]*$ ]]; then
  echo 'CUBIT_CUALLOC_PROOF_TIMEOUT must be a positive integer (seconds)' >&2
  exit 2
fi
cd "$(dirname "$0")/../../kernel"
alr exec -- gprbuild -p -P ../tests/cualloc/cualloc_tests.gpr
../tests/cualloc/build/main
model=(timeout 900 systemd-run --user --scope -q -p MemoryMax=24G -p MemorySwapMax=0 python3 ../tests/cualloc/model/inbox_model.py)
"${model[@]}"
for mutant in blind-store split-take adopt-held drop-on-exit; do
  "${model[@]}" --mutant "$mutant"
done
alr exec -- gnatprove -P ../tests/cualloc/cualloc_proof.gpr \
  -u cualloc_proof_heap.ads cualloc_proof_odd_heap.ads cualloc_requests.ads \
  --level=2 --timeout="$proof_timeout" --report=all --checks-as-errors=on -j2
python3 ../tests/cualloc/check-proof.py
