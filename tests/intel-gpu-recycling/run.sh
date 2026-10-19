#!/bin/bash
# intel-gpu session/GuC context recycling, Linux-hosted. Run from anywhere
# inside the CuBit Nix shell. Builds into this directory's build*/ trees.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here"
gprbuild -q -p -P recycling.gpr
./build/session_lifecycle_tests
./build/context_recycling_tests
./build/slot_storage_tests
./build/client_account_tests
python3 test-recycle-glue.py
# Further seeds for the random interleavings.
for seed in 1 2 3 4 5 6 7 8; do
   ./build/session_lifecycle_tests "$seed" | tail -1
   ./build/context_recycling_tests "$seed" | tail -1
   ./build/client_account_tests "$seed" | tail -1
done
# The existing hosted tests of the units this work changed.
cd ../intel-gpu
gprbuild -q -p -P render_sessions.gpr && ./build-render-sessions/render_sessions_tests
gprbuild -q -p -P render_control.gpr && ./build-render-control/render_control_tests
gprbuild -q -p -P render_admission.gpr && ./build-render-admission/render_admission_tests | tail -1
gprbuild -q -p -P continuous_submit.gpr
for t in continuous_submit_tests context_table_tests context_deregister_table_tests context_update_hold_tests; do
   ./build-continuous-submit/$t | tail -1
done
gprbuild -q -p -P gpu_queue.gpr && ./build-gpu-queue/gpu_queue_tests | tail -1
gprbuild -q -p -P live_ring_publish.gpr && ./build-live-ring-publish/ring_exhaustion_submit_tests | tail -1
gprbuild -q -p -P vm_update_pipeline.gpr && ./build-vm-update-pipeline/vm_update_pipeline_tests | tail -2
