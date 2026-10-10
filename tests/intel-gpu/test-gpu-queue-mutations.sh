#!/usr/bin/env bash
# Mutation check for the GPU-001 step 2 queue (Linux-hosted). Each mutation
# breaks one rule in a copy of the driver sources; gpu_queue_tests must then
# fail. A mutation whose text is not found is an error (the check is stale).
# Run from anywhere inside the Nix shell: bash tests/intel-gpu/test-gpu-queue-mutations.sh
set -uo pipefail
cd "$(dirname "$0")/../../kernel"
SRC=../userspace/services/intel-gpu
WORK=$(mktemp -d "${TMPDIR:-/tmp}/gpu-queue-mutations.XXXXXX")
trap 'rm -rf "$WORK"' EXIT

caught=0
survived=0
stale=0

mutate() {
    local name=$1 file=$2 from=$3 to=$4
    rm -rf "$WORK/src" "$WORK/obj"
    mkdir -p "$WORK/src"
    cp "$SRC"/*.ad? "$WORK/src/"
    if ! python3 - "$WORK/src/$file" "$from" "$to" <<'EOF'
import sys
path, old, new = sys.argv[1:4]
text = open(path).read()
if text.count(old) != 1:
    sys.exit(1)
open(path, 'w').write(text.replace(old, new))
EOF
    then
        echo "STALE    $name"
        stale=$((stale + 1))
        return
    fi
    # A mutant that does not compile proves nothing: count it as stale.
    if ! QUEUE_SRC="$WORK/src" QUEUE_OBJ="$WORK/obj" \
         alr exec -- gprbuild -p -q -P ../tests/intel-gpu/gpu_queue.gpr >"$WORK/build.log" 2>&1; then
        echo "NO-BUILD $name"
        stale=$((stale + 1))
    elif "$WORK/obj/gpu_queue_tests" >"$WORK/run.log" 2>&1; then
        echo "SURVIVED $name"
        survived=$((survived + 1))
    else
        echo "caught   $name ($(grep -m1 FAIL "$WORK/run.log" || echo 'assertion'))"
        caught=$((caught + 1))
    fi
}

mutate "window retires the protected segment" intel_gpu_segment_window.adb \
    "W.Items (2).Seq > Completed;" "W.Items (2).Seq > Completed + 1;"
mutate "window plans from the tail, not the retired head" intel_gpu_segment_window.adb \
    "(RR.Reserve (Ring_Offset (Head (W)), Ring_Offset (Tail (W)), Bytes));" \
    "(RR.Reserve (Ring_Offset (Tail (W)), Ring_Offset (Tail (W)), Bytes));"
mutate "ledger accepts a timeline past what was published" intel_gpu_context_ledger.adb \
    "if not Intel_GPU_Timeline.Accepted (Seen) then" \
    "if Seen = Intel_GPU_Timeline.Regressed then"
mutate "ledger accepts a regressed timeline" intel_gpu_context_ledger.adb \
    "if not Intel_GPU_Timeline.Accepted (Seen) then" \
    "if Seen = Intel_GPU_Timeline.Beyond_Published then"
mutate "ledger accepts a failed read" intel_gpu_context_ledger.adb \
    "elsif not Read_OK then" "elsif False then"
mutate "ledger reports lost jobs as done" intel_gpu_context_ledger.adb \
    "Status := (if V <= L.Done then Done else Lost_Job);" "Status := Done;"
mutate "no hang watchdog" intel_gpu_context_ledger.adb \
    "if Now - L.Progress >= Hang_Budget then" "if False then"
mutate "no deadline watchdog" intel_gpu_context_ledger.adb \
    "elsif L.Jobs (Slot_Of (L.Done + 1)).Deadline <= Now then" "elsif False then"
mutate "admission ignores the signal value" intel_gpu_queue_admission.adb \
    "Value (D.Signal_Value) /= View (D.Context).Accepted + 1" "False"
mutate "admission ignores the in-flight limit" intel_gpu_queue_admission.adb \
    "elsif View (D.Context).Owed >= Capacity then" "elsif False then"
mutate "admission ignores waits" intel_gpu_queue_admission.adb \
    "elsif not Wait_Reached (D.Wait_1_Context, D.Wait_1_Value, View) or else" \
    "elsif False and then not Wait_Reached (D.Wait_1_Context, D.Wait_1_Value, View) and then"
mutate "admission ignores quiesce" intel_gpu_queue_admission.adb \
    "elsif Quiescing then" "elsif False then"
mutate "admission ignores deadlines" intel_gpu_queue_admission.adb \
    "if Now >= Microseconds (D.Deadline) then" "if False then"
mutate "refusal does not fault the context" intel_gpu_session_queue.adb \
    "if Fault_It and then S.Opened (C) then" "if False then"
mutate "service ignores the completion gate" intel_gpu_queue_service.adb \
    "Gate := T.Kicks (S) (C) = No_Kick and then Scheduling_Resident;" "Gate := True;"
mutate "service drops backpressured kicks" intel_gpu_queue_service.adb \
    "T.Counters.Kick_Retries := T.Counters.Kick_Retries + 1;" \
    "T.Kicks (S) (C) := No_Kick;"
mutate "service reports failed calls as OK" intel_gpu_queue_service.adb \
    "Call_Finished (S, Unsigned_64 (V), Status = Ledgers.Done);" \
    "Call_Finished (S, Unsigned_64 (V), True);"
mutate "service skips the ownership check" intel_gpu_queue_service.adb \
    "            if not Select_Context (S, C) or else not Owner_Ready then
               Fail_Session (T, S, Q.Device_Fault, Now);
               return;
            end if;
            Read_Timeline (V, OK);" \
    "            if not Select_Context (S, C) then
               return;
            end if;
            Read_Timeline (V, OK);"
mutate "wake answered before its condition" intel_gpu_queue_wakes.adb \
    "Answer_Held := Item.Current = Held and then Holds;" "Answer_Held := Item.Current = Held;"
mutate "wake held without a slot" intel_gpu_queue_wakes.adb \
    "elsif Slot_Free or else Was_Held then" "elsif True then"

echo "GPU queue mutations: $caught caught, $survived survived, $stale stale"
[[ $survived -eq 0 && $stale -eq 0 ]]
