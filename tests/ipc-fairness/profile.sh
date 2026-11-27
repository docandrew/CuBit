#!/usr/bin/env bash
#  The startup profile of one endpoint fairness case (headless
#  bench-fairness-CASE, IPC revision 2 S0): bench-ipc-server and the case's
#  fairness-client processes, all timing the same window.
#
#  profile.sh CASE          print the profile
#  profile.sh CASE --count  print how many clients it starts
#
#  Cases: n1, n8, n64 (that many one-thread clients) and big-small (a
#  64-thread client, label 1, against a one-thread client, label 2).
set -euo pipefail
case_name=${1:?usage: profile.sh CASE [--count]}
#  Monotonic milliseconds since boot: every client has started and
#  calibrated its clock (600 ms) by then; the window is five seconds.
start=15000
window=5000
clients=()
case $case_name in
    n1|n8|n64)
        for ((i = 1; i <= ${case_name#n}; i++)); do clients+=("$i 1"); done ;;
    big-small)
        clients=("1 64" "2 1") ;;
    *)
        echo "profile.sh: unknown case $case_name" >&2; exit 2 ;;
esac
if [[ ${2:-} == --count ]]; then
    echo "${#clients[@]}"
    exit 0
fi
echo "# Endpoint fairness, case $case_name (tests/ipc-fairness/profile.sh)."
echo "(startup v1"
echo "  (start \"logstore.svc\" (test-console approve-declared))"
echo "  (start \"bench-ipc-server.app\")"
for c in "${clients[@]}"; do
    read -r label threads <<<"$c"
    echo "  (start \"fairness-client.app\""
    echo "    (arguments (Fair_Parameters label => $label threads => $threads start => $start window => $window)))"
done
echo ")"
