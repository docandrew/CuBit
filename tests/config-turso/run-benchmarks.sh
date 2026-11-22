#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")"
repeats=3
mode=both
while [ "$#" -gt 0 ]; do
    case "$1" in
        --syscall-only) mode=syscall; shift ;;
        --repeats) repeats=${2:?missing repeat count}; shift 2 ;;
        *) echo "usage: run-benchmarks.sh [--syscall-only] [--repeats 1..20]" >&2; exit 2 ;;
    esac
done
[[ "$repeats" =~ ^([1-9]|1[0-9]|20)$ ]] || exit 2
cargo build --locked --release -j4 --features linux-uring --example bench
mkdir -p results
run_dir=$(mktemp -d "$PWD/results/io.XXXXXX")
echo "Benchmark artifacts: $run_dir"
{
    date -u
    uname -a
    rustc --version
    sha256sum Cargo.lock target/release/examples/bench
    echo "repeats=$repeats mode=$mode; Linux buffered files, no cache dropping or host policy changes"
    echo "TESTING=${TESTING-unset}"
    echo "LIMBO_DISABLE_FILE_LOCK=${LIMBO_DISABLE_FILE_LOCK-unset}"
    df -T "$run_dir"
    lscpu
    taskset -pc $$
    ulimit -l
} > "$run_dir/environment.txt"
for ((round=1; round<=repeats; round++)); do
    # Alternate order to expose order/cache/thermal bias. Do not pool latency
    # samples from different runs and pretend they are independent trials.
    backends=(syscall io-uring)
    if (( round % 2 == 0 )); then backends=(io-uring syscall); fi
    if [ "$mode" = syscall ]; then backends=(syscall); fi
    for backend in "${backends[@]}"; do
        log="$run_dir/$round-$backend.log"
        if ! timeout 180 target/release/examples/bench "$backend" "$run_dir/$round-$backend" > "$log" 2>&1; then
            echo "FAILED $backend; no fallback or partial comparison. See $log" >&2
            tail -12 "$log" >&2
            exit 1
        fi
        echo "PASS round=$round backend=$backend"
    done
done
python3 summarize-benchmarks.py "$run_dir"/*.log | tee "$run_dir/summary.md"
