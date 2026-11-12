#!/usr/bin/env bash
set -euo pipefail
export TMPDIR=/home/doc/cubit-build-tmp
ulimit -s 65536
ulimit -c 0
suite_dir=$(cd -- "$(dirname -- "$0")" && pwd)
repo_dir=$(cd -- "$suite_dir/../.." && pwd)
# Functional mode deliberately has no timing claim and disjoint outputs.
if [[ "${1:-}" == --functional-only ]]; then
    shift
    exec nice -n 19 node "$suite_dir/run.mjs" --functional-only "$@"
fi
if [[ -n "${CUBIT_BUILD_LOCK:-}" ]]; then
    shared_lock=$CUBIT_BUILD_LOCK
else
    common_git_dir=$(git -C "$repo_dir" rev-parse --path-format=absolute --git-common-dir)
    if [[ $(basename -- "$common_git_dir") != .git ]]; then
        printf '%s\n' 'Set CUBIT_BUILD_LOCK for a repository with a separate Git directory.' >&2
        exit 2
    fi
    shared_lock="$(dirname -- "$common_git_dir")/coordination/build.lock"
fi
exec flock --exclusive "$shared_lock" nice -n 19 node "$suite_dir/run.mjs" "$@"
