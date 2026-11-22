#!/usr/bin/env bash
# A checkout of one of Penny's forks (FORKS) at its pinned commit:
#   fork-checkout.sh servo|mozjs DIR
# DIR is created when missing (a shallow fetch of exactly that commit). An
# existing DIR must be a checkout of the fork: one at another commit moves
# to the pin when its tracked files are unchanged; local edits stop the
# build (commit them on the fork and pin the new commit instead). The URL
# is FORKS's unless PENNY_SERVO_URL / PENNY_MOZJS_URL say otherwise.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
name=${1:?usage: fork-checkout.sh servo|mozjs DIR}
dir=${2:?usage: fork-checkout.sh servo|mozjs DIR}
read -r commit default_url < <(awk -v n="$name" '$1 == n { print $2, $3 }' "$here/FORKS")
[ -n "${commit:-}" ] || { echo "fork-checkout.sh: no fork $name in FORKS" >&2; exit 1; }
case $name in
    servo) url=${PENNY_SERVO_URL:-$default_url} ;;
    mozjs) url=${PENNY_MOZJS_URL:-$default_url} ;;
esac
# The checkout's directory name is part of the output: SpiderMonkey's
# assertions name source files by paths relative to its object directory
# (../../servo/cubit-build/...), which no prefix map rewrites. Callers keep
# servo-work/servo.
if [ -e "$dir" ] && [ "$(git -C "$dir" rev-parse -q --verify HEAD 2>/dev/null)" = "$(cat "$here/UPSTREAM_REVISION")" ]; then
    echo "fork-checkout.sh: $dir is an upstream Servo checkout patched by the retired patch_servo.py;" >&2
    echo "remove it (it is disposable) and the build fetches the fork" >&2
    exit 1
fi
if [ ! -e "$dir" ]; then
    mkdir -p "$dir"
    git -C "$dir" init -q
fi
if [ "$(git -C "$dir" rev-parse -q --verify HEAD 2>/dev/null)" != "$commit" ]; then
    if git -C "$dir" rev-parse -q --verify HEAD >/dev/null && ! git -C "$dir" diff --quiet HEAD -- . ':!Cargo.lock'; then
        echo "fork-checkout.sh: $dir has local changes and is not at the pinned $commit" >&2
        exit 1
    fi
    git -C "$dir" cat-file -e "$commit^{commit}" 2>/dev/null ||
        git -C "$dir" fetch -q --depth 1 "$url" "$commit"
    git -C "$dir" checkout -q --detach "$commit"
fi
# Penny's tree is the fork's commit exactly (servo-cargo.sh copies our
# Cargo.lock over Servo's; cubit-build/ is staged from this repository).
if ! git -C "$dir" diff --quiet HEAD -- . ':!Cargo.lock'; then
    echo "fork-checkout.sh: $dir differs from the pinned $commit:" >&2
    git -C "$dir" status --short -- . ':!Cargo.lock' >&2
    exit 1
fi
grep -qx '/cubit-build/' "$dir/.git/info/exclude" 2>/dev/null || echo '/cubit-build/' >> "$dir/.git/info/exclude"
