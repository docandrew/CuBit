#!/usr/bin/env bash
# The wallpaper catalog, its build and its readers (README.md). Run inside
# nix develop:
#   tests/wallpaper-catalog/run.sh           build checks + hosted tests
#   tests/wallpaper-catalog/run.sh --prove   prove the changed SPARK units
#                                             (under systemd-run MemoryMax=24G)
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/../.." && pwd)"
tool="$root/userspace/ccl/build/manifest/ccl-manifest"
cd "$root/kernel"
case "${1:-}" in
  --prove)
    # The decoder at its existing level 1; the store, the GPU backdrop owner,
    # its upload and capture, and the appearance type at level 2.
    prove() { alr exec -- gnatprove "$@" --mode=all --timeout=30 -j2 --checks-as-errors=on --report=fail; }
    prove -P "$root/tests/qoi/qoi.gpr" -u cubit-qoi.adb --level=1
    prove -P "$root/tests/qoi/store.gpr" -u desktop_wallpaper_store.adb --level=2
    prove -P "$root/tests/compositor/desktop_backdrop.gpr" --level=2 \
      -u desktop_backdrop_owner.adb desktop_backdrop_upload.adb desktop_gpu_scene-backdrop.adb
    prove -P "$root/tests/appearance/appearance.gpr" -u cubit-appearance.adb --level=2
    # The generated decoders: the catalog line's and the appearance setting's.
    prove -P "$here/prove_codec.gpr" -u ccl-asset_codecs-wallpaper.adb --level=2
    prove -P "$root/tests/setting-codecs/prove.gpr" -u ccl-setting_codecs-appearance.adb --level=2 ;;
  *)
    make -s ccl-manifest
    scratch="$(mktemp -d "${TMPDIR:-/tmp}/wallpaper-catalog.XXXXXX")"
    trap 'rm -rf "$scratch"' EXIT
    # The generated decoder in the tree is what the type generates.
    "$tool" --asset-codecs "$scratch"
    for f in ccl-asset_codecs.ads ccl-asset_codecs-wallpaper.ads ccl-asset_codecs-wallpaper.adb; do
      cmp "$scratch/$f" "$root/userspace/ccl/src/$f" || {
        echo "FAIL: userspace/ccl/src/$f is stale: make -C kernel asset-codecs" >&2; exit 1; }
    done
    # The real source list builds a one-wallpaper catalog.
    "$tool" --wallpaper-catalog "$root/assets/wallpapers/wallpapers.ccl" "$scratch/catalog.ccl" > "$scratch/plan"
    test "$(wc -l < "$scratch/catalog.ccl")" -eq 1
    grep -q '^cubes	wallpaper2.png	2048	576$' "$scratch/plan"
    # Source lists the type system or the catalog rules refuse.
    mkdir -p "$scratch/src"
    cp "$root/assets/wallpapers/wallpapers.ccl" "$scratch/src/ok.ccl"
    touch "$scratch/src/a.png" "$scratch/src/b.png"
    entry() { printf '(Wallpaper_Source name => "%s" label => "%s" source => "%s" width => %s height => 9 attribution => "%s")' "$@"; }
    refuse() {  # NAME TEXT EXPECTED-MESSAGE
      printf '%s\n' "$2" > "$scratch/src/$1.ccl"
      if "$tool" --wallpaper-catalog "$scratch/src/$1.ccl" "$scratch/out.ccl" > /dev/null 2> "$scratch/err"; then
        echo "FAIL: $1 was accepted" >&2; exit 1
      fi
      grep -q -- "$3" "$scratch/err" || { echo "FAIL: $1: $(cat "$scratch/err")" >&2; exit 1; }
      echo "refused $1: $(cat "$scratch/err")"
    }
    refuse duplicate "(Wallpaper_Sources images => [$(entry a A a.png 16 x) $(entry a B b.png 16 x)])" "declared twice"
    refuse missing-file "(Wallpaper_Sources images => [$(entry a A gone.png 16 x)])" "does not exist"
    refuse bad-name "(Wallpaper_Sources images => [$(entry Upper A a.png 16 x)])" "is not 1 to"
    refuse no-attribution "(Wallpaper_Sources images => [$(entry a A a.png 16 '')])" "attribution"
    refuse empty "(Wallpaper_Sources images => [])" "lists no wallpaper\\|empty list"
    refuse ill-typed "(Wallpaper_Sources images => [$(entry a A a.png 0 x)])" "not a Wallpaper_Sources\|does not evaluate"
    refuse escape "(Wallpaper_Sources images => [$(entry a A ../a.png 16 x)])" "is not a path beneath"
    # Three pictures make a three-line catalog, in order.
    printf '%s\n' "(Wallpaper_Sources images => [$(entry a A a.png 16 x) $(entry b B b.png 16 x) $(entry c C a.png 16 x)])" \
      > "$scratch/src/three.ccl"
    "$tool" --wallpaper-catalog "$scratch/src/three.ccl" "$scratch/three.ccl" > "$scratch/three.plan"
    test "$(wc -l < "$scratch/three.ccl")" -eq 3
    test "$(cut -f1 "$scratch/three.plan" | tr '\n' ' ')" = "a b c "
    echo "PASS wallpaper source lists"
    alr exec -- gprbuild -p -q -j8 -P "$here/wallpaper_catalog.gpr"
    "$here/build/wallpaper_catalog_tests"
    "$here/build/wallpaper_catalog_tests" fallback ;;
esac
