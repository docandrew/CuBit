#!/usr/bin/env bash
# ANV's isolated target compiler. Host generators use the host shell instead.
# Resolve the pinned libc toolchain; never inherit Linux headers from Nix's
# compiler wrapper or ambient include-path variables.
set -euo pipefail
language=${1:?c or cpp required}; shift
case $language in c|cpp) ;; *) exit 2 ;; esac
root=$(cd "$(dirname "$0")/../.." && pwd)
sysroot=$root/userspace/libc/build/sysroot
cross=$(cat "$root/userspace/libc/build/cross-gcc")
raw=$(cat "$cross/nix-support/orig-cc")
target=x86_64-unknown-linux-musl
cc=$raw/bin/$target-gcc
if [[ $language == cpp ]]; then cc=$raw/bin/$target-g++; fi
ld_wrapper=$(readlink -f "$cross/bin/$target-ld")
binutils=$(cat "$(dirname "$(dirname "$ld_wrapper")")/nix-support/orig-bintools")
unset CPATH C_INCLUDE_PATH CPLUS_INCLUDE_PATH OBJC_INCLUDE_PATH
export NIX_HARDENING_ENABLE=""
common=(-B"$binutils/$target/bin/" -nostdinc
        -fno-pie -no-pie -mno-red-zone -fno-stack-protector)
if [[ $language == cpp ]]; then
    version=$("$cc" -dumpfullversion)
    common+=(-nostdinc++ -isystem "$raw/include/c++/$version"
             -isystem "$raw/include/c++/$version/$target"
             -isystem "$raw/include/c++/$version/backward")
fi
common+=(-isystem "$sysroot/include" -isystem "$("$cc" -print-file-name=include)")
manifest=(); args=(); link=1
while [[ $# -gt 0 ]]; do
    case $1 in
        --manifest) manifest=("${2:?manifest object required}"); shift 2 ;;
        -c|-S|-E) link=0; args+=("$1"); shift ;;
        *) args+=("$1"); shift ;;
    esac
done
if [[ $link == 0 ]]; then exec "$cc" "${common[@]}" "${args[@]}"; fi
lib() { "$cc" -print-file-name="$1"; }
libraries=()
if [[ $language == cpp ]]; then libraries+=("$(lib libstdc++.a)" "$(lib libsupc++.a)"); fi
libraries+=("$sysroot/lib/libc.a" "$("$cc" -print-libgcc-file-name)")
if [[ $language == cpp ]]; then libraries+=("$(lib libgcc_eh.a)"); fi
exec "$cc" "${common[@]}" -nostdlib -static -L"$sysroot/lib" \
    -Wl,-T,"$root/userspace/mesa/anv/native_build_id.ld" \
    -Wl,-T,"$sysroot/lib/cubit.ld" -Wl,-z,stack-size="${CUBIT_STACK_SIZE:-1048576}" \
    -Wl,-z,noexecstack -Wl,-z,noseparate-code \
    "$sysroot/lib/cubit-crt1.o" "$sysroot/lib/crti.o" "$(lib crtbeginT.o)" \
    "${args[@]}" "${manifest[@]}" \
    -Wl,--start-group "${libraries[@]}" -Wl,--end-group \
    "$(lib crtend.o)" "$sysroot/lib/crtn.o"
