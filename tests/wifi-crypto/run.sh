#!/usr/bin/env bash
#  Known-answer tests for the Wi-Fi driver's crypto units (WiFi_Crypto in
#  userspace/lib/wifi/crypto; docs/wifi.md, W2); --prove also runs
#  gnatprove on every WiFi_Crypto unit. Linux-hosted. Run in the Nix shell
#  from anywhere: the crates are the flake's pinned SPARKTLSCrypto and
#  SPARKNaCl, unmodified (CUBIT_SPARK_CRATES).
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
crypto="$here/../../userspace/lib/wifi/crypto"
cd "$here"
gprbuild -p -q -j0 -P wifi_crypto_tests.gpr
./build/wifi_crypto_tests
if [[ ${1:-} == --prove ]]; then
    #  Memory-capped, as every CuBit proof run; level and switches are in
    #  the Prove package. Every WiFi_Crypto unit is named, so a new one is
    #  proved without editing this script; generics are proved through
    #  their instances.
    units=()
    for f in "$crypto"/wifi_crypto*.ad[sb]; do
        units+=(-u "$(basename "$f")")
    done
    systemd-run --user --scope -q -p MemoryMax=24G -p MemorySwapMax=0 \
        gnatprove -P wifi_crypto_proof.gpr -j2 --output=oneline "${units[@]}"
fi
