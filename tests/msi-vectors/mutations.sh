#!/usr/bin/env bash
# Mutation check for device interrupt vectors (docs/msi-interrupts.md).
# Each mutation breaks one rule in a copy of the allocator, the MSI / MSI-X
# plans or devmgr's programming sequence; the hosted tests (main.adb) must
# then fail. --prove also requires gnatprove (level 2) to reject the
# mutations that break a proved contract. A mutation whose text is not
# found, or that does not compile, is stale.
# Run inside the Nix shell: bash tests/msi-vectors/mutations.sh [--prove]
set -uo pipefail
here=$(cd "$(dirname "$0")" && pwd)
root=$here/../..
cd "$root/kernel"
WORK=$(mktemp -d "${TMPDIR:-/tmp}/msi-mutations.XXXXXX")
trap 'rm -rf "$WORK"' EXIT
prove=0
[[ ${1:-} == --prove ]] && prove=1

caught=0
survived=0
stale=0

mutate() {
    local name=$1 proved=$2 file=$3 from=$4 to=$5
    rm -rf "$WORK/shared" "$WORK/kernel" "$WORK/devmgr" "$WORK/obj"
    mkdir -p "$WORK/shared" "$WORK/kernel" "$WORK/devmgr"
    cp "$root"/shared/interrupts/device_vector_abi.ad? "$WORK/shared/"
    cp "$root"/kernel/src/device_vectors.ad? "$WORK/kernel/"
    cp "$root"/userspace/services/devmgr/pci_message_*.ad? "$WORK/devmgr/"
    if ! python3 - "$WORK/$file" "$from" "$to" <<'PY'
import sys
path, old, new = sys.argv[1:4]
text = open(path).read()
if text.count(old) != 1:
    sys.exit(1)
open(path, 'w').write(text.replace(old, new))
PY
    then
        echo "STALE    $name"
        stale=$((stale + 1))
        return
    fi
    local vars=(-XMSI_SHARED_SRC="$WORK/shared" -XMSI_KERNEL_SRC="$WORK/kernel"
                -XMSI_DEVMGR_SRC="$WORK/devmgr" -XMSI_VECTORS_OBJ="$WORK/obj"
                -XMSI_VECTORS_EXEC="$WORK/obj" -XMSI_VECTORS_ASSERTIONS=-gnatn)
    if ! alr exec -- gprbuild -p -q -P "$here/msi_vectors.gpr" "${vars[@]}" \
         >"$WORK/build.log" 2>&1; then
        echo "NO-BUILD $name"
        stale=$((stale + 1))
        return
    fi
    local result
    if timeout 60 "$WORK/obj/main" >"$WORK/run.log" 2>&1; then
        result="SURVIVED"
    else
        result="caught  "
    fi
    local proof=""
    if [[ $prove == 1 && $proved == proved ]]; then
        if alr exec -- gnatprove -P "$here/msi_vectors.gpr" "${vars[@]}" \
             -u device_vectors.adb -u pci_message_interrupts.adb \
             --level=2 -j4 --checks-as-errors=on >"$WORK/prove.log" 2>&1; then
            proof=" proof PASSED (mutation not rejected)"
            result="SURVIVED"
        else
            proof=" proof rejected"
        fi
    fi
    if [[ $result == SURVIVED ]]; then
        echo "SURVIVED $name$proof"
        survived=$((survived + 1))
    else
        echo "caught   $name ($(grep -m1 'FAIL\|raised' "$WORK/run.log"))$proof"
        caught=$((caught + 1))
    fi
}

# A double-allocated vector: the allocator binds a block without checking
# that its vectors are free.
mutate "allocate skips the free check (double allocation)" proved kernel/device_vectors.adb \
    "if Fits (H, Count) and then Run_Free (T, H, Count) then" \
    "if Fits (H, Count) and then (Run_Free (T, H, Count) or else True) then"
mutate "a retired vector counts as free (reuse before release)" proved kernel/device_vectors.adb \
    "            if T (V).State /= Free then
                return False;" \
    "            if T (V).State = Bound then
                return False;"
mutate "release frees a member that is not the block's head" proved kernel/device_vectors.adb \
    "        if not Is_Head (T, Head) then" \
    "        if T (Head).State = Free then"
mutate "retire keeps delivering to the dead owner" proved kernel/device_vectors.adb \
    "                T (V).State := Retired;" \
    "                null;"
# A missing mask: an entry's address and data written while it may send.
mutate "MSI-X entry programmed without masking it first" proved devmgr/pci_message_interrupts.adb \
    "      P.Writes (1) := (Vector_Control, VECTOR_MASKED);
      P.Writes (2) := (Address_Low, Message_Address (Destination));" \
    "      P.Writes (1) := (Vector_Control, VECTOR_UNMASKED);
      P.Writes (2) := (Address_Low, Message_Address (Destination));"
mutate "MSI-X teardown releases without masking the entries (missing mask)" unproved devmgr/pci_message_functions.adb \
    "         Execute (I, Mask_Entry);" \
    "         null;"
mutate "MSI reprogrammed while enabled (missing MSI mask)" proved devmgr/pci_message_interrupts.ads \
    "     (Control and not MSI_ENABLE);" \
    "     (Control);"
mutate "MSI teardown leaves MSI enabled" unproved devmgr/pci_message_functions.adb \
    "      Execute (Disable_MSI (Read_Config_16 (MSI_CONTROL_OFFSET)));" \
    "      null;"
mutate "MSI enables several messages" proved devmgr/pci_message_interrupts.ads \
    "     ((Control and not MSI_MULTIPLE_ENABLE) or MSI_ENABLE)" \
    "     (Control or MSI_ENABLE)"

echo "msi-vectors mutations: $caught caught, $survived survived, $stale stale"
[[ $survived == 0 && $stale == 0 ]]
