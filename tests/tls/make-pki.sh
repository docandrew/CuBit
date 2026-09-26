#!/usr/bin/env bash
# Test-only PKI for the TLS regressions, generated under tests/tls/build/pki
# (git-ignored; no keys are committed). Regenerated when missing or when the
# valid leaf expires within 30 days. ECDSA P-256 throughout.
#   ca          trusted root embedded in tls-probe
#   good        tls-test.cubit.internal, issued by ca
#   wronghost   other.cubit.internal, issued by ca
#   untrusted   tls-test.cubit.internal, issued by a root the guest lacks
#   expired     tls-test.cubit.internal, issued by ca, validity in the past
set -euo pipefail
dir="$(cd "$(dirname "$0")" && pwd)/build/pki"
mkdir -p "$dir"
cd "$dir"
if [ -f good.pem ] && openssl x509 -in good.pem -noout -checkend $((30 * 86400)) >/dev/null; then
    exit 0
fi
rm -f ./*.pem ./*.der ./*.key ./*.srl ./*.cnf

cat > ca.cnf <<'CNF'
[v3_ca]
basicConstraints = critical, CA:TRUE
keyUsage = critical, keyCertSign, cRLSign
subjectKeyIdentifier = hash
CNF

leaf_cnf() {
    cat > "$1.cnf" <<CNF
[v3_leaf]
basicConstraints = critical, CA:FALSE
keyUsage = critical, digitalSignature
extendedKeyUsage = serverAuth
subjectAltName = DNS:$2
subjectKeyIdentifier = hash
authorityKeyIdentifier = keyid:always
CNF
}

make_ca() {
    openssl ecparam -name prime256v1 -genkey -noout -out "$1.key"
    openssl req -new -x509 -key "$1.key" -sha256 -days 3650 \
        -subj "/O=CuBit Test/CN=$2" -extensions v3_ca -config ca.cnf \
        -out "$1.pem" 2>/dev/null
    openssl x509 -in "$1.pem" -outform DER -out "$1.der"
}

make_leaf() { # name host issuer [not_before not_after]
    openssl ecparam -name prime256v1 -genkey -noout -out "$1.key"
    openssl req -new -key "$1.key" -subj "/CN=$2" -out "$1.csr" 2>/dev/null
    leaf_cnf "$1" "$2"
    local validity=(-days 200)
    if [ $# -ge 5 ]; then validity=(-not_before "$4" -not_after "$5"); fi
    openssl x509 -req -in "$1.csr" -CA "$3.pem" -CAkey "$3.key" \
        -set_serial "0x$(openssl rand -hex 16)" \
        -sha256 "${validity[@]}" -extfile "$1.cnf" -extensions v3_leaf \
        -out "$1.pem" 2>/dev/null
    rm -f "$1.csr"
}

make_ca ca "CuBit Test Root"
make_ca other-ca "CuBit Untrusted Root"
make_leaf good tls-test.cubit.internal ca
make_leaf wronghost other.cubit.internal ca
make_leaf untrusted tls-test.cubit.internal other-ca
make_leaf expired tls-test.cubit.internal ca 20250101000000Z 20250601000000Z
echo "tests/tls: generated test PKI in $dir"
