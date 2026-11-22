#!/usr/bin/env bash
# Test-only certificate for the llm-mock regression: llm-mock.cubit.internal,
# issued by the TLS regressions' test CA (tests/tls/make-pki.sh), under
# tests/llm/build/pki (git-ignored; no keys are committed). Regenerated when
# missing, when the CA changed, or within 30 days of expiry.
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
bash "$here/../tls/make-pki.sh"
ca="$here/../tls/build/pki"
dir="$here/build/pki"
mkdir -p "$dir"
cd "$dir"
if [ -f llm.pem ] && openssl x509 -in llm.pem -noout -checkend $((30 * 86400)) >/dev/null &&
   openssl verify -CAfile "$ca/ca.pem" llm.pem >/dev/null 2>&1; then
    exit 0
fi
cat > llm.cnf <<'CNF'
[v3_leaf]
basicConstraints = critical, CA:FALSE
keyUsage = critical, digitalSignature
extendedKeyUsage = serverAuth
subjectAltName = DNS:llm-mock.cubit.internal
subjectKeyIdentifier = hash
authorityKeyIdentifier = keyid:always
CNF
openssl ecparam -name prime256v1 -genkey -noout -out llm.key
openssl req -new -key llm.key -subj "/CN=llm-mock.cubit.internal" -out llm.csr 2>/dev/null
openssl x509 -req -in llm.csr -CA "$ca/ca.pem" -CAkey "$ca/ca.key" \
    -set_serial "0x$(openssl rand -hex 16)" -sha256 -days 200 \
    -extfile llm.cnf -extensions v3_leaf -out llm.pem 2>/dev/null
rm -f llm.csr
echo "tests/llm: generated the mock's certificate in $dir"
