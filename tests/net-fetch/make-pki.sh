#!/usr/bin/env bash
# SPDX-License-Identifier: GPL-3.0-or-later
# Test-only certificate for the net-fetch regression: fetch-test.cubit.internal,
# issued by the TLS regressions' test CA (tests/tls/make-pki.sh), under
# tests/net-fetch/build/pki (git-ignored; no keys are committed). Regenerated when
# missing, when the CA changed, or within 30 days of expiry.
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
bash "$here/../tls/make-pki.sh"
ca="$here/../tls/build/pki"
dir="$here/build/pki"
mkdir -p "$dir"
cd "$dir"
if [ -f fetch.pem ] && openssl x509 -in fetch.pem -noout -checkend $((30 * 86400)) >/dev/null &&
   openssl verify -CAfile "$ca/ca.pem" fetch.pem >/dev/null 2>&1; then
    exit 0
fi
cat > fetch.cnf <<'CNF'
[v3_leaf]
basicConstraints = critical, CA:FALSE
keyUsage = critical, digitalSignature
extendedKeyUsage = serverAuth
subjectAltName = DNS:fetch-test.cubit.internal
subjectKeyIdentifier = hash
authorityKeyIdentifier = keyid:always
CNF
openssl ecparam -name prime256v1 -genkey -noout -out fetch.key
openssl req -new -key fetch.key -subj "/CN=fetch-test.cubit.internal" -out fetch.csr 2>/dev/null
openssl x509 -req -in fetch.csr -CA "$ca/ca.pem" -CAkey "$ca/ca.key" \
    -set_serial "0x$(openssl rand -hex 16)" -sha256 -days 200 \
    -extfile fetch.cnf -extensions v3_leaf -out fetch.pem 2>/dev/null
rm -f fetch.csr
echo "tests/net-fetch: generated the fixture's certificate in $dir"
