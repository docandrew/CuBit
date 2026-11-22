#!/usr/bin/env bash
# SPDX-License-Identifier: GPL-3.0-or-later
#
# Certificates for tests/signed-admission that only openssl can make:
# other algorithms (P-256, RSA, Ed25519, ECDSA with SHA-512), foreign or
# missing extensions, and chains with deliberately broken links. Written,
# with their throwaway private keys, into the empty directory $1 at test
# time; nothing here is committed. Each <name>.der is the certificate and
# <name>.key its key (SEC 1 DER for P-384, PKCS #8 DER otherwise).
set -euo pipefail
out=$1
mkdir -p "$out"
cd "$out"
[ -z "$(ls -A .)" ] || { echo "fixtures.sh: $out is not empty" >&2; exit 1; }

key() {  # name  ec384|p256|rsa|ed25519
  case $2 in
    ec384) openssl genpkey -algorithm EC -pkeyopt ec_paramgen_curve:P-384 \
             -pkeyopt ec_param_enc:named_curve -out "$1.pem" 2>/dev/null
           openssl ec -in "$1.pem" -outform DER -out "$1.key" 2>/dev/null ;;
    p256)  openssl genpkey -algorithm EC -pkeyopt ec_paramgen_curve:P-256 \
             -pkeyopt ec_param_enc:named_curve -out "$1.pem" 2>/dev/null
           openssl pkey -in "$1.pem" -outform DER -out "$1.key" ;;
    rsa)   openssl genpkey -algorithm RSA -pkeyopt rsa_keygen_bits:2048 -out "$1.pem" 2>/dev/null
           openssl pkey -in "$1.pem" -outform DER -out "$1.key" ;;
    ed25519) openssl genpkey -algorithm ED25519 -out "$1.pem"
           openssl pkey -in "$1.pem" -outform DER -out "$1.key" ;;
  esac
}

extensions() {  # name  extension lines...
  local name=$1; shift
  { echo "[req]"; echo "distinguished_name = dn"; echo "prompt = no"
    echo "[dn]"; echo "CN = $name"
    echo "[ext]"; printf '%s\n' "$@"; } > "$name.cnf"
}

digest_for() {  # signer key name: openssl refuses a digest for Ed25519
  if openssl pkey -in "$1.pem" -noout -text 2>/dev/null | grep -q ED25519; then echo ""; else echo "-sha384"; fi
}

self_signed() {  # name  keytype  days  extension lines...
  local name=$1 type=$2 days=$3; shift 3
  key "$name" "$type"
  extensions "$name" "$@"
  # shellcheck disable=SC2046
  openssl req -x509 -new -key "$name.pem" -config "$name.cnf" -extensions ext \
    -days "$days" $(digest_for "$name") -out "$name.crt" 2>/dev/null
  openssl x509 -in "$name.crt" -outform DER -out "$name.der"
}

issued() {  # name  keytype  issuer  days  [digest]  extension lines...
  local name=$1 type=$2 issuer=$3 days=$4 digest; shift 4
  digest=$(digest_for "$issuer")
  if [ "${1:-}" = -sha512 ]; then digest=-sha512; shift; fi
  key "$name" "$type"
  extensions "$name" "$@"
  openssl req -new -key "$name.pem" -config "$name.cnf" -out "$name.csr" 2>/dev/null
  # shellcheck disable=SC2086
  openssl x509 -req -in "$name.csr" -CA "$issuer.crt" -CAkey "$issuer.pem" \
    -set_serial "0x$(openssl rand -hex 16)" -days "$days" $digest \
    -extfile "$name.cnf" -extensions ext -out "$name.crt" 2>/dev/null
  openssl x509 -in "$name.crt" -outform DER -out "$name.der"
}

CA_EXT=("basicConstraints = critical,CA:TRUE" "keyUsage = critical,keyCertSign,cRLSign"
        "subjectKeyIdentifier = hash" "extendedKeyUsage = codeSigning")
INTERMEDIATE_EXT=("basicConstraints = critical,CA:TRUE,pathlen:0" "keyUsage = critical,keyCertSign,cRLSign"
                  "subjectKeyIdentifier = hash" "authorityKeyIdentifier = keyid:always")
LEAF_EXT=("keyUsage = critical,digitalSignature" "extendedKeyUsage = codeSigning"
          "subjectKeyIdentifier = hash" "authorityKeyIdentifier = keyid:always")
CA_DAYS=3650
LEAF_DAYS=365

# The sound chain: a P-384 root, a leaf under it, an intermediate and a
# leaf under that. The tests admit these first, so a refusal below is
# about the one thing each fixture changes.
self_signed ca ec384 $CA_DAYS "${CA_EXT[@]}"
issued control-leaf ec384 ca $LEAF_DAYS "${LEAF_EXT[@]}"
openssl pkey -in control-leaf.pem -pubout -outform DER -out control-leaf.spki
issued intermediate ec384 ca $CA_DAYS "${INTERMEDIATE_EXT[@]}"
issued under-intermediate-leaf ec384 intermediate $LEAF_DAYS "${LEAF_EXT[@]}"

# Leaves that break the leaf profile.
self_signed self-signed-leaf ec384 $LEAF_DAYS "keyUsage = critical,digitalSignature" \
  "extendedKeyUsage = codeSigning" "subjectKeyIdentifier = hash" "authorityKeyIdentifier = keyid:always"
issued no-eku-leaf ec384 ca $LEAF_DAYS "keyUsage = critical,digitalSignature" \
  "authorityKeyIdentifier = keyid:always"
issued server-auth-leaf ec384 ca $LEAF_DAYS "keyUsage = critical,digitalSignature" \
  "extendedKeyUsage = serverAuth" "authorityKeyIdentifier = keyid:always"
issued any-eku-leaf ec384 ca $LEAF_DAYS "keyUsage = critical,digitalSignature" \
  "extendedKeyUsage = codeSigning,anyExtendedKeyUsage" "authorityKeyIdentifier = keyid:always"
issued code-server-leaf ec384 ca $LEAF_DAYS "keyUsage = critical,digitalSignature" \
  "extendedKeyUsage = codeSigning,serverAuth" "authorityKeyIdentifier = keyid:always"
issued code-client-leaf ec384 ca $LEAF_DAYS "keyUsage = critical,digitalSignature" \
  "extendedKeyUsage = codeSigning,clientAuth" "authorityKeyIdentifier = keyid:always"
issued code-ocsp-leaf ec384 ca $LEAF_DAYS "keyUsage = critical,digitalSignature" \
  "extendedKeyUsage = codeSigning,OCSPSigning" "authorityKeyIdentifier = keyid:always"
issued ca-true-leaf ec384 ca $LEAF_DAYS "basicConstraints = critical,CA:TRUE" \
  "keyUsage = critical,digitalSignature" "extendedKeyUsage = codeSigning" \
  "subjectKeyIdentifier = hash" "authorityKeyIdentifier = keyid:always"
issued no-digital-signature-leaf ec384 ca $LEAF_DAYS "keyUsage = critical,nonRepudiation" \
  "extendedKeyUsage = codeSigning" "authorityKeyIdentifier = keyid:always"
issued cert-sign-leaf ec384 ca $LEAF_DAYS "keyUsage = critical,digitalSignature,keyCertSign" \
  "extendedKeyUsage = codeSigning" "authorityKeyIdentifier = keyid:always"
issued noncritical-ku-leaf ec384 ca $LEAF_DAYS "keyUsage = digitalSignature" \
  "extendedKeyUsage = codeSigning" "authorityKeyIdentifier = keyid:always"
issued p256-leaf p256 ca $LEAF_DAYS "${LEAF_EXT[@]}"
issued rsa-leaf rsa ca $LEAF_DAYS "${LEAF_EXT[@]}"
issued ed25519-leaf ed25519 ca $LEAF_DAYS "${LEAF_EXT[@]}"
issued sha512-leaf ec384 ca $LEAF_DAYS -sha512 "${LEAF_EXT[@]}"
issued unknown-critical-leaf ec384 ca $LEAF_DAYS "${LEAF_EXT[@]}" "1.3.6.1.4.1.32473.1 = critical,ASN1:NULL"
issued long-leaf ec384 ca $((CA_DAYS * 2)) "${LEAF_EXT[@]}"

# Anchors that break the CA profile.
self_signed p256-ca p256 $CA_DAYS "${CA_EXT[@]}"
self_signed rsa-ca rsa $CA_DAYS "${CA_EXT[@]}"
self_signed ed25519-ca ed25519 $CA_DAYS "${CA_EXT[@]}"
self_signed no-bc-ca ec384 $CA_DAYS "keyUsage = critical,keyCertSign,cRLSign" "subjectKeyIdentifier = hash"
self_signed noncritical-bc-ca ec384 $CA_DAYS "basicConstraints = CA:TRUE" \
  "keyUsage = critical,keyCertSign,cRLSign" "subjectKeyIdentifier = hash"
self_signed no-certsign-ca ec384 $CA_DAYS "basicConstraints = critical,CA:TRUE" \
  "keyUsage = critical,digitalSignature" "subjectKeyIdentifier = hash"
self_signed server-eku-ca ec384 $CA_DAYS "basicConstraints = critical,CA:TRUE" \
  "keyUsage = critical,keyCertSign,cRLSign" "subjectKeyIdentifier = hash" "extendedKeyUsage = serverAuth"
# Each of those issues a sound leaf, so only the anchor is at fault.
for bad in p256-ca no-bc-ca noncritical-bc-ca no-certsign-ca server-eku-ca; do
  issued "$bad-leaf" ec384 "$bad" $LEAF_DAYS "${LEAF_EXT[@]}"
done

# The root's own key and name with another subjectKeyIdentifier: the
# control leaf's AKI no longer names it.
extensions ca-other "basicConstraints = critical,CA:TRUE" "keyUsage = critical,keyCertSign,cRLSign" \
  "subjectKeyIdentifier = 0102030405060708090A0B0C0D0E0F1011121314" "extendedKeyUsage = codeSigning"
sed -i 's/^CN = ca-other/CN = ca/' ca-other.cnf
# A wide window, so the link fails on the identifier alone.
openssl req -x509 -new -key ca.pem -config ca-other.cnf -extensions ext -sha384 \
  -not_before 20200101000000Z -not_after 20450101000000Z -out other-ski-ca.crt 2>/dev/null
openssl x509 -in other-ski-ca.crt -outform DER -out other-ski-ca.der
cp ca.key other-ski-ca.key

# Path lengths and depth.
self_signed pathlen0-root ec384 $CA_DAYS "basicConstraints = critical,CA:TRUE,pathlen:0" \
  "keyUsage = critical,keyCertSign,cRLSign" "subjectKeyIdentifier = hash"
issued under-pathlen0-intermediate ec384 pathlen0-root $CA_DAYS "${INTERMEDIATE_EXT[@]}"
issued under-pathlen0-leaf ec384 under-pathlen0-intermediate $LEAF_DAYS "${LEAF_EXT[@]}"
issued pathlen1-intermediate ec384 ca $CA_DAYS "basicConstraints = critical,CA:TRUE,pathlen:1" \
  "keyUsage = critical,keyCertSign,cRLSign" "subjectKeyIdentifier = hash" "authorityKeyIdentifier = keyid:always"
issued under-pathlen1-leaf ec384 pathlen1-intermediate $LEAF_DAYS "${LEAF_EXT[@]}"
issued p256-intermediate p256 ca $CA_DAYS "${INTERMEDIATE_EXT[@]}"
issued under-p256-leaf ec384 p256-intermediate $LEAF_DAYS "${LEAF_EXT[@]}"
issued second-intermediate ec384 intermediate $CA_DAYS "${INTERMEDIATE_EXT[@]}"
issued under-second-leaf ec384 second-intermediate $LEAF_DAYS "${LEAF_EXT[@]}"

rm -f ./*.csr ./*.srl ./*.cnf ./*.crt
