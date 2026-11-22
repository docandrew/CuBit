#!/usr/bin/env bash
# SPDX-License-Identifier: GPL-3.0-or-later
#
# ccl-sign end to end (SIGN-001, docs/signed-binaries.md section 7): init,
# sign, verify, digest; determinism and idempotence; and the signing
# directories it must refuse to sign with: a weak key, a key that is not
# the certificate's, a CA without basicConstraints, a leaf without the
# code-signing EKU, an expired and a not-yet-valid leaf. Every key is made
# here, in a temporary directory removed afterwards. Run from the Nix shell
# after building ccl-sign (run.sh does both).
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/../.." && pwd)
sign=${CCL_SIGN:-$root/userspace/ccl/build/sign/ccl-sign}
work=$(mktemp -d "${TMPDIR:-/tmp}/ccl-sign-cli.XXXXXX")
trap 'rm -rf "$work"' EXIT
cd "$work"
failures=0
checks=0
pass() { checks=$((checks + 1)); }
fail() { checks=$((checks + 1)); failures=$((failures + 1)); echo "FAIL: $*"; }
expect() {  # status description command...
  local want=$1 what=$2; shift 2
  local got=0
  "$@" > out.txt 2>&1 || got=$?
  if [ "$got" = "$want" ]; then pass; else fail "$what: exit $got, wanted $want: $(head -c 300 out.txt)"; fi
}

head -c 200000 /dev/urandom > program
chmod 755 program

# init makes a private directory and refuses to overwrite one.
expect 0 "init" "$sign" init --signing keys --name "CLI test"
[ "$(stat -c %a keys)" = 700 ] && pass || fail "the signing directory is 0700"
[ "$(stat -c %a keys/builder.key)" = 600 ] && [ "$(stat -c %a keys/builder-ca.key)" = 600 ] \
  && pass || fail "key files are 0600"
expect 2 "init refuses a directory that holds files" "$sign" init --signing keys --name "Again"
expect 2 "init needs a name" "$sign" init --signing other
openssl verify -CAfile <(openssl x509 -inform DER -in keys/builder-ca.der) \
  <(openssl x509 -inform DER -in keys/builder.der) > /dev/null && pass || fail "openssl verifies the chain"
openssl x509 -inform DER -in keys/builder.der -noout -ext extendedKeyUsage | grep -q "Code Signing" \
  && pass || fail "the leaf carries codeSigning"
openssl x509 -inform DER -in keys/builder-ca.der -noout -ext basicConstraints | grep -q "critical" \
  && pass || fail "the CA carries a critical basicConstraints"

# sign, verify, digest.
expect 0 "sign" "$sign" sign --signing keys program program.signed
[ "$(stat -c %a program.signed)" = 755 ] && pass || fail "signing keeps the execute bits"
cmp -n 200000 program program.signed > /dev/null && pass || fail "the payload is unchanged"
expect 0 "verify" "$sign" verify --anchor keys/builder-ca.der program.signed
expect 1 "verify refuses the unsigned program" "$sign" verify --anchor keys/builder-ca.der program
grep -q NOT_SIGNED out.txt && pass || fail "unsigned is NOT_SIGNED"
[ "$("$sign" digest program.signed | cut -d' ' -f1)" = "$(sha384sum program | cut -d' ' -f1)" ] \
  && pass || fail "digest names the payload's SHA-384"
expect 0 "verify with an identity and the hardware ceiling" "$sign" verify --anchor keys/builder-ca.der \
  --identity com.cubit.test --authority hardware program.signed

# A program larger than a thread's stack (desktop.svc is tens of MiB):
# signing once copied the payload onto the stack and overflowed.
head -c $((64 * 1024 * 1024)) /dev/urandom > large
expect 0 "signing a 64 MiB program" "$sign" sign --signing keys large large.signed
expect 0 "verifying a 64 MiB program" "$sign" verify --anchor keys/builder-ca.der large.signed
rm -f large large.signed

# Determinism and idempotence.
"$sign" sign --signing keys program program.again > /dev/null
cmp program.signed program.again && pass || fail "signing twice gives the same bytes"
expect 0 "re-signing a file this key signed" "$sign" sign --signing keys program.signed program.same
cmp program.signed program.same && pass || fail "re-signing leaves the file as it was"
cp program.signed inplace && "$sign" sign --signing keys inplace inplace > /dev/null \
  && cmp inplace program.signed && pass || fail "signing in place"

# Another key: refuses what it did not sign, and a rogue CA is not trusted.
"$sign" init --signing rogue --name "Rogue" > /dev/null
expect 2 "signing over another key's signature" "$sign" sign --signing rogue program.signed program.rogue
"$sign" sign --signing rogue program program.rogue > /dev/null
expect 1 "a rogue-signed program under the real anchor" "$sign" verify --anchor keys/builder-ca.der program.rogue
grep -q UNTRUSTED_BUILDER out.txt && pass || fail "rogue is UNTRUSTED_BUILDER"
expect 0 "two anchors" "$sign" verify --anchor rogue/builder-ca.der --anchor keys/builder-ca.der program.signed
cp program.signed flipped && printf '\x01' | dd of=flipped bs=1 seek=1000 conv=notrunc status=none
expect 1 "a flipped payload byte" "$sign" verify --anchor keys/builder-ca.der flipped
grep -q DIGEST_MISMATCH out.txt && pass || fail "a flipped byte is DIGEST_MISMATCH"

# Signing directories ccl-sign refuses to sign with.
bad() {  # name: a copy of keys/ to break
  rm -rf "$1"; cp -r keys "$1"
}
bad mismatched && cp rogue/builder.key mismatched/builder.key
expect 2 "a key that is not the certificate's" "$sign" sign --signing mismatched program x
bad weak
python3 - weak/builder.key <<'PY'
import sys
# The SEC 1 template with d = 4 (the eMudhra key): refused before any use.
path = sys.argv[1]
data = bytearray(open(path, "rb").read())
data[8:56] = (4).to_bytes(48, "big")
open(path, "wb").write(bytes(data))
PY
expect 2 "a weak key (d = 4)" "$sign" sign --signing weak program x
grep -q "not a strong" out.txt && pass || fail "the weak key is named as such"
bad nokey && rm nokey/builder.key
expect 2 "no key" "$sign" sign --signing nokey program x
grep -q "signing-key" out.txt && pass || fail "a missing key names make signing-key"

# openssl-made directories: CA without basicConstraints, leaf without the
# EKU, and leaves outside their window by the signer's clock.
openssl_dir() {  # name  CA extension lines (;-separated)  leaf extension lines  leaf validity options...
  local name=$1 ca_ext=$2 leaf_ext=$3; shift 3
  mkdir "$name"
  openssl genpkey -algorithm EC -pkeyopt ec_paramgen_curve:P-384 -out "$name/ca.pem" 2>/dev/null
  openssl genpkey -algorithm EC -pkeyopt ec_paramgen_curve:P-384 -out "$name/leaf.pem" 2>/dev/null
  printf '[req]\ndistinguished_name=dn\nprompt=no\n[dn]\nCN=%s CA\n[ext]\n%s\n' "$name" \
    "$(echo "$ca_ext" | tr ';' '\n')" > "$name/ca.cnf"
  printf '[req]\ndistinguished_name=dn\nprompt=no\n[dn]\nCN=%s\n[ext]\n%s\n' "$name" \
    "$(echo "$leaf_ext" | tr ';' '\n')" > "$name/leaf.cnf"
  openssl req -x509 -new -key "$name/ca.pem" -config "$name/ca.cnf" -extensions ext \
    -not_before 20200101000000Z -not_after 20400101000000Z -sha384 -out "$name/ca.crt" 2>/dev/null
  openssl req -new -key "$name/leaf.pem" -config "$name/leaf.cnf" -out "$name/leaf.csr" 2>/dev/null
  openssl x509 -req -in "$name/leaf.csr" -CA "$name/ca.crt" -CAkey "$name/ca.pem" -set_serial 7 \
    -extfile "$name/leaf.cnf" -extensions ext -sha384 "$@" -out "$name/leaf.crt" 2>/dev/null
  openssl x509 -in "$name/ca.crt" -outform DER -out "$name/builder-ca.der"
  openssl x509 -in "$name/leaf.crt" -outform DER -out "$name/builder.der"
  openssl ec -in "$name/leaf.pem" -outform DER -out "$name/builder.key" 2>/dev/null
  rm "$name"/*.pem "$name"/*.csr "$name"/*.cnf "$name"/*.crt
}
CA_EXT="basicConstraints=critical,CA:TRUE;keyUsage=critical,keyCertSign,cRLSign;subjectKeyIdentifier=hash"
LEAF_EXT="keyUsage=critical,digitalSignature;extendedKeyUsage=codeSigning;authorityKeyIdentifier=keyid:always"
openssl_dir good "$CA_EXT" "$LEAF_EXT" -not_before 20250101000000Z -not_after 20350101000000Z
expect 0 "an openssl-made directory signs" "$sign" sign --signing good program good.signed
expect 0 "and verifies" "$sign" verify --anchor good/builder-ca.der good.signed
openssl_dir no-bc "keyUsage=critical,keyCertSign,cRLSign;subjectKeyIdentifier=hash" "$LEAF_EXT" \
  -not_before 20250101000000Z -not_after 20350101000000Z
expect 2 "a CA without basicConstraints" "$sign" sign --signing no-bc program x
openssl_dir no-eku "$CA_EXT" "keyUsage=critical,digitalSignature;authorityKeyIdentifier=keyid:always" \
  -not_before 20250101000000Z -not_after 20350101000000Z
expect 2 "a leaf without the code-signing EKU" "$sign" sign --signing no-eku program x
openssl_dir expired "$CA_EXT" "$LEAF_EXT" -not_before 20210101000000Z -not_after 20220101000000Z
expect 2 "an expired leaf" "$sign" sign --signing expired program x
grep -q "expired" out.txt && pass || fail "the expired leaf is named as such"
openssl_dir future "$CA_EXT" "$LEAF_EXT" -not_before 20390101000000Z -not_after 20391231000000Z
expect 2 "a leaf not yet valid" "$sign" sign --signing future program x
grep -q "not valid yet" out.txt && pass || fail "the future leaf is named as such"
[ ! -e x ] && pass || fail "no refused signing wrote an output"

echo "ccl-sign cli: $((checks - failures)) / $checks passed"
[ "$failures" = 0 ]
