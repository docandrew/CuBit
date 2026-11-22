#!/usr/bin/env bash
# SPDX-License-Identifier: GPL-3.0-or-later
#
# Mutation checks for signed admission (SIGN-001): each mutant removes or
# weakens one rule of the decision, the profile, the parser, the scope or
# the key checks; the hosted tests (quick rows) must fail on every one.
# The first mutant changes only a comment and must pass. Run inside nix
# develop: bash tests/signed-admission/mutations.sh. With
# SIGNED_ADMISSION_CONTRACTS=off the tests run without their executable
# contracts, so a kill there is the tests' own.
set -uo pipefail
here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/../.." && pwd)
library="$root/userspace/lib/signing"
fixtures=$(mktemp -d "${TMPDIR:-/tmp}/signed-admission-mutants.XXXXXX")
trap 'rm -rf "$fixtures"' EXIT
bash "$here/fixtures.sh" "$fixtures/certificates"
cd "$root/kernel"
failures=0
killed=0
mutant() {
  local name="$1" file="$2" old="$3" new="$4" expect="$5"
  local work; work=$(mktemp -d "$fixtures/mutant.XXXXXX")
  mkdir -p "$work/src"
  cp "$library"/*.ad? "$work/src/"
  if ! OLD="$old" NEW="$new" python3 - "$work/src/$file" <<'PY'
import os, sys
p = sys.argv[1]; s = open(p).read(); old = os.environ["OLD"]; new = os.environ["NEW"]
if s.count(old) != 1: sys.exit(1)
open(p, "w").write(s.replace(old, new))
PY
  then echo "BADTEXT  $name"; failures=$((failures + 1)); return; fi
  local result
  if SIGNED_ADMISSION_SOURCES="$work/src" SIGNED_ADMISSION_OBJ="$work/obj" \
       alr exec -- gprbuild -p -q -P "$here/signed_admission_tests.gpr" > "$work/build.log" 2>&1 &&
     "$work/obj/main" "$fixtures/certificates" "$work/obj/main" --quick > "$work/test.log" 2>&1
  then result=pass; else result=fail; fi
  local why; why=$(grep -m1 -E 'FAIL:|raised|error:' "$work/test.log" "$work/build.log" 2>/dev/null | sed 's/^[^:]*://' | head -c 140)
  if [ "$expect" = survive ]; then
    if [ "$result" = pass ]; then echo "control  $name (tests pass)";
    else echo "BROKEN   $name: control failed: $why"; failures=$((failures + 1)); fi
  elif [ "$result" = fail ]; then
    echo "killed   $name: $why"; killed=$((killed + 1))
  else
    echo "SURVIVED $name"; failures=$((failures + 1))
  fi
  rm -rf "$work"
}

mutant "control: comment only" cubit-signed_admission-decisions.adb \
  "--  2. The payload is the one the envelope names." "--  2. The payload is the one named." survive
mutant "purpose not checked" cubit-signed_admission-decisions.adb \
  "if E.Kind /= Expected then" "if False then" kill
mutant "payload digest not checked" cubit-signed_admission-decisions.adb \
  "if Payload_Digest (File, E.Payload_Length) /= E.Payload_Digest then" "if False then" kill
mutant "statement signature not checked" cubit-signed_admission-decisions.adb \
  "if not Certificates.Signature_Holds (Leaf, Statement_Hash (E, Leaf), E.Signature) then" "if False then" kill
mutant "anchor key revocation ignored" cubit-signed_admission-decisions.adb \
  "      elsif Key_In (Anchor_Certificate) then
         Detail := Key_Revoked;
" "" kill
mutant "payload revocation ignored" cubit-signed_admission-decisions.adb \
  "if Revocations.Contains (Set.Revoked_Programs, Payload) then" "if False then" kill
mutant "scope not checked" cubit-signed_admission-decisions.adb \
  "if not Scopes.In_Scope (Prefixes, A.First_Prefix, A.Last_Prefix, Identity) then" "if False then" kill
mutant "leaf not linked to its intermediate" cubit-signed_admission-certificates.ads \
  "                   and then Link_Holds (Leaf, Intermediate)
" "" kill
mutant "anchor pathLen not checked" cubit-signed_admission-certificates.ads \
  "                   Issues_Intermediate (Anchor)
                   and then " "                   " kill
mutant "leaf may carry anyExtendedKeyUsage" cubit-signed_admission-certificates.adb \
  "      and then not X509.Has_EKU_Any_Purpose (C)
" "" kill
mutant "leaf may be a CA" cubit-signed_admission-certificates.adb \
  "     (not X509.Is_CA (C)
      and then not X509.Has_Path_Len_Constraint (C)" "     (not X509.Has_Path_Len_Constraint (C)" kill
mutant "leaf without codeSigning" cubit-signed_admission-certificates.adb \
  "      and then X509.Has_EKU_Code_Signing (C)
      and then not X509.Has_EKU_Any_Purpose (C)" "      and then not X509.Has_EKU_Any_Purpose (C)" kill
mutant "CA basicConstraints need not be critical" cubit-signed_admission-certificates.adb \
  "      and then X509.Is_Basic_Constraints_Critical (C)
" "" kill
mutant "CA without keyCertSign" cubit-signed_admission-certificates.adb \
  "      and then X509.KU_Key_Cert_Sign (C)
      and then not X509.CA_Missing_Subject_Key_ID (C)" "      and then not X509.CA_Missing_Subject_Key_ID (C)" kill
mutant "CA EKU need not include codeSigning" cubit-signed_admission-certificates.adb \
  "      and then (not X509.Has_EKU (C) or else X509.Has_EKU_Code_Signing (C))
      and then X509.Issuer_EKU_Allows_Signing (C, X509.EKU_Code_Signing));" "      );" kill
mutant "intermediate pathLen may exceed 0" cubit-signed_admission-certificates.adb \
  "              and then X509.Has_Path_Len_Constraint (C)
              and then X509.Path_Len_Constraint (C) = 0;" ";" kill
mutant "AKI need not name the issuer's SKI" cubit-signed_admission-certificates.adb \
  "        or else not Spans_Equal (Child, X509.Authority_Key_ID (C), Issuer, X509.Subject_Key_ID (I))
" "" kill
mutant "validity need not nest" cubit-signed_admission-certificates.adb \
  "        or else not X509.DT_Before_Or_Equal (X509.Not_After (C), X509.Not_After (I))
" "" kill
mutant "unknown critical extension accepted" cubit-signed_admission-certificates.adb \
  "      and then not X509.Has_Unknown_Critical_Extension (C)
" "" kill
mutant "any key algorithm" cubit-signed_admission-certificates.adb \
  "      and then X509.PK_Algorithm (C) = X509.Algo_EC_P384
      and then X509.PK_Length (C) = Point_Length
" "" kill
mutant "footer version not checked" cubit-signed_admission-envelopes.adb \
  "if Read_U32 (File, Footer + Footer_Version_Offset) /= Footer_Version_V1 then" "if False then" kill
mutant "payload length field not checked" cubit-signed_admission-envelopes.adb \
  "if Read_U64 (File, Start + Payload_Length_Offset) /= Interfaces.Unsigned_64 (Start) then" "if False then" kill
mutant "reserved field not checked" cubit-signed_admission-envelopes.adb \
  "if Read_U16 (File, Start + Reserved_Offset) /= Reserved_Value then" "if False then" kill
mutant "statement does not bind the leaf" cubit-signed_admission-envelopes.adb \
  "         Result (Digests + Digest_Length + I) := Leaf_Digest (I);" "         Result (Digests + Digest_Length + I) := 0;" kill
mutant "prefix matched off a label boundary" cubit-signed_admission-scopes.adb \
  "        or else Identity.Data (Prefix.Length + 1) = Label_Separator;" "        or else True;" kill
mutant "a service under an applications-only anchor" cubit-signed_admission-scopes.ads \
  "when Service_Authority            => Ceiling in Services | Hardware_And_System," \
  "when Service_Authority            => True," kill
mutant "binary search skips an entry" cubit-signed_admission-revocations.adb \
  "            Low := Mid + 1;" "            Low := Mid + 2;" kill
mutant "weak scalars accepted" cubit-signing_keys.ads \
  "(At_Most (Lowest_Strong, D) and then At_Most (D, Highest_Strong));" "(At_Most (D, Highest_Strong));" kill
mutant "signer ignores the leaf's window" cubit-program_signing.adb \
  "         elsif X509.DT_Before (X509.Not_After (C), Now) then" "         elsif False then" kill
mutant "encoder issues under a non-CA" cubit-certificate_encoder.adb \
  "         if not Certificates.Profile_Holds (Issuer, Certificates.Anchor_Role)" "         if False" kill
echo "signed-admission mutants: $killed killed, $failures problems"
[ "$failures" = 0 ]
