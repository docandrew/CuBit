-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
-- SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------
with SPARKNaCl;
with SPARKTLSCrypto.Hashing.SHA384;
with SPARKTLSCrypto.P384.ECDSA;
with SPARKTLSCrypto.RFC6979;
with CuBit.Signed_Admission.Certificates;

package body CuBit.Program_Signing with SPARK_Mode => On is
   use type SPARKNaCl.Byte_Seq;
   package Certificates renames CuBit.Signed_Admission.Certificates;
   package Keys renames CuBit.Signing_Keys;

   procedure Sign
     (Payload      : Bytes;
      Kind         : Purpose;
      Leaf         : X509.Byte_Seq;
      Count        : Certificate_Count;
      Intermediate : X509.Byte_Seq;
      Key          : Keys.Scalar;
      Now          : X509.Date_Time;
      Mask         : Keys.Blind;
      Output       : out Envelopes.Trailer;
      Length       : out File_Index;
      Result       : out Signing_Result)
   is
      Leaf_Point, Key_Point : Keys.Point;
      OK : Boolean;
      Payload_Digest, Hash, K : Digest;
      R, S : SPARKNaCl.Byte_Seq (0 .. Coordinate_Length - 1) := [others => 0];
      Signature : Signature_Bytes := [others => 0];
   begin
      Output := [others => 0];
      Length := 0;

      --  The key, and that it is the leaf's.
      if not Keys.Strong (Key) then
         Result := Weak_Key;
         return;
      end if;
      Certificates.Public_Point (Leaf, Leaf_Point, OK);
      Keys.Public_Of (Key, Mask, Key_Point);
      if not OK or else Leaf_Point /= Key_Point then
         Result := Key_Mismatch;
         return;
      end if;
      Keys.Pairwise_Consistent (Key, Key_Point, Mask, OK);
      if not OK then
         Result := Key_Mismatch;
         return;
      end if;

      --  The certificates it will carry.
      if not Certificates.Profile_Holds (Leaf, Certificates.Leaf_Role) then
         Result := Leaf_Refused;
         return;
      elsif Count = 2
        and then (not Certificates.Profile_Holds (Intermediate, Certificates.Intermediate_Role)
                  or else not Certificates.Link_Holds (Leaf, Intermediate))
      then
         Result := Intermediate_Refused;
         return;
      end if;

      --  The leaf's window, by the signer's clock.
      declare
         C : X509.Certificate;
         Parsed : Boolean;
      begin
         X509.Parse (Leaf, C, Parsed);
         if not Parsed then
            Result := Leaf_Refused;
            return;
         elsif X509.DT_Before (Now, X509.Not_Before (C)) then
            Result := Leaf_Not_Yet_Valid;
            return;
         elsif X509.DT_Before (X509.Not_After (C), Now) then
            Result := Leaf_Expired;
            return;
         end if;
      end;

      --  The statement, its hash, and the signature.
      Payload_Digest := SPARKTLSCrypto.Hashing.SHA384.Hash (Payload);
      Hash := SPARKTLSCrypto.Hashing.SHA384.Hash
        (Envelopes.Statement_Of (Kind, Payload'Length, Payload_Digest, Certificates.Certificate_Digest (Leaf)));
      SPARKTLSCrypto.RFC6979.Derive_K_P384 (Key, Hash, K, OK);
      if OK then
         SPARKTLSCrypto.P384.ECDSA.Sign (Hash, Key, K, Mask, R, S, OK);
      end if;
      if not OK then
         Result := Signing_Failed;
         return;
      end if;
      Signature (0 .. Coordinate_Length - 1) := R;
      Signature (Coordinate_Length .. Signature_Length - 1) := S;
      if not Certificates.Signature_Holds (Leaf, Hash, Signature) then
         Result := Self_Check_Failed;
         return;
      end if;

      Envelopes.Encode
        (Kind, Payload'Length, Payload_Digest, Count, Leaf, Intermediate, Signature, Output, Length);
      Result := Signed;
   end Sign;

end CuBit.Program_Signing;
