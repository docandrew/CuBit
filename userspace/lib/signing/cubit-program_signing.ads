-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
-- SPDX-License-Identifier: GPL-3.0-or-later
--
-- @summary
-- The signer's half of SIGN-001 (docs/signed-binaries.md sections 1 and
-- 7): the envelope and footer for a payload, signed by a leaf key. Sign
-- builds the statement itself from typed fields, so the key signs only
-- statements of this shape, and refuses (and emits nothing) when:
--   * the key is weak or is not the leaf certificate's key;
--   * the leaf is not a code-signing leaf under the profile, or the
--     intermediate is not an intermediate CA that issued it;
--   * the leaf's window does not contain the signer's clock (expired or
--     not yet valid); this is a correctness check, not a boundary;
--   * its own signature does not verify.
-- The nonce is RFC 6979's, so the same key over the same payload gives
-- the same bytes; Mask only blinds the computation.
-------------------------------------------------------------------------------
with Interfaces;
with X509;
with CuBit.Signed_Admission;
with CuBit.Signed_Admission.Envelopes;
with CuBit.Signing_Keys;

package CuBit.Program_Signing with SPARK_Mode => On is
   use CuBit.Signed_Admission;
   use type X509.N32;
   use type Interfaces.Integer_32;

   type Signing_Result is
     (Signed, Weak_Key, Key_Mismatch, Leaf_Refused, Intermediate_Refused,
      Leaf_Not_Yet_Valid, Leaf_Expired, Signing_Failed, Self_Check_Failed);

   procedure Sign
     (Payload      : Bytes;
      Kind         : Purpose;
      Leaf         : X509.Byte_Seq;
      Count        : Certificate_Count;
      Intermediate : X509.Byte_Seq;
      Key          : CuBit.Signing_Keys.Scalar;
      Now          : X509.Date_Time;
      Mask         : CuBit.Signing_Keys.Blind;
      Output       : out Envelopes.Trailer;
      Length       : out File_Index;
      Result       : out Signing_Result)
   with Pre  => Payload'First = 0 and then Payload'Length <= Max_File_Length - Envelopes.Max_Trailer_Length
                and then Leaf'First = 0 and then Leaf'Length in Certificate_Length
                and then Intermediate'First = 0 and then Intermediate'Length in Certificate_Length,
        Post => (if Result = Signed then
                   CuBit.Signing_Keys.Strong (Key)
                   and then Length in Min_Envelope_Length + Footer_Length .. Envelopes.Max_Trailer_Length
                 else Length = 0);

end CuBit.Program_Signing;
