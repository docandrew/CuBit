-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
-- SPDX-License-Identifier: GPL-3.0-or-later
--
-- @summary
-- The admission decision (SIGN-001, docs/signed-binaries.md section 5),
-- in two steps a spawner takes in this order:
--
--   1. Verify, on the signed file in private or immutable memory, before
--      anything else in it is parsed: footer, envelope, SHA-384 of the
--      payload, the chain to an anchor of the trust list, the statement
--      signature, revocations. Only the envelope parser, SHA-384 and the
--      X.509 and ECDSA code see unverified bytes.
--   2. Admit, after the manifest of the verified payload is read: the
--      identity against the anchor's prefixes, the requested authority
--      against its ceiling.
--
-- Proved: Verify'Result.Verified implies Accepts (the profile and every
-- link of the chain hold, the payload digest matches, the statement
-- signature verifies, the anchor is one of the trust list's and nothing
-- on the chain or the payload is revoked); Admit admits exactly a
-- verified file in scope and under the ceiling. Those are stated over the
-- Boolean results of SPARKX509 and SPARKTLSCrypto; that those compute
-- RFC 5280 parsing, SHA-384 and ECDSA correctly is assumed.
-------------------------------------------------------------------------------
with CuBit.Signed_Admission.Certificates;
with CuBit.Signed_Admission.Envelopes;
with CuBit.Signed_Admission.Revocations;
with CuBit.Signed_Admission.Scopes;

package CuBit.Signed_Admission.Decisions with SPARK_Mode => On is
   use CuBit.Signed_Admission.Envelopes;
   use type Interfaces.Unsigned_8;
   use type SPARKNaCl.Byte_Seq;

   function Inside (File : Bytes; S : Certificate_Span) return Boolean is
     (S.First <= File'Last and then File'Last - S.First >= File_Index (S.Length) - 1);

   --  A certificate of the envelope, copied out of the file.
   function Copy_Of (File : Bytes; S : Certificate_Span) return X509.Byte_Seq
   with Pre  => File'First = 0 and then Inside (File, S),
        Post => Copy_Of'Result'First = 0 and then Copy_Of'Result'Length = S.Length
                and then (for all I in Copy_Of'Result'Range =>
                            Copy_Of'Result (I) = File (S.First + File_Index (I)));

   function Parsed (File : Bytes; E : Envelope) return Boolean is
     (File'First = 0 and then File'Length <= Max_File_Length
      and then Accounts_For (E, File'Length)
      and then Inside (File, E.Leaf)
      and then (E.Count = 1 or else Inside (File, E.Intermediate)));

   function Leaf_Of (File : Bytes; E : Envelope) return X509.Byte_Seq is
     (Copy_Of (File, E.Leaf))
   with Pre => Parsed (File, E);

   --  The intermediate when there is one, the leaf otherwise (unused then).
   function Intermediate_Of (File : Bytes; E : Envelope) return X509.Byte_Seq is
     (if E.Count = 2 then Copy_Of (File, E.Intermediate) else Copy_Of (File, E.Leaf))
   with Pre => Parsed (File, E);

   function Anchor_DER (A : Anchor) return X509.Byte_Seq is (A.DER (0 .. A.Length - 1));

   --  SHA-384 of File (0 .. Length - 1).
   function Payload_Digest (File : Bytes; Length : File_Length) return Digest
   with Pre => File'First = 0 and then (Length = 0 or else Length - 1 <= File'Last);

   --  SHA-384 of the statement the envelope's signature covers.
   function Statement_Hash (E : Envelope; Leaf : X509.Byte_Seq) return Digest
   with Pre => Certificates.Well_Formed (Leaf);

   --  Executable checks a caller runs on a decoded trust list, so it can
   --  establish Well_Formed before it calls Verify.
   function Is_Sorted (List : Digest_Array) return Boolean
   with Pre  => List'Last < Positive'Last,
        Post => Is_Sorted'Result = Revocations.Sorted (List);

   function Revocations_Usable (Set : Revocation_Set) return Boolean
   with Post => (if Revocations_Usable'Result then Revocations.Well_Formed (Set));

   --  Something on the chain, or the payload, is revoked.
   function Chain_Revoked
     (Set : Revocation_Set; Leaf : X509.Byte_Seq; Has_Intermediate : Boolean;
      Intermediate, Anchor_Certificate : X509.Byte_Seq; Payload : Digest) return Boolean
   is (Revocations.Member (Set.Revoked_Programs, Payload)
       or else Revocations.Member (Set.Revoked_Keys, Certificates.Key_Digest (Leaf))
       or else Revocations.Member (Set.Revoked_Certificates, Certificates.Certificate_Digest (Leaf))
       or else (Has_Intermediate
                and then (Revocations.Member (Set.Revoked_Keys, Certificates.Key_Digest (Intermediate))
                          or else Revocations.Member
                                    (Set.Revoked_Certificates, Certificates.Certificate_Digest (Intermediate))))
       or else Revocations.Member (Set.Revoked_Keys, Certificates.Key_Digest (Anchor_Certificate))
       or else Revocations.Member
                 (Set.Revoked_Certificates, Certificates.Certificate_Digest (Anchor_Certificate)))
   with Ghost,
        Pre => Certificates.Well_Formed (Leaf) and then Certificates.Well_Formed (Intermediate)
               and then Certificates.Well_Formed (Anchor_Certificate);

   --  What admission means, written once: File is a signed artifact of
   --  purpose Expected whose payload digest matches, whose chain holds to
   --  Anchors (A), whose statement signature verifies, and nothing on
   --  whose chain is revoked.
   function Accepts
     (File : Bytes; Expected : Purpose; Anchors : Anchor_Array; Revoked_Set : Revocation_Set;
      A : Anchor_Index) return Boolean
   is (File'First = 0 and then File'Length <= Max_File_Length
       and then Parse (File).Status = Parsed
       and then Parsed (File, Parse (File))
       and then Parse (File).Kind = Expected
       and then Payload_Digest (File, Parse (File).Payload_Length) = Parse (File).Payload_Digest
       and then A in Anchors'Range
       and then Certificates.Chain_Holds
                  (Leaf_Of (File, Parse (File)), Parse (File).Count = 2,
                   Intermediate_Of (File, Parse (File)), Anchor_DER (Anchors (A)))
       and then Certificates.Signature_Holds
                  (Leaf_Of (File, Parse (File)),
                   Statement_Hash (Parse (File), Leaf_Of (File, Parse (File))),
                   Parse (File).Signature)
       and then not Chain_Revoked
                  (Revoked_Set, Leaf_Of (File, Parse (File)), Parse (File).Count = 2,
                   Intermediate_Of (File, Parse (File)), Anchor_DER (Anchors (A)),
                   Parse (File).Payload_Digest))
   with Ghost;

   function Verify
     (File : Bytes; Expected : Purpose; Anchors : Anchor_Array; Revoked_Set : Revocation_Set)
      return Verification
   with Pre  => File'First = 0 and then File'Length <= Max_File_Length
                and then Revocations.Well_Formed (Revoked_Set),
        Post => (if Verify'Result.Verified then
                   Verify'Result.Signer.Anchor in Anchors'Range
                   and then Accepts (File, Expected, Anchors, Revoked_Set, Verify'Result.Signer.Anchor)
                   and then Verify'Result.Signer.Payload_Length = Parse (File).Payload_Length);

   function Admit
     (V : Verification; Anchors : Anchor_Array; Prefixes : Prefix_Array;
      Identity : Identity_Text; Requested : Requested_Authority) return Admission
   with Post =>
     Admit'Result.Admitted =
       (V.Verified
        and then V.Signer.Anchor in Anchors'Range
        and then Scopes.In_Scope (Prefixes, Anchors (V.Signer.Anchor).First_Prefix,
                                  Anchors (V.Signer.Anchor).Last_Prefix, Identity)
        and then Scopes.Ceiling_Allows (Anchors (V.Signer.Anchor).Ceiling, Requested))
     and then (if Admit'Result.Admitted then Admit'Result.Signer = V.Signer);

end CuBit.Signed_Admission.Decisions;
