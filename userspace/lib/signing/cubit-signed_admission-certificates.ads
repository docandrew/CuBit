-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
-- SPDX-License-Identifier: GPL-3.0-or-later
--
-- @summary
-- The "CuBit builder v1" certificate profile and the chain checks
-- (SIGN-001, docs/signed-binaries.md section 2), over SPARKX509's parser
-- and SPARKTLSCrypto's SHA-384 and P-384 ECDSA. Each function parses the
-- DER it is given; the decision states its result in terms of these
-- functions, so what "verified" means is written down once, here.
--
-- Every certificate: X.509 v3, strict DER, no unknown critical and no
-- duplicate extension, an EC P-384 key, ECDSA P-384 with SHA-384, a
-- subject common name, notBefore <= notAfter.
-- A CA (anchor or intermediate): basicConstraints critical with CA, a
-- critical keyUsage with keyCertSign, a subjectKeyIdentifier, and an EKU,
-- if present, that includes codeSigning. An intermediate has pathLen 0.
-- A leaf: not a CA, a critical keyUsage with digitalSignature and none of
-- keyCertSign, cRLSign or keyEncipherment, an EKU with codeSigning and
-- without anyExtendedKeyUsage, and an authorityKeyIdentifier.
-- A link: the child's issuer is the issuer's subject, its AKI is the
-- issuer's SKI, its validity lies inside the issuer's, and the issuer's
-- key verifies its signature. No clock is read.
--
-- Assumed, not proved here: SPARKX509's parse matches RFC 5280 and
-- SPARKTLSCrypto's SHA-384 and ECDSA compute the right functions.
-------------------------------------------------------------------------------
package CuBit.Signed_Admission.Certificates with SPARK_Mode => On is

   function Well_Formed (DER : X509.Byte_Seq) return Boolean is
     (DER'First = 0 and then DER'Length in Certificate_Length);

   type Role is (Anchor_Role, Intermediate_Role, Leaf_Role);

   function Profile_Holds (DER : X509.Byte_Seq; As : Role) return Boolean
   with Pre => Well_Formed (DER);

   --  The issuer's basicConstraints let it issue a CA below it: no
   --  pathLenConstraint, or one of at least 1.
   function Issues_Intermediate (Issuer : X509.Byte_Seq) return Boolean
   with Pre => Well_Formed (Issuer);

   function Link_Holds (Child, Issuer : X509.Byte_Seq) return Boolean
   with Pre => Well_Formed (Child) and then Well_Formed (Issuer);

   --  Anchor <- [Intermediate] <- Leaf, every profile and every link.
   function Chain_Holds
     (Leaf : X509.Byte_Seq; Has_Intermediate : Boolean; Intermediate, Anchor : X509.Byte_Seq)
      return Boolean
   is (Profile_Holds (Anchor, Anchor_Role)
       and then Profile_Holds (Leaf, Leaf_Role)
       and then (if Has_Intermediate then
                   Issues_Intermediate (Anchor)
                   and then Profile_Holds (Intermediate, Intermediate_Role)
                   and then Link_Holds (Intermediate, Anchor)
                   and then Link_Holds (Leaf, Intermediate)
                 else Link_Holds (Leaf, Anchor)))
   with Pre => Well_Formed (Leaf) and then Well_Formed (Intermediate) and then Well_Formed (Anchor);

   --  The leaf's key verifies Signature (r || s) over Message_Hash.
   function Signature_Holds
     (Leaf : X509.Byte_Seq; Message_Hash : Digest; Signature : Signature_Bytes) return Boolean
   with Pre => Well_Formed (Leaf);

   --  SHA-384 of the DER, and of the SubjectPublicKeyInfo (for a P-384
   --  key, the fixed algorithm header and the point; zero when the key is
   --  not a P-384 point).
   function Certificate_Digest (DER : X509.Byte_Seq) return Digest
   with Pre => Well_Formed (DER);
   function Key_Digest (DER : X509.Byte_Seq) return Digest
   with Pre => Well_Formed (DER);

   --  The subject common name, cut at Max_Signer_Name_Length.
   function Subject_Name (DER : X509.Byte_Seq) return Signer_Name
   with Pre => Well_Formed (DER);

   Point_Length : constant := 1 + 2 * Coordinate_Length;   --  04 || X || Y
   subtype Point_Bytes is SPARKNaCl.Byte_Seq (0 .. Point_Length - 1);

   --  The certificate's P-384 public point; OK False for anything else.
   procedure Public_Point (DER : X509.Byte_Seq; Point : out Point_Bytes; OK : out Boolean)
   with Pre => Well_Formed (DER), Always_Terminates;

   --  The DER-encoded ECDSA-Sig-Value { r INTEGER, s INTEGER }, strict:
   --  minimal integers, no trailing bytes, each value at most 48 bytes.
   procedure Decode_ECDSA_Signature
     (Encoded : X509.Byte_Seq; Signature : out Signature_Bytes; OK : out Boolean)
   with Pre => Encoded'First = 0 and then Encoded'Length <= X509.Max_Sig_Bytes;

end CuBit.Signed_Admission.Certificates;
