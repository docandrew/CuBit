-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
-- SPDX-License-Identifier: GPL-3.0-or-later
--
-- @summary
-- The certificate encoder for the "CuBit builder v1" profile (SIGN-001,
-- docs/signed-binaries.md section 2): one fixed template per kind with the
-- variable fields filled, so the only certificates it can emit are:
--
--   Root_CA          self-signed; basicConstraints critical CA with a
--                    pathLen; keyUsage critical keyCertSign, cRLSign;
--                    SKI; AKI; EKU codeSigning.
--   Intermediate_CA  the same, issued by a CA, pathLen 0.
--   Code_Signing_Leaf  no basicConstraints; keyUsage critical
--                    digitalSignature; SKI; AKI; EKU codeSigning.
--
-- All with a P-384 key, ECDSA P-384 with SHA-384, and a UTF8String common
-- name as the whole subject. Issue refuses (and emits nothing) when the
-- issuer's key is weak or is not the key of the issuer certificate, when
-- the issuer is not a CA under the profile, when the window is invalid,
-- not yet open, already closed or not inside the issuer's, and when its
-- own output does not parse back through SPARKX509 into the profile with
-- a verifying link: an emitted certificate always passes the verifier's
-- profile check (tests/signed-admission round-trips every kind).
-------------------------------------------------------------------------------
with Interfaces;
with SPARKNaCl;
with X509;
with CuBit.Signed_Admission;
with CuBit.Signing_Keys;

package CuBit.Certificate_Encoder with SPARK_Mode => On is
   use type Interfaces.Unsigned_32;
   use type Interfaces.Integer_32;

   type Certificate_Kind is (Root_CA, Intermediate_CA, Code_Signing_Leaf);

   --  A common name: 1 .. 64 characters of letters, digits, space and
   --  - . _ ( ) (X.520's upper bound for a common name is 64).
   Max_Name_Length : constant := 64;
   subtype Name_Length is Natural range 0 .. Max_Name_Length;
   type Name_Text is record
      Length : Name_Length := 0;
      Data   : String (1 .. Max_Name_Length) := [others => ' '];
   end record;
   function Valid_Name (N : Name_Text) return Boolean is
     (N.Length >= 1
      and then (for all I in 1 .. N.Length =>
                  N.Data (I) in 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | ' ' | '-' | '.' | '_' | '(' | ')'));

   Serial_Length : constant := 16;
   subtype Serial_Bytes is SPARKNaCl.Byte_Seq (0 .. Serial_Length - 1);

   --  A CA's pathLenConstraint: 0 for a CA that signs leaves directly.
   Max_Path_Length : constant := 8;
   subtype Path_Length is Natural range 0 .. Max_Path_Length;

   type Request is record
      Kind        : Certificate_Kind := Code_Signing_Leaf;
      Subject     : Name_Text;
      Serial      : Serial_Bytes := [others => 0];   --  random; the encoder makes it positive
      Not_Before  : X509.Date_Time;
      Not_After   : X509.Date_Time;
      Subject_Key : CuBit.Signing_Keys.Point := [others => 0];
      Path        : Path_Length := 0;                 --  CAs only
   end record;

   type Issue_Result is
     (Issued, Name_Invalid, Window_Invalid, Not_Yet_Valid, Expired, Weak_Issuer_Key,
      Key_Mismatch, Issuer_Not_CA, Issuer_Name_Unsupported, Window_Outside_Issuer,
      Signing_Failed, Self_Check_Failed);

   Max_Encoded_Length : constant := 1024;
   subtype Encoded_Certificate is X509.Byte_Seq (0 .. Max_Encoded_Length - 1);

   --  Issues Request, signed by Issuer_Key. For a Root_CA, Issuer is
   --  ignored and Issuer_Key must be the key of Request.Subject_Key; else
   --  Issuer is the issuing CA's DER and Issuer_Key its key. Now is the
   --  signer's clock: the window must contain it.
   procedure Issue
     (R          : Request;
      Issuer     : X509.Byte_Seq;
      Issuer_Key : CuBit.Signing_Keys.Scalar;
      Now        : X509.Date_Time;
      Mask       : CuBit.Signing_Keys.Blind;
      Output     : out Encoded_Certificate;
      Length     : out X509.N32;
      Result     : out Issue_Result)
   with Pre  => Issuer'First = 0 and then Issuer'Length in CuBit.Signed_Admission.Certificate_Length,
        Post => (if Result = Issued then
                   Length in 1 .. Max_Encoded_Length
                   and then CuBit.Signing_Keys.Strong (Issuer_Key)
                 else Length = 0);

   --  A Date_Time that names a real instant between 1950 and 9999.
   function Valid_Date (T : X509.Date_Time) return Boolean;

end CuBit.Certificate_Encoder;
