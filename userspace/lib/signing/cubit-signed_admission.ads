-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
-- SPDX-License-Identifier: GPL-3.0-or-later
--
-- @summary
-- Signed admission (SIGN-001, docs/signed-binaries.md): the types every
-- spawner and the host signer share. A signed file is
--
--    offset 0      the payload (the ELF bytes), N bytes, unchanged
--    offset N      the envelope (Envelope_Format_V1)
--    end - 16      the footer: "CUBITSIG" | envelope length u32 | version u32
--
-- The children hold the parts, each pure and proved:
--   Envelopes     footer and envelope parser, statement encoder, encoder
--   Certificates  the "CuBit builder v1" profile and the chain checks
--   Revocations   sorted-digest membership
--   Scopes        identity prefixes on label boundaries, ceilings
--   Decisions     Verify (before anything else is parsed) and Admit
--
-- A valid signature is provenance, never authority: an Admission can only
-- refuse a launch; it never names a capability.
-------------------------------------------------------------------------------
with Interfaces;
with SPARKNaCl;
with X509;

package CuBit.Signed_Admission with SPARK_Mode => On is
   use type Interfaces.Integer_32;
   use type Interfaces.Unsigned_32;

   subtype Byte is Interfaces.Unsigned_8;
   subtype Bytes is SPARKNaCl.Byte_Seq;
   subtype File_Index is SPARKNaCl.N32;

   --  A signed file's length: at most what one SHA-384 stream over a
   --  Byte_Seq accepts (SPARKTLSCrypto: Data'Last < N32'Last - 256). The
   --  storehouse's own ceiling (realize.py MAX_FILE, 512 MiB) is lower.
   Max_File_Length : constant := SPARKNaCl.N32'Last - 512;
   subtype File_Length is File_Index range 0 .. Max_File_Length;

   ---------------------------------------------------------------------------
   --  Fixed sizes of the format (docs/signed-binaries.md section 1)
   ---------------------------------------------------------------------------

   Digest_Length    : constant := 48;   --  SHA-384
   Coordinate_Length : constant := 48;  --  a P-384 scalar or coordinate
   Signature_Length : constant := 2 * Coordinate_Length;   --  r || s
   subtype Digest is SPARKNaCl.Bytes_48;
   subtype Signature_Bytes is SPARKNaCl.Byte_Seq (0 .. Signature_Length - 1);

   Magic_Length         : constant := 8;
   Footer_Length        : constant := 16;
   Max_Envelope_Length  : constant := 16 * 1024;
   Min_Certificate_Length : constant := 1;
   Max_Certificate_Length : constant := 4096;
   Max_Certificates     : constant := 2;   --  the leaf, then an intermediate

   --  "CBSIGENV", "CUBITSIG" and the statement's domain prefix, as bytes.
   type Magic is array (File_Index range 0 .. Magic_Length - 1) of Byte;
   Envelope_Magic : constant Magic :=
     [Character'Pos ('C'), Character'Pos ('B'), Character'Pos ('S'), Character'Pos ('I'),
      Character'Pos ('G'), Character'Pos ('E'), Character'Pos ('N'), Character'Pos ('V')];
   Footer_Magic : constant Magic :=
     [Character'Pos ('C'), Character'Pos ('U'), Character'Pos ('B'), Character'Pos ('I'),
      Character'Pos ('T'), Character'Pos ('S'), Character'Pos ('I'), Character'Pos ('G')];

   Statement_Prefix_Text : constant String := "CuBit signed artifact v1";
   Statement_Prefix_Length : constant := 24;

   --  The statement ECDSA signs: the prefix, format, suite, purpose,
   --  reserved (u16 each), payload length (u64), payload digest, leaf
   --  certificate digest. The design note says 144 bytes; the fields it
   --  lists add up to 136, and the fields are what is encoded.
   Statement_Length : constant :=
     Statement_Prefix_Length + 4 * 2 + 8 + Digest_Length + Digest_Length;
   subtype Statement is SPARKNaCl.Byte_Seq (0 .. Statement_Length - 1);

   --  The envelope's fixed part before the certificate lengths: magic,
   --  format, suite, purpose, reserved, payload length, payload digest,
   --  certificate count.
   Envelope_Header_Length : constant := Magic_Length + 4 * 2 + 8 + Digest_Length + 1;
   Min_Envelope_Length : constant :=
     Envelope_Header_Length + 2 + Min_Certificate_Length + Signature_Length;

   ---------------------------------------------------------------------------
   --  Versioned codes: each refuses unknown values, no negotiation.
   ---------------------------------------------------------------------------

   Envelope_Format_V1 : constant := 1;
   Footer_Version_V1  : constant := 1;
   Reserved_Value     : constant := 0;

   type Suite is (ECDSA_P384_SHA384);
   for Suite use (ECDSA_P384_SHA384 => 1);

   --  What a signature is for. A program signature never admits a shared
   --  image, and the reverse (the statement carries the purpose).
   type Purpose is (Program, Shared_Image, Boot_Image, Generation_Record, Trust_List);
   for Purpose use
     (Program => 1, Shared_Image => 2, Boot_Image => 3, Generation_Record => 4,
      Trust_List => 5);

   subtype Certificate_Length is X509.N32 range Min_Certificate_Length .. Max_Certificate_Length;
   subtype Certificate_Count is Natural range 1 .. Max_Certificates;
   subtype Certificate_Bytes_Index is X509.N32 range 0 .. Max_Certificate_Length - 1;
   subtype Certificate_Buffer is X509.Byte_Seq (Certificate_Bytes_Index);

   ---------------------------------------------------------------------------
   --  The trust list (docs/signed-binaries.md section 4), as the verifier
   --  receives it: the caller decodes the generation's typed value into
   --  these arrays, whose sizes come from that value.
   ---------------------------------------------------------------------------

   type Builder_Kind is (Release, Organisation, Device_Local, Host_Development, Test);
   type Authority_Ceiling is (Applications_Only, Services, Hardware_And_System);

   --  What a program's manifest asks for, classified by the caller after
   --  verification: no service and no hardware; registers a service; or
   --  hardware authority (device memory, I/O ports, IRQs, DMA, ACPI
   --  windows, platform power, firmware data) or a system role.
   type Requested_Authority is (Application_Authority, Service_Authority, Hardware_Or_System_Authority);

   --  A reverse-DNS identity (com.cubit.secrets) or prefix; "*" is every
   --  identity. CuBit's identities are at most 64 bytes.
   Max_Identity_Length : constant := 64;
   subtype Identity_Length is Natural range 0 .. Max_Identity_Length;
   type Identity_Text is record
      Length : Identity_Length := 0;
      Data   : String (1 .. Max_Identity_Length) := [others => ' '];
   end record;
   Every_Identity : constant Character := '*';

   subtype Prefix_Index is Positive;
   type Prefix_Array is array (Prefix_Index range <>) of Identity_Text;

   --  One builder: its CA certificate, its kind, the identities it may
   --  sign (Prefixes (First_Prefix .. Last_Prefix) of the trust list; none
   --  when Last_Prefix < First_Prefix) and its authority ceiling.
   type Anchor is record
      Kind         : Builder_Kind := Test;
      Ceiling      : Authority_Ceiling := Applications_Only;
      DER          : Certificate_Buffer := [others => 0];
      Length       : Certificate_Length := Min_Certificate_Length;
      First_Prefix : Prefix_Index := 1;
      Last_Prefix  : Natural := 0;
   end record;
   subtype Anchor_Index is Positive;
   type Anchor_Array is array (Anchor_Index range <>) of Anchor;

   --  Revocations: each list sorted strictly ascending (bytewise), so a
   --  lookup is a binary search. Keys are SHA-384 of SubjectPublicKeyInfo,
   --  certificates SHA-384 of their DER, programs the payload digest.
   subtype Revocation_Count is Natural;
   type Digest_Array is array (Positive range <>) of Digest;
   type Revocation_Set (Keys, Certificates, Programs : Revocation_Count) is record
      Revoked_Keys         : Digest_Array (1 .. Keys);
      Revoked_Certificates : Digest_Array (1 .. Certificates);
      Revoked_Programs     : Digest_Array (1 .. Programs);
   end record;

   ---------------------------------------------------------------------------
   --  Decisions
   ---------------------------------------------------------------------------

   --  The refusal a launcher reports (Launch_Failure literals, with the
   --  texts in the design's section 5).
   type Refusal_Reason is
     (Not_Signed, Envelope_Malformed, Digest_Mismatch, Signature_Invalid,
      Untrusted_Builder, Revoked, Outside_Builder_Scope, Below_Security_Floor);

   --  Why, for logs and for the tests that check each refusal is the
   --  intended one.
   type Refusal_Detail is
     (No_Footer, Footer_Malformed, Envelope_Length_Invalid, Envelope_Magic_Wrong,
      Format_Unknown, Suite_Unknown, Purpose_Unknown, Reserved_Not_Zero,
      Payload_Length_Wrong, Certificate_Count_Invalid, Certificate_Length_Invalid,
      Lengths_Disagree, Footer_Version_Unknown,
      Wrong_Purpose, Payload_Changed, Certificate_Unparsable,
      Leaf_Profile, Intermediate_Profile, No_Anchor, Statement_Signature,
      Key_Revoked, Certificate_Revoked, Program_Revoked,
      Identity_Outside_Scope, Authority_Above_Ceiling);

   --  The leaf's subject common name, for "signed by '<CN>'" (X.520's
   --  upper bound for a common name is 64).
   Max_Signer_Name_Length : constant := 64;
   subtype Signer_Name_Length is Natural range 0 .. Max_Signer_Name_Length;
   type Signer_Name is record
      Length : Signer_Name_Length := 0;
      Data   : String (1 .. Max_Signer_Name_Length) := [others => ' '];
   end record;

   --  Who signed what: the leaf certificate's digest and name, the anchor
   --  it chains to, and the payload.
   type Signer_Record is record
      Leaf_Digest    : Digest := [others => 0];
      Name           : Signer_Name;
      Anchor         : Anchor_Index := 1;
      Anchor_Digest  : Digest := [others => 0];
      Payload_Digest : Digest := [others => 0];
      Payload_Length : File_Length := 0;
   end record;

   --  The outcome of Verify, before the manifest is read.
   type Verification (Verified : Boolean := False) is record
      case Verified is
         when True =>
            Signer : Signer_Record;
         when False =>
            Reason : Refusal_Reason := Not_Signed;
            Detail : Refusal_Detail := No_Footer;
      end case;
   end record;

   --  The decision, after the manifest's identity and authority are known.
   type Admission (Admitted : Boolean := False) is record
      case Admitted is
         when True =>
            Signer : Signer_Record;
         when False =>
            Reason : Refusal_Reason := Not_Signed;
            Detail : Refusal_Detail := No_Footer;
      end case;
   end record;

end CuBit.Signed_Admission;
