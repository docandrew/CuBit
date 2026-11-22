-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
-- SPDX-License-Identifier: GPL-3.0-or-later
--
-- @summary
-- The footer and envelope parser, the statement encoder and the envelope
-- encoder (SIGN-001, docs/signed-binaries.md section 1). Fixed layout,
-- little-endian integers, no optional fields; every length is bounded
-- before it is used. Parse touches only the footer and the envelope: the
-- payload is hashed by the caller, never parsed here.
-------------------------------------------------------------------------------
package CuBit.Signed_Admission.Envelopes with SPARK_Mode => On is
   use type Interfaces.Unsigned_8;
   use type Interfaces.Unsigned_16;
   use type Interfaces.Unsigned_64;

   ---------------------------------------------------------------------------
   --  Little-endian fields
   ---------------------------------------------------------------------------

   function Read_U16 (B : Bytes; At_Index : File_Index) return Interfaces.Unsigned_16 is
     (Interfaces.Unsigned_16 (B (At_Index))
      or Interfaces.Shift_Left (Interfaces.Unsigned_16 (B (At_Index + 1)), 8))
   with Pre => At_Index >= B'First and then At_Index < B'Last;

   function Read_U32 (B : Bytes; At_Index : File_Index) return Interfaces.Unsigned_32 is
     (Interfaces.Unsigned_32 (B (At_Index))
      or Interfaces.Shift_Left (Interfaces.Unsigned_32 (B (At_Index + 1)), 8)
      or Interfaces.Shift_Left (Interfaces.Unsigned_32 (B (At_Index + 2)), 16)
      or Interfaces.Shift_Left (Interfaces.Unsigned_32 (B (At_Index + 3)), 24))
   with Pre => At_Index >= B'First and then B'Last >= 3 and then At_Index <= B'Last - 3;

   function Read_U64 (B : Bytes; At_Index : File_Index) return Interfaces.Unsigned_64 is
     (Interfaces.Unsigned_64 (Read_U32 (B, At_Index))
      or Interfaces.Shift_Left (Interfaces.Unsigned_64 (Read_U32 (B, At_Index + 4)), 32))
   with Pre => At_Index >= B'First and then B'Last >= 7 and then At_Index <= B'Last - 7;

   ---------------------------------------------------------------------------
   --  Envelope offsets, relative to the envelope's first byte
   ---------------------------------------------------------------------------

   Format_Offset         : constant := Magic_Length;
   Suite_Offset          : constant := Format_Offset + 2;
   Purpose_Offset        : constant := Suite_Offset + 2;
   Reserved_Offset       : constant := Purpose_Offset + 2;
   Payload_Length_Offset : constant := Reserved_Offset + 2;
   Digest_Offset         : constant := Payload_Length_Offset + 8;
   Count_Offset          : constant := Digest_Offset + Digest_Length;
   Lengths_Offset        : constant := Count_Offset + 1;
   pragma Assert (Lengths_Offset = Envelope_Header_Length);

   --  Footer offsets, relative to the footer's first byte.
   Footer_Length_Offset  : constant := Magic_Length;
   Footer_Version_Offset : constant := Footer_Length_Offset + 4;

   ---------------------------------------------------------------------------
   --  Parsing
   ---------------------------------------------------------------------------

   type Parse_Status is (Parsed, Unsigned, Malformed);

   --  A certificate inside the file: File (First .. First + Length - 1).
   type Certificate_Span is record
      First  : File_Index := 0;
      Length : Certificate_Length := Min_Certificate_Length;
   end record;

   type Envelope is record
      Status          : Parse_Status := Unsigned;
      Detail          : Refusal_Detail := No_Footer;
      Kind            : Purpose := Program;
      Payload_Length  : File_Length := 0;
      Envelope_Length : File_Index := 0;
      Payload_Digest  : Digest := [others => 0];
      Count           : Certificate_Count := 1;
      Leaf            : Certificate_Span;
      Intermediate    : Certificate_Span;   --  meaningful when Count = 2
      Signature       : Signature_Bytes := [others => 0];
   end record;

   function Span_Inside (S : Certificate_Span; Low, High : File_Index) return Boolean is
     (S.First >= Low and then S.First <= High and then High - S.First >= File_Index (S.Length));

   --  What Parse guarantees of a parsed envelope: the payload, envelope and
   --  footer account for the file exactly, and each certificate lies inside
   --  the envelope.
   function Accounts_For (E : Envelope; File_Size : File_Length) return Boolean is
     (E.Status = Parsed
      and then E.Envelope_Length >= Min_Envelope_Length
      and then E.Envelope_Length <= Max_Envelope_Length
      and then E.Payload_Length <= File_Size
      and then File_Size - E.Payload_Length >= Footer_Length
      and then File_Size - E.Payload_Length - Footer_Length = E.Envelope_Length
      and then Span_Inside (E.Leaf, E.Payload_Length, E.Payload_Length + E.Envelope_Length)
      and then (E.Count = 1
                or else Span_Inside (E.Intermediate, E.Payload_Length,
                                     E.Payload_Length + E.Envelope_Length)));

   function Parse (File : Bytes) return Envelope
   with Pre  => File'First = 0 and then File'Length <= Max_File_Length,
        Post => (if Parse'Result.Status = Parsed then Accounts_For (Parse'Result, File'Length));

   ---------------------------------------------------------------------------
   --  The statement (never stored; signer and verifier rebuild it)
   ---------------------------------------------------------------------------

   function Statement_Of
     (Kind           : Purpose;
      Payload_Length : File_Length;
      Payload_Digest : Digest;
      Leaf_Digest    : Digest) return Statement
   with Post =>
     Read_U16 (Statement_Of'Result, Statement_Prefix_Length) = Envelope_Format_V1
     and then Read_U16 (Statement_Of'Result, Statement_Prefix_Length + 2) = Suite'Enum_Rep (ECDSA_P384_SHA384)
     and then Read_U16 (Statement_Of'Result, Statement_Prefix_Length + 4) = Purpose'Enum_Rep (Kind)
     and then Read_U16 (Statement_Of'Result, Statement_Prefix_Length + 6) = Reserved_Value
     and then Read_U64 (Statement_Of'Result, Statement_Prefix_Length + 8) = Interfaces.Unsigned_64 (Payload_Length)
     and then (for all I in Digest'Range =>
                 Statement_Of'Result (Statement_Prefix_Length + 16 + I) = Payload_Digest (I))
     and then (for all I in Digest'Range =>
                 Statement_Of'Result (Statement_Prefix_Length + 16 + Digest_Length + I) = Leaf_Digest (I));

   ---------------------------------------------------------------------------
   --  Encoding (the signer's half)
   ---------------------------------------------------------------------------

   Max_Trailer_Length : constant := Max_Envelope_Length + Footer_Length;
   subtype Trailer is SPARKNaCl.Byte_Seq (0 .. Max_Trailer_Length - 1);

   --  The envelope and footer to append to the payload, in
   --  Output (0 .. Length - 1). Intermediate is ignored when Count = 1.
   procedure Encode
     (Kind           : Purpose;
      Payload_Length : File_Length;
      Payload_Digest : Digest;
      Count          : Certificate_Count;
      Leaf           : X509.Byte_Seq;
      Intermediate   : X509.Byte_Seq;
      Signature      : Signature_Bytes;
      Output         : out Trailer;
      Length         : out File_Index)
   with Pre  => Leaf'First = 0 and then Leaf'Length in Certificate_Length
                and then Intermediate'First = 0 and then Intermediate'Length in Certificate_Length,
        Post => Length >= Min_Envelope_Length + Footer_Length and then Length <= Max_Trailer_Length;

end CuBit.Signed_Admission.Envelopes;
