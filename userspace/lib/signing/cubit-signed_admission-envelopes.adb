-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
-- SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------
package body CuBit.Signed_Admission.Envelopes with SPARK_Mode => On is

   function Matches (B : Bytes; At_Index : File_Index; M : Magic) return Boolean is
     (for all I in M'Range => B (At_Index + I) = M (I))
   with Pre => At_Index >= B'First and then B'Last >= Magic_Length - 1
               and then At_Index <= B'Last - (Magic_Length - 1);

   function Parse (File : Bytes) return Envelope is
      Result : Envelope;
      Size   : constant File_Length := File'Length;
      Footer : File_Index;
      Start  : File_Index;
      E_Len  : Interfaces.Unsigned_32;
      Count  : Byte;
      Leaf_Length, Inter_Length : Interfaces.Unsigned_16 := 0;
      Expected : File_Index;

      procedure Refuse (Detail : Refusal_Detail) is
      begin
         Result.Status := Malformed;
         Result.Detail := Detail;
      end Refuse;
   begin
      if Size < Footer_Length then
         return Result;   --  Unsigned, No_Footer
      end if;
      Footer := Size - Footer_Length;
      if not Matches (File, Footer, Footer_Magic) then
         return Result;
      end if;
      if Read_U32 (File, Footer + Footer_Version_Offset) /= Footer_Version_V1 then
         Refuse (Footer_Version_Unknown);
         return Result;
      end if;
      E_Len := Read_U32 (File, Footer + Footer_Length_Offset);
      if E_Len < Min_Envelope_Length or else E_Len > Max_Envelope_Length
        or else File_Index (E_Len) > Footer
      then
         Refuse (Envelope_Length_Invalid);
         return Result;
      end if;
      Start := Footer - File_Index (E_Len);
      pragma Assert (Start + File_Index (E_Len) = Footer);
      if not Matches (File, Start, Envelope_Magic) then
         Refuse (Envelope_Magic_Wrong);
         return Result;
      end if;
      if Read_U16 (File, Start + Format_Offset) /= Envelope_Format_V1 then
         Refuse (Format_Unknown);
         return Result;
      elsif Read_U16 (File, Start + Suite_Offset) /= Suite'Enum_Rep (ECDSA_P384_SHA384) then
         Refuse (Suite_Unknown);
         return Result;
      end if;
      declare
         Code : constant Interfaces.Unsigned_16 := Read_U16 (File, Start + Purpose_Offset);
         Found : Boolean := False;
      begin
         for P in Purpose loop
            pragma Loop_Invariant (not Found);
            if Purpose'Enum_Rep (P) = Code then
               Result.Kind := P;
               Found := True;
               exit;
            end if;
         end loop;
         if not Found then
            Refuse (Purpose_Unknown);
            return Result;
         end if;
      end;
      if Read_U16 (File, Start + Reserved_Offset) /= Reserved_Value then
         Refuse (Reserved_Not_Zero);
         return Result;
      end if;
      if Read_U64 (File, Start + Payload_Length_Offset) /= Interfaces.Unsigned_64 (Start) then
         Refuse (Payload_Length_Wrong);
         return Result;
      end if;
      Count := File (Start + Count_Offset);
      if Count not in 1 .. Max_Certificates then
         Refuse (Certificate_Count_Invalid);
         return Result;
      end if;
      Leaf_Length := Read_U16 (File, Start + Lengths_Offset);
      if Count = 2 then
         Inter_Length := Read_U16 (File, Start + Lengths_Offset + 2);
      end if;
      if Leaf_Length not in Min_Certificate_Length .. Max_Certificate_Length
        or else (Count = 2 and then Inter_Length not in Min_Certificate_Length .. Max_Certificate_Length)
      then
         Refuse (Certificate_Length_Invalid);
         return Result;
      end if;
      Expected := Envelope_Header_Length + 2 * File_Index (Count) + File_Index (Leaf_Length)
        + (if Count = 2 then File_Index (Inter_Length) else 0) + Signature_Length;
      if Expected /= File_Index (E_Len) then
         Refuse (Lengths_Disagree);
         return Result;
      end if;

      Result.Payload_Length := Start;
      Result.Envelope_Length := File_Index (E_Len);
      for I in Digest'Range loop
         Result.Payload_Digest (I) := File (Start + Digest_Offset + I);
      end loop;
      Result.Count := Certificate_Count (Count);
      Result.Leaf :=
        (First  => Start + Lengths_Offset + 2 * File_Index (Count),
         Length => Certificate_Length (Leaf_Length));
      if Count = 2 then
         Result.Intermediate :=
           (First  => Result.Leaf.First + File_Index (Leaf_Length),
            Length => Certificate_Length (Inter_Length));
      end if;
      for I in Signature_Bytes'Range loop
         Result.Signature (I) := File (Footer - Signature_Length + I);
      end loop;
      Result.Status := Parsed;
      Result.Detail := No_Footer;
      return Result;
   end Parse;

   procedure Put_U16 (B : in out Bytes; At_Index : File_Index; Value : Interfaces.Unsigned_16)
   with Pre  => At_Index >= B'First and then At_Index < B'Last,
        Post => Read_U16 (B, At_Index) = Value
                and then (for all I in B'Range =>
                            (if I not in At_Index .. At_Index + 1 then B (I) = B'Old (I)))
   is
   begin
      B (At_Index) := Byte (Value and 16#FF#);
      B (At_Index + 1) := Byte (Interfaces.Shift_Right (Value, 8));
   end Put_U16;

   procedure Put_U32 (B : in out Bytes; At_Index : File_Index; Value : Interfaces.Unsigned_32)
   with Pre  => At_Index >= B'First and then B'Last >= 3 and then At_Index <= B'Last - 3,
        Post => Read_U32 (B, At_Index) = Value
                and then (for all I in B'Range =>
                            (if I not in At_Index .. At_Index + 3 then B (I) = B'Old (I)))
   is
   begin
      B (At_Index) := Byte (Value and 16#FF#);
      B (At_Index + 1) := Byte (Interfaces.Shift_Right (Value, 8) and 16#FF#);
      B (At_Index + 2) := Byte (Interfaces.Shift_Right (Value, 16) and 16#FF#);
      B (At_Index + 3) := Byte (Interfaces.Shift_Right (Value, 24));
   end Put_U32;

   procedure Put_U64 (B : in out Bytes; At_Index : File_Index; Value : Interfaces.Unsigned_64)
   with Pre  => At_Index >= B'First and then B'Last >= 7 and then At_Index <= B'Last - 7,
        Post => Read_U64 (B, At_Index) = Value
                and then (for all I in B'Range =>
                            (if I not in At_Index .. At_Index + 7 then B (I) = B'Old (I)))
   is
   begin
      Put_U32 (B, At_Index, Interfaces.Unsigned_32 (Value and 16#FFFF_FFFF#));
      Put_U32 (B, At_Index + 4, Interfaces.Unsigned_32 (Interfaces.Shift_Right (Value, 32)));
   end Put_U64;

   function Statement_Of
     (Kind           : Purpose;
      Payload_Length : File_Length;
      Payload_Digest : Digest;
      Leaf_Digest    : Digest) return Statement
   is
      Result : Statement := [others => 0];
      Fields : constant File_Index := Statement_Prefix_Length;
      Digests : constant File_Index := Fields + 16;
   begin
      for I in 0 .. Statement_Prefix_Length - 1 loop
         Result (File_Index (I)) := Character'Pos (Statement_Prefix_Text (Statement_Prefix_Text'First + I));
      end loop;
      Put_U16 (Result, Fields, Envelope_Format_V1);
      Put_U16 (Result, Fields + 2, Suite'Enum_Rep (ECDSA_P384_SHA384));
      Put_U16 (Result, Fields + 4, Purpose'Enum_Rep (Kind));
      Put_U16 (Result, Fields + 6, Reserved_Value);
      Put_U64 (Result, Fields + 8, Interfaces.Unsigned_64 (Payload_Length));
      for I in Digest'Range loop
         Result (Digests + I) := Payload_Digest (I);
         pragma Loop_Invariant (for all J in 0 .. I => Result (Digests + J) = Payload_Digest (J));
         pragma Loop_Invariant (for all J in 0 .. Digests - 1 => Result (J) = Result'Loop_Entry (J));
      end loop;
      for I in Digest'Range loop
         Result (Digests + Digest_Length + I) := Leaf_Digest (I);
         pragma Loop_Invariant
           (for all J in 0 .. I => Result (Digests + Digest_Length + J) = Leaf_Digest (J));
         pragma Loop_Invariant
           (for all J in 0 .. Digests + Digest_Length - 1 => Result (J) = Result'Loop_Entry (J));
      end loop;
      return Result;
   end Statement_Of;

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
   is
      Leaf_Length  : constant File_Index := File_Index (Leaf'Length);
      Inter_Length : constant File_Index := (if Count = 2 then File_Index (Intermediate'Length) else 0);
      Envelope_Size : constant File_Index :=
        Envelope_Header_Length + 2 * File_Index (Count) + Leaf_Length + Inter_Length + Signature_Length;
      At_Cert : constant File_Index := Lengths_Offset + 2 * File_Index (Count);
   begin
      Output := [others => 0];
      for I in Magic'Range loop
         Output (I) := Envelope_Magic (I);
      end loop;
      Put_U16 (Output, Format_Offset, Envelope_Format_V1);
      Put_U16 (Output, Suite_Offset, Suite'Enum_Rep (ECDSA_P384_SHA384));
      Put_U16 (Output, Purpose_Offset, Purpose'Enum_Rep (Kind));
      Put_U16 (Output, Reserved_Offset, Reserved_Value);
      Put_U64 (Output, Payload_Length_Offset, Interfaces.Unsigned_64 (Payload_Length));
      for I in Digest'Range loop
         Output (Digest_Offset + I) := Payload_Digest (I);
      end loop;
      Output (Count_Offset) := Byte (Count);
      Put_U16 (Output, Lengths_Offset, Interfaces.Unsigned_16 (Leaf_Length));
      if Count = 2 then
         Put_U16 (Output, Lengths_Offset + 2, Interfaces.Unsigned_16 (Inter_Length));
      end if;
      for I in Leaf'Range loop
         Output (At_Cert + File_Index (I)) := Leaf (I);
      end loop;
      if Count = 2 then
         for I in Intermediate'Range loop
            Output (At_Cert + Leaf_Length + File_Index (I)) := Intermediate (I);
         end loop;
      end if;
      for I in Signature_Bytes'Range loop
         Output (Envelope_Size - Signature_Length + I) := Signature (I);
      end loop;
      for I in Magic'Range loop
         Output (Envelope_Size + I) := Footer_Magic (I);
      end loop;
      Put_U32 (Output, Envelope_Size + Footer_Length_Offset, Interfaces.Unsigned_32 (Envelope_Size));
      Put_U32 (Output, Envelope_Size + Footer_Version_Offset, Footer_Version_V1);
      Length := Envelope_Size + Footer_Length;
   end Encode;

end CuBit.Signed_Admission.Envelopes;
