------------------------------------------------------------------------------
--  CuBit OS
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  hash_rate [MiB]: SHA-384 and SHA-256 throughput of SPARKTLSCrypto over
--  one buffer (default 256 MiB), best of Repeats runs, then the costs a
--  launch adds besides the hash: ECDSA P-384 verification, and Verify on
--  a signed 1 MiB file with a one-certificate and a two-certificate chain.
--  Keys are made here and thrown away.
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Command_Line;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with SPARKNaCl;
with SPARKTLSCrypto.Hashing.SHA256;
with SPARKTLSCrypto.Hashing.SHA384;
with SPARKTLSCrypto.P384.ECDSA;
with SPARKTLSCrypto.RFC6979;
with X509;
with CuBit.Certificate_Encoder;
with CuBit.Signed_Admission; use CuBit.Signed_Admission;
with CuBit.Signed_Admission.Decisions;
with CuBit.Signed_Admission.Envelopes;
with CuBit.Program_Signing;
with CuBit.Signing_Keys;
with Host_Signing;

procedure Hash_Rate is
   use type SPARKNaCl.N32;
   use type X509.N32;
   use type CuBit.Certificate_Encoder.Certificate_Kind;
   use type SPARKNaCl.Byte_Seq;
   package Keys renames CuBit.Signing_Keys;
   package Encoder renames CuBit.Certificate_Encoder;
   Mebibyte : constant := 1024 * 1024;
   Repeats : constant := 5;
   Size_MiB : constant Positive :=
     (if Ada.Command_Line.Argument_Count >= 1 then Positive'Value (Ada.Command_Line.Argument (1)) else 256);
   type Buffer_Access is access SPARKNaCl.Byte_Seq;
   Data : constant Buffer_Access := new SPARKNaCl.Byte_Seq (0 .. SPARKNaCl.N32 (Size_MiB) * Mebibyte - 1);

   function Seconds (D : Time_Span) return Float is (Float (To_Duration (D)));

   procedure Rate (Name : String; Hash : access procedure) is
      Best : Time_Span := Time_Span_Last;
   begin
      for R in 1 .. Repeats loop
         declare
            Start : constant Time := Clock;
         begin
            Hash.all;
            if Clock - Start < Best then
               Best := Clock - Start;
            end if;
         end;
      end loop;
      Put_Line (Name & ":" & Integer'Image (Integer (Float (Size_MiB) / Seconds (Best))) & " MiB/s ("
                & Size_MiB'Image & " MiB in" & Integer'Image (Integer (Seconds (Best) * 1000.0)) & " ms)");
   end Rate;

   procedure SHA384 is
      D : SPARKNaCl.Bytes_48;
   begin
      SPARKTLSCrypto.Hashing.SHA384.Hash (D, Data.all);
      pragma Unreferenced (D);
   end SHA384;

   procedure SHA256 is
      D : SPARKNaCl.Bytes_32;
   begin
      SPARKTLSCrypto.Hashing.SHA256.Hash (D, Data.all);
      pragma Unreferenced (D);
   end SHA256;

   function Microseconds_Per (Count : Positive; Action : access procedure) return Integer is
      Start : constant Time := Clock;
   begin
      for I in 1 .. Count loop
         Action.all;
      end loop;
      return Integer (Seconds (Clock - Start) * 1.0E6 / Float (Count));
   end Microseconds_Per;

   function New_Key return Keys.Scalar is
      D : Keys.Scalar;
   begin
      loop
         Host_Signing.Random (D);
         exit when Keys.Strong (D);
      end loop;
      return D;
   end New_Key;

   Root_D : constant Keys.Scalar := New_Key;
   Inter_D : constant Keys.Scalar := New_Key;
   Leaf_D : constant Keys.Scalar := New_Key;

   function Point (D : Keys.Scalar) return Keys.Point is
      Q : Keys.Point;
   begin
      Keys.Public_Of (D, Host_Signing.Mask, Q);
      return Q;
   end Point;

   No_Issuer : constant X509.Byte_Seq (0 .. 0) := [0];
   function Issue (Kind : Encoder.Certificate_Kind; Subject : Keys.Scalar; Issuer : X509.Byte_Seq;
                   Issuer_Key : Keys.Scalar) return X509.Byte_Seq is
      R : Encoder.Request;
      Out_DER : Encoder.Encoded_Certificate;
      Length : X509.N32;
      Result : Encoder.Issue_Result;
      Now : constant X509.Date_Time := Host_Signing.Now;
   begin
      R.Kind := Kind;
      R.Subject.Length := 9;
      R.Subject.Data (1 .. 9) := "Benchmark";
      R.Not_Before := Now;
      R.Not_After := (Now.Year + 1, 1, 1, 0, 0, 0);
      R.Subject_Key := Point (Subject);
      R.Path := (if Kind = Encoder.Root_CA then 1 else 0);
      Encoder.Issue (R, Issuer, Issuer_Key, Now, Host_Signing.Mask, Out_DER, Length, Result);
      return Out_DER (0 .. Length - 1);
   end Issue;

   Root : constant X509.Byte_Seq := Issue (Encoder.Root_CA, Root_D, No_Issuer, Root_D);
   Inter : constant X509.Byte_Seq := Issue (Encoder.Intermediate_CA, Inter_D, Root, Root_D);
   Leaf : constant X509.Byte_Seq := Issue (Encoder.Code_Signing_Leaf, Leaf_D, Root, Root_D);
   Leaf2 : constant X509.Byte_Seq := Issue (Encoder.Code_Signing_Leaf, Leaf_D, Inter, Inter_D);
   Payload : constant SPARKNaCl.Byte_Seq (0 .. Mebibyte - 1) := [others => 16#5A#];

   function Signed (L : X509.Byte_Seq; Count : Certificate_Count; I : X509.Byte_Seq) return SPARKNaCl.Byte_Seq is
      T : Envelopes.Trailer;
      Length : SPARKNaCl.N32;
      R : CuBit.Program_Signing.Signing_Result;
   begin
      CuBit.Program_Signing.Sign (Payload, Program, L, Count, I, Leaf_D, Host_Signing.Now, Host_Signing.Mask,
                                  T, Length, R);
      return Payload & T (0 .. Length - 1);
   end Signed;

   Direct : constant SPARKNaCl.Byte_Seq := Signed (Leaf, 1, Leaf);
   Chained : constant SPARKNaCl.Byte_Seq := Signed (Leaf2, 2, Inter);
   Anchors : Anchor_Array (1 .. 1);
   None : constant Revocation_Set := (Keys => 0, Certificates => 0, Programs => 0, others => <>);

   Hash : constant SPARKNaCl.Bytes_48 := [others => 7];
   Q : constant Keys.Point := Point (Leaf_D);
   R_Value, S_Value : SPARKNaCl.Byte_Seq (0 .. 47);

   procedure Verify_ECDSA is
      OK : constant Boolean := SPARKTLSCrypto.P384.ECDSA.Verify (Hash, Q (1 .. 48), Q (49 .. 96), R_Value, S_Value);
   begin
      if not OK then
         raise Program_Error;
      end if;
   end Verify_ECDSA;

   procedure Verify_Direct is
   begin
      if not Decisions.Verify (Direct, Program, Anchors, None).Verified then
         raise Program_Error;
      end if;
   end Verify_Direct;

   procedure Verify_Chained is
   begin
      if not Decisions.Verify (Chained, Program, Anchors, None).Verified then
         raise Program_Error;
      end if;
   end Verify_Chained;

   procedure Hash_Payload is
      D : SPARKNaCl.Bytes_48;
   begin
      SPARKTLSCrypto.Hashing.SHA384.Hash (D, Payload);
      pragma Unreferenced (D);
   end Hash_Payload;
begin
   for I in Data'Range loop
      Data (I) := Unsigned_8 (I mod 251);
   end loop;
   Put_Line ("SHA-NI for SHA-256: " & Boolean'Image (SPARKTLSCrypto.Hashing.SHA256.Has_HW_Accel));
   Rate ("SHA-384", SHA384'Access);
   Rate ("SHA-256", SHA256'Access);
   declare
      K : SPARKNaCl.Bytes_48;
      OK : Boolean;
   begin
      SPARKTLSCrypto.RFC6979.Derive_K_P384 (Leaf_D, Hash, K, OK);
      SPARKTLSCrypto.P384.ECDSA.Sign (Hash, Leaf_D, K, Host_Signing.Mask, R_Value, S_Value, OK);
   end;
   Anchors (1).Kind := Host_Development;
   Anchors (1).Ceiling := Hardware_And_System;
   Anchors (1).Length := Root'Length;
   Anchors (1).DER (0 .. Root'Length - 1) := Root;
   Put_Line ("ECDSA P-384 verify:" & Microseconds_Per (20, Verify_ECDSA'Access)'Image & " us");
   Put_Line ("SHA-384 of 1 MiB:" & Microseconds_Per (20, Hash_Payload'Access)'Image & " us");
   Put_Line ("Verify, 1 MiB, leaf under the anchor:" & Microseconds_Per (20, Verify_Direct'Access)'Image & " us");
   Put_Line ("Verify, 1 MiB, leaf under an intermediate:" & Microseconds_Per (20, Verify_Chained'Access)'Image & " us");
end Hash_Rate;
