-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
-- SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------
with SPARKTLSCrypto.Hashing.SHA256;
with SPARKTLSCrypto.Hashing.SHA384;
with SPARKTLSCrypto.P384.ECDSA;
with SPARKTLSCrypto.RFC6979;
with CuBit.Signed_Admission.Certificates;

package body CuBit.Certificate_Encoder with SPARK_Mode => On is
   use type Interfaces.Unsigned_8;
   use type SPARKNaCl.Byte_Seq;
   package Certificates renames CuBit.Signed_Admission.Certificates;
   package Keys renames CuBit.Signing_Keys;

   subtype Byte is Interfaces.Unsigned_8;

   --  DER tags.
   Tag_Boolean      : constant := 16#01#;
   Tag_Integer      : constant := 16#02#;
   Tag_Bit_String   : constant := 16#03#;
   Tag_Octet_String : constant := 16#04#;
   Tag_UTF8_String  : constant := 16#0C#;
   Tag_UTC_Time     : constant := 16#17#;
   Tag_General_Time : constant := 16#18#;
   Tag_Sequence     : constant := 16#30#;
   Tag_Set          : constant := 16#31#;
   Tag_Explicit_0   : constant := 16#A0#;
   Tag_Explicit_3   : constant := 16#A3#;
   Tag_Key_ID       : constant := 16#80#;   --  [0] IMPLICIT keyIdentifier
   Boolean_True     : constant := 16#FF#;
   X509_Version_3   : constant := 2;
   Short_Form_Limit : constant := 16#80#;
   One_Length_Byte  : constant := 16#81#;
   Two_Length_Bytes : constant := 16#82#;
   Byte_Modulus     : constant := 16#100#;
   High_Bit         : constant := 16#80#;

   --  Object identifiers, as their DER TLVs.
   OID_ECDSA_SHA384 : constant SPARKNaCl.Byte_Seq :=     --  1.2.840.10045.4.3.3
     [16#06#, 16#08#, 16#2A#, 16#86#, 16#48#, 16#CE#, 16#3D#, 16#04#, 16#03#, 16#03#];
   OID_Common_Name : constant SPARKNaCl.Byte_Seq :=      --  2.5.4.3
     [16#06#, 16#03#, 16#55#, 16#04#, 16#03#];
   OID_Basic_Constraints : constant SPARKNaCl.Byte_Seq := [16#06#, 16#03#, 16#55#, 16#1D#, 16#13#];
   OID_Key_Usage : constant SPARKNaCl.Byte_Seq := [16#06#, 16#03#, 16#55#, 16#1D#, 16#0F#];
   OID_Subject_Key_ID : constant SPARKNaCl.Byte_Seq := [16#06#, 16#03#, 16#55#, 16#1D#, 16#0E#];
   OID_Authority_Key_ID : constant SPARKNaCl.Byte_Seq := [16#06#, 16#03#, 16#55#, 16#1D#, 16#23#];
   OID_Extended_Key_Usage : constant SPARKNaCl.Byte_Seq := [16#06#, 16#03#, 16#55#, 16#1D#, 16#25#];
   OID_Code_Signing : constant SPARKNaCl.Byte_Seq :=     --  1.3.6.1.5.5.7.3.3
     [16#06#, 16#08#, 16#2B#, 16#06#, 16#01#, 16#05#, 16#05#, 16#07#, 16#03#, 16#03#];
   --  SubjectPublicKeyInfo up to the point (id-ecPublicKey, secp384r1).
   SPKI_Header : constant SPARKNaCl.Byte_Seq :=
     [16#30#, 16#76#, 16#30#, 16#10#, 16#06#, 16#07#, 16#2A#, 16#86#,
      16#48#, 16#CE#, 16#3D#, 16#02#, 16#01#, 16#06#, 16#05#, 16#2B#,
      16#81#, 16#04#, 16#00#, 16#22#, 16#03#, 16#62#, 16#00#];
   --  keyUsage BIT STRINGs: keyCertSign and cRLSign (bits 5, 6; one
   --  unused bit), digitalSignature (bit 0; seven unused bits).
   KU_CA   : constant SPARKNaCl.Byte_Seq := [Tag_Bit_String, 16#02#, 16#01#, 16#06#];
   KU_Leaf : constant SPARKNaCl.Byte_Seq := [Tag_Bit_String, 16#02#, 16#07#, 16#80#];

   --  RFC 7093 method 1: the leftmost 160 bits of SHA-256 of the key.
   Key_ID_Length : constant := 20;
   --  Bounds of what an issuer certificate may hand down.
   Max_Issuer_Name : constant := 128;
   Max_Issuer_Key_ID : constant := 64;

   Earliest_Year : constant := 1950;
   UTC_Time_Last_Year : constant := 2049;
   Latest_Year : constant := 9999;
   Months : constant := 12;
   Last_Hour : constant := 23;
   Last_Minute : constant := 59;
   Last_Second : constant := 59;
   Leap_Cycle : constant := 4;
   Century : constant := 100;
   Leap_Century : constant := 400;
   February : constant := 2;
   type Month_Days is array (1 .. Months) of Natural;
   Days_In : constant Month_Days := [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];

   function Leap (Year : Natural) return Boolean is
     (Year mod Leap_Cycle = 0 and then (Year mod Century /= 0 or else Year mod Leap_Century = 0));

   function Valid_Date (T : X509.Date_Time) return Boolean is
     (T.Year in Earliest_Year .. Latest_Year
      and then T.Month in 1 .. Months
      and then T.Day >= 1
      and then T.Day <= Days_In (T.Month) + (if T.Month = February and then Leap (T.Year) then 1 else 0)
      and then T.Hour <= Last_Hour and then T.Minute <= Last_Minute and then T.Second <= Last_Second);

   UTC_Time_Length : constant := 13;       --  YYMMDDHHMMSSZ
   General_Time_Length : constant := 15;   --  YYYYMMDDHHMMSSZ

   function Header_Size (Content : Natural) return Natural is
     (if Content < Short_Form_Limit then 2 elsif Content < Byte_Modulus then 3 else 4);
   function TLV (Content : Natural) return Natural is (Header_Size (Content) + Content)
   with Pre => Content < Byte_Modulus * Byte_Modulus;

   function Time_TLV (T : X509.Date_Time) return Natural is
     (if T.Year <= UTC_Time_Last_Year then 2 + UTC_Time_Length else 2 + General_Time_Length);

   --  Minimal positive INTEGER content of a 48-byte big-endian value.
   function Integer_Content (V : SPARKNaCl.Byte_Seq) return Natural
   with Pre => V'First = 0 and then V'Length = Keys.Scalar_Length,
        Post => Integer_Content'Result in 1 .. Keys.Scalar_Length + 1
   is
      First : Natural := 0;
   begin
      while First < Keys.Scalar_Length - 1 and then V (SPARKNaCl.N32 (First)) = 0 loop
         First := First + 1;
         pragma Loop_Invariant (First <= Keys.Scalar_Length - 1);
         pragma Loop_Variant (Increases => First);
      end loop;
      return Keys.Scalar_Length - First
        + (if V (SPARKNaCl.N32 (First)) >= High_Bit then 1 else 0);
   end Integer_Content;

   procedure Issue
     (R          : Request;
      Issuer     : X509.Byte_Seq;
      Issuer_Key : Keys.Scalar;
      Now        : X509.Date_Time;
      Mask       : Keys.Blind;
      Output     : out Encoded_Certificate;
      Length     : out X509.N32;
      Result     : out Issue_Result)
   is
      Work : SPARKNaCl.Byte_Seq (0 .. Max_Encoded_Length - 1) := [others => 0];
      Pos  : Natural := 0;

      --  What comes from the issuer: its subject (the new issuer name),
      --  its key identifier and its key.
      Issuer_Name : SPARKNaCl.Byte_Seq (0 .. Max_Issuer_Name - 1) := [others => 0];
      Issuer_Name_Length : Natural range 0 .. Max_Issuer_Name := 0;
      Authority_ID : SPARKNaCl.Byte_Seq (0 .. Max_Issuer_Key_ID - 1) := [others => 0];
      Authority_ID_Length : Natural range 1 .. Max_Issuer_Key_ID := Key_ID_Length;
      Subject_ID : SPARKNaCl.Bytes_32;
      Is_CA : constant Boolean := R.Kind /= Code_Signing_Leaf;

      procedure Put (B : Byte)
      with Pre => Pos < Max_Encoded_Length, Post => Pos = Pos'Old + 1
      is
      begin
         Work (SPARKNaCl.N32 (Pos)) := B;
         Pos := Pos + 1;
      end Put;

      procedure Put_All (S : SPARKNaCl.Byte_Seq)
      with Pre  => S'Length <= Max_Encoded_Length and then Pos <= Max_Encoded_Length - S'Length,
           Post => Pos = Pos'Old + S'Length
      is
      begin
         for I in S'Range loop
            pragma Loop_Invariant (Pos = Pos'Loop_Entry + Natural (I - S'First));
            Put (S (I));
         end loop;
      end Put_All;

      procedure Put_Header (Tag : Byte; Content : Natural)
      with Pre  => Content < Byte_Modulus * Byte_Modulus
                   and then Pos <= Max_Encoded_Length - Header_Size (Content),
           Post => Pos = Pos'Old + Header_Size (Content)
      is
      begin
         Put (Tag);
         if Content < Short_Form_Limit then
            Put (Byte (Content));
         elsif Content < Byte_Modulus then
            Put (One_Length_Byte);
            Put (Byte (Content));
         else
            Put (Two_Length_Bytes);
            Put (Byte (Content / Byte_Modulus));
            Put (Byte (Content mod Byte_Modulus));
         end if;
      end Put_Header;

      procedure Put_Digit (Value : Natural)
      with Pre  => Value <= 9 and then Pos < Max_Encoded_Length,
           Post => Pos = Pos'Old + 1
      is
      begin
         Put (Byte (Character'Pos ('0') + Value));
      end Put_Digit;

      procedure Put_Digits (Value, Count : Natural)
      with Pre  => (Count = 2 and then Value <= 99 and then Pos <= Max_Encoded_Length - 2)
                   or else (Count = 4 and then Value <= 9999 and then Pos <= Max_Encoded_Length - 4),
           Post => Pos = Pos'Old + Count
      is
      begin
         if Count = 4 then
            Put_Digit (Value / 1000);
            Put_Digit (Value / 100 mod 10);
         end if;
         Put_Digit (Value / 10 mod 10);
         Put_Digit (Value mod 10);
      end Put_Digits;

      procedure Put_Time (T : X509.Date_Time)
      with Pre  => Valid_Date (T) and then Pos <= Max_Encoded_Length - Time_TLV (T),
           Post => Pos = Pos'Old + Time_TLV (T)
      is
      begin
         if T.Year <= UTC_Time_Last_Year then
            Put_Header (Tag_UTC_Time, UTC_Time_Length);
            Put_Digits (T.Year mod Century, 2);
         else
            Put_Header (Tag_General_Time, General_Time_Length);
            Put_Digits (T.Year, 4);
         end if;
         Put_Digits (T.Month, 2);
         Put_Digits (T.Day, 2);
         Put_Digits (T.Hour, 2);
         Put_Digits (T.Minute, 2);
         Put_Digits (T.Second, 2);
         Put (Character'Pos ('Z'));
      end Put_Time;

      --  Name ::= SEQUENCE { SET { SEQUENCE { id-at-commonName, UTF8String } } }
      function Name_Content (N : Name_Text) return Natural is
        (TLV (TLV (OID_Common_Name'Length + TLV (N.Length))))
      with Pre => Valid_Name (N);

      procedure Put_Name (N : Name_Text)
      with Pre  => Valid_Name (N) and then Pos <= Max_Encoded_Length - TLV (Name_Content (N)),
           Post => Pos = Pos'Old + TLV (Name_Content (N))
      is
      begin
         Put_Header (Tag_Sequence, Name_Content (N));
         Put_Header (Tag_Set, TLV (OID_Common_Name'Length + TLV (N.Length)));
         Put_Header (Tag_Sequence, OID_Common_Name'Length + TLV (N.Length));
         Put_All (OID_Common_Name);
         Put_Header (Tag_UTF8_String, N.Length);
         for I in 1 .. N.Length loop
            pragma Loop_Invariant (Pos = Pos'Loop_Entry + I - 1);
            Put (Byte (Character'Pos (N.Data (I))));
         end loop;
      end Put_Name;

      procedure Put_Integer (V : SPARKNaCl.Byte_Seq)
      with Pre  => V'First = 0 and then V'Length = Keys.Scalar_Length
                   and then Pos <= Max_Encoded_Length - TLV (Integer_Content (V)),
           Post => Pos = Pos'Old + TLV (Integer_Content (V))
      is
         Content : constant Natural := Integer_Content (V);
         Skip : constant Natural :=
           (if Content > Keys.Scalar_Length then 0 else Keys.Scalar_Length - Content);
         Start : constant Natural := Pos;
      begin
         Put_Header (Tag_Integer, Content);
         if Content > Keys.Scalar_Length then
            Put (0);
         end if;
         for I in Skip .. Keys.Scalar_Length - 1 loop
            pragma Loop_Invariant (Pos = Pos'Loop_Entry + (I - Skip));
            pragma Loop_Invariant (Pos'Loop_Entry = Start + Header_Size (Content)
                                     + (if Content > Keys.Scalar_Length then 1 else 0));
            Put (V (SPARKNaCl.N32 (I)));
         end loop;
      end Put_Integer;

      Subject_Name_TLV : Natural;
      Issuer_Name_TLV : Natural;
      Basic_Constraints_TLV : constant Natural :=
        TLV (OID_Basic_Constraints'Length + 3 + TLV (TLV (3 + 3)));
      Key_Usage_TLV : constant Natural := TLV (OID_Key_Usage'Length + 3 + TLV (KU_CA'Length));
      Subject_ID_TLV : constant Natural := TLV (OID_Subject_Key_ID'Length + TLV (TLV (Key_ID_Length)));
      Authority_ID_TLV : Natural;
      EKU_TLV : constant Natural := TLV (OID_Extended_Key_Usage'Length + TLV (TLV (OID_Code_Signing'Length)));
      Extensions_Content : Natural;
      TBS_Content : Natural;
      TBS_Length : Natural;
      Signature_Content : Natural;
      Certificate_Content : Natural;
      Hash : SPARKNaCl.Bytes_48;
      K : SPARKNaCl.Bytes_48;
      Sig_R, Sig_S : SPARKNaCl.Byte_Seq (0 .. Keys.Scalar_Length - 1) := [others => 0];
      OK : Boolean;
      Issuer_Point : Keys.Point;
      Key_Point : Keys.Point;
   begin
      Output := [others => 0];
      Length := 0;

      --  The request itself.
      if not Valid_Name (R.Subject) then
         Result := Name_Invalid;
         return;
      elsif not Valid_Date (R.Not_Before) or else not Valid_Date (R.Not_After)
        or else not Valid_Date (Now)
        or else not X509.DT_Before (R.Not_Before, R.Not_After)
      then
         Result := Window_Invalid;
         return;
      elsif X509.DT_Before (Now, R.Not_Before) then
         Result := Not_Yet_Valid;
         return;
      elsif X509.DT_Before (R.Not_After, Now) then
         Result := Expired;
         return;
      elsif not Keys.Strong (Issuer_Key) then
         Result := Weak_Issuer_Key;
         return;
      elsif R.Subject_Key (0) /= 16#04# then
         Result := Key_Mismatch;
         return;
      end if;

      Subject_ID := SPARKTLSCrypto.Hashing.SHA256.Hash (R.Subject_Key);
      Keys.Public_Of (Issuer_Key, Mask, Key_Point);

      --  The issuer.
      if R.Kind = Root_CA then
         if Key_Point /= R.Subject_Key then
            Result := Key_Mismatch;
            return;
         end if;
         Authority_ID (0 .. Key_ID_Length - 1) := Subject_ID (0 .. Key_ID_Length - 1);
         Authority_ID_Length := Key_ID_Length;
      else
         if not Certificates.Profile_Holds (Issuer, Certificates.Anchor_Role)
           and then not Certificates.Profile_Holds (Issuer, Certificates.Intermediate_Role)
         then
            Result := Issuer_Not_CA;
            return;
         elsif R.Kind = Intermediate_CA and then not Certificates.Issues_Intermediate (Issuer) then
            Result := Issuer_Not_CA;
            return;
         end if;
         Certificates.Public_Point (Issuer, Issuer_Point, OK);
         if not OK or else Issuer_Point /= Key_Point then
            Result := Key_Mismatch;
            return;
         end if;
         declare
            C : X509.Certificate;
            Parsed : Boolean;
         begin
            X509.Parse (Issuer, C, Parsed);
            if not Parsed then
               Result := Issuer_Not_CA;
               return;
            end if;
            if X509.DT_Before (R.Not_Before, X509.Not_Before (C))
              or else X509.DT_Before (X509.Not_After (C), R.Not_After)
            then
               Result := Window_Outside_Issuer;
               return;
            end if;
            declare
               Name : constant X509.Span := X509.Subject_Raw (C);
               Key_ID : constant X509.Span := X509.Subject_Key_ID (C);
            begin
               if not Name.Present or else not Key_ID.Present
                 or else Name.Last < Name.First or else Key_ID.Last < Key_ID.First
                 or else Name.Last > Issuer'Last or else Key_ID.Last > Issuer'Last
                 or else Name.Last - Name.First >= Max_Issuer_Name
                 or else Key_ID.Last - Key_ID.First >= Max_Issuer_Key_ID
               then
                  Result := Issuer_Name_Unsupported;
                  return;
               end if;
               Issuer_Name_Length := Natural (Name.Last - Name.First) + 1;
               for K in Name.First .. Name.Last loop
                  Issuer_Name (SPARKNaCl.N32 (K - Name.First)) := Issuer (K);
               end loop;
               Authority_ID_Length := Natural (Key_ID.Last - Key_ID.First) + 1;
               for K in Key_ID.First .. Key_ID.Last loop
                  Authority_ID (SPARKNaCl.N32 (K - Key_ID.First)) := Issuer (K);
               end loop;
            end;
         end;
      end if;

      --  Sizes, inside out.
      Subject_Name_TLV := TLV (Name_Content (R.Subject));
      Issuer_Name_TLV :=
        (if R.Kind = Root_CA then Subject_Name_TLV else TLV (Issuer_Name_Length));
      Authority_ID_TLV :=
        TLV (OID_Authority_Key_ID'Length + TLV (TLV (TLV (Authority_ID_Length))));
      Extensions_Content :=
        (if Is_CA then Basic_Constraints_TLV else 0)
        + Key_Usage_TLV + Subject_ID_TLV + Authority_ID_TLV + EKU_TLV;
      TBS_Content :=
        5 + TLV (Serial_Length) + TLV (OID_ECDSA_SHA384'Length)
        + Issuer_Name_TLV + TLV (Time_TLV (R.Not_Before) + Time_TLV (R.Not_After))
        + Subject_Name_TLV + SPKI_Header'Length + Keys.Point_Length
        + TLV (TLV (Extensions_Content));
      TBS_Length := TLV (TBS_Content);

      --  TBSCertificate.
      Put_Header (Tag_Sequence, TBS_Content);
      Put_Header (Tag_Explicit_0, 3);
      Put_Header (Tag_Integer, 1);
      Put (X509_Version_3);
      Put_Header (Tag_Integer, Serial_Length);
      Put (R.Serial (0) mod (High_Bit - 1) + 1);   --  1 .. 127: positive, minimal
      Put_All (R.Serial (1 .. Serial_Length - 1));
      Put_Header (Tag_Sequence, OID_ECDSA_SHA384'Length);
      Put_All (OID_ECDSA_SHA384);
      if R.Kind = Root_CA then
         Put_Name (R.Subject);
      else
         Put_Header (Tag_Sequence, Issuer_Name_Length);
         Put_All (Issuer_Name (0 .. SPARKNaCl.N32 (Issuer_Name_Length) - 1));
      end if;
      Put_Header (Tag_Sequence, Time_TLV (R.Not_Before) + Time_TLV (R.Not_After));
      Put_Time (R.Not_Before);
      Put_Time (R.Not_After);
      Put_Name (R.Subject);
      Put_All (SPKI_Header);
      Put_All (R.Subject_Key);
      Put_Header (Tag_Explicit_3, TLV (Extensions_Content));
      Put_Header (Tag_Sequence, Extensions_Content);
      if Is_CA then
         --  basicConstraints, critical: SEQUENCE { cA TRUE, pathLen }
         Put_Header (Tag_Sequence, OID_Basic_Constraints'Length + 3 + TLV (TLV (3 + 3)));
         Put_All (OID_Basic_Constraints);
         Put (Tag_Boolean); Put (1); Put (Boolean_True);
         Put_Header (Tag_Octet_String, TLV (3 + 3));
         Put_Header (Tag_Sequence, 3 + 3);
         Put (Tag_Boolean); Put (1); Put (Boolean_True);
         Put (Tag_Integer); Put (1);
         Put (Byte (if R.Kind = Intermediate_CA then 0 else R.Path));
      end if;
      --  keyUsage, critical.
      Put_Header (Tag_Sequence, OID_Key_Usage'Length + 3 + TLV (KU_CA'Length));
      Put_All (OID_Key_Usage);
      Put (Tag_Boolean); Put (1); Put (Boolean_True);
      Put_Header (Tag_Octet_String, KU_CA'Length);
      Put_All (if Is_CA then KU_CA else KU_Leaf);
      --  subjectKeyIdentifier.
      Put_Header (Tag_Sequence, OID_Subject_Key_ID'Length + TLV (TLV (Key_ID_Length)));
      Put_All (OID_Subject_Key_ID);
      Put_Header (Tag_Octet_String, TLV (Key_ID_Length));
      Put_Header (Tag_Octet_String, Key_ID_Length);
      Put_All (Subject_ID (0 .. Key_ID_Length - 1));
      --  authorityKeyIdentifier: SEQUENCE { [0] keyIdentifier }
      Put_Header (Tag_Sequence, OID_Authority_Key_ID'Length + TLV (TLV (TLV (Authority_ID_Length))));
      Put_All (OID_Authority_Key_ID);
      Put_Header (Tag_Octet_String, TLV (TLV (Authority_ID_Length)));
      Put_Header (Tag_Sequence, TLV (Authority_ID_Length));
      Put_Header (Tag_Key_ID, Authority_ID_Length);
      Put_All (Authority_ID (0 .. SPARKNaCl.N32 (Authority_ID_Length) - 1));
      --  extKeyUsage: codeSigning.
      Put_Header (Tag_Sequence, OID_Extended_Key_Usage'Length + TLV (TLV (OID_Code_Signing'Length)));
      Put_All (OID_Extended_Key_Usage);
      Put_Header (Tag_Octet_String, TLV (OID_Code_Signing'Length));
      Put_Header (Tag_Sequence, OID_Code_Signing'Length);
      Put_All (OID_Code_Signing);
      pragma Assert (Pos = TBS_Length);

      --  The signature over the TBSCertificate.
      Hash := SPARKTLSCrypto.Hashing.SHA384.Hash (Work (0 .. SPARKNaCl.N32 (TBS_Length) - 1));
      SPARKTLSCrypto.RFC6979.Derive_K_P384 (Issuer_Key, Hash, K, OK);
      if OK then
         SPARKTLSCrypto.P384.ECDSA.Sign (Hash, Issuer_Key, K, Mask, Sig_R, Sig_S, OK);
      end if;
      if not OK then
         Result := Signing_Failed;
         return;
      end if;
      Signature_Content := TLV (TLV (Integer_Content (Sig_R)) + TLV (Integer_Content (Sig_S)));
      Certificate_Content :=
        TBS_Length + TLV (OID_ECDSA_SHA384'Length) + TLV (1 + Signature_Content);

      --  Certificate ::= SEQUENCE { tbs, algorithm, BIT STRING signature }:
      --  the TBS moves up behind the outer header.
      declare
         Header : constant Natural := Header_Size (Certificate_Content);
      begin
         for I in reverse 0 .. TBS_Length - 1 loop
            Work (SPARKNaCl.N32 (I + Header)) := Work (SPARKNaCl.N32 (I));
         end loop;
         Pos := 0;
         Put_Header (Tag_Sequence, Certificate_Content);
         Pos := Header + TBS_Length;
      end;
      Put_Header (Tag_Sequence, OID_ECDSA_SHA384'Length);
      Put_All (OID_ECDSA_SHA384);
      Put_Header (Tag_Bit_String, 1 + Signature_Content);
      Put (0);   --  no unused bits
      Put_Header (Tag_Sequence, TLV (Integer_Content (Sig_R)) + TLV (Integer_Content (Sig_S)));
      Put_Integer (Sig_R);
      Put_Integer (Sig_S);

      for I in 0 .. Pos - 1 loop
         Output (X509.N32 (I)) := Work (SPARKNaCl.N32 (I));
      end loop;

      --  It must parse back into the profile, and its link must verify.
      declare
         Emitted : constant X509.Byte_Seq := Output (0 .. X509.N32 (Pos) - 1);
         Role : constant Certificates.Role :=
           (case R.Kind is
              when Root_CA => Certificates.Anchor_Role,
              when Intermediate_CA => Certificates.Intermediate_Role,
              when Code_Signing_Leaf => Certificates.Leaf_Role);
      begin
         if not Certificates.Profile_Holds (Emitted, Role)
           or else not Certificates.Link_Holds (Emitted, (if R.Kind = Root_CA then Emitted else Issuer))
         then
            Output := [others => 0];
            Result := Self_Check_Failed;
            return;
         end if;
      end;
      Length := X509.N32 (Pos);
      Result := Issued;
   end Issue;

end CuBit.Certificate_Encoder;
