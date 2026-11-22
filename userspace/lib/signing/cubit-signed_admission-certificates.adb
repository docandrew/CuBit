-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
-- SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------
with SPARKTLSCrypto.Hashing.SHA384;
with SPARKTLSCrypto.P384.ECDSA;

package body CuBit.Signed_Admission.Certificates with SPARK_Mode => On is
   use type Interfaces.Unsigned_8;
   use type X509.Algorithm_ID;

   --  SubjectPublicKeyInfo for an id-ecPublicKey key on secp384r1, up to
   --  the point: SEQUENCE { SEQUENCE { OID, OID }, BIT STRING (0 unused) }.
   SPKI_Header_Length : constant := 23;
   SPKI_Header : constant SPARKNaCl.Byte_Seq (0 .. SPKI_Header_Length - 1) :=
     [16#30#, 16#76#, 16#30#, 16#10#, 16#06#, 16#07#, 16#2A#, 16#86#,
      16#48#, 16#CE#, 16#3D#, 16#02#, 16#01#, 16#06#, 16#05#, 16#2B#,
      16#81#, 16#04#, 16#00#, 16#22#, 16#03#, 16#62#, 16#00#];
   Uncompressed_Point : constant := 16#04#;

   --  DER tags of the ECDSA-Sig-Value.
   Tag_Sequence : constant := 16#30#;
   Tag_Integer  : constant := 16#02#;
   Short_Form_Limit : constant := 16#80#;

   --  A span that lies inside DER with at least one byte.
   function Usable (S : X509.Span; DER : X509.Byte_Seq) return Boolean is
     (S.Present and then S.First <= S.Last and then S.Last <= DER'Last and then S.First >= DER'First);

   function Spans_Equal
     (A_DER : X509.Byte_Seq; A : X509.Span; B_DER : X509.Byte_Seq; B : X509.Span) return Boolean is
     (Usable (A, A_DER) and then Usable (B, B_DER)
      and then A.Last - A.First = B.Last - B.First
      and then (for all I in 0 .. A.Last - A.First => A_DER (A.First + I) = B_DER (B.First + I)));

   --  Every rule the profile shares across roles.
   function Common_Holds (C : X509.Certificate) return Boolean is
     (X509.Is_Valid (C)
      and then X509.Version (C) = 3
      and then not X509.Has_Bad_DER (C)
      and then not X509.Has_Unknown_Critical_Extension (C)
      and then not X509.Has_Duplicate_Extension (C)
      and then not X509.Has_Bad_Extension_Criticality (C)
      and then not X509.Has_Bad_Serial (C)
      and then not X509.Has_Bad_Time_Format (C)
      and then not X509.Has_Bad_SAN (C)
      and then not X509.Has_Empty_Key_Usage_Value (C)
      and then not X509.Has_Unique_ID_Version_Error (C)
      and then not X509.Has_SAN_Subject_Error (C)
      and then not X509.Has_Bad_Ext_Content (C)
      and then not X509.Has_Bad_PubKey (C)
      and then not X509.Has_AKID_Missing_Key_ID (C)
      and then not X509.Has_Name_Constraints_NonCA (C)
      and then not X509.Has_Bad_Inhibit_Value (C)
      and then not X509.Has_Bad_Cert_Policy (C)
      and then not X509.Has_Bad_AKID (C)
      and then not X509.Has_Bad_Subject_Encoding (C)
      and then not X509.Has_Bad_EKU_Content (C)
      and then not X509.Has_Bad_CRL_DP (C)
      and then not X509.Has_SAN_Critical_With_Subject (C)
      and then not X509.Has_V3_UniqueID_NoExts (C)
      and then not X509.Has_Path_Len_Without_CA (C)
      and then not X509.Has_Key_Cert_Sign_Without_CA (C)
      and then X509.PK_Algorithm (C) = X509.Algo_EC_P384
      and then X509.PK_Length (C) = Point_Length
      and then X509.Sig_Algorithm (C) = X509.Algo_ECDSA_P384_SHA384
      and then X509.Sig_Algorithm_2 (C) = X509.Algo_ECDSA_P384_SHA384
      and then X509.TBS (C).Present
      and then X509.Has_Subject_CN (C)
      and then X509.DT_Before_Or_Equal (X509.Not_Before (C), X509.Not_After (C)));

   function CA_Holds (C : X509.Certificate; DER : X509.Byte_Seq) return Boolean is
     (X509.Is_CA (C)
      and then X509.Is_Basic_Constraints_Critical (C)
      and then X509.Has_Key_Usage (C)
      and then X509.Is_Key_Usage_Critical (C)
      and then X509.KU_Key_Cert_Sign (C)
      and then not X509.CA_Missing_Subject_Key_ID (C)
      and then Usable (X509.Subject_Key_ID (C), DER)
      and then (not X509.Has_EKU (C) or else X509.Has_EKU_Code_Signing (C))
      and then X509.Issuer_EKU_Allows_Signing (C, X509.EKU_Code_Signing));

   function Leaf_Holds (C : X509.Certificate; DER : X509.Byte_Seq) return Boolean is
     (not X509.Is_CA (C)
      and then not X509.Has_Path_Len_Constraint (C)
      and then X509.Has_Key_Usage (C)
      and then X509.Is_Key_Usage_Critical (C)
      and then X509.KU_Digital_Signature (C)
      and then not X509.KU_Key_Cert_Sign (C)
      and then not X509.KU_CRL_Sign (C)
      and then not X509.KU_Key_Encipherment (C)
      and then X509.Has_EKU_Code_Signing (C)
      and then not X509.Has_EKU_Any_Purpose (C)
      --  One key, one purpose: a code-signing leaf carries no TLS or OCSP
      --  purpose beside codeSigning, so a key that signs programs is
      --  never also a TLS or OCSP-responder key (and the reverse).
      and then not X509.Has_EKU_Server_Auth (C)
      and then not X509.Has_EKU_Client_Auth (C)
      and then not X509.Has_EKU_OCSP_Signing (C)
      and then Usable (X509.Authority_Key_ID (C), DER));

   function Profile_Holds (DER : X509.Byte_Seq; As : Role) return Boolean is
      C  : X509.Certificate;
      OK : Boolean;
   begin
      X509.Parse (DER, C, OK);
      pragma Annotate
        (GNATprove, Intentional, "call to ""Parse"" might be nonterminating",
         "SPARKX509's Parse is a bounded recursive descent without the Always_Terminates aspect (assumed)");
      if not OK or else not Common_Holds (C) then
         return False;
      end if;
      case As is
         when Anchor_Role =>
            return CA_Holds (C, DER);
         when Intermediate_Role =>
            return CA_Holds (C, DER)
              and then X509.Has_Path_Len_Constraint (C)
              and then X509.Path_Len_Constraint (C) = 0;
         when Leaf_Role =>
            return Leaf_Holds (C, DER);
      end case;
   end Profile_Holds;

   function Issues_Intermediate (Issuer : X509.Byte_Seq) return Boolean is
      C  : X509.Certificate;
      OK : Boolean;
   begin
      X509.Parse (Issuer, C, OK);
      pragma Annotate
        (GNATprove, Intentional, "call to ""Parse"" might be nonterminating",
         "SPARKX509's Parse is a bounded recursive descent without the Always_Terminates aspect (assumed)");
      return OK
        and then (not X509.Has_Path_Len_Constraint (C) or else X509.Path_Len_Constraint (C) >= 1);
   end Issues_Intermediate;

   procedure Point_Of (C : X509.Certificate; Point : out Point_Bytes; OK : out Boolean) is
   begin
      Point := [others => 0];
      OK := False;
      if X509.PK_Algorithm (C) /= X509.Algo_EC_P384 or else X509.PK_Length (C) /= Point_Length then
         return;
      end if;
      declare
         Data : constant X509.Byte_Seq := X509.PK_Data (C);
      begin
         if Data (0) /= Uncompressed_Point then
            return;
         end if;
         for I in Point'Range loop
            Point (I) := Data (X509.N32 (I));
         end loop;
         OK := True;
      end;
   end Point_Of;

   procedure Public_Point (DER : X509.Byte_Seq; Point : out Point_Bytes; OK : out Boolean) is
      C : X509.Certificate;
      Parsed : Boolean;
   begin
      X509.Parse (DER, C, Parsed);
      pragma Annotate
        (GNATprove, Intentional, "call to ""Parse"" might be nonterminating",
         "SPARKX509's Parse is a bounded recursive descent without the Always_Terminates aspect (assumed)");
      if not Parsed then
         Point := [others => 0];
         OK := False;
         return;
      end if;
      Point_Of (C, Point, OK);
   end Public_Point;

   --  ECDSA verification of Hash under Point (04 || X || Y).
   function Verifies (Point : Point_Bytes; Hash : Digest; Signature : Signature_Bytes) return Boolean is
      Qx, Qy, R, S : SPARKNaCl.Byte_Seq (0 .. Coordinate_Length - 1);
   begin
      for I in Qx'Range loop
         Qx (I) := Point (1 + I);
         Qy (I) := Point (1 + Coordinate_Length + I);
         R (I) := Signature (I);
         S (I) := Signature (Coordinate_Length + I);
      end loop;
      return SPARKTLSCrypto.P384.ECDSA.Verify (Hash => Hash, Qx => Qx, Qy => Qy, R => R, S => S);
   end Verifies;

   --  One INTEGER of the ECDSA-Sig-Value at Encoded (At_Index), right-aligned
   --  into Value (Value'First .. Value'First + 47); Next is past it.
   procedure Decode_Integer
     (Encoded  : X509.Byte_Seq;
      At_Index : X509.N32;
      Value    : in out Signature_Bytes;
      Offset   : SPARKNaCl.N32;
      Next     : out X509.N32;
      OK       : out Boolean)
   with Pre => Encoded'First = 0 and then Encoded'Length <= X509.Max_Sig_Bytes
               and then (Offset = 0 or else Offset = Coordinate_Length),
        Post => (if OK then Next > At_Index and then Next <= Encoded'Last + 1)
   is
      Length : X509.N32;
      First  : X509.N32;
   begin
      Next := At_Index;
      OK := False;
      if Encoded'Length < 2 or else At_Index > Encoded'Last - 1
        or else Encoded (At_Index) /= Tag_Integer
        or else Encoded (At_Index + 1) >= Short_Form_Limit
      then
         return;
      end if;
      Length := X509.N32 (Encoded (At_Index + 1));
      First := At_Index + 2;
      if Length = 0 or else Length > Encoded'Last + 1 - First then
         return;
      end if;
      --  Minimal and positive: a leading zero only before a high bit.
      if Encoded (First) = 0 then
         if Length = 1 then
            Next := First + 1;   --  the value zero, which Verify refuses
            OK := True;
            return;
         elsif Encoded (First + 1) < Short_Form_Limit then
            return;
         end if;
         First := First + 1;
         Length := Length - 1;
      elsif Encoded (First) >= Short_Form_Limit then
         return;
      end if;
      if Length > Coordinate_Length then
         return;
      end if;
      for I in 0 .. Length - 1 loop
         Value (Offset + Coordinate_Length - SPARKNaCl.N32 (Length) + SPARKNaCl.N32 (I)) :=
           Encoded (First + I);
      end loop;
      Next := First + Length;
      OK := True;
   end Decode_Integer;

   procedure Decode_ECDSA_Signature
     (Encoded : X509.Byte_Seq; Signature : out Signature_Bytes; OK : out Boolean)
   is
      Next : X509.N32;
   begin
      Signature := [others => 0];
      OK := False;
      if Encoded'Length < 2 or else Encoded (0) /= Tag_Sequence
        or else Encoded (1) >= Short_Form_Limit
        or else X509.N32 (Encoded (1)) /= Encoded'Last - 1
      then
         return;
      end if;
      Decode_Integer (Encoded, 2, Signature, 0, Next, OK);
      if not OK then
         return;
      end if;
      Decode_Integer (Encoded, Next, Signature, Coordinate_Length, Next, OK);
      if OK and then Next /= Encoded'Last + 1 then
         Signature := [others => 0];
         OK := False;
      end if;
   end Decode_ECDSA_Signature;

   function Link_Holds (Child, Issuer : X509.Byte_Seq) return Boolean is
      C, I : X509.Certificate;
      C_OK, I_OK : Boolean;
      Point : Point_Bytes;
      Point_OK : Boolean;
      Signature : Signature_Bytes;
      Signature_OK : Boolean;
      To_Be_Signed : SPARKNaCl.Byte_Seq (0 .. Max_Certificate_Length - 1) := [others => 0];
      Hash : Digest;
   begin
      X509.Parse (Child, C, C_OK);
      pragma Annotate
        (GNATprove, Intentional, "call to ""Parse"" might be nonterminating",
         "SPARKX509's Parse is a bounded recursive descent without the Always_Terminates aspect (assumed)");
      X509.Parse (Issuer, I, I_OK);
      pragma Annotate
        (GNATprove, Intentional, "call to ""Parse"" might be nonterminating",
         "SPARKX509's Parse is a bounded recursive descent without the Always_Terminates aspect (assumed)");
      if not C_OK or else not I_OK then
         return False;
      end if;
      if not X509.Issuer_Matches (C, Child, I, Issuer)
        or else not Spans_Equal (Child, X509.Authority_Key_ID (C), Issuer, X509.Subject_Key_ID (I))
        or else not X509.DT_Before_Or_Equal (X509.Not_Before (I), X509.Not_Before (C))
        or else not X509.DT_Before_Or_Equal (X509.Not_After (C), X509.Not_After (I))
      then
         return False;
      end if;
      declare
         TBS : constant X509.Span := X509.TBS (C);
      begin
         if not Usable (TBS, Child) then
            return False;
         end if;
         for K in 0 .. TBS.Last - TBS.First loop
            To_Be_Signed (SPARKNaCl.N32 (K)) := Child (TBS.First + K);
         end loop;
         SPARKTLSCrypto.Hashing.SHA384.Hash
           (Hash, To_Be_Signed (0 .. SPARKNaCl.N32 (TBS.Last - TBS.First)));
      end;
      if X509.Sig_Length (C) = 0 or else X509.Sig_Length (C) > X509.Max_Sig_Bytes then
         return False;
      end if;
      Decode_ECDSA_Signature (X509.Sig_Data (C), Signature, Signature_OK);
      Point_Of (I, Point, Point_OK);
      return Signature_OK and then Point_OK and then Verifies (Point, Hash, Signature);
   end Link_Holds;

   function Signature_Holds
     (Leaf : X509.Byte_Seq; Message_Hash : Digest; Signature : Signature_Bytes) return Boolean
   is
      Point : Point_Bytes;
      OK : Boolean;
   begin
      Public_Point (Leaf, Point, OK);
      return OK and then Verifies (Point, Message_Hash, Signature);
   end Signature_Holds;

   function Certificate_Digest (DER : X509.Byte_Seq) return Digest is
      Copy : SPARKNaCl.Byte_Seq (0 .. SPARKNaCl.N32 (DER'Last)) := [others => 0];
   begin
      for I in DER'Range loop
         Copy (SPARKNaCl.N32 (I)) := DER (I);
      end loop;
      return SPARKTLSCrypto.Hashing.SHA384.Hash (Copy);
   end Certificate_Digest;

   function Key_Digest (DER : X509.Byte_Seq) return Digest is
      Point : Point_Bytes;
      OK : Boolean;
      SPKI : SPARKNaCl.Byte_Seq (0 .. SPKI_Header_Length + Point_Length - 1) := [others => 0];
   begin
      Public_Point (DER, Point, OK);
      if not OK then
         return [others => 0];
      end if;
      SPKI (0 .. SPKI_Header_Length - 1) := SPKI_Header;
      SPKI (SPKI_Header_Length .. SPKI'Last) := Point;
      return SPARKTLSCrypto.Hashing.SHA384.Hash (SPKI);
   end Key_Digest;

   function Subject_Name (DER : X509.Byte_Seq) return Signer_Name is
      C : X509.Certificate;
      OK : Boolean;
      Result : Signer_Name;
   begin
      X509.Parse (DER, C, OK);
      pragma Annotate
        (GNATprove, Intentional, "call to ""Parse"" might be nonterminating",
         "SPARKX509's Parse is a bounded recursive descent without the Always_Terminates aspect (assumed)");
      if not OK then
         return Result;
      end if;
      declare
         CN : constant X509.Span := X509.Subject_CN (C);
      begin
         if not Usable (CN, DER) then
            return Result;
         end if;
         for I in 0 .. X509.N32'Min (CN.Last - CN.First, Max_Signer_Name_Length - 1) loop
            Result.Data (Natural (I) + 1) := Character'Val (DER (CN.First + I));
         end loop;
         Result.Length := Natural (X509.N32'Min (CN.Last - CN.First, Max_Signer_Name_Length - 1)) + 1;
      end;
      return Result;
   end Subject_Name;

end CuBit.Signed_Admission.Certificates;
