------------------------------------------------------------------------------
--  CuBit OS
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  Hosted tests of signed admission (SIGN-001, docs/signed-binaries.md
--  section 10): the Accept, Wrong trust, Profile, Bytes, Shape,
--  Substitution and Determinism rows, plus revocation, scope and the
--  encoder's and signer's refusals. Every key is generated here, at test
--  time, from /dev/urandom; run.sh makes the certificates only openssl can
--  (other algorithms, foreign extensions) in a fresh directory, argument 1,
--  and argument 2 is an ELF used as the payload (a CuBit program when one
--  is built).
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Command_Line;
with Ada.Streams.Stream_IO;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Interfaces; use Interfaces;
with SPARKNaCl;
with SPARKTLSCrypto.Hashing.SHA384;
with SPARKTLSCrypto.P384.ECDSA;
with SPARKTLSCrypto.RFC6979;
with X509;
with CuBit.Signed_Admission; use CuBit.Signed_Admission;
with CuBit.Signed_Admission.Certificates;
with CuBit.Signed_Admission.Decisions;
with CuBit.Signed_Admission.Envelopes;
with CuBit.Signed_Admission.Revocations;
with CuBit.Certificate_Encoder;
with CuBit.Program_Signing;
with CuBit.Signing_Keys;

procedure Main is
   use type SPARKNaCl.Byte_Seq;
   use type X509.Byte_Seq;
   use type CuBit.Certificate_Encoder.Issue_Result;
   use type CuBit.Program_Signing.Signing_Result;
   package Keys renames CuBit.Signing_Keys;
   package Encoder renames CuBit.Certificate_Encoder;
   package Envelopes renames CuBit.Signed_Admission.Envelopes;
   package Decisions renames CuBit.Signed_Admission.Decisions;
   package Certificates renames CuBit.Signed_Admission.Certificates;
   subtype N32 is SPARKNaCl.N32;

   Failures : Natural := 0;
   Checks   : Natural := 0;

   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Put_Line ("FAIL: " & What);
      end if;
   end Check;

   type File_Access is access SPARKNaCl.Byte_Seq;
   procedure Free is new Ada.Unchecked_Deallocation (SPARKNaCl.Byte_Seq, File_Access);

   Fixtures : constant String := Ada.Command_Line.Argument (1);
   Payload_Path : constant String := Ada.Command_Line.Argument (2);

   ---------------------------------------------------------------------------
   --  Files and randomness
   ---------------------------------------------------------------------------

   function Read_File (Path : String) return File_Access is
      use Ada.Streams;
      F : Stream_IO.File_Type;
   begin
      Stream_IO.Open (F, Stream_IO.In_File, Path);
      declare
         Size : constant Stream_Element_Offset := Stream_Element_Offset (Stream_IO.Size (F));
         Raw  : Stream_Element_Array (1 .. Size);
         Last : Stream_Element_Offset;
         Data : constant File_Access := new SPARKNaCl.Byte_Seq (0 .. N32 (Size) - 1);
      begin
         Stream_IO.Read (F, Raw, Last);
         Stream_IO.Close (F);
         for I in Data'Range loop
            Data (I) := Unsigned_8 (Raw (Stream_Element_Offset (I) + 1));
         end loop;
         return Data;
      end;
   end Read_File;

   function Fixture (Name : String) return X509.Byte_Seq is
      Data : File_Access := Read_File (Fixtures & "/" & Name);
      Result : X509.Byte_Seq (0 .. X509.N32 (Data'Length) - 1);
   begin
      for I in Result'Range loop
         Result (I) := Data (N32 (I));
      end loop;
      Free (Data);
      return Result;
   end Fixture;

   Random_Source : Ada.Streams.Stream_IO.File_Type;
   procedure Random (Out_Bytes : out SPARKNaCl.Byte_Seq) is
      use Ada.Streams;
      Raw  : Stream_Element_Array (1 .. Stream_Element_Offset (Out_Bytes'Length));
      Last : Stream_Element_Offset;
   begin
      Stream_IO.Read (Random_Source, Raw, Last);
      for I in Out_Bytes'Range loop
         Out_Bytes (I) := Unsigned_8 (Raw (Stream_Element_Offset (I - Out_Bytes'First) + 1));
      end loop;
   end Random;

   function Mask return Keys.Blind is
      B : Keys.Blind;
   begin
      Random (B);
      return B;
   end Mask;

   ---------------------------------------------------------------------------
   --  Keys and certificates
   ---------------------------------------------------------------------------

   type Key_Pair is record
      D : Keys.Scalar;
      Q : Keys.Point;
   end record;

   function New_Key return Key_Pair is
      K : Key_Pair;
      OK : Boolean;
   begin
      loop
         Random (K.D);
         exit when Keys.Strong (K.D);
      end loop;
      Keys.Public_Of (K.D, Mask, K.Q);
      Keys.Check_Pair (K.D, K.Q, Mask, OK);
      if not OK then
         raise Program_Error with "pairwise consistency";
      end if;
      return K;
   end New_Key;

   function Fixture_Key (Name : String) return Keys.Scalar is
      Key_File : File_Access := Read_File (Fixtures & "/" & Name);
      D : Keys.Scalar := [others => 0];
      Q : Keys.Point;
      OK : Boolean;
   begin
      Keys.Decode_Key (Key_File.all, D, Q, OK);
      Free (Key_File);
      if not OK then
         Random (D);   --  not a P-384 key: the profile refuses before any signature
      end if;
      return D;
   end Fixture_Key;

   function Date (Year : Natural; Month : Natural := 1; Day : Natural := 1) return X509.Date_Time is
     ((Year => Year, Month => Month, Day => Day, Hour => 0, Minute => 0, Second => 0));
   Now : constant X509.Date_Time := (2026, 11, 20, 12, 0, 0);

   function Name (Text : String) return Encoder.Name_Text is
      N : Encoder.Name_Text;
   begin
      N.Length := Text'Length;
      N.Data (1 .. Text'Length) := Text;
      return N;
   end Name;

   No_Issuer : constant X509.Byte_Seq (0 .. 0) := [0];

   function Issue
     (Kind : Encoder.Certificate_Kind; Subject : String; Subject_Key : Key_Pair;
      Issuer : X509.Byte_Seq; Issuer_Key : Key_Pair;
      From : X509.Date_Time := Date (2026); Until_Date : X509.Date_Time := Date (2030);
      Path : Encoder.Path_Length := 0) return X509.Byte_Seq
   is
      R : Encoder.Request;
      Out_DER : Encoder.Encoded_Certificate;
      Length : X509.N32;
      Result : Encoder.Issue_Result;
   begin
      R.Kind := Kind;
      R.Subject := Name (Subject);
      Random (R.Serial);
      R.Not_Before := From;
      R.Not_After := Until_Date;
      R.Subject_Key := Subject_Key.Q;
      R.Path := Path;
      Encoder.Issue (R, Issuer, Issuer_Key.D, Now, Mask, Out_DER, Length, Result);
      if Result /= Encoder.Issued then
         raise Program_Error with "issue " & Subject & ": " & Result'Image;
      end if;
      return Out_DER (0 .. Length - 1);
   end Issue;

   ---------------------------------------------------------------------------
   --  Signing
   ---------------------------------------------------------------------------

   function Concat (Payload : SPARKNaCl.Byte_Seq; T : Envelopes.Trailer; Length : N32) return File_Access is
      Result : constant File_Access := new SPARKNaCl.Byte_Seq (0 .. Payload'Length + Length - 1);
   begin
      Result (0 .. Payload'Length - 1) := Payload;
      Result (Payload'Length .. Result'Last) := T (0 .. Length - 1);
      return Result;
   end Concat;

   function Sign
     (Payload : SPARKNaCl.Byte_Seq; Leaf : X509.Byte_Seq; Key : Key_Pair;
      Intermediate : X509.Byte_Seq := No_Issuer; Kind : Purpose := Program) return File_Access
   is
      T : Envelopes.Trailer;
      Length : N32;
      Result : CuBit.Program_Signing.Signing_Result;
      Count : constant Certificate_Count := (if Intermediate'Length > 1 then 2 else 1);
   begin
      CuBit.Program_Signing.Sign
        (Payload, Kind, Leaf, Count, (if Count = 2 then Intermediate else Leaf), Key.D, Now, Mask,
         T, Length, Result);
      if Result /= CuBit.Program_Signing.Signed then
         raise Program_Error with "sign: " & Result'Image;
      end if;
      return Concat (Payload, T, Length);
   end Sign;

   --  An envelope made without the signer's checks: what an attacker, or a
   --  careless tool, could write. Key signs the statement for Leaf.
   function Forge
     (Payload : SPARKNaCl.Byte_Seq; Leaf : X509.Byte_Seq; Key : Keys.Scalar;
      Intermediate : X509.Byte_Seq := No_Issuer; Kind : Purpose := Program) return File_Access
   is
      Payload_Digest : constant Digest := SPARKTLSCrypto.Hashing.SHA384.Hash (Payload);
      Hash : constant Digest := SPARKTLSCrypto.Hashing.SHA384.Hash
        (Envelopes.Statement_Of (Kind, Payload'Length, Payload_Digest, Certificates.Certificate_Digest (Leaf)));
      K : SPARKNaCl.Bytes_48;
      R, S : SPARKNaCl.Byte_Seq (0 .. 47) := [others => 0];
      OK : Boolean;
      Signature : Signature_Bytes;
      T : Envelopes.Trailer;
      Length : N32;
      Count : constant Certificate_Count := (if Intermediate'Length > 1 then 2 else 1);
   begin
      SPARKTLSCrypto.RFC6979.Derive_K_P384 (Key, Hash, K, OK);
      SPARKTLSCrypto.P384.ECDSA.Sign (Hash, Key, K, Mask, R, S, OK);
      Signature (0 .. 47) := R;
      Signature (48 .. 95) := S;
      Envelopes.Encode
        (Kind, Payload'Length, Payload_Digest, Count, Leaf,
         (if Count = 2 then Intermediate else Leaf), Signature, T, Length);
      return Concat (Payload, T, Length);
   end Forge;

   ---------------------------------------------------------------------------
   --  The trust list
   ---------------------------------------------------------------------------

   function Anchor_Of
     (DER : X509.Byte_Seq; Ceiling : Authority_Ceiling := Hardware_And_System;
      First_Prefix : Positive := 1; Last_Prefix : Natural := 1) return Anchor
   is
      A : Anchor;
   begin
      A.Kind := Host_Development;
      A.Ceiling := Ceiling;
      A.DER (0 .. DER'Length - 1) := DER;
      A.Length := DER'Length;
      A.First_Prefix := First_Prefix;
      A.Last_Prefix := Last_Prefix;
      return A;
   end Anchor_Of;

   function Identity (Text : String) return Identity_Text is
      T : Identity_Text;
   begin
      T.Length := Text'Length;
      T.Data (1 .. Text'Length) := Text;
      return T;
   end Identity;

   Everything : constant Prefix_Array := [1 => Identity ("*")];
   No_Revocations : constant Revocation_Set :=
     (Keys => 0, Certificates => 0, Programs => 0, others => <>);

   function Verify (File : SPARKNaCl.Byte_Seq; Anchors : Anchor_Array;
                    Revoked_Set : Revocation_Set := No_Revocations; Kind : Purpose := Program)
                    return Verification
   is
      Copy : constant SPARKNaCl.Byte_Seq (0 .. File'Length - 1) := File;
   begin
      return Decisions.Verify (Copy, Kind, Anchors, Revoked_Set);
   end Verify;

   procedure Expect_Verified (File : SPARKNaCl.Byte_Seq; Anchors : Anchor_Array; What : String;
                              Anchor_Expected : Positive := 1) is
      V : constant Verification := Verify (File, Anchors);
   begin
      Check (V.Verified, What & ": verifies"
             & (if V.Verified then "" else " (refused " & V.Reason'Image & "/" & V.Detail'Image & ")"));
      if V.Verified then
         Check (V.Signer.Anchor = Anchor_Expected, What & ": anchor");
      end if;
   end Expect_Verified;

   procedure Expect_Refused (File : SPARKNaCl.Byte_Seq; Anchors : Anchor_Array;
                             Reason : Refusal_Reason; Detail : Refusal_Detail; What : String;
                             Revoked_Set : Revocation_Set := No_Revocations; Kind : Purpose := Program) is
      V : constant Verification := Verify (File, Anchors, Revoked_Set, Kind);
   begin
      if V.Verified then
         Check (False, What & ": refused (it verified)");
      else
         Check (V.Reason = Reason and then V.Detail = Detail,
                What & ": refused as " & Reason'Image & "/" & Detail'Image
                & " (got " & V.Reason'Image & "/" & V.Detail'Image & ")");
      end if;
   end Expect_Refused;

   procedure Expect_Any_Refusal (File : SPARKNaCl.Byte_Seq; Anchors : Anchor_Array; What : String) is
      V : constant Verification := Verify (File, Anchors);
   begin
      Check (not V.Verified, What & ": refused");
   end Expect_Any_Refusal;

   ---------------------------------------------------------------------------
   --  The keys and certificates every row shares
   ---------------------------------------------------------------------------

   Root_Key  : Key_Pair;
   Inter_Key : Key_Pair;
   Leaf_Key  : Key_Pair;
   Leaf2_Key : Key_Pair;
   Rogue_Key : Key_Pair;
   Payload   : File_Access;
   Small_Payload : constant SPARKNaCl.Byte_Seq (0 .. 4095) := [for I in 0 .. 4095 => Unsigned_8 (I mod 251)];

   procedure Rows is
      Root : constant X509.Byte_Seq :=
        Issue (Encoder.Root_CA, "Test builder root", Root_Key, No_Issuer, Root_Key,
               Date (2026), Date (2036), Path => 1);
      Inter : constant X509.Byte_Seq :=
        Issue (Encoder.Intermediate_CA, "Test builder CA", Inter_Key, Root, Root_Key, Date (2026), Date (2034));
      Leaf : constant X509.Byte_Seq :=
        Issue (Encoder.Code_Signing_Leaf, "Test builder", Leaf_Key, Root, Root_Key);
      Leaf_Via_Inter : constant X509.Byte_Seq :=
        Issue (Encoder.Code_Signing_Leaf, "Test builder via CA", Leaf2_Key, Inter, Inter_Key);
      Rogue_Root : constant X509.Byte_Seq :=
        Issue (Encoder.Root_CA, "Rogue builder root", Rogue_Key, No_Issuer, Rogue_Key, Date (2026), Date (2036));
      Rogue_Leaf : constant X509.Byte_Seq :=
        Issue (Encoder.Code_Signing_Leaf, "Test builder", Leaf_Key, Rogue_Root, Rogue_Key);
      Trusted : constant Anchor_Array := [1 => Anchor_Of (Root)];

      ------------------------------------------------------------------------
      procedure Statement_Row is
         D1 : constant Digest := [others => 1];
         D2 : constant Digest := [others => 2];
         S : constant Statement := Envelopes.Statement_Of (Program, 16#0102_0304#, D1, D2);
         Prefix : String (1 .. Statement_Prefix_Length);
      begin
         for I in Prefix'Range loop
            Prefix (I) := Character'Val (S (N32 (I - 1)));
         end loop;
         Check (S'Length = 136, "statement: 136 bytes");
         Check (Prefix = Statement_Prefix_Text, "statement: prefix");
         Check (S (24 .. 31) = [1, 0, 1, 0, 1, 0, 0, 0], "statement: format, suite, purpose, reserved");
         Check (S (32 .. 39) = [4, 3, 2, 1, 0, 0, 0, 0], "statement: payload length little-endian");
         Check (S (40 .. 87) = D1 and then S (88 .. 135) = D2, "statement: digests");
         Check (Envelopes.Statement_Of (Shared_Image, 16#0102_0304#, D1, D2) /= S, "statement: purpose separates");
         Check (Envelopes.Statement_Of (Program, 16#0102_0304#, D2, D1) /= S, "statement: digest order matters");
      end Statement_Row;

      ------------------------------------------------------------------------
      procedure Keys_Row is
         Weak : Keys.Scalar := [others => 0];
         Q : Keys.Point;
         OK : Boolean;
         Encoded : Keys.Key_File;
         D2 : Keys.Scalar;
         Q2 : Keys.Point;
         Near_N : Keys.Scalar := Keys.Order;
         Openssl_Key : File_Access := Read_File (Fixtures & "/control-leaf.key");
      begin
         Check (not Keys.Strong (Weak), "keys: zero is refused");
         Weak (47) := 4;
         Check (not Keys.Strong (Weak), "keys: d = 4 is refused");
         Weak (45) := 1;
         Weak (46) := 0;
         Weak (47) := 0;
         Check (not Keys.Strong (Weak), "keys: d = 2^16 is refused");
         Weak (47) := 1;
         Check (Keys.Strong (Weak), "keys: d = 2^16 + 1 is the first strong key");
         Check (not Keys.Strong (Keys.Order), "keys: d = n is refused");
         Check (not Keys.Strong ([others => 16#FF#]), "keys: d > n is refused");
         Near_N (47) := Near_N (47) - 4;   --  n - 4: the negation of d = 4
         Check (not Keys.Strong (Near_N), "keys: d = n - 4 is refused");
         Check (Keys.Strong (Keys.Highest_Strong), "keys: n - 2^16 - 1 is the last strong key");
         Keys.Encode_Key (Leaf_Key.D, Leaf_Key.Q, Encoded);
         Keys.Decode_Key (Encoded, D2, Q2, OK);
         Check (OK and then D2 = Leaf_Key.D and then Q2 = Leaf_Key.Q, "keys: key file round trip");
         Check (Openssl_Key'Length = Keys.Key_File_Length, "keys: openssl's SEC 1 DER for P-384 is the template");
         Keys.Decode_Key (Openssl_Key.all, D2, Q2, OK);
         Check (OK, "keys: openssl's key file decodes");
         Keys.Check_Pair (D2, Q2, Mask, OK);
         Check (OK, "keys: openssl's key passes the pairwise test");
         Free (Openssl_Key);
         Weak := [others => 0];
         Weak (47) := 4;
         Keys.Public_Of (Weak, Mask, Q);
         Keys.Encode_Key (Weak, Q, Encoded);
         Keys.Decode_Key (Encoded, D2, Q2, OK);
         Check (not OK, "keys: a key file holding d = 4 is refused");
         Keys.Check_Pair (Weak, Q, Mask, OK);
         Check (not OK, "keys: d = 4 fails the pair check");
         Keys.Check_Pair (Leaf_Key.D, Inter_Key.Q, Mask, OK);
         Check (not OK, "keys: a scalar and another key's point are refused");
         Keys.Encode_Key (Leaf_Key.D, Leaf_Key.Q, Encoded);
         Encoded (0) := 16#31#;
         Keys.Decode_Key (Encoded, D2, Q2, OK);
         Check (not OK, "keys: another encoding is refused");
      end Keys_Row;

      ------------------------------------------------------------------------
      procedure Encoder_Row is
         R : Encoder.Request;
         Out_DER : Encoder.Encoded_Certificate;
         Length : X509.N32;
         Result : Encoder.Issue_Result;
         Weak : Keys.Scalar := [others => 0];

         procedure Try (What : String; Expected : Encoder.Issue_Result;
                        Issuer : X509.Byte_Seq; Issuer_Key : Keys.Scalar) is
         begin
            Encoder.Issue (R, Issuer, Issuer_Key, Now, Mask, Out_DER, Length, Result);
            Check (Result = Expected and then (Expected = Encoder.Issued or else Length = 0),
                   "encoder: " & What & " (" & Result'Image & ")");
         end Try;

         procedure Reset (Kind : Encoder.Certificate_Kind) is
         begin
            R := (Kind => Kind, Subject => Name ("Encoder test"), Serial => [others => 7],
                  Not_Before => Date (2026), Not_After => Date (2028), Subject_Key => Leaf_Key.Q, Path => 0);
         end Reset;
         C : X509.Certificate;
         OK : Boolean;
      begin
         --  Every kind round-trips through SPARKX509 into the profile.
         Check (Certificates.Profile_Holds (Root, Certificates.Anchor_Role), "encoder: root CA is in the profile");
         Check (Certificates.Profile_Holds (Inter, Certificates.Intermediate_Role),
                "encoder: intermediate CA is in the profile");
         Check (Certificates.Profile_Holds (Leaf, Certificates.Leaf_Role), "encoder: leaf is in the profile");
         Check (Certificates.Link_Holds (Leaf, Root) and then Certificates.Link_Holds (Leaf_Via_Inter, Inter)
                and then Certificates.Link_Holds (Inter, Root), "encoder: links verify");
         Check (Certificates.Issues_Intermediate (Root), "encoder: root with pathLen 1 issues an intermediate");
         X509.Parse (Leaf, C, OK);
         Check (OK and then X509.Has_EKU_Code_Signing (C) and then not X509.Is_CA (C),
                "encoder: a leaf always carries codeSigning");
         X509.Parse (Root, C, OK);
         Check (OK and then X509.Is_CA (C) and then X509.Is_Basic_Constraints_Critical (C),
                "encoder: a CA always carries a critical basicConstraints");

         Reset (Encoder.Code_Signing_Leaf);
         Try ("a leaf under the root", Encoder.Issued, Root, Root_Key.D);
         Weak (47) := 4;
         Try ("a weak issuer key", Encoder.Weak_Issuer_Key, Root, Weak);
         Try ("an issuer key that is not the issuer certificate's", Encoder.Key_Mismatch, Root, Inter_Key.D);
         Try ("an issuer that is a leaf", Encoder.Issuer_Not_CA, Leaf, Leaf_Key.D);
         Try ("an issuer without basicConstraints", Encoder.Issuer_Not_CA,
              Fixture ("no-bc-ca.der"), Fixture_Key ("no-bc-ca.key"));
         R.Not_Before := Date (2027);
         Try ("not yet valid", Encoder.Not_Yet_Valid, Root, Root_Key.D);
         R.Not_Before := Date (2020);
         R.Not_After := Date (2025);
         Try ("expired", Encoder.Expired, Root, Root_Key.D);
         R.Not_After := Date (2020);
         Try ("an empty window", Encoder.Window_Invalid, Root, Root_Key.D);
         Reset (Encoder.Code_Signing_Leaf);
         R.Not_Before := (2026, 2, 30, 0, 0, 0);
         Try ("an impossible date", Encoder.Window_Invalid, Root, Root_Key.D);
         Reset (Encoder.Code_Signing_Leaf);
         R.Not_After := Date (2040);
         Try ("a window outside the issuer's", Encoder.Window_Outside_Issuer, Root, Root_Key.D);
         Reset (Encoder.Code_Signing_Leaf);
         R.Subject := Name ("bad/name");
         Try ("a name outside the alphabet", Encoder.Name_Invalid, Root, Root_Key.D);
         Reset (Encoder.Root_CA);
         R.Subject_Key := Leaf_Key.Q;
         Try ("a root whose key is not its issuer key", Encoder.Key_Mismatch, No_Issuer, Root_Key.D);
         Reset (Encoder.Intermediate_CA);
         R.Subject_Key := Leaf2_Key.Q;
         Try ("an intermediate under the root", Encoder.Issued, Root, Root_Key.D);
         Try ("an intermediate under an intermediate (pathLen 0)", Encoder.Issuer_Not_CA, Inter, Inter_Key.D);
         Reset (Encoder.Root_CA);
         R.Subject_Key := Root_Key.Q;
         R.Not_After := (2051, 6, 30, 23, 59, 59);
         Try ("a root past 2049 (GeneralizedTime)", Encoder.Issued, No_Issuer, Root_Key.D);
      end Encoder_Row;

      ------------------------------------------------------------------------
      procedure Signer_Row is
         T : Envelopes.Trailer;
         Length : N32;
         Result : CuBit.Program_Signing.Signing_Result;
         Weak : Keys.Scalar := [others => 0];
         Expired_Leaf : constant X509.Byte_Seq :=
           Issue (Encoder.Code_Signing_Leaf, "Expired builder", Leaf_Key, Root, Root_Key,
                  (2026, 1, 1, 0, 0, 0), (2026, 11, 20, 12, 30, 0));

         procedure Try (What : String; Expected : CuBit.Program_Signing.Signing_Result;
                        Leaf_DER : X509.Byte_Seq; Key : Keys.Scalar; At_Time : X509.Date_Time := Now;
                        Intermediate : X509.Byte_Seq := No_Issuer) is
            Count : constant Certificate_Count := (if Intermediate'Length > 1 then 2 else 1);
         begin
            CuBit.Program_Signing.Sign
              (Small_Payload, Program, Leaf_DER, Count, (if Count = 2 then Intermediate else Leaf_DER), Key,
               At_Time, Mask, T, Length, Result);
            Check (Result = Expected and then (Expected = CuBit.Program_Signing.Signed or else Length = 0),
                   "signer: " & What & " (" & Result'Image & ")");
         end Try;
         Old : File_Access;
      begin
         Try ("a good leaf", CuBit.Program_Signing.Signed, Leaf, Leaf_Key.D);
         Weak (47) := 4;
         Try ("a weak key", CuBit.Program_Signing.Weak_Key, Leaf, Weak);
         Try ("another key than the leaf's", CuBit.Program_Signing.Key_Mismatch, Leaf, Inter_Key.D);
         Try ("a CA as the leaf", CuBit.Program_Signing.Leaf_Refused, Root, Root_Key.D);
         Try ("a leaf without codeSigning", CuBit.Program_Signing.Leaf_Refused,
              Fixture ("no-eku-leaf.der"), Fixture_Key ("no-eku-leaf.key"));
         Try ("a leaf with serverAuth only", CuBit.Program_Signing.Leaf_Refused,
              Fixture ("server-auth-leaf.der"), Fixture_Key ("server-auth-leaf.key"));
         Try ("an expired leaf", CuBit.Program_Signing.Leaf_Expired, Expired_Leaf, Leaf_Key.D,
              (2026, 11, 21, 0, 0, 0));
         Try ("a leaf not yet valid", CuBit.Program_Signing.Leaf_Not_Yet_Valid, Leaf, Leaf_Key.D,
              (2025, 6, 1, 0, 0, 0));
         Try ("an intermediate that did not issue the leaf", CuBit.Program_Signing.Intermediate_Refused,
              Leaf, Leaf_Key.D, Intermediate => Inter);
         Try ("a root as the intermediate", CuBit.Program_Signing.Intermediate_Refused,
              Leaf, Leaf_Key.D, Intermediate => Root);
         Try ("a leaf with an intermediate", CuBit.Program_Signing.Signed,
              Leaf_Via_Inter, Leaf2_Key.D, Intermediate => Inter);
         --  An expired leaf's old signatures stay valid: no clock on admission.
         CuBit.Program_Signing.Sign
           (Small_Payload, Program, Expired_Leaf, 1, Expired_Leaf, Leaf_Key.D, Now, Mask, T, Length, Result);
         Old := Concat (Small_Payload, T, Length);
         Expect_Verified (Old.all, Trusted, "time: a signature by a leaf that has since expired");
         Free (Old);
      end Signer_Row;

      ------------------------------------------------------------------------
      procedure Accept_Row is
         Signed_Direct : File_Access := Sign (Payload.all, Leaf, Leaf_Key);
         Signed_Inter  : File_Access := Sign (Payload.all, Leaf_Via_Inter, Leaf2_Key, Inter);
         Two : constant Anchor_Array := [1 => Anchor_Of (Rogue_Root), 2 => Anchor_Of (Root)];
         V : constant Verification := Verify (Signed_Direct.all, Trusted);
      begin
         Expect_Verified (Signed_Direct.all, Trusted, "accept: leaf under the anchor");
         Expect_Verified (Signed_Inter.all, Trusted, "accept: leaf under one intermediate");
         Expect_Verified (Signed_Direct.all, Two, "accept: second anchor of two", Anchor_Expected => 2);
         if V.Verified then
            Check (V.Signer.Payload_Length = Payload'Length, "accept: payload length");
            Check (V.Signer.Payload_Digest = SPARKTLSCrypto.Hashing.SHA384.Hash (Payload.all),
                   "accept: payload digest");
            Check (V.Signer.Leaf_Digest = Certificates.Certificate_Digest (Leaf), "accept: signer digest");
            Check (V.Signer.Anchor_Digest = Certificates.Certificate_Digest (Root), "accept: anchor digest");
            Check (V.Signer.Name.Data (1 .. V.Signer.Name.Length) = "Test builder", "accept: signer name");
            --  The payload ends where the envelope starts: a loader given
            --  Payload_Length bytes gets exactly the ELF.
            Check (Signed_Direct (0 .. Payload'Length - 1) = Payload.all, "accept: payload prefix unchanged");
         end if;
         Check (Decisions.Admit (V, Trusted, Everything, Identity ("com.cubit.sleep"),
                                 Hardware_Or_System_Authority).Admitted,
                "accept: admitted under every identity and the hardware ceiling");
         Free (Signed_Direct);
         Free (Signed_Inter);
      end Accept_Row;

      ------------------------------------------------------------------------
      procedure Wrong_Trust_Row is
         Signed_Self : File_Access :=
           Forge (Small_Payload, Fixture ("self-signed-leaf.der"), Fixture_Key ("self-signed-leaf.key"));
         Signed_Rogue : File_Access := Sign (Small_Payload, Rogue_Leaf, Leaf_Key);
         Signed : File_Access := Sign (Small_Payload, Leaf, Leaf_Key);
         Scoped : constant Prefix_Array := [Identity ("com.example"), Identity ("org.other.app")];
         Apps : constant Anchor_Array := [1 => Anchor_Of (Root, Applications_Only, 1, 2)];
         Services_Only : constant Anchor_Array := [1 => Anchor_Of (Root, Services, 1, 2)];
         V : constant Verification := Verify (Signed.all, Apps);
         Empty : constant Anchor_Array (1 .. 0) := [others => <>];
         type Identity_List is array (Positive range <>) of Identity_Text;
         Outside : constant Identity_List :=
           [Identity ("com.examplefoo"), Identity ("com.exampl"), Identity ("com"),
            Identity ("org.other"), Identity ("com.cubit.secrets"), Identity ("")];
         function Admit (Id : String; Requested : Requested_Authority := Application_Authority;
                         Anchors : Anchor_Array := Apps) return Admission is
           (Decisions.Admit (V, Anchors, Scoped, Identity (Id), Requested));
      begin
         Expect_Refused (Signed_Self.all, Trusted, Untrusted_Builder, No_Anchor, "wrong trust: self-signed leaf");
         Expect_Refused (Signed_Self.all, [1 => Anchor_Of (Fixture ("self-signed-leaf.der"))], Untrusted_Builder,
                         No_Anchor, "wrong trust: self-signed leaf installed as its own anchor");
         Expect_Refused (Signed_Rogue.all, Trusted, Untrusted_Builder, No_Anchor,
                         "wrong trust: leaf under an unknown CA (same key and name)");
         Expect_Refused (Signed_Rogue.all, Empty, Untrusted_Builder, No_Anchor, "wrong trust: empty trust list");

         Check (V.Verified, "scope: verifies before the manifest is read");
         Check (Admit ("com.example").Admitted, "scope: the prefix itself");
         Check (Admit ("com.example.app").Admitted, "scope: below the prefix");
         Check (Admit ("org.other.app.sub").Admitted, "scope: second prefix");
         for Bad of Outside loop
            declare
               A : constant Admission := Decisions.Admit (V, Apps, Scoped, Bad, Application_Authority);
            begin
               Check (not A.Admitted and then A.Reason = Outside_Builder_Scope
                      and then A.Detail = Identity_Outside_Scope,
                      "wrong trust: identity outside the scope: '" & Bad.Data (1 .. Bad.Length) & "'");
            end;
         end loop;
         Check (not Decisions.Admit (V, [1 => Anchor_Of (Root, Hardware_And_System, 1, 0)], Scoped,
                                     Identity ("com.example"), Application_Authority).Admitted,
                "wrong trust: an anchor with no identities admits nothing");
         declare
            A : constant Admission := Admit ("com.example.app", Hardware_Or_System_Authority);
         begin
            Check (not A.Admitted and then A.Reason = Outside_Builder_Scope and then A.Detail = Authority_Above_Ceiling,
                   "wrong trust: hardware program under an Applications_Only anchor");
         end;
         Check (not Admit ("com.example.app", Service_Authority).Admitted,
                "wrong trust: a service under an Applications_Only anchor");
         Check (Admit ("com.example.app", Service_Authority, Services_Only).Admitted,
                "scope: a service under a Services anchor");
         Check (not Admit ("com.example.app", Hardware_Or_System_Authority, Services_Only).Admitted,
                "wrong trust: hardware program under a Services anchor");
         Check (not Decisions.Admit (Verify (Signed_Rogue.all, Apps), Apps, Scoped, Identity ("com.example"),
                                     Application_Authority).Admitted,
                "wrong trust: a refused verification is never admitted");
         Free (Signed_Self);
         Free (Signed_Rogue);
         Free (Signed);
      end Wrong_Trust_Row;

      ------------------------------------------------------------------------
      procedure Profile_Row is
         --  An envelope with a leaf (and an intermediate) from openssl, the
         --  statement signed with the fixture key.
         procedure Openssl_Case (Leaf_Name : String; Anchor_Name : String;
                                 Detail : Refusal_Detail; What : String;
                                 Intermediate_Name : String := "") is
            Anchors : constant Anchor_Array := [1 => Anchor_Of (Fixture (Anchor_Name & ".der"))];
            D : constant Keys.Scalar := Fixture_Key (Leaf_Name & ".key");
            Signed : File_Access :=
              (if Intermediate_Name = "" then Forge (Small_Payload, Fixture (Leaf_Name & ".der"), D)
               else Forge (Small_Payload, Fixture (Leaf_Name & ".der"), D, Fixture (Intermediate_Name & ".der")));
         begin
            Expect_Refused (Signed.all, Anchors, Untrusted_Builder, Detail, "profile: " & What);
            Free (Signed);
         end Openssl_Case;
         Control : File_Access :=
           Forge (Small_Payload, Fixture ("control-leaf.der"), Fixture_Key ("control-leaf.key"));
         Control_Inter : File_Access :=
           Forge (Small_Payload, Fixture ("under-intermediate-leaf.der"), Fixture_Key ("under-intermediate-leaf.key"),
                  Fixture ("intermediate.der"));
         Edited : X509.Byte_Seq := Leaf;
         Signed_Edited : File_Access;
      begin
         --  The fixtures are sound where they should be: openssl's control
         --  leaf, and its leaf under an intermediate, are admitted.
         Expect_Verified (Control.all, [1 => Anchor_Of (Fixture ("ca.der"))], "profile: openssl control leaf");
         Expect_Verified (Control_Inter.all, [1 => Anchor_Of (Fixture ("ca.der"))],
                          "profile: openssl leaf under an intermediate");
         Free (Control);
         Free (Control_Inter);
         Openssl_Case ("no-eku-leaf", "ca", Leaf_Profile, "leaf without an EKU");
         Openssl_Case ("server-auth-leaf", "ca", Leaf_Profile, "leaf with serverAuth only");
         Openssl_Case ("any-eku-leaf", "ca", Leaf_Profile, "leaf with codeSigning and anyExtendedKeyUsage");
         Openssl_Case ("code-server-leaf", "ca", Leaf_Profile, "dual-use leaf: codeSigning and serverAuth");
         Openssl_Case ("code-client-leaf", "ca", Leaf_Profile, "dual-use leaf: codeSigning and clientAuth");
         Openssl_Case ("code-ocsp-leaf", "ca", Leaf_Profile, "dual-use leaf: codeSigning and OCSPSigning");
         Openssl_Case ("ca-true-leaf", "ca", Leaf_Profile, "leaf with CA TRUE");
         Openssl_Case ("no-digital-signature-leaf", "ca", Leaf_Profile, "keyUsage without digitalSignature");
         Openssl_Case ("cert-sign-leaf", "ca", Leaf_Profile, "leaf keyUsage with keyCertSign too");
         Openssl_Case ("noncritical-ku-leaf", "ca", Leaf_Profile, "keyUsage not critical");
         Openssl_Case ("p256-leaf", "ca", Leaf_Profile, "P-256 leaf");
         Openssl_Case ("rsa-leaf", "ca", Leaf_Profile, "RSA leaf");
         Openssl_Case ("ed25519-leaf", "ca", Leaf_Profile, "Ed25519 leaf");
         Openssl_Case ("sha512-leaf", "ca", Leaf_Profile, "ECDSA with SHA-512 signature");
         Openssl_Case ("unknown-critical-leaf", "ca", Leaf_Profile, "unknown critical extension");
         Openssl_Case ("p256-ca-leaf", "p256-ca", No_Anchor, "P-256 anchor");
         Openssl_Case ("control-leaf", "rsa-ca", No_Anchor, "RSA anchor");
         Openssl_Case ("control-leaf", "ed25519-ca", No_Anchor, "Ed25519 anchor");
         Openssl_Case ("under-p256-leaf", "ca", Intermediate_Profile, "P-256 intermediate",
                       Intermediate_Name => "p256-intermediate");
         Openssl_Case ("no-bc-ca-leaf", "no-bc-ca", No_Anchor, "anchor without basicConstraints");
         Openssl_Case ("noncritical-bc-ca-leaf", "noncritical-bc-ca", No_Anchor, "anchor with basicConstraints not critical");
         Openssl_Case ("no-certsign-ca-leaf", "no-certsign-ca", No_Anchor, "anchor without keyCertSign");
         Openssl_Case ("server-eku-ca-leaf", "server-eku-ca", No_Anchor, "anchor whose EKU lacks codeSigning");
         Openssl_Case ("control-leaf", "other-ski-ca", No_Anchor, "AKI/SKI mismatch (same key and name)");
         Openssl_Case ("long-leaf", "ca", No_Anchor, "validity not nested in the issuer's");
         Openssl_Case ("under-pathlen0-leaf", "pathlen0-root", No_Anchor, "pathLen 0 anchor above an intermediate",
                       Intermediate_Name => "under-pathlen0-intermediate");
         Openssl_Case ("under-pathlen1-leaf", "ca", Intermediate_Profile, "intermediate with pathLen 1",
                       Intermediate_Name => "pathlen1-intermediate");
         Openssl_Case ("under-second-leaf", "ca", No_Anchor, "two intermediates (the envelope carries one)",
                       Intermediate_Name => "second-intermediate");
         --  A leaf the root issued, carried with an intermediate that did not
         --  issue it: every certificate is sound, the leaf-to-intermediate
         --  link is not.
         declare
            Unlinked : File_Access := Forge (Small_Payload, Leaf, Leaf_Key.D, Inter);
         begin
            Expect_Refused (Unlinked.all, Trusted, Untrusted_Builder, No_Anchor,
                            "profile: a leaf with an intermediate that did not issue it");
            Free (Unlinked);
         end;
         --  A bit flipped inside this encoder's leaf: its link signature fails.
         Edited (Edited'Last - 120) := Edited (Edited'Last - 120) xor 1;
         Signed_Edited := Forge (Small_Payload, Edited, Leaf_Key.D);
         Expect_Any_Refusal (Signed_Edited.all, Trusted, "profile: a flipped bit in the leaf");
         Free (Signed_Edited);
      end Profile_Row;

      ------------------------------------------------------------------------
      procedure Revocation_Row is
         Signed : File_Access := Sign (Small_Payload, Leaf_Via_Inter, Leaf2_Key, Inter);
         Filler : constant Digest := [others => 16#80#];
         Low : constant Digest := [others => 0];
         High : constant Digest := [others => 16#FF#];
         None : constant Digest_Array (1 .. 0) := [others => [others => 0]];

         function Set_Of (Keys_List, Certificate_List, Program_List : Digest_Array) return Revocation_Set is
           ((Keys => Keys_List'Length, Certificates => Certificate_List'Length, Programs => Program_List'Length,
             Revoked_Keys => Keys_List, Revoked_Certificates => Certificate_List,
             Revoked_Programs => Program_List));

         --  A sorted list of three with Item in the middle.
         function Around (Item : Digest) return Digest_Array is ([Low, Item, High]);
         Payload_Digest : constant Digest := SPARKTLSCrypto.Hashing.SHA384.Hash (Small_Payload);
         Unrelated : constant Revocation_Set := Set_Of ([Low, Filler, High], [Low, High], [1 => Filler]);
         SPKI : File_Access := Read_File (Fixtures & "/control-leaf.spki");
      begin
         Check (Verify (Signed.all, Trusted, Unrelated).Verified, "revocation: unrelated entries admit");
         Expect_Refused (Signed.all, Trusted, Revoked, Program_Revoked, "revocation: payload",
                         Set_Of (None, None, Around (Payload_Digest)));
         Expect_Refused (Signed.all, Trusted, Revoked, Key_Revoked, "revocation: leaf key",
                         Set_Of (Around (Certificates.Key_Digest (Leaf_Via_Inter)), None, None));
         Expect_Refused (Signed.all, Trusted, Revoked, Certificate_Revoked, "revocation: leaf certificate",
                         Set_Of (None, Around (Certificates.Certificate_Digest (Leaf_Via_Inter)), None));
         Expect_Refused (Signed.all, Trusted, Revoked, Key_Revoked, "revocation: intermediate key",
                         Set_Of (Around (Certificates.Key_Digest (Inter)), None, None));
         Expect_Refused (Signed.all, Trusted, Revoked, Certificate_Revoked, "revocation: intermediate certificate",
                         Set_Of (None, Around (Certificates.Certificate_Digest (Inter)), None));
         Expect_Refused (Signed.all, Trusted, Revoked, Key_Revoked, "revocation: anchor key",
                         Set_Of (Around (Certificates.Key_Digest (Root)), None, None));
         Expect_Refused (Signed.all, Trusted, Revoked, Certificate_Revoked, "revocation: anchor certificate",
                         Set_Of (None, Around (Certificates.Certificate_Digest (Root)), None));
         Check (not Decisions.Revocations_Usable (Set_Of ([High, Low], None, None)),
                "revocation: an unsorted list is refused before use");
         Check (not Decisions.Revocations_Usable (Set_Of ([Low, Low], None, None)),
                "revocation: a duplicate is refused before use");
         Check (Decisions.Revocations_Usable (Set_Of ([Low, Filler, High], None, [1 => Filler])),
                "revocation: a sorted list is usable");
         for Probe of Digest_Array'([Low, Filler, High, Payload_Digest]) loop
            Check (CuBit.Signed_Admission.Revocations.Contains ([Low, Filler, High], Probe)
                   = (Probe = Low or else Probe = Filler or else Probe = High),
                   "revocation: binary search agrees with membership");
         end loop;
         --  The key digest is SHA-384 of the SubjectPublicKeyInfo, as openssl writes it.
         Check (Certificates.Key_Digest (Fixture ("control-leaf.der")) = SPARKTLSCrypto.Hashing.SHA384.Hash (SPKI.all),
                "revocation: key digest is SHA-384 of the SubjectPublicKeyInfo");
         Free (SPKI);
         Free (Signed);
      end Revocation_Row;

      ------------------------------------------------------------------------
      procedure Substitution_Row is
         B_Payload : SPARKNaCl.Byte_Seq := Small_Payload;
         Signed_A : File_Access := Sign (Small_Payload, Leaf, Leaf_Key);
         Signed_Image : File_Access := Sign (Small_Payload, Leaf, Leaf_Key, Kind => Shared_Image);
         Reissued : constant X509.Byte_Seq :=
           Issue (Encoder.Code_Signing_Leaf, "Test builder", Leaf_Key, Root, Root_Key);
         Envelope_A : constant SPARKNaCl.Byte_Seq := Signed_A (Small_Payload'Length .. Signed_A'Last);
         Parsed_A : constant Envelopes.Envelope := Envelopes.Parse (Signed_A.all);
         T : Envelopes.Trailer;
         Length : N32;
         Replayed : File_Access;
      begin
         B_Payload (100) := B_Payload (100) xor 16#55#;
         Expect_Refused (B_Payload & Envelope_A, Trusted, Digest_Mismatch, Payload_Changed,
                         "substitution: program A's envelope on program B");
         Expect_Refused (Small_Payload (0 .. 99) & Envelope_A, Trusted, Envelope_Malformed, Payload_Length_Wrong,
                         "substitution: A's envelope on a shorter B");
         Expect_Refused (Signed_Image.all, Trusted, Signature_Invalid, Wrong_Purpose,
                         "substitution: a Shared_Image signature used for a program");
         Check (Verify (Signed_Image.all, Trusted, Kind => Shared_Image).Verified,
                "substitution: the Shared_Image signature verifies as an image");
         Expect_Refused (Signed_A.all, Trusted, Signature_Invalid, Wrong_Purpose,
                         "substitution: a program signature used for a shared image", Kind => Shared_Image);
         --  The same key in another certificate: the statement names the leaf.
         Check (Reissued /= Leaf, "substitution: the re-issued leaf differs");
         Envelopes.Encode (Program, Parsed_A.Payload_Length, Parsed_A.Payload_Digest, 1, Reissued, Reissued,
                           Parsed_A.Signature, T, Length);
         Replayed := Concat (Small_Payload, T, Length);
         Expect_Refused (Replayed.all, Trusted, Signature_Invalid, Statement_Signature,
                         "substitution: statement replayed under a re-issued leaf (same key)");
         Free (Replayed);
         Free (Signed_A);
         Free (Signed_Image);
      end Substitution_Row;

      ------------------------------------------------------------------------
      procedure Determinism_Row is
         First  : File_Access := Sign (Payload.all, Leaf, Leaf_Key);
         Second : File_Access := Sign (Payload.all, Leaf, Leaf_Key);
         Third  : File_Access := Sign (Payload.all, Leaf_Via_Inter, Leaf2_Key, Inter);
         Fourth : File_Access := Sign (Payload.all, Leaf_Via_Inter, Leaf2_Key, Inter);
      begin
         Check (First.all = Second.all,
                "determinism: same key, same payload, same bytes (fresh blinding each time)");
         Check (Third.all = Fourth.all, "determinism: with an intermediate");
         Free (First);
         Free (Second);
         Free (Third);
         Free (Fourth);
      end Determinism_Row;

      ------------------------------------------------------------------------
      procedure Shape_Row is
         Signed : File_Access := Sign (Small_Payload, Leaf_Via_Inter, Leaf2_Key, Inter);
         N : constant N32 := Small_Payload'Length;
         Total : constant N32 := Signed'Length;
         E : constant N32 := Total - N - Footer_Length;
         Length_At : constant N32 := Total - Footer_Length + 8;

         procedure With_Edit (At_Index : N32; Value : Unsigned_8; Reason : Refusal_Reason;
                              Detail : Refusal_Detail; What : String) is
            Copy : SPARKNaCl.Byte_Seq := Signed.all;
         begin
            Copy (At_Index) := Value;
            Expect_Refused (Copy, Trusted, Reason, Detail, "shape: " & What);
         end With_Edit;

         procedure Footer_Length_Is (Value : Natural; Detail : Refusal_Detail; What : String) is
            Copy : SPARKNaCl.Byte_Seq := Signed.all;
         begin
            Copy (Length_At) := Unsigned_8 (Value mod 256);
            Copy (Length_At + 1) := Unsigned_8 (Value / 256 mod 256);
            Copy (Length_At + 2) := Unsigned_8 (Value / 65536 mod 256);
            Copy (Length_At + 3) := 0;
            Expect_Refused (Copy, Trusted, Envelope_Malformed, Detail, "shape: " & What);
         end Footer_Length_Is;

         Copy : SPARKNaCl.Byte_Seq := Signed.all;
         Zero : constant SPARKNaCl.Byte_Seq (0 .. 15) := [others => 0];
      begin
         for Cut in N .. Total - 1 loop
            Expect_Any_Refusal (Signed (0 .. Cut - 1), Trusted, "shape: truncated to" & Cut'Image);
         end loop;
         Expect_Refused (Signed (0 .. N - 1), Trusted, Not_Signed, No_Footer, "shape: the unsigned payload");
         Expect_Refused (Signed (Total - Footer_Length .. Total - 1), Trusted, Envelope_Malformed,
                         Envelope_Length_Invalid, "shape: footer only");
         Expect_Refused (Zero (0 .. -1), Trusted, Not_Signed, No_Footer, "shape: a file of 0 bytes");
         Expect_Refused (Zero (0 .. 14), Trusted, Not_Signed, No_Footer, "shape: a file of 15 bytes");
         Expect_Refused (Zero, Trusted, Not_Signed, No_Footer, "shape: a file of 16 zero bytes");
         Expect_Refused (Signed.all & [0 .. 0 => 0], Trusted, Not_Signed, No_Footer,
                         "shape: trailing byte after the footer");
         Footer_Length_Is (Max_Envelope_Length + 1, Envelope_Length_Invalid, "envelope longer than 16 KiB");
         Footer_Length_Is (Natural (Total), Envelope_Length_Invalid, "envelope longer than the file");
         Footer_Length_Is (Natural (E) + 1, Envelope_Magic_Wrong, "envelope length one too long");
         Footer_Length_Is (Natural (E) - 1, Envelope_Magic_Wrong, "envelope length one too short");
         With_Edit (Total - 4, 2, Envelope_Malformed, Footer_Version_Unknown, "footer version 2");
         With_Edit (N + 8, 2, Envelope_Malformed, Format_Unknown, "format 2");
         With_Edit (N + 10, 2, Envelope_Malformed, Suite_Unknown, "suite 2");
         With_Edit (N + 12, 0, Envelope_Malformed, Purpose_Unknown, "purpose 0");
         With_Edit (N + 12, 6, Envelope_Malformed, Purpose_Unknown, "purpose 6");
         With_Edit (N + 14, 1, Envelope_Malformed, Reserved_Not_Zero, "reserved 1");
         With_Edit (N + 16, Unsigned_8 ((N + 1) mod 256), Envelope_Malformed, Payload_Length_Wrong,
                    "payload length disagreeing with the file");
         With_Edit (N + 72, 0, Envelope_Malformed, Certificate_Count_Invalid, "certificate count 0");
         With_Edit (N + 72, 3, Envelope_Malformed, Certificate_Count_Invalid, "certificate count 3");
         With_Edit (N + 72, 1, Envelope_Malformed, Lengths_Disagree, "count 1 with two certificates");
         Copy (N + 73) := 16#01#;   --  4097
         Copy (N + 74) := 16#10#;
         Expect_Refused (Copy, Trusted, Envelope_Malformed, Certificate_Length_Invalid,
                         "shape: certificate longer than 4 KiB");
         Copy (N + 73) := 0;
         Copy (N + 74) := 0;
         Expect_Refused (Copy, Trusted, Envelope_Malformed, Certificate_Length_Invalid,
                         "shape: certificate length 0");
         Free (Signed);
      end Shape_Row;

      ------------------------------------------------------------------------
      function Read_U16 (B : SPARKNaCl.Byte_Seq; At_Index : N32) return Natural is
        (Natural (B (At_Index)) + 256 * Natural (B (At_Index + 1)));
      function Read_U32 (B : SPARKNaCl.Byte_Seq; At_Index : N32) return Natural is
        (Read_U16 (B, At_Index) + 65536 * Read_U16 (B, At_Index + 2));

      procedure Bytes_Row is
         Signed : File_Access := Sign (Payload.all, Leaf, Leaf_Key);
         Small : File_Access := Sign (Small_Payload, Leaf_Via_Inter, Leaf2_Key, Inter);
         Section_Count : Natural := 0;
         Cubit_Sections : Natural := 0;
         No_Bits : constant := 8;   --  SHT_NOBITS: no file contents
         type Offsets is array (1 .. 3) of N32;
         Program_Header_Size : constant := 56;

         procedure Flip (File : in out SPARKNaCl.Byte_Seq; At_Index : N32; Bit : Natural; What : String) is
            Saved : constant Unsigned_8 := File (At_Index);
         begin
            File (At_Index) := Saved xor Shift_Left (1, Bit);
            Expect_Any_Refusal (File, Trusted, What & " (byte" & At_Index'Image & " bit" & Bit'Image & ")");
            File (At_Index) := Saved;
         end Flip;
         I : N32 := 0;
      begin
         --  The ELF header, the program headers, and the first, middle and
         --  last byte of every section with file contents (.text, .rodata
         --  and every .cubit.* section the payload has).
         for B in N32 range 0 .. 63 loop
            Flip (Signed.all, B, Natural (B) mod 8, "bytes: ELF header");
         end loop;
         if Payload'Length > 64 and then Payload (0) = 16#7F# and then Payload (1) = Character'Pos ('E') then
            declare
               Program_Headers : constant N32 := N32 (Read_U32 (Payload.all, 32));
               Program_Count : constant N32 := N32 (Read_U16 (Payload.all, 56));
               Section_Headers : constant N32 := N32 (Read_U32 (Payload.all, 40));
               Entry_Size : constant N32 := N32 (Read_U16 (Payload.all, 58));
               Section_Total : constant N32 := N32 (Read_U16 (Payload.all, 60));
               Names_Index : constant N32 := N32 (Read_U16 (Payload.all, 62));
               Names_At : constant N32 :=
                 N32 (Read_U32 (Payload.all, Section_Headers + Names_Index * Entry_Size + 24));
            begin
               for P in 0 .. Program_Count * Program_Header_Size - 1 loop
                  if P mod 7 = 0 then
                     Flip (Signed.all, Program_Headers + P, Natural (P) mod 8, "bytes: program header");
                  end if;
               end loop;
               for S in 1 .. Section_Total - 1 loop
                  declare
                     Header : constant N32 := Section_Headers + S * Entry_Size;
                     Kind : constant Natural := Read_U32 (Payload.all, Header + 4);
                     Offset : constant N32 := N32 (Read_U32 (Payload.all, Header + 24));
                     Size : constant N32 := N32 (Read_U32 (Payload.all, Header + 32));
                     Name_At : constant N32 := Names_At + N32 (Read_U32 (Payload.all, Header));
                     Section_Name : String (1 .. 32) := [others => ' '];
                     Length : Natural := 0;
                  begin
                     while Length < 32 and then Payload (Name_At + N32 (Length)) /= 0 loop
                        Length := Length + 1;
                        Section_Name (Length) := Character'Val (Payload (Name_At + N32 (Length - 1)));
                     end loop;
                     if Kind /= No_Bits and then Size > 0 then
                        Section_Count := Section_Count + 1;
                        if Length > 7 and then Section_Name (1 .. 7) = ".cubit." then
                           Cubit_Sections := Cubit_Sections + 1;
                        end if;
                        for At_Index of Offsets'([Offset, Offset + Size / 2, Offset + Size - 1]) loop
                           Flip (Signed.all, At_Index, Natural (At_Index) mod 8,
                                 "bytes: section " & Section_Name (1 .. Length));
                        end loop;
                     end if;
                  end;
               end loop;
               Put_Line ("bytes: flipped bits in" & Section_Count'Image & " sections ("
                         & Cubit_Sections'Image & " .cubit.*) of " & Payload_Path);
            end;
         end if;
         --  A sparse sweep over the whole payload.
         while I < Payload'Length loop
            Flip (Signed.all, I, Natural (I) mod 8, "bytes: payload sweep");
            I := I + 4093;
         end loop;
         --  Every byte of the envelope and footer, low and high bit, with an
         --  intermediate so both certificates and both lengths are covered.
         for B in N32 range Small_Payload'Length .. Small'Last loop
            Flip (Small.all, B, 0, "bytes: envelope or footer");
            Flip (Small.all, B, 7, "bytes: envelope or footer");
         end loop;
         Expect_Verified (Small.all, Trusted, "bytes: the original still verifies");
         Expect_Verified (Signed.all, Trusted, "bytes: the original payload still verifies");
         Free (Signed);
         Free (Small);
      end Bytes_Row;
   begin
      Statement_Row;
      Keys_Row;
      Encoder_Row;
      Signer_Row;
      Accept_Row;
      Wrong_Trust_Row;
      Profile_Row;
      Revocation_Row;
      Substitution_Row;
      Determinism_Row;
      Shape_Row;
      if Ada.Command_Line.Argument_Count < 3 or else Ada.Command_Line.Argument (3) /= "--quick" then
         Bytes_Row;
      end if;
   end Rows;

begin
   Ada.Streams.Stream_IO.Open (Random_Source, Ada.Streams.Stream_IO.In_File, "/dev/urandom");
   Root_Key := New_Key;
   Inter_Key := New_Key;
   Leaf_Key := New_Key;
   Leaf2_Key := New_Key;
   Rogue_Key := New_Key;
   Payload := Read_File (Payload_Path);
   Rows;
   Free (Payload);
   Put_Line ("signed-admission:" & Natural'Image (Checks - Failures) & " /" & Natural'Image (Checks) & " passed");
   if Failures > 0 then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Main;
