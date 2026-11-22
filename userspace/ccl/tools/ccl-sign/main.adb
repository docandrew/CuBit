-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
-- SPDX-License-Identifier: GPL-3.0-or-later
--
-- ccl-sign, the host signer and verifier (SIGN-001, docs/signed-binaries.md
-- section 7), over the proved units in userspace/lib/signing:
--
--   ccl-sign init   --signing DIR --name NAME
--        Makes DIR (mode 0700, empty or absent) with a "built by" CA
--        (NAME builder CA, ten years, pathLen 0) and a code-signing leaf
--        under it (NAME builder, two years); the key files are 0600.
--   ccl-sign sign   --signing DIR [--purpose program|shared-image] INPUT OUTPUT
--        OUTPUT is INPUT with the envelope appended. A file this key already
--        signed is copied unchanged; one signed by another key is refused.
--   ccl-sign verify --anchor CA.der... [--purpose P] [--identity ID
--                   --authority application|service|hardware] FILE
--        Exit 0 when admitted, 1 when refused.
--   ccl-sign digest FILE
--        SHA-384 of the payload (the whole file when it is unsigned), to
--        compare builds signed by different keys.
--
-- Exit 2 for a usage error or a refusal to sign. No key is printed.
-------------------------------------------------------------------------------
with Ada.Command_Line;
with Ada.Containers.Indefinite_Vectors;
with Ada.Exceptions;
with Ada.Text_IO;
with SPARKNaCl;
with SPARKTLSCrypto.Hashing.SHA384;
with X509;
with CuBit.Certificate_Encoder;
with CuBit.Signed_Admission;
with CuBit.Signed_Admission.Certificates;
with CuBit.Signed_Admission.Decisions;
with CuBit.Signed_Admission.Envelopes;
with CuBit.Signing_Keys;
with Host_Signing;

procedure Main is
   use Ada.Command_Line;
   use Ada.Text_IO;
   use type SPARKNaCl.Byte_Seq;
   use type SPARKNaCl.N32;
   use type X509.N32;
   use type CuBit.Certificate_Encoder.Issue_Result;
   use type CuBit.Signed_Admission.Envelopes.Parse_Status;
   package Admission renames CuBit.Signed_Admission;
   package Encoder renames CuBit.Certificate_Encoder;
   package Keys renames CuBit.Signing_Keys;
   package Text_Vectors is new Ada.Containers.Indefinite_Vectors (Positive, String);

   Usage_Error : exception;

   --  Certificate lifetimes for host development keys (named, not
   --  defaults a caller must know): the CA outlives many leaves.
   CA_Years   : constant := 10;
   Leaf_Years : constant := 2;
   CA_Path_Length : constant := 0;   --  the host CA signs leaves directly

   Exit_Refused : constant Exit_Status := 1;
   Exit_Usage   : constant Exit_Status := 2;

   function "/" (Directory, Name : String) return String is (Directory & "/" & Name);

   --  Options and operands.
   Command : constant String := (if Argument_Count >= 1 then Argument (1) else "");
   type Text_Access is access String;
   Signing_Directory, Name_Option, Purpose_Option, Identity_Option, Authority_Option : Text_Access := null;
   Anchor_Paths : Text_Vectors.Vector;
   Operands : Text_Vectors.Vector;

   procedure Parse is
      I : Positive := 2;
      procedure Value (Target : in out Text_Access) is
      begin
         if Target /= null then
            raise Usage_Error with Argument (I) & " is given twice";
         elsif I = Argument_Count then
            raise Usage_Error with Argument (I) & " needs a value";
         end if;
         Target := new String'(Argument (I + 1));
         I := I + 1;
      end Value;
   begin
      while I <= Argument_Count loop
         declare
            A : constant String := Argument (I);
         begin
            if A = "--signing" then Value (Signing_Directory);
            elsif A = "--name" then Value (Name_Option);
            elsif A = "--purpose" then Value (Purpose_Option);
            elsif A = "--identity" then Value (Identity_Option);
            elsif A = "--authority" then Value (Authority_Option);
            elsif A = "--anchor" then
               if I = Argument_Count then
                  raise Usage_Error with "--anchor needs a value";
               end if;
               Anchor_Paths.Append (Argument (I + 1));
               I := I + 1;
            elsif A'Length > 2 and then A (A'First .. A'First + 1) = "--" then
               raise Usage_Error with "unknown option " & A;
            else
               Operands.Append (A);
            end if;
         end;
         I := I + 1;
      end loop;
   end Parse;

   function Required (Option : Text_Access; Name : String) return String is
   begin
      if Option = null then
         raise Usage_Error with Command & " needs " & Name;
      end if;
      return Option.all;
   end Required;

   procedure Operand_Count (Count : Natural) is
   begin
      if Natural (Operands.Length) /= Count then
         raise Usage_Error with Command & " takes" & Count'Image & " operand" & (if Count = 1 then "" else "s");
      end if;
   end Operand_Count;

   function Purpose_Of return Admission.Purpose is
   begin
      if Purpose_Option = null or else Purpose_Option.all = "program" then
         return Admission.Program;
      elsif Purpose_Option.all = "shared-image" then
         return Admission.Shared_Image;
      end if;
      raise Usage_Error with "--purpose is program or shared-image";
   end Purpose_Of;

   ---------------------------------------------------------------------------
   --  init
   ---------------------------------------------------------------------------

   function Plus_Years (T : X509.Date_Time; Years : Natural) return X509.Date_Time is
      Result : X509.Date_Time := T;
      February : constant := 2;
      Last_Common_Day : constant := 28;
   begin
      Result.Year := T.Year + Years;
      if T.Month = February and then T.Day > Last_Common_Day then
         Result.Day := Last_Common_Day;
      end if;
      return Result;
   end Plus_Years;

   procedure New_Key (D : out Keys.Scalar; Q : out Keys.Point) is
      OK : Boolean;
   begin
      loop
         Host_Signing.Random (D);
         exit when Keys.Strong (D);
      end loop;
      Keys.Public_Of (D, Host_Signing.Mask, Q);
      Keys.Check_Pair (D, Q, Host_Signing.Mask, OK);
      if not OK then
         D := [others => 0];
         raise Host_Signing.Signing_Error with "a new key failed the pairwise consistency test";
      end if;
   end New_Key;

   function Issue
     (Kind : Encoder.Certificate_Kind; Subject : String; Subject_Key : Keys.Point;
      Issuer : X509.Byte_Seq; Issuer_Key : Keys.Scalar; From, Until_Date : X509.Date_Time;
      Path : Encoder.Path_Length) return X509.Byte_Seq
   is
      R : Encoder.Request;
      Out_DER : Encoder.Encoded_Certificate;
      Length : X509.N32;
      Result : Encoder.Issue_Result;
   begin
      if Subject'Length > Encoder.Max_Name_Length then
         raise Usage_Error with "--name is too long for a certificate name";
      end if;
      R.Kind := Kind;
      R.Subject.Length := Subject'Length;
      R.Subject.Data (1 .. Subject'Length) := Subject;
      Host_Signing.Random (R.Serial);
      R.Not_Before := From;
      R.Not_After := Until_Date;
      R.Subject_Key := Subject_Key;
      R.Path := Path;
      Encoder.Issue (R, Issuer, Issuer_Key, From, Host_Signing.Mask, Out_DER, Length, Result);
      if Result /= Encoder.Issued then
         raise Host_Signing.Signing_Error with "cannot issue " & Subject & ": " & Result'Image;
      end if;
      return Out_DER (0 .. Length - 1);
   end Issue;

   function To_Bytes (B : X509.Byte_Seq) return SPARKNaCl.Byte_Seq is
      Result : SPARKNaCl.Byte_Seq (0 .. SPARKNaCl.N32 (B'Length) - 1);
   begin
      for I in Result'Range loop
         Result (I) := B (B'First + X509.N32 (I));
      end loop;
      return Result;
   end To_Bytes;

   Development_CA_Label   : constant String := " development builder CA (not for production)";
   Development_Leaf_Label : constant String := " development builder (not for production)";
   Development_Name_Room  : constant Natural :=
     Encoder.Max_Name_Length - Development_CA_Label'Length;

   procedure Init is
      Directory : constant String := Required (Signing_Directory, "--signing DIR");
      Given : constant String := Required (Name_Option, "--name NAME");
      --  A host key is a development key, and its certificates say so, so a
      --  program signed with it is never mistaken for a production build:
      --  "<name> development builder CA (not for production)". X.520 bounds
      --  a common name at 64 characters, so the given name is cut to leave
      --  room for the label.
      Name : constant String :=
        Given (Given'First .. Given'First - 1 + Natural'Min (Given'Length, Development_Name_Room));
      CA_D, Leaf_D : Keys.Scalar;
      CA_Q, Leaf_Q : Keys.Point;
      Key_File : Keys.Key_File;
      From : constant X509.Date_Time := Host_Signing.Now;
   begin
      Operand_Count (0);
      Host_Signing.Make_Private_Directory (Directory);
      New_Key (CA_D, CA_Q);
      New_Key (Leaf_D, Leaf_Q);
      declare
         No_Issuer : constant X509.Byte_Seq (0 .. 0) := [0];
         CA : constant X509.Byte_Seq :=
           Issue (Encoder.Root_CA, Name & Development_CA_Label, CA_Q, No_Issuer, CA_D,
                  From, Plus_Years (From, CA_Years), CA_Path_Length);
         Leaf : constant X509.Byte_Seq :=
           Issue (Encoder.Code_Signing_Leaf, Name & Development_Leaf_Label, Leaf_Q, CA, CA_D,
                  From, Plus_Years (From, Leaf_Years), 0);
      begin
         Keys.Encode_Key (CA_D, CA_Q, Key_File);
         Host_Signing.Write_File (Directory / Host_Signing.CA_Key_Name, Key_File, Host_Signing.Private_File);
         Keys.Encode_Key (Leaf_D, Leaf_Q, Key_File);
         Host_Signing.Write_File (Directory / Host_Signing.Leaf_Key_Name, Key_File, Host_Signing.Private_File);
         Key_File := [others => 0];
         CA_D := [others => 0];
         Leaf_D := [others => 0];
         Host_Signing.Write_File
           (Directory / Host_Signing.CA_Certificate_Name, To_Bytes (CA), Host_Signing.Public_File);
         Host_Signing.Write_File
           (Directory / Host_Signing.Leaf_Certificate_Name, To_Bytes (Leaf), Host_Signing.Public_File);
         Put_Line ("ccl-sign: made a host development signing key in " & Directory);
         Put_Line ("  CA     " & Name & Development_CA_Label & ", SHA-384 "
                   & Host_Signing.Hex (Admission.Certificates.Certificate_Digest (CA)));
         Put_Line ("  leaf   " & Name & Development_Leaf_Label & ", valid until" & Plus_Years (From, Leaf_Years).Year'Image);
         Put_Line ("  images name the CA with --import cubit-signing-ca=" & (Directory / Host_Signing.CA_Certificate_Name));
      end;
   end Init;

   ---------------------------------------------------------------------------
   --  sign
   ---------------------------------------------------------------------------

   procedure Sign is
      Directory : constant String := Required (Signing_Directory, "--signing DIR");
      Signer : Host_Signing.Signer;
   begin
      Operand_Count (2);
      Host_Signing.Load (Signer, Directory);
      declare
         Input : constant String := Operands (1);
         Output : constant String := Operands (2);
         Data : Host_Signing.Bytes_Access := Host_Signing.Read_File (Input);
         Mode : constant Natural := Host_Signing.Mode_Of (Input);
      begin
         if Host_Signing.Has_Footer (Data.all) then
            declare
               Anchors : Host_Signing.Anchor_List :=
                 [1 => new SPARKNaCl.Byte_Seq'(To_Bytes (Host_Signing.Anchor_DER (Signer)))];
               V : constant Admission.Verification := Host_Signing.Verify (Data.all, Anchors, Purpose_Of);
            begin
               Host_Signing.Free (Anchors (1));
               if V.Verified
                 and then V.Signer.Leaf_Digest =
                   Admission.Certificates.Certificate_Digest (Host_Signing.Leaf_DER (Signer))
               then
                  if Input /= Output then
                     Host_Signing.Write_File (Output, Data.all, Mode);
                  end if;
                  Put_Line ("ccl-sign: " & Input & " is already signed by this key");
                  Host_Signing.Free (Data);
                  return;
               end if;
               Host_Signing.Free (Data);
               raise Host_Signing.Signing_Error with Input & " already carries a signature by another key or"
                 & " a damaged one; sign the unsigned program instead";
            end;
         end if;
         Host_Signing.Write_File (Output, Data.all, Mode, Host_Signing.Sign (Signer, Data.all, Purpose_Of));
         Host_Signing.Free (Data);
      end;
   end Sign;

   ---------------------------------------------------------------------------
   --  verify
   ---------------------------------------------------------------------------

   function Authority_Of (Text : String) return Admission.Requested_Authority is
   begin
      if Text = "application" then
         return Admission.Application_Authority;
      elsif Text = "service" then
         return Admission.Service_Authority;
      elsif Text = "hardware" then
         return Admission.Hardware_Or_System_Authority;
      end if;
      raise Usage_Error with "--authority is application, service or hardware";
   end Authority_Of;

   procedure Verify is
   begin
      Operand_Count (1);
      if Anchor_Paths.Is_Empty then
         raise Usage_Error with "verify needs at least one --anchor CA.der";
      end if;
      declare
         Anchors : Host_Signing.Anchor_List (1 .. Natural (Anchor_Paths.Length));
         Data : Host_Signing.Bytes_Access := Host_Signing.Read_File (Operands (1));
      begin
         for I in Anchors'Range loop
            Anchors (I) := Host_Signing.Read_File (Anchor_Paths (I), Admission.Max_Certificate_Length);
         end loop;
         declare
            V : constant Admission.Verification := Host_Signing.Verify (Data.all, Anchors, Purpose_Of);
            Everything : constant Admission.Prefix_Array := [1 => (Length => 1, Data => [others => '*'])];
            Trust : Admission.Anchor_Array (Anchors'Range);
            Identity : Admission.Identity_Text;
         begin
            Host_Signing.Free (Data);
            if not V.Verified then
               Put_Line ("refused: " & V.Reason'Image & " (" & V.Detail'Image & ")");
               Set_Exit_Status (Exit_Refused);
               return;
            end if;
            if Identity_Option /= null then
               if Identity_Option'Length > Admission.Max_Identity_Length then
                  raise Usage_Error with "--identity is longer than" & Admission.Max_Identity_Length'Image;
               end if;
               Identity.Length := Identity_Option'Length;
               Identity.Data (1 .. Identity.Length) := Identity_Option.all;
               for I in Trust'Range loop
                  Trust (I).Ceiling := Admission.Hardware_And_System;
                  Trust (I).Last_Prefix := 1;
               end loop;
               declare
                  A : constant Admission.Admission :=
                    Admission.Decisions.Admit
                      (V, Trust, Everything, Identity,
                       Authority_Of (Required (Authority_Option, "--authority")));
               begin
                  if not A.Admitted then
                     Put_Line ("refused: " & A.Reason'Image & " (" & A.Detail'Image & ")");
                     Set_Exit_Status (Exit_Refused);
                     return;
                  end if;
               end;
            end if;
            Put_Line ("admitted: signed by '" & V.Signer.Name.Data (1 .. V.Signer.Name.Length)
                      & "' under anchor" & V.Signer.Anchor'Image & " ("
                      & Anchor_Paths (V.Signer.Anchor) & ")");
            Put_Line ("  payload " & Host_Signing.Hex (V.Signer.Payload_Digest) & "  "
                      & V.Signer.Payload_Length'Image & " bytes");
            for A of Anchors loop
               Host_Signing.Free (A);
            end loop;
         end;
      end;
   end Verify;

   ---------------------------------------------------------------------------
   --  digest
   ---------------------------------------------------------------------------

   procedure Digest is
   begin
      Operand_Count (1);
      declare
         Data : Host_Signing.Bytes_Access := Host_Signing.Read_File (Operands (1));
         E : constant Admission.Envelopes.Envelope := Admission.Envelopes.Parse (Data.all);
         Length : constant SPARKNaCl.N32 :=
           (if E.Status = Admission.Envelopes.Parsed then E.Payload_Length else Data'Length);
      begin
         Put_Line (Host_Signing.Hex (SPARKTLSCrypto.Hashing.SHA384.Hash (Data (0 .. Length - 1)))
                   & "  " & Operands (1)
                   & (if E.Status = Admission.Envelopes.Parsed then " (signed payload)"
                      elsif E.Status = Admission.Envelopes.Malformed then " (damaged envelope; whole file)"
                      else " (unsigned)"));
         Host_Signing.Free (Data);
      end;
   end Digest;

begin
   Parse;
   if Command = "init" then
      Init;
   elsif Command = "sign" then
      Sign;
   elsif Command = "verify" then
      Verify;
   elsif Command = "digest" then
      Digest;
   else
      raise Usage_Error with (if Command = "" then "no command" else "unknown command " & Command);
   end if;
exception
   when E : Usage_Error =>
      Put_Line (Standard_Error, "ccl-sign: " & Ada.Exceptions.Exception_Message (E));
      Put_Line (Standard_Error, "usage: ccl-sign init|sign|verify|digest ... (see the header of "
                & "userspace/ccl/tools/ccl-sign/main.adb)");
      Set_Exit_Status (Exit_Usage);
   when E : Host_Signing.Signing_Error =>
      Put_Line (Standard_Error, "ccl-sign: " & Ada.Exceptions.Exception_Message (E));
      Set_Exit_Status (Exit_Usage);
end Main;
