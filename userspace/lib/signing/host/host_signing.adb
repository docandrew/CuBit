-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
-- SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------
with Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Directories;
with Ada.Streams.Stream_IO;
with Ada.Unchecked_Deallocation;
with GNAT.OS_Lib;
with Interfaces.C;
with Interfaces.C.Strings;
with System;
with CuBit.Program_Signing;
with CuBit.Signed_Admission.Certificates;
with CuBit.Signed_Admission.Decisions;
with CuBit.Signed_Admission.Envelopes;

package body Host_Signing is
   use type SPARKNaCl.Byte_Seq;
   use type X509.N32;
   use type Interfaces.C.int;
   use type Interfaces.C.long;
   use type Interfaces.Integer_32;
   use type Interfaces.Unsigned_8;
   use type Ada.Streams.Stream_IO.Count;
   use type CuBit.Program_Signing.Signing_Result;
   package C renames Interfaces.C;
   package Admission renames CuBit.Signed_Admission;
   package Certificates renames CuBit.Signed_Admission.Certificates;
   package Keys renames CuBit.Signing_Keys;
   subtype N32 is SPARKNaCl.N32;

   procedure Free_Bytes is new Ada.Unchecked_Deallocation (SPARKNaCl.Byte_Seq, Bytes_Access);
   procedure Free (B : in out Bytes_Access) is
   begin
      if B /= null then
         B.all := [others => 0];
         Free_Bytes (B);
      end if;
   end Free;

   function Read_File (Path : String; Limit : Natural := CuBit.Signed_Admission.Max_File_Length)
     return Bytes_Access
   is
      use Ada.Streams;
      F : Stream_IO.File_Type;
   begin
      begin
         Stream_IO.Open (F, Stream_IO.In_File, Path);
      exception
         when Stream_IO.Name_Error | Stream_IO.Use_Error =>
            raise Signing_Error with "cannot read " & Path;
      end;
      if Stream_IO.Size (F) > Stream_IO.Count (Limit) then
         Stream_IO.Close (F);
         raise Signing_Error with Path & " is longer than" & Limit'Image & " bytes";
      end if;
      declare
         Size : constant Stream_Element_Offset := Stream_Element_Offset (Stream_IO.Size (F));
         Result : constant Bytes_Access := new SPARKNaCl.Byte_Seq (0 .. N32 (Size) - 1);
         Chunk : constant := 1024 * 1024;
         Raw : Stream_Element_Array (1 .. Chunk);
         Last : Stream_Element_Offset;
         At_Index : N32 := 0;
      begin
         while At_Index < Result'Length loop
            Stream_IO.Read (F, Raw, Last);
            exit when Last < 1;
            for I in 1 .. Last loop
               Result (At_Index + N32 (I) - 1) := Interfaces.Unsigned_8 (Raw (I));
            end loop;
            At_Index := At_Index + N32 (Last);
         end loop;
         Stream_IO.Close (F);
         if At_Index /= Result'Length then
            raise Signing_Error with Path & " changed while it was read";
         end if;
         return Result;
      end;
   end Read_File;

   O_WRONLY : constant := 8#1#;
   O_CREAT  : constant := 8#100#;
   O_EXCL   : constant := 8#200#;
   function c_open (Path : C.Strings.chars_ptr; Flags : C.int; Mode : C.unsigned) return C.int
     with Import, Convention => C_Variadic_2, External_Name => "open";
   function c_write (FD : C.int; Buffer : System.Address; Count : C.size_t) return C.long
     with Import, Convention => C, External_Name => "write";
   function c_fsync (FD : C.int) return C.int
     with Import, Convention => C, External_Name => "fsync";
   function c_close (FD : C.int) return C.int
     with Import, Convention => C, External_Name => "close";
   function c_rename (From, To : C.Strings.chars_ptr) return C.int
     with Import, Convention => C, External_Name => "rename";
   function c_mkdir (Path : C.Strings.chars_ptr; Mode : C.unsigned) return C.int
     with Import, Convention => C, External_Name => "mkdir";
   function c_getpid return C.int
     with Import, Convention => C, External_Name => "getpid";

   procedure Write_File
     (Path : String; Data : SPARKNaCl.Byte_Seq; Mode : Natural;
      Trailer : SPARKNaCl.Byte_Seq := [1 .. 0 => 0])
   is
      Pid : constant String := C.int'Image (c_getpid);
      Temporary : constant String := Path & ".tmp" & Pid (Pid'First + 1 .. Pid'Last);
      Name : C.Strings.chars_ptr := C.Strings.New_String (Temporary);
      Target : C.Strings.chars_ptr := C.Strings.New_String (Path);
      FD : constant C.int := c_open (Name, O_WRONLY + O_CREAT + O_EXCL, C.unsigned (Mode));

      procedure Put (B : SPARKNaCl.Byte_Seq) is
         At_Index : N32 := B'First;
         Written : C.long;
      begin
         while At_Index <= B'Last loop
            Written := c_write (FD, B (At_Index)'Address, C.size_t (B'Last - At_Index + 1));
            if Written <= 0 then
               raise Signing_Error with "cannot write " & Temporary;
            end if;
            At_Index := At_Index + N32 (Written);
         end loop;
      end Put;
   begin
      if FD < 0 then
         C.Strings.Free (Name);
         C.Strings.Free (Target);
         raise Signing_Error with "cannot create " & Temporary;
      end if;
      Put (Data);
      Put (Trailer);
      if c_fsync (FD) /= 0 or else c_close (FD) /= 0 or else c_rename (Name, Target) /= 0 then
         C.Strings.Free (Name);
         C.Strings.Free (Target);
         raise Signing_Error with "cannot write " & Path;
      end if;
      C.Strings.Free (Name);
      C.Strings.Free (Target);
   end Write_File;

   function Mode_Of (Path : String) return Natural is
     (if GNAT.OS_Lib.Is_Executable_File (Path) then Executable_File else Public_File);

   Private_Directory : constant := 8#700#;

   procedure Make_Private_Directory (Directory : String) is
      Name : C.Strings.chars_ptr;
      Result : C.int;
      use type Ada.Directories.File_Kind;
   begin
      if Ada.Directories.Exists (Directory) then
         if Ada.Directories.Kind (Directory) /= Ada.Directories.Directory then
            raise Signing_Error with Directory & " exists and is not a directory";
         end if;
         declare
            Search : Ada.Directories.Search_Type;
            Item : Ada.Directories.Directory_Entry_Type;
         begin
            Ada.Directories.Start_Search (Search, Directory, "");
            while Ada.Directories.More_Entries (Search) loop
               Ada.Directories.Get_Next_Entry (Search, Item);
               if Ada.Directories.Simple_Name (Item) not in "." | ".." then
                  Ada.Directories.End_Search (Search);
                  raise Signing_Error with Directory & " already holds files; nothing was changed";
               end if;
            end loop;
            Ada.Directories.End_Search (Search);
         end;
         return;
      end if;
      declare
         Parent : constant String := Ada.Directories.Containing_Directory (Directory);
      begin
         if not Ada.Directories.Exists (Parent) then
            Ada.Directories.Create_Path (Parent);
         end if;
      end;
      Name := C.Strings.New_String (Directory);
      Result := c_mkdir (Name, Private_Directory);
      C.Strings.Free (Name);
      if Result /= 0 then
         raise Signing_Error with "cannot create " & Directory;
      end if;
   end Make_Private_Directory;

   Random_Device : constant String := "/dev/urandom";

   procedure Random (Output : out SPARKNaCl.Byte_Seq) is
      use Ada.Streams;
      F : Stream_IO.File_Type;
      Raw : Stream_Element_Array (1 .. Stream_Element_Offset (Output'Length));
      Last : Stream_Element_Offset;
   begin
      Stream_IO.Open (F, Stream_IO.In_File, Random_Device);
      Stream_IO.Read (F, Raw, Last);
      Stream_IO.Close (F);
      if Last /= Raw'Last then
         raise Signing_Error with "short read from " & Random_Device;
      end if;
      for I in Output'Range loop
         Output (I) := Interfaces.Unsigned_8 (Raw (Stream_Element_Offset (I - Output'First) + 1));
      end loop;
   end Random;

   function Mask return Keys.Blind is
      B : Keys.Blind;
   begin
      Random (B);
      return B;
   end Mask;

   function Now return X509.Date_Time is
      Year : Ada.Calendar.Year_Number;
      Month : Ada.Calendar.Month_Number;
      Day : Ada.Calendar.Day_Number;
      Hour : Ada.Calendar.Formatting.Hour_Number;
      Minute : Ada.Calendar.Formatting.Minute_Number;
      Second : Ada.Calendar.Formatting.Second_Number;
      Fraction : Ada.Calendar.Formatting.Second_Duration;
   begin
      Ada.Calendar.Formatting.Split
        (Ada.Calendar.Clock, Year, Month, Day, Hour, Minute, Second, Fraction, Time_Zone => 0);
      return (Year => Year, Month => Month, Day => Day, Hour => Hour, Minute => Minute, Second => Second);
   end Now;

   function To_X509 (B : SPARKNaCl.Byte_Seq) return X509.Byte_Seq is
      Result : X509.Byte_Seq (0 .. X509.N32 (B'Length) - 1);
   begin
      for I in Result'Range loop
         Result (I) := B (B'First + N32 (I));
      end loop;
      return Result;
   end To_X509;

   function Hex (B : SPARKNaCl.Byte_Seq) return String is
      Digits_Of : constant String := "0123456789abcdef";
      Result : String (1 .. 2 * B'Length);
      Next : Positive := 1;
   begin
      for Value of B loop
         Result (Next) := Digits_Of (Natural (Value / 16) + 1);
         Result (Next + 1) := Digits_Of (Natural (Value mod 16) + 1);
         Next := Next + 2;
      end loop;
      return Result;
   end Hex;

   overriding procedure Finalize (S : in out Signer) is
   begin
      S.Key := [others => 0];
   end Finalize;

   procedure Load_Certificate
     (Path : String; Buffer : out Admission.Certificate_Buffer; Length : out X509.N32)
   is
      Data : Bytes_Access := Read_File (Path, Admission.Max_Certificate_Length);
   begin
      Buffer := [others => 0];
      if Data'Length = 0 then
         Free (Data);
         raise Signing_Error with Path & " is empty";
      end if;
      Length := X509.N32 (Data'Length);
      Buffer (0 .. Length - 1) := To_X509 (Data.all);
      Free (Data);
   end Load_Certificate;

   function Leaf_DER (S : Signer) return X509.Byte_Seq is (S.Leaf (0 .. S.Leaf_Length - 1));
   function Anchor_DER (S : Signer) return X509.Byte_Seq is (S.Anchor (0 .. S.Anchor_Length - 1));
   function Intermediate_DER (S : Signer) return X509.Byte_Seq is
     (if S.Intermediate_Length > 0 then S.Intermediate (0 .. S.Intermediate_Length - 1)
      else Leaf_DER (S));

   procedure Load (S : in out Signer; Directory : String) is
      function Path (Name : String) return String is (Directory & "/" & Name);
      Key_File : Bytes_Access;
      Point : Keys.Point;
      Leaf_Point : Certificates.Point_Bytes;
      OK : Boolean;
   begin
      if not Ada.Directories.Exists (Path (Leaf_Key_Name)) then
         raise Signing_Error with "no signing key in " & Directory
           & "; make one with: make -C kernel signing-key (ccl-sign init)";
      end if;
      Key_File := Read_File (Path (Leaf_Key_Name), Keys.Key_File_Length);
      Keys.Decode_Key (Key_File.all, S.Key, Point, OK);
      Free (Key_File);
      if OK then
         Keys.Check_Pair (S.Key, Point, Mask, OK);
      end if;
      if not OK then
         S.Key := [others => 0];
         raise Signing_Error with Path (Leaf_Key_Name)
           & " is not a strong P-384 key pair in the expected encoding; refused";
      end if;
      Load_Certificate (Path (Leaf_Certificate_Name), S.Leaf, S.Leaf_Length);
      Load_Certificate (Path (CA_Certificate_Name), S.Anchor, S.Anchor_Length);
      if Ada.Directories.Exists (Path (Intermediate_Certificate_Name)) then
         Load_Certificate (Path (Intermediate_Certificate_Name), S.Intermediate, S.Intermediate_Length);
      end if;
      Certificates.Public_Point (Leaf_DER (S), Leaf_Point, OK);
      if not OK or else Leaf_Point /= Point then
         raise Signing_Error with Path (Leaf_Key_Name) & " is not the key of " & Path (Leaf_Certificate_Name);
      end if;
      if not Certificates.Chain_Holds
          (Leaf_DER (S), S.Intermediate_Length > 0, Intermediate_DER (S), Anchor_DER (S))
      then
         raise Signing_Error with Path (Leaf_Certificate_Name) & " does not chain to "
           & Path (CA_Certificate_Name) & " under the CuBit builder profile";
      end if;
   end Load;

   function Explain (R : CuBit.Program_Signing.Signing_Result) return String is
     (case R is
        when CuBit.Program_Signing.Signed => "signed",
        when CuBit.Program_Signing.Weak_Key => "the key is weak",
        when CuBit.Program_Signing.Key_Mismatch => "the key is not the leaf certificate's",
        when CuBit.Program_Signing.Leaf_Refused => "the leaf is not a code-signing certificate",
        when CuBit.Program_Signing.Intermediate_Refused => "the intermediate did not issue the leaf",
        when CuBit.Program_Signing.Leaf_Not_Yet_Valid => "the leaf certificate is not valid yet",
        when CuBit.Program_Signing.Leaf_Expired =>
          "the leaf certificate has expired; make a new key (make -C kernel signing-key)",
        when CuBit.Program_Signing.Signing_Failed => "the signature computation failed",
        when CuBit.Program_Signing.Self_Check_Failed => "the signature did not verify");

   function Sign
     (S : Signer; Payload : SPARKNaCl.Byte_Seq;
      Kind : CuBit.Signed_Admission.Purpose := CuBit.Signed_Admission.Program)
      return SPARKNaCl.Byte_Seq
   is
      T : Admission.Envelopes.Trailer;
      Length : N32;
      Result : CuBit.Program_Signing.Signing_Result;
      Count : constant Admission.Certificate_Count := (if S.Intermediate_Length > 0 then 2 else 1);
   begin
      if Payload'Length > Admission.Max_File_Length - Admission.Envelopes.Max_Trailer_Length then
         raise Signing_Error with "the payload is too long to sign";
      end if;
      if Payload'First /= 0 then
         --  On the heap: a program can be hundreds of megabytes.
         declare
            Zero_Based : Bytes_Access := new SPARKNaCl.Byte_Seq (0 .. Payload'Length - 1);
         begin
            Zero_Based.all := Payload;
            return Result : constant SPARKNaCl.Byte_Seq := Sign (S, Zero_Based.all, Kind) do
               Free (Zero_Based);
            end return;
         end;
      end if;
      declare
         Zero_Based : SPARKNaCl.Byte_Seq renames Payload;
      begin
         CuBit.Program_Signing.Sign
           (Zero_Based, Kind, Leaf_DER (S), Count, Intermediate_DER (S), S.Key, Now, Mask, T, Length, Result);
         if Result /= CuBit.Program_Signing.Signed then
            raise Signing_Error with "not signed: " & Explain (Result);
         end if;
         declare
            Whole : Bytes_Access := new SPARKNaCl.Byte_Seq (0 .. Payload'Length + Length - 1);
            Anchors : Anchor_List (1 .. 1);
         begin
            Whole (0 .. Payload'Length - 1) := Zero_Based;
            Whole (Payload'Length .. Whole'Last) := T (0 .. Length - 1);
            Anchors (1) := new SPARKNaCl.Byte_Seq (0 .. N32 (S.Anchor_Length) - 1);
            for I in Anchors (1)'Range loop
               Anchors (1) (I) := S.Anchor (X509.N32 (I));
            end loop;
            declare
               Verdict : constant Admission.Verification := Verify (Whole.all, Anchors, Kind);
            begin
               Free (Whole);
               Free (Anchors (1));
               if not Verdict.Verified then
                  raise Signing_Error with "the signed file does not verify under "
                    & CA_Certificate_Name & ": " & Verdict.Reason'Image & "/" & Verdict.Detail'Image;
               end if;
            end;
         end;
         return T (0 .. Length - 1);
      end;
   end Sign;

   function Has_Footer (File : SPARKNaCl.Byte_Seq) return Boolean is
     (File'Length >= Admission.Footer_Length
      and then (for all I in Admission.Magic'Range =>
                  File (File'Last - Admission.Footer_Length + 1 + I) = Admission.Footer_Magic (I)));

   function Verify
     (File : SPARKNaCl.Byte_Seq; Anchors : Anchor_List;
      Kind : CuBit.Signed_Admission.Purpose := CuBit.Signed_Admission.Program)
      return CuBit.Signed_Admission.Verification
   is
      Trust : Admission.Anchor_Array (1 .. Anchors'Length);
      Revoked : constant Admission.Revocation_Set :=
        (Keys => 0, Certificates => 0, Programs => 0, others => <>);
   begin
      if File'First /= 0 or else File'Length > Admission.Max_File_Length then
         raise Signing_Error with "file too long to verify";
      end if;
      for I in Anchors'Range loop
         if Anchors (I)'Length not in Admission.Certificate_Length then
            raise Signing_Error with "an anchor certificate is empty or longer than"
              & Admission.Max_Certificate_Length'Image & " bytes";
         end if;
         declare
            A : Admission.Anchor renames Trust (I - Anchors'First + 1);
         begin
            A.Kind := Admission.Host_Development;
            A.Ceiling := Admission.Hardware_And_System;
            A.Length := X509.N32 (Anchors (I)'Length);
            A.DER (0 .. A.Length - 1) := To_X509 (Anchors (I).all);
            A.First_Prefix := 1;
            A.Last_Prefix := 1;
         end;
      end loop;
      return Admission.Decisions.Verify (File, Kind, Trust, Revoked);
   end Verify;

end Host_Signing;
