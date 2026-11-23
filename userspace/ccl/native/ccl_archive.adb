------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  archive.unpack on CuBit: Tar_Unpack over the filesystem service
--  (CCL_Place_Files), which checks every path against this program's
--  filesystem scope on every request.
------------------------------------------------------------------------------
with Tar_Archives;
with Tar_Unpack;
with CCL_Place_Files;

package body CCL_Archive is
   package Files renames CCL_Place_Files;
   use type Files.Result_Kind;
   use type Tar_Archives.Byte_Count;
   use type Tar_Archives.Refusal;

   function To_Text (Item : String) return Text is
      N : constant Natural := Natural'Min (Item'Length, MAXIMUM_TEXT);
      Result : Text;
   begin
      Result.Data (1 .. N) := Item (Item'First .. Item'First + N - 1);
      Result.Length := N;
      return Result;
   end To_Text;

   type Store is limited record
      Archive, Output : Files.File;
      --  What the last failed request said, for the report.
      Last_Failure : Files.Result_Kind := Files.Done;
   end record;

   procedure Note (S : in out Store; Result : Files.Result_Kind) is
   begin
      if Result /= Files.Done then
         S.Last_Failure := Result;
      end if;
   end Note;

   procedure Read
     (S : in out Store; Offset : Tar_Archives.Byte_Count; Into : out String; Last : out Natural; Ok : out Boolean)
   is
      Got : Natural;
      Result : Files.Result_Kind;
   begin
      Into := [others => ' '];
      Files.Read (S.Archive, Unsigned_64 (Offset), Into'Address, Into'Length, Got, Result);
      Note (S, Result);
      Ok := Result = Files.Done;
      Last := Into'First + (if Ok then Got else 0) - 1;
   end Read;

   procedure Make_Directory (S : in out Store; Path : String; Ok : out Boolean) is
      Result : Files.Result_Kind;
   begin
      Files.Make_Directory (Path, Result);
      Note (S, Result);
      Ok := Result = Files.Done;
   end Make_Directory;

   procedure Create_File (S : in out Store; Path : String; Ok : out Boolean) is
      Result : Files.Result_Kind;
   begin
      Files.Create (Path, Exclusive => True, Item => S.Output, Result => Result);
      Note (S, Result);
      Ok := Result = Files.Done;
   end Create_File;

   procedure Write (S : in out Store; Offset : Tar_Archives.Byte_Count; Data : String; Ok : out Boolean) is
      Result : Files.Result_Kind;
   begin
      Files.Write (S.Output, Unsigned_64 (Offset), Data'Address, Data'Length, Result);
      Note (S, Result);
      Ok := Result = Files.Done;
   end Write;

   procedure Close_File (S : in out Store) is
   begin
      Files.Close (S.Output);
   end Close_File;

   procedure Remove_File (S : in out Store; Path : String) is
      pragma Unreferenced (S);
      Ignored : Files.Result_Kind;
   begin
      Files.Remove_File (Path, Ignored);
   end Remove_File;

   procedure Remove_Directory (S : in out Store; Path : String) is
      pragma Unreferenced (S);
      Ignored : Files.Result_Kind;
   begin
      Files.Remove_Directory (Path, Ignored);
   end Remove_Directory;

   procedure Rename (S : in out Store; From, To : String; Ok : out Boolean) is
      Result : Files.Result_Kind;
   begin
      Files.Rename (From, To, Result);
      Note (S, Result);
      Ok := Result = Files.Done;
   end Rename;

   function Exists (S : in out Store; Path : String) return Boolean is
      pragma Unreferenced (S);
   begin
      return Files.Exists (Path);
   end Exists;

   package Native_Unpack is new Tar_Unpack
     (Store, Read, Make_Directory, Create_File, Write, Close_File, Remove_File, Remove_Directory, Rename,
      Exists);
   use type Native_Unpack.Outcome;

   function Joined (Root, Path : String) return String is
     (if Path'Length = 0 then Root else Root & "/" & Path);

   --  A report's text: printable ASCII, anything else as '?'.
   function Printable (Item : String) return String is
      Result : String := Item;
   begin
      for C of Result loop
         if C not in ' ' .. '~' then
            C := '?';
         end if;
      end loop;
      return Result;
   end Printable;

   procedure Unpack
     (From_Root, From_Path, Name, Into_Root, Into_Path, Directory : String; Skip_Specials : Boolean;
      Max_Entries : Natural; Max_Bytes : Unsigned_64; Result : out Report; Available : out Boolean)
   is
      S : Store;
      Opened : Files.Result_Kind;
      Source : constant String := Joined (Joined (From_Root, From_Path), Name);
      Parent : constant String := Joined (Into_Root, Into_Path);
      R : Native_Unpack.Report;
   begin
      Result := (others => <>);
      Available := True;
      Files.Open_Read (Source, S.Archive, Opened);
      if Opened /= Files.Done then
         Result.Ended_As := (if Opened = Files.Not_Granted then Not_Granted else Not_Found);
         Result.Detail := To_Text
           ((if Opened = Files.Not_Granted then Source & " is outside this program's readable places"
             elsif Opened = Files.Unavailable then "the filesystem service is not available"
             else "nothing is at " & Source));
         return;
      end if;
      Native_Unpack.Unpack
        (S, Parent, Directory,
         (Maximum_Entries => Max_Entries,
          Maximum_Bytes => Tar_Archives.Byte_Count (Unsigned_64'Min (Max_Bytes, 2 ** 62))),
         (if Skip_Specials then Tar_Archives.Skip_Specials else Tar_Archives.Refuse_Specials), R);
      Files.Close (S.Archive);
      Result.Entries := Unsigned_64 (R.Entries);
      Result.Bytes := Unsigned_64 (R.Bytes);
      Result.Skipped := R.Skipped;
      Result.Entry_Name := To_Text (Printable (R.Entry_Path (1 .. R.Entry_Length)));
      if R.Why /= Tar_Archives.No_Refusal then
         Result.Reason := To_Text (Tar_Archives.Name (R.Why));
      end if;
      case R.Ended_As is
         when Native_Unpack.Unpacked =>
            Result.Ended_As := Unpacked;
            Result.Detail := To_Text (Parent & "/" & Directory);
         when Native_Unpack.Unreadable =>
            Result.Ended_As := Not_Found;
            Result.Detail := To_Text ("the archive " & Source & " could not be read");
         when Native_Unpack.Target_Exists =>
            Result.Ended_As := Exists;
            Result.Detail := To_Text (Parent & "/" & Directory & " (or its .unpacking name) exists");
         when Native_Unpack.Bad_Directory =>
            Result.Ended_As := Refused;
            Result.Detail := To_Text ("directory must be one name: no '/', '.' or '..'");
         when Native_Unpack.Refused =>
            Result.Ended_As := Refused;
            Result.Detail := To_Text ("the archive was refused; nothing was written");
         when Native_Unpack.Too_Large =>
            Result.Ended_As := Too_Large;
            Result.Detail := To_Text ("the archive passes max_entries or max_bytes; nothing was written");
         when Native_Unpack.Write_Failed =>
            Result.Ended_As := (if S.Last_Failure = Files.Not_Granted then Not_Granted else Write_Failed);
            Result.Detail := To_Text
              ((if S.Last_Failure = Files.Not_Granted then Parent & " is outside this program's writable places"
                else "writing " & Printable (R.Failed_Path (1 .. R.Failed_Length)) & " failed") &
               "; what was made is removed");
      end case;
   end Unpack;
end CCL_Archive;
