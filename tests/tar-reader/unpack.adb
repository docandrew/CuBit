------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  unpack ARCHIVE PARENT DIRECTORY [skip]: Tar_Unpack over the Linux host's
--  directories, printing the outcome ("UNPACKED entries bytes skipped" or
--  the refusal), for run.sh to compare the tree it made (or that nothing
--  was left). A Linux-hosted check of the unpack logic, not CuBit: on
--  CuBit the same generic runs over the filesystem service.
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Directories;
with Ada.Streams.Stream_IO;
with Ada.Text_IO; use Ada.Text_IO;
with Tar_Archives; use Tar_Archives;
with Tar_Unpack;

procedure Unpack is
   use Ada.Streams;
   package Dirs renames Ada.Directories;

   type Host_Store is limited record
      Archive : Stream_IO.File_Type;
      Output : Stream_IO.File_Type;
   end record;

   procedure Read (S : in out Host_Store; Offset : Byte_Count; Into : out String; Last : out Natural;
                   Ok : out Boolean) is
      Raw : Stream_Element_Array (1 .. Into'Length);
      Got : Stream_Element_Offset;
   begin
      Into := [others => ' '];
      Last := Into'First - 1;
      Ok := True;
      if Offset >= Byte_Count (Stream_IO.Size (S.Archive)) then
         return;
      end if;
      Stream_IO.Set_Index (S.Archive, Stream_IO.Positive_Count (Offset + 1));
      Stream_IO.Read (S.Archive, Raw, Got);
      for K in 1 .. Natural (Got) loop
         Into (Into'First + K - 1) := Character'Val (Raw (Stream_Element_Offset (K)));
      end loop;
      Last := Into'First + Natural (Got) - 1;
   exception
      when others => Ok := False;
   end Read;

   procedure Make_Directory (S : in out Host_Store; Path : String; Ok : out Boolean) is
      pragma Unreferenced (S);
   begin
      Ok := not Dirs.Exists (Path);
      if Ok then
         Dirs.Create_Directory (Path);
      end if;
   exception
      when others => Ok := False;
   end Make_Directory;

   procedure Create_File (S : in out Host_Store; Path : String; Ok : out Boolean) is
   begin
      Ok := not Dirs.Exists (Path);
      if Ok then
         Stream_IO.Create (S.Output, Stream_IO.Out_File, Path);
      end if;
   exception
      when others => Ok := False;
   end Create_File;

   procedure Write (S : in out Host_Store; Offset : Byte_Count; Data : String; Ok : out Boolean) is
      Raw : Stream_Element_Array (1 .. Data'Length);
   begin
      for K in Data'Range loop
         Raw (Stream_Element_Offset (K - Data'First + 1)) := Character'Pos (Data (K));
      end loop;
      Stream_IO.Set_Index (S.Output, Stream_IO.Positive_Count (Offset + 1));
      Stream_IO.Write (S.Output, Raw);
      Ok := True;
   exception
      when others => Ok := False;
   end Write;

   procedure Close_File (S : in out Host_Store) is
   begin
      Stream_IO.Close (S.Output);
   end Close_File;

   procedure Remove_File (S : in out Host_Store; Path : String) is
      pragma Unreferenced (S);
   begin
      Dirs.Delete_File (Path);
   exception
      when others => null;
   end Remove_File;

   procedure Remove_Directory (S : in out Host_Store; Path : String) is
      pragma Unreferenced (S);
   begin
      Dirs.Delete_Directory (Path);
   exception
      when others => null;
   end Remove_Directory;

   procedure Rename (S : in out Host_Store; From, To : String; Ok : out Boolean) is
      pragma Unreferenced (S);
   begin
      Dirs.Rename (From, To);
      Ok := True;
   exception
      when others => Ok := False;
   end Rename;

   function Exists (S : in out Host_Store; Path : String) return Boolean is
      pragma Unreferenced (S);
   begin
      return Dirs.Exists (Path);
   end Exists;

   package Host_Unpack is new Tar_Unpack
     (Host_Store, Read, Make_Directory, Create_File, Write, Close_File, Remove_File, Remove_Directory,
      Rename, Exists);

   S : Host_Store;
   R : Host_Unpack.Report;
   use type Host_Unpack.Outcome;
begin
   Stream_IO.Open (S.Archive, Stream_IO.In_File, Argument (1));
   Host_Unpack.Unpack
     (S, Argument (2), Argument (3), (Maximum_Entries => 1_000, Maximum_Bytes => 2**30),
      (if Argument_Count >= 4 and then Argument (4) = "skip" then Skip_Specials else Refuse_Specials), R);
   Stream_IO.Close (S.Archive);
   if R.Ended_As = Host_Unpack.Unpacked then
      Put_Line ("UNPACKED" & R.Entries'Image & R.Bytes'Image & R.Skipped'Image);
   else
      Put_Line (R.Ended_As'Image & " " & R.Why'Image & " " & R.Entry_Path (1 .. R.Entry_Length));
   end if;
end Unpack;
