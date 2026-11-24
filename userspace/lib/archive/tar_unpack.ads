------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Unpacking a tar archive into a new directory (archive.unpack, SELF-003
--  step 1), over any store that has files and directories: the CuBit
--  filesystem service natively, the Linux host's directories in the hosted
--  tests.
--
--  @description
--  The temporary directory beside the target, ".DIRECTORY.unpacking", is
--  made first, so a target that may not be written is refused before the
--  archive is read. Then two passes: the first reads the archive whole
--  with the proved walker (Tar_Archives) and writes nothing, so a refused
--  archive leaves no trace; the second makes the tree, every path being
--  Parent/temporary/P for a Confined P, and renames it to DIRECTORY last.
--  Every file and directory it makes is recorded; when a write fails they
--  are removed, newest first, and so is the temporary directory. A directory already at
--  the target, or at the temporary name (a crashed unpack), is refused.
--
--  Not SPARK: the record of what was made grows on the heap. The paths it
--  makes come only from the walker's Confined paths.
------------------------------------------------------------------------------
pragma Ada_2022;
with Tar_Archives;

generic
   type Store is limited private;
   --  Up to Into'Length bytes of the archive from Offset; Last < Into'First
   --  at its end. Ok False: it could not be read.
   with procedure Read
     (S : in out Store; Offset : Tar_Archives.Byte_Count; Into : out String; Last : out Natural;
      Ok : out Boolean);
   --  A new directory; Ok False when it cannot be made (or exists).
   with procedure Make_Directory (S : in out Store; Path : String; Ok : out Boolean);
   --  A new file, open for writing (one at a time); Ok False when it
   --  exists or cannot be made.
   with procedure Create_File (S : in out Store; Path : String; Ok : out Boolean);
   with procedure Write (S : in out Store; Offset : Tar_Archives.Byte_Count; Data : String; Ok : out Boolean);
   with procedure Close_File (S : in out Store);
   with procedure Remove_File (S : in out Store; Path : String);
   with procedure Remove_Directory (S : in out Store; Path : String);
   with procedure Rename (S : in out Store; From, To : String; Ok : out Boolean);
   with function Exists (S : in out Store; Path : String) return Boolean;
package Tar_Unpack is

   type Outcome is
     (Unpacked,
      Unreadable,       --  the archive could not be read
      Target_Exists,    --  DIRECTORY, or its temporary name, is there already
      Bad_Directory,    --  DIRECTORY is not one name (no '/', ".", "..")
      Refused,          --  the reader refused the archive (Why, Entry_Path)
      Too_Large,        --  the reader's entry or byte ceiling (Why)
      Write_Failed);    --  making Failed_Path failed; what was made is gone

   type Report is record
      Ended_As : Outcome := Unreadable;
      Why : Tar_Archives.Refusal := Tar_Archives.No_Refusal;
      Entries : Tar_Archives.Entry_Count := 0;
      Bytes : Tar_Archives.Byte_Count := 0;
      Skipped : Natural := 0;
      --  The last entry the walker named (the refused one, on a refusal).
      Entry_Path : String (1 .. Tar_Archives.MAXIMUM_PATH_BYTES) := [others => ' '];
      Entry_Length : Tar_Archives.Path_Length := 0;
      --  Where a write failed.
      Failed_Path : String (1 .. 2 * Tar_Archives.MAXIMUM_PATH_BYTES) := [others => ' '];
      Failed_Length : Natural := 0;
   end record;

   --  The archive (read through S) as Parent/Directory.
   procedure Unpack
     (S : in out Store; Parent, Directory : String; Ceiling : Tar_Archives.Limits;
      Specials : Tar_Archives.Specials_Policy; Result : out Report);
end Tar_Unpack;
