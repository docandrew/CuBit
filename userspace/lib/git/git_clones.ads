------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Writing a clone (SELF-003 step 2, git.clone): from the refs ls-refs
--  listed and the pack fetch answered, a repository git accepts and a
--  checkout of its default branch, in a new directory of any store (the
--  CuBit filesystem service natively, the Linux host's directories in the
--  hosted tests).
--
--  @description
--  Like Tar_Unpack: everything is made under ".DIRECTORY.cloning" beside
--  the target, created first, so a target that may not be written is
--  refused before anything is fetched; it is renamed to DIRECTORY last.
--  On any failure what was made is removed, newest first.
--
--  The pack goes to .git/objects/pack as it arrives, through the proved
--  pack reader (Git_Packs), which keeps every entry's inflated bytes in
--  memory under the caller's byte ceiling. At the end the deltas are
--  resolved (Git_Deltas), every object is named (Git_Objects), every ref
--  must name an object of the pack, and then it writes:
--  * the pack's index, version 2 (gitformat-pack, "Version 2 pack-*.idx
--    files"), naming the pack pack-<checksum>;
--  * HEAD (the server's HEAD symref, or its commit, detached), the
--    default branch as a loose ref, every other ref in packed-refs
--    (branches as refs/remotes/origin/*, tags with their peeled ids);
--  * config: remote origin with the URL, and core.filemode and
--    core.symlinks false (CuBit files have no mode bits and no links, so
--    executables are plain files and links are files holding their
--    target, as git checks them out on such file systems);
--  * the default branch's tree, read by the proved tree reader, so every
--    name is one Safe_Name component: nothing is written outside the
--    directory or into .git; submodules become empty directories;
--  * the index (gitformat-index, version 2) with no stat data, so git
--    compares the files' contents the first time it looks.
--
--  Not SPARK: the objects and the record of what was made grow on the
--  heap, under the caller's ceilings. Every untrusted byte goes through
--  the proved codecs first.
------------------------------------------------------------------------------
pragma Ada_2022;
private with Interfaces;
private with Git_Lists;
private with Git_Strings;
with Inflate_Streams; use Inflate_Streams;
with Git_Protocol; use Git_Protocol;
with Git_Packs;

generic
   type Store is limited private;
   --  A new directory; Ok False when it cannot be made (or exists).
   with procedure Make_Directory (S : in out Store; Path : String; Ok : out Boolean);
   --  A new file, open for writing (one at a time); Ok False when it
   --  exists or cannot be made.
   with procedure Create_File (S : in out Store; Path : String; Ok : out Boolean);
   with procedure Write (S : in out Store; Offset : Byte_Count; Data : Byte_Array; Ok : out Boolean);
   with procedure Close_File (S : in out Store);
   with procedure Remove_File (S : in out Store; Path : String);
   with procedure Remove_Directory (S : in out Store; Path : String);
   with procedure Rename (S : in out Store; From, To : String; Ok : out Boolean);
   with function Exists (S : in out Store; Path : String) return Boolean;
package Git_Clones is

   type Limits is record
      Maximum_Objects : Git_Packs.Object_Count := 0;
      --  The pack, and every object inflated, together.
      Maximum_Bytes : Byte_Count := 0;
   end record;

   type Outcome is
     (In_Progress,
      Cloned,
      Target_Exists,      --  DIRECTORY, or its temporary name, is there already
      Bad_Directory,      --  DIRECTORY is not one Safe_Name component
      Bad_Url,            --  the URL has a byte config could not hold (space, control, '"', '\')
      Write_Failed,       --  making Failed_Path failed; what was made is gone
      Bad_Pack,           --  the pack reader refused the pack (Pack_Why)
      Bad_Delta,          --  a delta was refused, or its base is not in the pack
      Too_Many_Objects,   --  past the ceiling
      Too_Large,          --  past the byte ceiling
      Missing_Object,     --  a ref, commit or tree names an object not in the pack
      Bad_Object,         --  a commit without a tree line, or a tree the reader refused (Tree_Why)
      Path_Too_Long,      --  a checked-out path past 4096 bytes
      No_Refs);           --  the server listed no refs (an empty repository)

   function Name (O : Outcome) return String is
     (case O is
         when In_Progress => "In_Progress",
         when Cloned => "Cloned",
         when Target_Exists => "Target_Exists",
         when Bad_Directory => "Bad_Directory",
         when Bad_Url => "Bad_Url",
         when Write_Failed => "Write_Failed",
         when Bad_Pack => "Bad_Pack",
         when Bad_Delta => "Bad_Delta",
         when Too_Many_Objects => "Too_Many_Objects",
         when Too_Large => "Too_Large",
         when Missing_Object => "Missing_Object",
         when Bad_Object => "Bad_Object",
         when Path_Too_Long => "Path_Too_Long",
         when No_Refs => "No_Refs");

   type Clone is limited private;

   function Result (C : Clone) return Outcome;
   function Detail (C : Clone) return String;
   function Objects (C : Clone) return Natural;
   function Files (C : Clone) return Natural;

   --  Make ".DIRECTORY.cloning" and its .git under Parent. The object
   --  format is SHA-1 until Set_Format says otherwise.
   procedure Start (C : in out Clone; S : in out Store; Parent, Directory, Url : String; Ceiling : Limits);

   --  The repository's object format, as the server advertised it: before
   --  any pack data. A SHA-256 clone is a repository of format version 1
   --  with extensions.objectformat = sha256, its pack index, index and
   --  names all of 32-byte object names.
   procedure Set_Format (C : in out Clone; Format : Object_Format);
   function Format (C : Clone) return Object_Format;

   --  A ref from ls-refs (in the order listed). HEAD's symref target, if
   --  any, is the default branch.
   procedure Add_Ref
     (C : in out Clone; Name : String; Id : Object_Id; Symref : String; Has_Peeled : Boolean; Peeled : Object_Id)
   with Pre => Safe_Ref_Name (Name) and then (Symref'Length = 0 or else Safe_Ref_Name (Symref));

   --  The next bytes of the pack (fetch's channel 1).
   procedure Pack_Data (C : in out Clone; S : in out Store; Data : Byte_Array)
   with Pre => Data'Last < Positive'Last - 128;

   --  The fetch ended: index, refs, checkout, and the rename. Result says
   --  how it ended.
   procedure Finish (C : in out Clone; S : in out Store);

   --  Remove everything made (a failure elsewhere, a cancel).
   procedure Abandon (C : in out Clone; S : in out Store);
private
   use Git_Strings;

   type Bytes_Access is access Byte_Array;

   type Pack_Entry is record
      Kind : Git_Packs.Object_Kind := Git_Packs.Blob_Object;
      Offset : Git_Packs.Pack_Offset := 0;
      Base_Offset : Git_Packs.Pack_Offset := 0;
      Base_Id : Object_Id := [others => 0];
      CRC : Interfaces.Unsigned_32 := 0;
      Data : Bytes_Access;
      Resolved : Boolean := False;
      Whole : Git_Packs.Whole_Kind := Git_Packs.Blob_Object;
      Content : Bytes_Access;
      Id : Object_Id := [others => 0];
   end record;
   package Entry_Lists is new Git_Lists (Pack_Entry);
   --  Entries by object name: an open-addressed table of entry positions
   --  (0: empty), a power of two at least twice as large as the names in it.
   package Position_Lists is new Git_Lists (Natural);

   type Ref_Record is record
      Name : Text;
      Id : Object_Id;
      Has_Peeled : Boolean := False;
      Peeled : Object_Id := [others => 0];
   end record;
   package Ref_Lists is new Git_Lists (Ref_Record);

   type Made_Kind is (Made_File, Made_Directory);
   type Made_Record is record
      Kind : Made_Kind;
      Path : Text;
   end record;
   package Made_Lists is new Git_Lists (Made_Record);

   type Index_Record is record
      Path : Text;
      Mode : Natural;
      Size : Byte_Count;
      Id : Object_Id;
   end record;
   package Index_Lists is new Git_Lists (Index_Record);

   type Clone is limited record
      Result : Outcome := In_Progress;
      Detail : Text;
      Parent : Text;
      Directory : Text;
      Temporary : Text;
      Url : Text;
      Ceiling : Limits;
      Format : Object_Format := Sha1_Format;
      Reader : Git_Packs.Pack_Reader;
      Pack_Open : Boolean := False;
      Pack_Written : Byte_Count := 0;
      Held : Byte_Count := 0;
      Current : Bytes_Access;
      Filled : Natural := 0;
      Entries : Entry_Lists.List;
      Id_Slots : Position_Lists.List;
      Named_Count : Natural := 0;
      Refs : Ref_Lists.List;
      Head_Target : Text;
      Has_Head : Boolean := False;
      Head_Id : Object_Id := [others => 0];
      Made : Made_Lists.List;
      Index : Index_Lists.List;
      Files : Natural := 0;
   end record;
end Git_Clones;
