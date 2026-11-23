------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  git objects (SELF-003 step 2): their names, and what a checkout reads
--  from untrusted commits and trees.
--
--  @description
--  git's documentation (gitformat-pack, git-cat-file, gitformat-index's
--  tree notes); facts only. An object's name is the SHA-1 of
--  "<type> <decimal size>" NUL and its bytes (SPARKTLSCrypto's SHA-1). A
--  commit begins "tree <40 hex digits>" LF. A tree is a run of entries
--  "<octal mode> <name>" NUL and a 20-byte name, sorted by name (a
--  directory's name compared as if it ended in '/').
--
--  The tree reader hands out only Safe_Name names: one path component of
--  printable ASCII, not "." or "..", without '/' or '\', and not ".git"
--  in any letter case, so a checkout writes only below its directory
--  and never into a repository's own metadata. Modes are the five git
--  writes (100644, 100755, 120000, 40000, 160000); others are refused.
--  Names must strictly increase in git's order, so there are no
--  duplicates.
--
--  SHA-1 because git's object format "sha1" is what servers speak
--  (GitHub has no SHA-256 repositories). SPARKTLSCrypto scopes its SHA-1
--  unit to OCSP identifiers; naming git objects is a second identifier
--  use, pending the crate owner's decision. git itself hashes with
--  SHA-1DC (collision detection); this does not.
--
--  Proved at level 2 (tests/git-codecs): no run-time error; every entry
--  handed out lies inside the tree and has a Safe_Name; the reader moves
--  forward.
------------------------------------------------------------------------------
pragma Ada_2022;
with Inflate_Streams; use Inflate_Streams;
with Git_Protocol; use Git_Protocol;
with Git_Packs;
private with Git_Hashes;

package Git_Objects with SPARK_Mode is
   subtype Whole_Kind is Git_Packs.Whole_Kind;

   function Type_Name (Kind : Whole_Kind) return String is
     (case Kind is
         when Git_Packs.Commit_Object => "commit",
         when Git_Packs.Tree_Object => "tree",
         when Git_Packs.Blob_Object => "blob",
         when Git_Packs.Tag_Object => "tag");

   --  The decimal digits of N.
   function Decimal (N : Byte_Count) return String
   with Post => Decimal'Result'Length in 1 .. 19;

   ---------------------------------------------------------------------------
   --  Object names.

   type Namer is private;

   procedure Start (H : out Namer; Format : Object_Format; Kind : Whole_Kind; Size : Byte_Count);
   procedure Update (H : in out Namer; Data : Byte_Array)
   with Pre => Data'Last < Positive'Last - 128;
   procedure Finish (H : in out Namer; Id : out Object_Id);

   ---------------------------------------------------------------------------
   --  Commits.

   --  The tree a commit names on its first line.
   procedure Commit_Tree
     (Commit : Byte_Array; Format : Object_Format; Tree : out Object_Id; Found : out Boolean)
   with Pre => Commit'Last < Positive'Last;

   ---------------------------------------------------------------------------
   --  Trees.

   MAXIMUM_NAME_BYTES : constant := 255;

   function Safe_Name (S : String) return Boolean is
     (S'Length in 1 .. MAXIMUM_NAME_BYTES
      and then (for all C of S => C in ' ' .. '~' and then C /= '/' and then C /= '\')
      and then S /= "." and then S /= ".."
      and then not (S'Length = 4 and then S (S'First) = '.'
                    and then S (S'First + 1) in 'g' | 'G' and then S (S'First + 2) in 'i' | 'I'
                    and then S (S'First + 3) in 't' | 'T'));

   type Entry_Mode is (Regular_File, Executable_File, Symbolic_Link, Directory, Gitlink);

   type Tree_Refusal is
     (No_Refusal,
      Bad_Mode,         --  not one of the five modes
      Unsafe_Name,      --  not Safe_Name
      Out_Of_Order,     --  not after the entry before it (or the same name)
      Truncated_Entry); --  the tree ends inside an entry

   function Name (Why : Tree_Refusal) return String is
     (case Why is
         when No_Refusal => "No_Refusal",
         when Bad_Mode => "Bad_Mode",
         when Unsafe_Name => "Unsafe_Name",
         when Out_Of_Order => "Out_Of_Order",
         when Truncated_Entry => "Truncated_Entry");

   type Tree_Entry is record
      Mode : Entry_Mode := Regular_File;
      Name_First : Positive := 1;
      Name_Last : Natural := 0;
      Id : Object_Id := [others => 0];
   end record;

   function Name_Text (Tree : Byte_Array; E : Tree_Entry) return String
   with Pre => E.Name_First <= E.Name_Last and then E.Name_First in Tree'Range and then E.Name_Last in Tree'Range,
        Post => Name_Text'Result'Length = E.Name_Last - E.Name_First + 1;

   --  The entry at Pos, checked against the one before it (Previous, or
   --  none when Has_Previous is false); Pos moves past it.
   procedure Next_Entry
     (Tree : Byte_Array;
      Format : Object_Format;
      Pos : in out Positive;
      Has_Previous : Boolean;
      Previous : Tree_Entry;
      E : out Tree_Entry;
      Why : out Tree_Refusal)
   with
     Pre => Tree'Last < Positive'Last and then Pos in Tree'Range
       and then (if Has_Previous then Previous.Name_First <= Previous.Name_Last
                   and then Previous.Name_First in Tree'Range and then Previous.Name_Last in Tree'Range),
     Post => (if Why = No_Refusal then
                Pos in Pos'Old + 1 .. Tree'Last + 1
                and then E.Name_First <= E.Name_Last
                and then E.Name_First in Tree'Range and then E.Name_Last in Tree'Range
                and then Safe_Name (Name_Text (Tree, E)));
private
   type Namer is record
      Hash : Git_Hashes.Context;
   end record;
end Git_Objects;
