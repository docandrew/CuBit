------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The files an install puts on a freshly formatted volume (the image
--  closure on the root, the boot files on the ESP): a tree of directories
--  and files with their sizes, parents before children. The formatters lay
--  it out at format time (as mke2fs -d does) and stream each file's bytes
--  from a generic source; nothing is held in memory but the names.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package Install_Tree with SPARK_Mode, Pure is

   --  A named ceiling, not a fit: the device-build image installs its
   --  toolchain, Storehouse and source subset (about 3,500 entries) beside
   --  the image closure; LIM-001.
   Maximum_Entries : constant := 16_384;
   subtype Entry_Count is Natural range 0 .. Maximum_Entries;
   subtype Entry_Index is Positive range 1 .. Maximum_Entries;
   --  An entry's parent: 0 is the volume's root directory.
   subtype Parent_Index is Natural range 0 .. Maximum_Entries;
   Volume_Root : constant Parent_Index := 0;

   Maximum_Name : constant := 255;   --  ext2's limit; FAT names are 8.3
   subtype Name_Length is Natural range 1 .. Maximum_Name;

   --  At most what ext2 reaches without triple indirect blocks (4 GiB).
   Maximum_File_Bytes : constant := 2 ** 32;
   subtype File_Size is Unsigned_64 range 0 .. Maximum_File_Bytes;

   type Entry_Kind is (Directory, Regular_File);

   --  Names live in one pool per tree (an average of 64 bytes each), so a
   --  full tree stays small (a few hundred KiB).
   Pool_Bytes : constant := Maximum_Entries * 64;
   subtype Pool_Index is Positive range 1 .. Pool_Bytes;

   type Tree_Entry is record
      Kind   : Entry_Kind := Regular_File;
      --  A regular file that runs (its mode 0755, not 0644): a tree's
      --  identity covers it (ccl-build's toolchain pin).
      Executable : Boolean := False;
      Parent : Parent_Index := Volume_Root;
      First  : Pool_Index := 1;
      Length : Name_Length := 1;
      Size   : File_Size := 0;
   end record;

   type Entry_Array is array (Entry_Index) of Tree_Entry;

   type Tree is record
      Count   : Entry_Count := 0;
      Entries : Entry_Array;
      Used    : Natural range 0 .. Pool_Bytes := 0;
      Names   : String (Pool_Index) := [others => ' '];
   end record;

   --  Entry E's name.
   function Name_Of (Item : Tree; E : Entry_Index) return String is
     (if Item.Entries (E).First <= Pool_Bytes - Item.Entries (E).Length + 1
      then Item.Names (Item.Entries (E).First .. Item.Entries (E).First + Item.Entries (E).Length - 1)
      else "?");

   function Parent_Valid (Item : Tree; Parent : Parent_Index) return Boolean is
     (Parent = Volume_Root
      or else (Parent <= Item.Count and then Item.Entries (Parent).Kind = Directory));

   --  Add keeps every tree well formed: parents come first and are
   --  directories; names are printable ASCII without '/', are not "." or
   --  "..", and are unique within their directory.
   type Add_Result is (Added, Tree_Full, Bad_Name, Bad_Parent);

   procedure Add
     (Item : in out Tree; Kind : Entry_Kind; Parent : Parent_Index;
      Name : String; Size : File_Size; Result : out Add_Result;
      Executable : Boolean := False)
     with Pre => Item.Count <= Maximum_Entries,
          Post => (if Result = Added then Item.Count = Item.Count'Old + 1
                   else Item.Count = Item.Count'Old);

end Install_Tree;
