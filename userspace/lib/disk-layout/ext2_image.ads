------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Makes an ext2 volume for the root, already holding the install's files
--  (the mkfs the installer needs, populating like mke2fs -d; a typed
--  library, not a Unix tool). Revision 1 with typed directory records,
--  sparse superblock backups and large files: exactly the profile the
--  filesystem service admits (Ext2_Support.Supported_Volume) and Linux
--  mounts. 4 KiB blocks, 256-byte inodes, one bitmap block per group.
--
--  Allocation is sequential, so every bitmap and count follows from three
--  numbers (Allocation): inodes 2 (root) and 11 (lost+found), then one per
--  directory of the tree, then one per file; blocks from group 0's first
--  data block onwards, skipping each group's metadata: lost+found's block,
--  the root's blocks, each directory's, then each file's indirect blocks
--  followed by its data.
--
--  Pure and proved free of run-time errors. e2fsck checks the result
--  (tests/disk-layout). A journal (ext3) is not made yet.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with Disk_Layout; use Disk_Layout;
with Ext2_Format;
with Install_Tree; use Install_Tree;

package Ext2_Image with SPARK_Mode, Pure is

   Block_Bytes      : constant := Ext2_Format.Maximum_Block_Bytes;   --  4096
   Inode_Bytes      : constant := 256;
   Inodes_Per_Block : constant := Block_Bytes / Inode_Bytes;
   --  One block bitmap block maps a group.
   Blocks_Per_Group : constant := Block_Bytes * Ext2_Format.Bits_Per_Byte;
   Descriptors_Per_Block : constant := Block_Bytes / Ext2_Format.Group_Descriptor_Bytes;
   Pointers_Per_Block : constant := Block_Bytes / Ext2_Format.Block_Pointer_Bytes;
   Direct_Blocks    : constant := 12;   --  i_block[0 .. 11]

   Root_Inode       : constant := Ext2_Format.Root_Inode;
   First_Inode      : constant := Ext2_Format.Good_Old_First_Inode;
   Lost_Found_Inode : constant := First_Inode;

   --  At most 2 ** 32 - 1 blocks (32-bit block numbers): 16 TiB.
   Maximum_Blocks : constant := 2 ** 32 - 1;
   Maximum_Groups : constant := (Maximum_Blocks + Blocks_Per_Group - 1) / Blocks_Per_Group;
   subtype Group_Index is Unsigned_32 range 0 .. Maximum_Groups - 1;
   subtype Group_Total is Unsigned_32 range 1 .. Maximum_Groups;

   --  Bytes of volume per inode: the caller's explicit choice (mke2fs's
   --  default is 16 KiB; fewer inodes mean less inode table to zero).
   subtype Bytes_Per_Inode is Unsigned_32 range 8192 .. 2 ** 24;

   subtype Volume_Name is String (1 .. 16);   --  NUL padded

   type Volume_Request is record
      Inode_Ratio : Bytes_Per_Inode := 16_384;
      Id          : GUID := [others => 0];
      Name        : Volume_Name := [others => ASCII.NUL];
      --  Seconds since the Unix epoch for every timestamp written.
      Created     : Unsigned_32 := 0;
   end record;

   type Volume is record
      Blocks            : Unsigned_32 := 0;
      Groups            : Group_Total := 1;
      Inodes_Per_Group  : Unsigned_32 := Inodes_Per_Block;
      Inode_Table_Blocks : Unsigned_32 := 1;
      Descriptor_Blocks : Unsigned_32 := 1;
   end record;

   --  A group past the last needs this much room beyond its metadata to be
   --  kept (as mke2fs does), else the volume ends a group early.
   Minimum_Group_Data : constant := 64;

   function Group_Overhead_Limit (Item : Volume) return Unsigned_64 is
     (1 + Unsigned_64 (Item.Descriptor_Blocks) + 2 + Unsigned_64 (Item.Inode_Table_Blocks));

   function Last_Group_Blocks (Item : Volume) return Unsigned_64 is
     (Unsigned_64 (Item.Blocks) - Unsigned_64 (Item.Groups - 1) * Blocks_Per_Group)
     with Pre => Unsigned_64 (Item.Blocks) > Unsigned_64 (Item.Groups - 1) * Blocks_Per_Group;

   function Valid (Item : Volume) return Boolean is
     (Item.Inodes_Per_Group in Inodes_Per_Block .. Blocks_Per_Group
      and then Item.Inodes_Per_Group mod Inodes_Per_Block = 0
      and then Item.Inode_Table_Blocks = Item.Inodes_Per_Group / Inodes_Per_Block
      and then Unsigned_64 (Item.Descriptor_Blocks) =
        (Unsigned_64 (Item.Groups) + Descriptors_Per_Block - 1) / Descriptors_Per_Block
      and then Unsigned_64 (Item.Blocks) > Unsigned_64 (Item.Groups - 1) * Blocks_Per_Group
      and then Unsigned_64 (Item.Blocks) <= Unsigned_64 (Item.Groups) * Blocks_Per_Group
      and then Group_Overhead_Limit (Item) + Minimum_Group_Data <= Last_Group_Blocks (Item)
      and then Unsigned_64 (Item.Groups) * Unsigned_64 (Item.Inodes_Per_Group)
                 <= Unsigned_64 (Unsigned_32'Last));

   function Plan (Partition_Bytes : Unsigned_64; Ratio : Bytes_Per_Inode)
     return Volume
     with Post => Unsigned_64 (Plan'Result.Blocks) * Block_Bytes <= Partition_Bytes;

   --  sparse_super: groups 0, 1 and powers of 3, 5 and 7 carry backups.
   function Has_Backup (Group : Group_Index) return Boolean;

   function Group_First (Group : Group_Index) return Unsigned_64 is
     (Unsigned_64 (Group) * Blocks_Per_Group);
   function Group_Blocks (Item : Volume; Group : Group_Index) return Unsigned_64 is
     (if Group = Item.Groups - 1 then Last_Group_Blocks (Item) else Blocks_Per_Group)
     with Pre => Valid (Item) and then Group < Item.Groups;
   function Block_Bitmap_Block (Item : Volume; Group : Group_Index) return Unsigned_64 is
     (Group_First (Group)
      + (if Has_Backup (Group) then 1 + Unsigned_64 (Item.Descriptor_Blocks) else 0));
   function Inode_Bitmap_Block (Item : Volume; Group : Group_Index) return Unsigned_64 is
     (Block_Bitmap_Block (Item, Group) + 1);
   function Inode_Table_Block (Item : Volume; Group : Group_Index) return Unsigned_64 is
     (Block_Bitmap_Block (Item, Group) + 2);
   function First_Data_Block (Item : Volume; Group : Group_Index) return Unsigned_64 is
     (Inode_Table_Block (Item, Group) + Unsigned_64 (Item.Inode_Table_Blocks));

   --  The data block after Block in allocation order (skipping the next
   --  group's metadata), and Steps blocks on. Past the last group they
   --  return the volume's block count (allocation then does not fit).
   function Next_Block (Item : Volume; Block : Unsigned_64) return Unsigned_64
     with Pre => Valid (Item) and then Block < Unsigned_64 (Item.Blocks),
          Post => Next_Block'Result <= Unsigned_64 (Item.Blocks);
   function Advance (Item : Volume; Block, Steps : Unsigned_64) return Unsigned_64
     with Pre => Valid (Item) and then Block <= Unsigned_64 (Item.Blocks),
          Post => Advance'Result <= Unsigned_64 (Item.Blocks);

   ---------------------------------------------------------------------------
   --  The files' layout.
   ---------------------------------------------------------------------------
   type Block_Numbers is array (Entry_Index) of Unsigned_64;
   type Inode_Numbers is array (Entry_Index) of Unsigned_32;
   type Entry_Map is array (Entry_Index) of Entry_Index;

   type Allocation is record
      --  One past the last block allocated, in allocation order.
      Data_End    : Unsigned_64 := 0;
      --  The highest inode in use (First_Inode: none but the reserved ones).
      Last_Inode  : Unsigned_32 := First_Inode;
      --  Directory inodes First_Inode + 1 .. First_Inode + Directories.
      Directories : Unsigned_32 := 0;
   end record;

   type Tree_Layout is record
      --  The tree fits the volume: blocks, inodes, directories of at most
      --  Direct_Blocks blocks, files of at most what double indirection maps.
      Fits         : Boolean := False;
      Used         : Allocation;
      Lost_Found   : Unsigned_64 := 0;
      Root_Start   : Unsigned_64 := 0;
      Root_Blocks  : Unsigned_64 := 1;
      Inode        : Inode_Numbers := [others => 0];
      Start        : Block_Numbers := [others => 0];
      --  Directories: their blocks. Files: data blocks, after Meta indirect.
      Data         : Block_Numbers := [others => 0];
      Meta         : Block_Numbers := [others => 0];
      --  Which entry inode First_Inode + I is.
      By_Inode     : Entry_Map := [others => Entry_Index'First];
   end record;

   function Data_Blocks (Size : File_Size) return Unsigned_64 is
     ((Size + Block_Bytes - 1) / Block_Bytes);

   --  Indirect blocks a file of Count data blocks needs.
   function Indirect_Blocks (Count : Unsigned_64) return Unsigned_64 is
     (if Count <= Direct_Blocks then 0
      elsif Count <= Direct_Blocks + Pointers_Per_Block then 1
      else 2 + (Count - Direct_Blocks - Pointers_Per_Block + Pointers_Per_Block - 1)
                 / Pointers_Per_Block)
     with Pre => Count <= Data_Blocks (Maximum_File_Bytes);

   procedure Plan_Tree (Item : Volume; Files : Tree; Result : out Tree_Layout)
     with Pre => Valid (Item);

   subtype Block_Image is Bytes (1 .. Block_Bytes);
   subtype Superblock_Image is Bytes (1 .. Ext2_Format.Superblock_Bytes);

   --  The superblock copy kept in Group (s_block_group_nr).
   function Superblock
     (Item : Volume; Layout : Tree_Layout; Request : Volume_Request; Group : Group_Index)
      return Superblock_Image
     with Pre => Valid (Item) and then Layout.Fits and then Group < Item.Groups;

   --  Descriptor block Index (0-based) of the group descriptor table.
   function Descriptor_Block
     (Item : Volume; Layout : Tree_Layout; Index : Unsigned_32) return Block_Image
     with Pre => Valid (Item) and then Layout.Fits and then Index < Item.Descriptor_Blocks;

   function Block_Bitmap
     (Item : Volume; Layout : Tree_Layout; Group : Group_Index) return Block_Image
     with Pre => Valid (Item) and then Layout.Fits and then Group < Item.Groups;
   function Inode_Bitmap
     (Item : Volume; Layout : Tree_Layout; Group : Group_Index) return Block_Image
     with Pre => Valid (Item) and then Layout.Fits and then Group < Item.Groups;

   --  How many of Group's inode table blocks hold inodes in use (the rest
   --  are zero).
   function Used_Table_Blocks
     (Item : Volume; Layout : Tree_Layout; Group : Group_Index) return Unsigned_32
     with Pre => Valid (Item) and then Layout.Fits and then Group < Item.Groups,
          Post => Used_Table_Blocks'Result <= Item.Inode_Table_Blocks;

   function Inode_Table
     (Item : Volume; Layout : Tree_Layout; Files : Tree; Request : Volume_Request;
      Group : Group_Index; Index : Unsigned_32) return Block_Image
     with Pre => Valid (Item) and then Layout.Fits and then Group < Item.Groups
                 and then Index < Item.Inode_Table_Blocks;

   --  Block Index of a directory (Volume_Root: the root).
   function Directory_Block
     (Layout : Tree_Layout; Files : Tree; Directory : Parent_Index; Index : Unsigned_64)
      return Block_Image
     with Pre => Layout.Fits and then Directory <= Files.Count;

   function Lost_Found_Directory return Block_Image;

   --  A file's indirect block Index (0 .. Meta - 1): its single indirect
   --  block, its double indirect block, then the latter's children.
   function Indirect_Block
     (Item : Volume; Layout : Tree_Layout; File : Entry_Index; Index : Unsigned_64)
      return Block_Image
     with Pre => Valid (Item) and then Layout.Fits;

end Ext2_Image;
