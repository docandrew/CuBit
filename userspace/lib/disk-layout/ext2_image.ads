------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Makes an empty ext2 volume for the root (the mkfs the installer needs;
--  a typed library, not a Unix tool). Revision 1 with typed directory
--  records, sparse superblock backups and large files: exactly the profile
--  the filesystem service admits (Ext2_Support.Supported_Volume) and Linux
--  mounts. 4 KiB blocks, 256-byte inodes, one bitmap block per group. The
--  root directory (inode 2) holds lost+found (inode 11).
--
--  Pure and proved free of run-time errors. e2fsck checks the result
--  (tests/disk-layout). A journal (ext3) is not made yet.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with Disk_Layout; use Disk_Layout;
with Ext2_Format;

package Ext2_Image with SPARK_Mode, Pure is

   Block_Bytes      : constant := Ext2_Format.Maximum_Block_Bytes;   --  4096
   Inode_Bytes      : constant := 256;
   Inodes_Per_Block : constant := Block_Bytes / Inode_Bytes;
   --  One block bitmap block maps a group.
   Blocks_Per_Group : constant := Block_Bytes * Ext2_Format.Bits_Per_Byte;
   Descriptors_Per_Block : constant := Block_Bytes / Ext2_Format.Group_Descriptor_Bytes;

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

   --  The most metadata a group carries: a superblock and descriptor copy,
   --  two bitmaps, its inode table.
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

   --  The volume for a partition of Partition_Bytes. Valid (Plan) when the
   --  partition holds at least one group's metadata and Minimum_Group_Data.
   function Plan (Partition_Bytes : Unsigned_64; Ratio : Bytes_Per_Inode)
     return Volume
     with Post => Unsigned_64 (Plan'Result.Blocks) * Block_Bytes <= Partition_Bytes;

   --  sparse_super: groups 0, 1 and powers of 3, 5 and 7 carry backups.
   function Has_Backup (Group : Group_Index) return Boolean;

   function Group_First (Group : Group_Index) return Unsigned_64 is
     (Unsigned_64 (Group) * Blocks_Per_Group);
   function Block_Bitmap_Block (Item : Volume; Group : Group_Index) return Unsigned_64 is
     (Group_First (Group)
      + (if Has_Backup (Group) then 1 + Unsigned_64 (Item.Descriptor_Blocks) else 0));
   function Inode_Bitmap_Block (Item : Volume; Group : Group_Index) return Unsigned_64 is
     (Block_Bitmap_Block (Item, Group) + 1);
   function Inode_Table_Block (Item : Volume; Group : Group_Index) return Unsigned_64 is
     (Block_Bitmap_Block (Item, Group) + 2);
   function First_Data_Block (Item : Volume; Group : Group_Index) return Unsigned_64 is
     (Inode_Table_Block (Item, Group) + Unsigned_64 (Item.Inode_Table_Blocks));

   --  The root directory's and lost+found's one block each.
   function Root_Directory_Block (Item : Volume) return Unsigned_64 is
     (First_Data_Block (Item, 0));
   function Lost_Found_Block (Item : Volume) return Unsigned_64 is
     (First_Data_Block (Item, 0) + 1);

   subtype Block_Image is Bytes (1 .. Block_Bytes);
   subtype Superblock_Image is Bytes (1 .. Ext2_Format.Superblock_Bytes);

   --  The superblock copy kept in Group (s_block_group_nr).
   function Superblock
     (Item : Volume; Request : Volume_Request; Group : Group_Index)
      return Superblock_Image
     with Pre => Valid (Item) and then Group < Item.Groups;

   --  Descriptor block Index (0-based) of the group descriptor table.
   function Descriptor_Block (Item : Volume; Index : Unsigned_32) return Block_Image
     with Pre => Valid (Item) and then Index < Item.Descriptor_Blocks;

   function Block_Bitmap (Item : Volume; Group : Group_Index) return Block_Image
     with Pre => Valid (Item) and then Group < Item.Groups;
   function Inode_Bitmap (Item : Volume; Group : Group_Index) return Block_Image
     with Pre => Valid (Item) and then Group < Item.Groups;

   --  Group 0's first inode table block: inodes 1 .. 16, of which the root
   --  and lost+found are in use.
   function First_Inode_Block (Item : Volume; Request : Volume_Request)
     return Block_Image
     with Pre => Valid (Item);

   function Root_Directory (Item : Volume) return Block_Image
     with Pre => Valid (Item);
   function Lost_Found_Directory (Item : Volume) return Block_Image
     with Pre => Valid (Item);

end Ext2_Image;
