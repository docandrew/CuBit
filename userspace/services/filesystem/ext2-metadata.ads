------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Ext2 metadata I/O: the superblock, block group descriptors and inodes,
--  located by Ext2_Geometry and written through the block cache/journal
--  (Ext2.Journal.writeBytes); inode times.
------------------------------------------------------------------------------
pragma Ada_2022;
with Ext2_Format; use Ext2_Format;
with Ext2_Geometry;

private package Ext2.Metadata is

   pragma Compile_Time_Error
     (BlockGroupDescriptor'Size /= Group_Descriptor_Bytes * Bits_Per_Byte,
      "group descriptor layout changed");

   --  The superblock fields admission checks, and the geometry of an
   --  admitted volume (valid: initBlockDevice admits only a superblock
   --  Ext2_Geometry.Supported accepts).
   function Fields_Of (sb : Superblock) return Ext2_Geometry.Superblock_Fields;
   function Shape (fs : Filesystem) return Ext2_Geometry.Geometry;
   pragma Inline_Always (Shape);

   --  Validate the geometry assumptions used by the bounded ext2
   --  implementation. Doing this once at volume admission keeps malformed
   --  on-disk values from becoming divisors, array bounds, or bitmap
   --  indices in the I/O path.
   function supportedSuperblock (sb : Superblock) return Boolean is
     (Ext2_Geometry.Supported (Fields_Of (sb)));

   --  An inode is NULL_INODE unless read completely.
   procedure readInode
     (fs : Filesystem; inodeNum : Unsigned_32;
      ino : out Inode; status : out Read_Status)
     with Post => (if status /= Read_Complete then ino = NULL_INODE);

   --  UTC seconds now, from the wall-clock offset the clock service publishes
   --  (CuBit.System_Facts), or 0 while it is unknown.
   function Now_Seconds return Unsigned_32;

   --  What an inode change means for its times (POSIX): every change sets
   --  ctime; changed content (data, size, directory entries) sets mtime too;
   --  a new inode starts with all three. Times change only when the second
   --  does, so a burst of overwrites costs at most one inode write a second.
   --  While wall time is unknown, times are left as they are.
   type Inode_Touch is (Inode_Changed, Content_Changed, Inode_Created);

   procedure Stamp (ino : in out Inode; touch : Inode_Touch);

   --  Stamp, reporting whether a time changed: an inode that is written
   --  only for its times need not be written when none did.
   procedure Stamp (ino : in out Inode; touch : Inode_Touch; changed : out Boolean)
     with Post => changed = (ino /= ino'Old);

   --  Exact_Inode writes every field as given; Update_Existing_Inode keeps
   --  the on-disk dtime of an unlinked inode of a journaled volume, which
   --  is its ext3 orphan-list link: only the orphan-list code sets it, and
   --  the copies open handles hold may be older.
   type Inode_Write_Mode is (Update_Existing_Inode, Initialize_New_Inode, Exact_Inode);

   --  Existing inodes preserve their extended metadata. A newly reserved slot
   --  must be initialized in full before any directory entry can expose it.
   procedure writeInode
     (fs       : in out Filesystem;
      inodeNum : Unsigned_32;
      ino      : Inode;
      status   : out Write_Status;
      mode     : Inode_Write_Mode := Update_Existing_Inode);

   --  Write the superblock back to disk.
   procedure writeSuperblock
     (fs : in out Filesystem;
      status : out Write_Status);

   --  Write a block group descriptor back to disk.
   procedure writeBGD
     (fs         : in out Filesystem;
      blockGroup : Group_Number;
      bgd        : BlockGroupDescriptor;
      status     : out Write_Status);

   --  Read the block group descriptor of a group.
   procedure readBGD
     (fs         : Filesystem;
      blockGroup : Group_Number;
      bgd        : out BlockGroupDescriptor;
      status     : out Read_Status);

end Ext2.Metadata;
