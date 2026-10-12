------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Name removal and directory creation by path: unlink, rmdir, mkdir,
--  freeing inodes, and the ext3 orphan list (s_last_orphan) for files
--  unlinked while open.
------------------------------------------------------------------------------
pragma Ada_2022;
with Directory_Blocks;
with Block_Cache_Index;
with Ext2_Format;

private package Ext2.Removal is

   use type Directory_Blocks.Prepare_Result;

   use all type Block_Cache_Index.Block_Class;

   --  The parent directory of path and where its leaf name starts.
   procedure resolveParent
     (fs : Filesystem; path : String; parent : out Unsigned_32;
      leafFirst : out Positive; status : out Directory_Lookup_Status);

   --  A writable directory with direct pointers only: what rmdir can
   --  release today (a grown directory's pointer blocks would need the
   --  truncate path).
   function directOnlyDirectory (fs : Filesystem; dir : Inode) return Boolean;

   --  The dtime of a freed inode: ext2 requires it nonzero. Without a clock
   --  the volume's last write time serves.
   --  Wall time when known; otherwise the volume's last write time (dtime
   --  must be nonzero on a deleted inode).
   function deletionTime (fs : Filesystem) return Unsigned_32;

   --  Remove name's record (which must name expected) from a plain
   --  directory: one block rewritten, restored if that write fails.
   procedure removeEntry
     (fs : in out Filesystem; dir : Inode; name : String;
      expected : Unsigned_32; status : out Remove_Status);

   --  Return an inode to its group's bitmap. Counts are checked before the
   --  first write; a failed write quarantines the volume.
   procedure freeInode
     (fs : in out Filesystem; inodeNum : Unsigned_32; directory : Boolean;
      status : out Write_Status);

   ---------------------------------------------------------------------------
   --  The ext3 orphan list (journaled volumes): unlinked inodes still to be
   --  freed, from the superblock's s_last_orphan through each inode's dtime,
   --  as Linux keeps it. An inode unlinked while open joins it with its
   --  unlink and leaves it with its release, so a crash in between leaves
   --  it for the next mount (this service, Linux or e2fsck) to free.
   ---------------------------------------------------------------------------
   Last_Orphan_Offset : constant := Ext2_Format.Last_Orphan_Offset;

   procedure readLastOrphan
     (fs : Filesystem; head : out Unsigned_32; status : out Read_Status);

   function validOrphan (fs : Filesystem; number : Unsigned_32) return Boolean;

   --  Take an inode off the list; found is False if it is not on it. The
   --  walk is bounded by the inode count (a cyclic list is malformed).
   procedure orphanRemove
     (fs : in out Filesystem; inodeNum : Unsigned_32; found : out Boolean;
      status : out Write_Status);

   procedure reclaimInode
     (fs : in out Filesystem; inodeNum : Unsigned_32;
      status : out Remove_Status);

   --  Drop a regular file's last link once its name is gone, under the
   --  handle that removed the name (stopped here): freed now when that is
   --  safe, otherwise put on the orphan list and reclaimed unless still
   --  held (keepOrphan). inodeNum and unlinked are set once that is done.
   procedure releaseUnlinked
     (fs : in out Filesystem; target : Unsigned_32; ino : in out Inode;
      keepOrphan : Boolean; inodeNum : out Unsigned_32; unlinked : out Inode;
      status : out Remove_Status);

   procedure unlinkPath
     (fs : in out Filesystem; path : String; keepOrphan : Boolean;
      inodeNum : out Unsigned_32; unlinked : out Inode;
      status : out Remove_Status);

   procedure makeDirectoryPath
     (fs : in out Filesystem; path : String; inodeNum : out Unsigned_32;
      lookup : out Directory_Lookup_Status; status : out Write_Status);

   procedure removeDirectoryPath
     (fs : in out Filesystem; path : String; inodeNum : out Unsigned_32;
      status : out Remove_Status);

end Ext2.Removal;
