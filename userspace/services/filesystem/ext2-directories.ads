------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Directory records: finding room (growing a directory by a block),
--  creating files and directories, and the in-place rename within one
--  directory block.
------------------------------------------------------------------------------
pragma Ada_2022;
with System.Storage_Elements; use System.Storage_Elements;
with Directory_Blocks;
with Block_Cache_Index;

private package Ext2.Directories is

   use type Directory_Blocks.Prepare_Result;

   use all type Block_Cache_Index.Block_Class;

   --  A directory whose records this service may rewrite: whole blocks
   --  within directoryBlockLimit, and no inode flag but the htree index,
   --  which a change clears first (unindexDirectory).
   function writableDirectory (fs : Filesystem; dir : Inode) return Boolean;

   --  The device block holding a directory's logical block index; valid is
   --  False for a hole or a pointer outside the volume.
   procedure directoryBlock
     (fs : Filesystem; dir : Inode; index : Natural; number : out Unsigned_32;
      valid : out Boolean; readStatus : out Read_Status);

   --  Before changing an htree directory's records, clear its index flag, so
   --  a Linux reader never trusts a stale index; it reads the directory
   --  linearly instead (e2fsck -fD can rebuild the index). Index blocks
   --  already read as unused records.
   procedure unindexDirectory
     (fs : in out Filesystem; number : Unsigned_32; dir : in out Inode;
      status : out Write_Status);

   --  Find room for a record of needed bytes in a writable directory, the
   --  last blocks first (names are appended, so free space is usually at
   --  the end; any block with room will do), growing it by an empty block
   --  when none has room (growDirectory). An htree index is cleared first.
   --  On success buffer holds targetBlock's contents, and the record goes
   --  at entryOffset with entrySpan bytes (a live record it is split from
   --  is already shortened in buffer).
   procedure findRoom
     (fs : in out Filesystem; number : Unsigned_32; dir : in out Inode;
      needed : Unsigned_32; targetBlock : out Unsigned_32; buffer : out String;
      entryOffset : out Storage_Offset; entrySpan : out Unsigned_16;
      status : out Write_Status);

   procedure renameEntry
     (fs : in out Filesystem; dirInodeNum : Unsigned_32;
      oldName, newName : String; status : out Rename_Status);

   --  Create an empty regular file or directory named name in directory
   --  dirInodeNum. No inode is named unless the creation completed.
   procedure createFile
     (fs : in out Filesystem; dirInodeNum : Unsigned_32; name : String;
      inodeNum : out Unsigned_32; status : out Write_Status)
     with Post => (if status /= Write_Complete then inodeNum = 0);

   procedure makeDirectory
     (fs : in out Filesystem; dirInodeNum : Unsigned_32; name : String;
      inodeNum : out Unsigned_32; status : out Write_Status)
     with Post => (if status /= Write_Complete then inodeNum = 0);

end Ext2.Directories;
