------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  File data: reads, and writes that allocate runs of blocks under one
--  pointer leaf, publishing payload before pointers and pointers before
--  the inode.
------------------------------------------------------------------------------
pragma Ada_2022;
with Block_Paths;
with Block_Cache_Index;
with Ext2.Block_Cache; use Ext2.Block_Cache;

private package Ext2.Files is

   use type Block_Paths.Path_Kind;
   use all type Block_Cache_Index.Block_Class;

   procedure readData
     (fs     : Filesystem;
      ino    : Inode;
      offset : Unsigned_64;
      buf    : System.Address;
      count  : Unsigned_64;
      bytesRead : out Unsigned_64;
      status    : out Read_Status)
     with Post => bytesRead <= count;

   --  writeBytes uses the session's described logical block and transfer bound.
   --  Allocate, attach and write a run of unmapped logical blocks starting at
   --  byte pos, all under one pointer leaf (or the inode's direct slots), for
   --  up to available bytes from source. The run's allocation metadata is
   --  published once; then the payload in coalesced transfers (fresh blocks
   --  are zero-filled around it, so no previously freed bytes can appear);
   --  then each modified pointer block, children before parents. Each block
   --  is thus on disk before any pointer to it, and allocation metadata
   --  before the pointers that reference its blocks. The caller publishes the
   --  inode. A failure before commit has written nothing; any failure after
   --  it quarantines the volume. consumed is zero unless status is Complete.
   procedure appendRun
     (fs : in out Filesystem; ino : in out Inode; pos : Unsigned_64;
      source : System.Address; available : Unsigned_64;
      consumed : out Unsigned_64; status : out Write_Status;
      payloadClass : Block_Class := File_Data);

   --  Clear only mapped storage in a newly exposed byte range. Holes already
   --  read as zero and must not cause allocation. This is shared by explicit
   --  resize and positioned writes past EOF, including a retained partial block
   --  after shrink. Never publish the larger size before this completes.
   procedure zeroExposedRange
     (fs : in out Filesystem; ino : Inode; first, limit : Unsigned_64;
      status : out Write_Status);

   --  Write file data to an inode starting at the given offset.
   --  Supports file growth via block allocation.
   procedure writeData
     (fs       : in out Filesystem;
      inodeNum : Unsigned_32;
      ino      : in out Inode;
      offset   : Unsigned_64;
      buf      : System.Address;
      count    : Unsigned_64;
      bytesWritten : out Unsigned_64;
      status       : out Write_Status)
     --  A quarantined volume promises no published prefix.
     with Post => (if fs.writeQuarantined then
                     bytesWritten = 0 and status = Write_Recovery_Required);

end Ext2.Files;
