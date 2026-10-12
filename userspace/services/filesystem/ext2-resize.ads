------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Truncate and resize within the indirect extents: growth is sparse;
--  shrink detaches blocks durably (or in the running transaction) before
--  they are released.
------------------------------------------------------------------------------
pragma Ada_2022;
with Block_Cache_Index;

private package Ext2.Resize is

   use all type Block_Cache_Index.Block_Class;

   procedure resizeInode
     (fs : in out Filesystem; inodeNum : Unsigned_32;
      newSize : Unsigned_64; resizedInode : out Inode;
      status : out Truncate_Status);

   procedure resizeFile
     (fs : in out Filesystem; inodeNum : Unsigned_32;
      newSize : Unsigned_64; resizedInode : out Inode;
      status : out Truncate_Status);

   --  OPEN_TRUNCATE uses the same mutation implementation as general resize.
   procedure truncateToEmpty
     (fs : in out Filesystem; inodeNum : Unsigned_32;
      emptyInode : out Inode; status : out Truncate_Status);

end Ext2.Resize;
