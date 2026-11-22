------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  POSIX rename across directories and blocks (renamePath), replacing an
--  existing name, moving directories (their ".." and link counts).
------------------------------------------------------------------------------
pragma Ada_2022;
with Directory_Blocks;
with Block_Cache_Index;

private package Ext2.Rename is

   use type Directory_Blocks.Prepare_Result;

   use all type Block_Cache_Index.Block_Class;

   procedure renamePath
     (fs : in out Filesystem; oldPath, newPath : String;
      keepReplaced : Boolean; replacedNumber : out Unsigned_32;
      replaced : out Inode; status : out Rename_Status);

end Ext2.Rename;
