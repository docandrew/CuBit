------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  File block maps: logical-to-physical resolution through direct,
--  single-, double- and triple-indirect pointers (Block_Paths), and
--  validation of a file's block tree before it is changed.
------------------------------------------------------------------------------
pragma Ada_2022;
with Block_Paths;

private package Ext2.Mapping is

   use type Block_Paths.Path_Kind;

   --  Get the block number for a given logical block index in a file.
   --  Checked direct, single-, double- and triple-indirect resolution.
   --  pointerLeaf is the pointer block holding the data slot; pointerMiddle
   --  is the triple-tree middle block holding that leaf. Zero means absent.
   procedure resolveBlock
     (fs : Filesystem; ino : Inode; logBlock : Unsigned_32;
      physical, pointerLeaf, pointerMiddle : out Unsigned_32;
      status : out Read_Status)
     with Post => (if status /= Read_Complete then physical = 0);

   procedure getDataBlock
     (fs : Filesystem; ino : Inode; logBlock : Unsigned_32;
      physical : out Unsigned_32; status : out Read_Status)
     with Post => (if status /= Read_Complete then physical = 0);

   --  Validate ownership within this inode BEFORE any mutation. The inventory
   --  is sized by an admitted allocation count, not by logical file size.
   --  Sorting is O(n log n), replacing the old quadratic duplicate walk.
   --  Allocations beyond the scratch inventory capacity (only reachable with
   --  triple-indirect trees) are rejected as unsupported, never partially
   --  checked.
   procedure validateBlockTree
     (fs : Filesystem; ino : Inode; status : out Truncate_Status);

   --  Hot paths: inlined into the other units' loops, as they were
   --  when ext2 was one unit.
   pragma Inline_Always (getDataBlock);

end Ext2.Mapping;
