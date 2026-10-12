------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The ext3 journal (JBD2, data=ordered, Linux-compatible): operation
--  handles with credits, transactions committed through the log ring and
--  checkpointed home, Flush; writeBytes, the one write path for metadata
--  (journaled through the cache, or write-through on ext2).
------------------------------------------------------------------------------
pragma Ada_2022;
with System.Storage_Elements; use System.Storage_Elements;
with Block_Cache_Index;
with Ext2.Block_Cache; use Ext2.Block_Cache;

private package Ext2.Journal is

   use type Block_Cache_Index.Block_Key;

   use all type Block_Cache_Index.Block_Class;

   Journal_Start_Offset : constant := Jbd2_Format.Superblock_Start_Offset;

   Journal_Sequence_Offset : constant := Jbd2_Format.Superblock_Sequence_Offset;

   function dirtyBlocks (fs : Filesystem) return Boolean;

   procedure commitTransaction (fs : in out Filesystem; status : out Write_Status);

   procedure Flush (fs : in out Filesystem; status : out Flush_Status);

   --  Operation credits: an upper bound on the distinct metadata blocks
   --  one operation dirties (JBD2 handle credits).
   --  An allocation run: inode, three pointer levels, a bitmap and a
   --  descriptor block per reserved group, the superblock.
   Write_Credits : constant := 1 + 3 + 2 * 4 + 1;

   --  create/mkdir: parent and new inode blocks, the name's directory block
   --  and a new directory's own, inode bitmap and descriptor, a single-block
   --  allocation (bitmap + descriptor), the superblock, and the parent's
   --  growth by one block (an allocation run).
   Create_Credits : constant := 2 + 2 + 2 + 2 + 1 + Write_Credits;

   --  unlink/rmdir/inode release: directory block, inode and parent inode
   --  blocks, inode bitmap and descriptor, the superblock, and for rmdir
   --  a bitmap and descriptor per directory block.
   Remove_Credits : constant := 3 + 2 + 1 + 2 * 12;

   --  rename: the directory block and the directory's inode block.
   Rename_Credits : constant := 2;

   --  Orphan list: the superblock, the inode and its predecessor.
   Orphan_Credits : constant := 3;

   --  Open an operation. Outermost only: commit first unless its credits
   --  fit in half the log and in the spill, so nothing in it forces a
   --  commit before stopHandle. A failed commit quarantines the volume,
   --  which the operation's own writes then report.
   procedure startHandle (fs : in out Filesystem; credits : Natural)
     with Post => fs.journal.Handle_Depth =
                    (if fs.journal.Active then fs.journal.Handle_Depth'Old + 1
                     else fs.journal.Handle_Depth'Old);

   --  Close an operation; the outermost commits if the transaction reached
   --  half the log.
   procedure stopHandle (fs : in out Filesystem)
     with Post => fs.journal.Handle_Depth =
                    (if fs.journal.Active and then fs.journal.Handle_Depth'Old > 0
                     then fs.journal.Handle_Depth'Old - 1
                     else fs.journal.Handle_Depth'Old);

   --  Write through the block cache: patch cached copies only after the
   --  device acknowledged; after any failure the device state of the range
   --  is uncertain, so drop the volume's cached blocks.
   procedure writeBytes
     (fs     : in out Filesystem;
      offset : Storage_Offset;
      src    : System.Address;
      len    : Storage_Count;
      status : out Write_Status;
      class  : Block_Class);

end Ext2.Journal;
