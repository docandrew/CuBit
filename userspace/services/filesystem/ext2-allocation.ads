------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Ext2 block and inode allocation. Bitmaps are searched and changed by
--  the proved Ext2_Bitmaps and located by Ext2_Geometry; this unit reads
--  and publishes them (bitmaps, then descriptors, then the superblock).
------------------------------------------------------------------------------
pragma Ada_2022;
with Ext2_Format; use Ext2_Format;
with Ext2_Bitmaps;

private package Ext2.Allocation is

   --  Blocks reserved in memory for one request batch, with each touched
   --  group's updated bitmap and descriptor. Nothing reaches the disk before
   --  commitReservation, so discarding an uncommitted plan needs no I/O.
   Maximum_Reserved_Groups : constant := 4;

   --  Payload staged per batch: the per-volume transfer buffer's size.
   Maximum_Batch_Bytes : constant := 512 * 1024;

   Maximum_Batch_Data_Blocks : constant := Maximum_Batch_Bytes / Minimum_Block_Bytes;

   --  A batch stays under one leaf: at most a new leaf, middle and root.
   Maximum_Batch_Pointer_Blocks : constant := 3;

   Maximum_Reserved_Blocks : constant :=
     Maximum_Batch_Data_Blocks + Maximum_Batch_Pointer_Blocks;

   subtype Reserved_Count is Natural range 0 .. Maximum_Reserved_Blocks;

   subtype Reserved_Index is Positive range 1 .. Maximum_Reserved_Blocks;

   subtype Group_Change_Count is Natural range 0 .. Maximum_Reserved_Groups;

   subtype Group_Change_Index is Positive range 1 .. Maximum_Reserved_Groups;

   --  One bitmap block (the largest this driver reads).
   subtype Bitmap_Buffer is Ext2_Bitmaps.Bitmap (0 .. Maximum_Bitmap_Bytes - 1);

   type Group_Change is record
      Group : Group_Number := 0;
      Descriptor : BlockGroupDescriptor;
      Bitmap : Bitmap_Buffer;
      Taken : Reserved_Count := 0;
   end record;

   type Group_Changes is array (Group_Change_Index) of Group_Change;

   type Reserved_Blocks is array (Reserved_Index) of Block_Number;

   type Reservation is record
      Blocks : Reserved_Blocks := [others => No_Block];
      Count : Reserved_Count := 0;
      Groups : Group_Changes;
      Group_Total : Group_Change_Count := 0;
   end record;

   --  Reserve up to wanted free blocks in memory, at least minimum or none,
   --  scanning from goal (or the last allocation's end) and then onward
   --  through the groups. Bitmap bits are relative to their group, not to
   --  firstDataBlock globally. Free-space counters advertising space that
   --  no bitmap bit backs are inconsistent metadata, not a normal no-space
   --  condition.
   procedure reserveBlocks
     (fs : Filesystem; goal : Block_Number; wanted, minimum : Reserved_Count;
      plan : out Reservation; status : out Write_Status)
     with Post => (if status = Write_Complete
                   then plan.Count in Natural'Max (minimum, 1) .. wanted
                   else plan.Count = 0 and plan.Group_Total = 0);

   --  Publish a reservation: every touched bitmap, then every descriptor,
   --  then the superblock, each written once. Error replies do not prove a
   --  metadata write left storage unchanged: never attempt blind rollback or
   --  hand out an uncertain reservation.
   procedure commitReservation
     (fs : in out Filesystem; plan : Reservation; status : out Write_Status)
     with Post => (if status /= Write_Complete then fs.writeQuarantined);

   --  Allocate one free block. Returns a block only after all reservation
   --  metadata writes complete.
   procedure allocateBlock
     (fs       : in out Filesystem;
      blockNum : out Unsigned_32;
      status   : out Write_Status)
     with Post => (if status /= Write_Complete then blockNum = 0);

   type Release_List is array (Positive range <>) of Block_Number;

   --  Return blocks to their groups' free pools: each touched group's bitmap
   --  and descriptor are written once, then the superblock once. An out-of-
   --  range block, or one already free (a duplicate or corrupt free must not
   --  inflate allocator counts), is rejected before its group's writes. Any
   --  failed write quarantines further mutation.
   procedure releaseBlocks
     (fs : in out Filesystem; blocks : Release_List; status : out Write_Status);

   --  Free the pending released blocks. The list is emptied first, so a
   --  commit the release itself might force does not free them twice.
   procedure applyReleases (fs : in out Filesystem; status : out Write_Status)
     with Post => fs.pendingCount = 0;

   --  Release blocks a detach freed. Journaled: queued (see
   --  Pending_Blocks) for the commit of the running transaction; a full
   --  queue commits first. Write-through: after a flush makes the detach
   --  durable (a volatile device has nothing to order), at once.
   procedure deferRelease
     (fs : in out Filesystem; blocks : Release_List; status : out Write_Status);

   --  Allocate a free inode from any block group, from the last allocation
   --  on. Returns an initialized inode (1-based) only on complete
   --  reservation.
   procedure allocateInode
     (fs       : in out Filesystem;
      inodeNum : out Unsigned_32;
      status   : out Write_Status;
      directory : Boolean := False)
     with Post => (status = Write_Complete) = (inodeNum /= 0);

end Ext2.Allocation;
