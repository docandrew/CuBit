------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Ext2 block and inode allocation (see the spec).
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Block_Devices; use CuBit.Block_Devices;
with System.Storage_Elements; use System.Storage_Elements;
with Block_Cache_Index;
with Ext2_Geometry;
with Ext2.Block_Cache; use Ext2.Block_Cache;
with Ext2.Journal; use Ext2.Journal;
with Ext2.Metadata; use Ext2.Metadata;

package body Ext2.Allocation is

   use all type Block_Cache_Index.Block_Class;
   package Geometry renames Ext2_Geometry;
   package Bitmaps renames Ext2_Bitmaps;

   --  The bytes of a block bitmap this driver reads and writes.
   function bitmapBytes (fs : Filesystem) return Unsigned_32 is
     (Unsigned_32 (Geometry.Block_Bitmap_Bytes (Shape (fs))));

   function bitmapOffset (fs : Filesystem; bitmap : Unsigned_32) return Storage_Offset is
     (Storage_Offset (Geometry.Block_Offset (Shape (fs), Block_Number (bitmap))));

   procedure reserveBlocks
     (fs : Filesystem; goal : Block_Number; wanted, minimum : Reserved_Count;
      plan : out Reservation; status : out Write_Status)
   is
      layout : constant Geometry.Geometry := Shape (fs);
      readSize : Count_32;
      groupCount : Count_32;
      startGroup : Group_Number := 0;
      startBit : Count_32 := 0;
      readStatus : Read_Status;
      sawAdvertisedSpace : Boolean := False;
      taken : Bitmaps.Bit_List (Reserved_Index);

      procedure Discard (reason : Write_Status) is
      begin
         plan.Count := 0;
         plan.Group_Total := 0;
         status := reason;
      end Discard;
   begin
      plan.Count := 0;
      plan.Group_Total := 0;
      status := Write_No_Space;
      if fs.writeQuarantined then
         status := Write_Recovery_Required;
         return;
      elsif Is_Read_Only (fs.device.description) then
         status := Write_Read_Only;
         return;
      elsif fs.sb.blocksPerBlockGroup = 0 or else
        fs.sb.blockCount <= fs.sb.firstDataBlock
      then
         status := Write_Device_Error;
         return;
      end if;
      readSize := Geometry.Block_Bitmap_Bytes (layout);
      if wanted = 0 or else fs.sb.freeBlocks = 0 then
         return;
      end if;

      --  Commit rewrites the superblock: read its block now, so publishing
      --  the reservation needs no read that could fail half way through.
      declare
         current : Superblock;
      begin
         readBytes (fs, SUPERBLOCK_OFFSET, current'Address, Superblock'Size / 8,
                    readStatus);
         if readStatus /= Read_Complete then
            status := (if readStatus = Read_Out_Of_Range then Write_Out_Of_Range
                       else Write_Device_Error);
            return;
         end if;
      end;
      groupCount := Geometry.Block_Group_Count (layout);
      declare
         start : constant Block_Number :=
           (if goal /= No_Block then goal else fs.allocationHint);
      begin
         if Geometry.In_Volume (layout, start) then
            startGroup := Geometry.Group_Of_Block (layout, start);
            startBit := Geometry.Bit_Of_Block (layout, start);
         end if;
      end;

      for step in 0 .. groupCount - 1 loop
         exit when plan.Count = wanted or else
           plan.Group_Total = Maximum_Reserved_Groups or else
           Unsigned_32 (plan.Count) = fs.sb.freeBlocks;
         declare
            group : constant Group_Number := Group_Number
              ((Long_Long_Integer (startGroup) + Long_Long_Integer (step)) mod
                 Long_Long_Integer (groupCount));
            change : Group_Change renames plan.Groups (plan.Group_Total + 1);
            groupFirst : Block_Number;
            scanBits : Count_32;
            count : Bitmaps.Take_Count;
         begin
            readBGD (fs, group, change.Descriptor, readStatus);
            if readStatus /= Read_Complete then
               Discard (if readStatus = Read_Out_Of_Range then Write_Out_Of_Range
                        else Write_Device_Error);
               return;
            end if;
            if change.Descriptor.numFreeBlocks /= 0 then
               sawAdvertisedSpace := True;
               groupFirst := Geometry.Group_First_Block (layout, group);
               readBytes
                 (fs, bitmapOffset (fs, change.Descriptor.blockBitmapAddr),
                  change.Bitmap'Address, Storage_Count (readSize), readStatus);
               if readStatus /= Read_Complete then
                  Discard (if readStatus = Read_Out_Of_Range then Write_Out_Of_Range
                           else Write_Device_Error);
                  return;
               end if;
               scanBits := Count_32'Min (Geometry.Blocks_In_Group (layout, group),
                                         readSize * Bits_Per_Byte);
               change.Group := group;
               change.Taken := 0;
               --  Up to the request, the group's advertised free blocks and
               --  the volume's; in the first group from the goal's bit on.
               Bitmaps.Take_Free
                 (Map       => change.Bitmap,
                  Scan_Bits => Positive (scanBits),
                  Start     => (if step = 0 then Natural (startBit) else 0),
                  Limit     => Natural'Min
                    (wanted - plan.Count,
                     Natural (Unsigned_32'Min
                       (Unsigned_32 (change.Descriptor.numFreeBlocks),
                        fs.sb.freeBlocks - Unsigned_32 (plan.Count)))),
                  Taken     => taken,
                  Count     => count);
               for index in 1 .. count loop
                  plan.Count := plan.Count + 1;
                  plan.Blocks (plan.Count) := groupFirst + Block_Number (taken (index));
               end loop;
               change.Taken := count;
               if change.Taken > 0 then
                  plan.Group_Total := plan.Group_Total + 1;
               end if;
            end if;
         end;
      end loop;

      if plan.Count > 0 and then plan.Count >= minimum then
         status := Write_Complete;
      elsif plan.Count = 0 and then sawAdvertisedSpace then
         Discard (Write_Device_Error);
      else
         Discard (Write_No_Space);
      end if;
   end reserveBlocks;

   procedure commitReservation
     (fs : in out Filesystem; plan : Reservation; status : out Write_Status)
   is
      updated : BlockGroupDescriptor;

      procedure Uncertain is
      begin
         fs.writeQuarantined := True;
         status := Write_Recovery_Required;
      end Uncertain;
   begin
      status := Write_Complete;
      for index in 1 .. plan.Group_Total loop
         writeBytes
           (fs, bitmapOffset (fs, plan.Groups (index).Descriptor.blockBitmapAddr),
            plan.Groups (index).Bitmap'Address, Storage_Count (bitmapBytes (fs)),
            status, Allocation_Metadata);
         if status /= Write_Complete then
            Uncertain;
            return;
         end if;
      end loop;
      for index in 1 .. plan.Group_Total loop
         updated := plan.Groups (index).Descriptor;
         updated.numFreeBlocks :=
           updated.numFreeBlocks - Unsigned_16 (plan.Groups (index).Taken);
         writeBGD (fs, plan.Groups (index).Group, updated, status);
         if status /= Write_Complete then
            Uncertain;
            return;
         end if;
      end loop;
      fs.sb.freeBlocks := fs.sb.freeBlocks - Unsigned_32 (plan.Count);
      writeSuperblock (fs, status);
      if status /= Write_Complete then
         Uncertain;
      elsif plan.Count > 0 then
         fs.allocationHint := plan.Blocks (plan.Count) + 1;
      end if;
   end commitReservation;

   procedure allocateBlock
     (fs       : in out Filesystem;
      blockNum : out Unsigned_32;
      status   : out Write_Status)
   is
      plan : Reservation;
   begin
      blockNum := 0;
      reserveBlocks (fs, No_Block, 1, 1, plan, status);
      if status /= Write_Complete then
         return;
      end if;
      commitReservation (fs, plan, status);
      if status = Write_Complete then
         blockNum := Unsigned_32 (plan.Blocks (Reserved_Index'First));
      end if;
   end allocateBlock;

   procedure releaseBlocks
     (fs : in out Filesystem; blocks : Release_List; status : out Write_Status)
   is
      layout : constant Geometry.Geometry := Shape (fs);
      readSize : constant Unsigned_32 := bitmapBytes (fs);
      bitmap : Bitmap_Buffer;
      bgd : BlockGroupDescriptor;
      done : array (blocks'Range) of Boolean := [others => False];
      released : Unsigned_64 := 0;
      allocatableBlocks : Unsigned_32;
      readStatus : Read_Status;
   begin
      status := Write_Out_Of_Range;
      if fs.writeQuarantined then
         status := Write_Read_Only;
         return;
      elsif fs.sb.blocksPerBlockGroup = 0 or else
        (for some number of blocks => not Geometry.In_Volume (layout, number))
      then
         return;
      end if;
      allocatableBlocks := fs.sb.blockCount - fs.sb.firstDataBlock;
      for first in blocks'Range loop
         if not done (first) then
            declare
               group : constant Group_Number :=
                 Geometry.Group_Of_Block (layout, blocks (first));
               validBlocks : constant Unsigned_32 :=
                 Unsigned_32 (Geometry.Blocks_In_Group (layout, group));
               cleared : Unsigned_32 := 0;
            begin
               readBGD (fs, group, bgd, readStatus);
               if readStatus /= Read_Complete then
                  status := Write_Device_Error;
                  return;
               end if;
               readBytes
                 (fs, bitmapOffset (fs, bgd.blockBitmapAddr),
                  bitmap'Address, Storage_Count (readSize), readStatus);
               if readStatus /= Read_Complete then
                  status := Write_Device_Error;
                  return;
               end if;
               for index in first .. blocks'Last loop
                  if not done (index) and then
                    Geometry.Group_Of_Block (layout, blocks (index)) = group
                  then
                     declare
                        bit : constant Count_32 :=
                          Geometry.Bit_Of_Block (layout, blocks (index));
                     begin
                        --  Only a block in use may be freed.
                        if Unsigned_32 (bit) / Bits_Per_Byte >= readSize or else
                          not Bitmaps.Is_Set (bitmap, Natural (bit))
                        then
                           return;
                        end if;
                        Bitmaps.Clear (bitmap, Natural (bit));
                        cleared := cleared + 1;
                        done (index) := True;
                     end;
                  end if;
               end loop;
               if Unsigned_64 (bgd.numFreeBlocks) + Unsigned_64 (cleared) >
                    Unsigned_64 (validBlocks) or else
                 Unsigned_64 (fs.sb.freeBlocks) + released + Unsigned_64 (cleared) >
                    Unsigned_64 (allocatableBlocks)
               then
                  return;
               end if;
               writeBytes
                 (fs, bitmapOffset (fs, bgd.blockBitmapAddr),
                  bitmap'Address, Storage_Count (readSize), status, Allocation_Metadata);
               if status /= Write_Complete then
                  fs.writeQuarantined := True;
                  return;
               end if;
               bgd.numFreeBlocks := bgd.numFreeBlocks + Unsigned_16 (cleared);
               writeBGD (fs, group, bgd, status);
               if status /= Write_Complete then
                  fs.writeQuarantined := True;
                  return;
               end if;
               released := released + Unsigned_64 (cleared);
            end;
         end if;
      end loop;
      fs.sb.freeBlocks := fs.sb.freeBlocks + Unsigned_32 (released);
      writeSuperblock (fs, status);
      if status /= Write_Complete then
         fs.writeQuarantined := True;
      end if;
   end releaseBlocks;

   procedure applyReleases (fs : in out Filesystem; status : out Write_Status) is
      count : constant Pending_Count := fs.pendingCount;
      blocks : constant Release_List (1 .. count) :=
        Release_List (fs.pending (1 .. count));
   begin
      fs.pendingCount := 0;
      status := Write_Complete;
      if count > 0 then
         releaseBlocks (fs, blocks, status);
      end if;
   end applyReleases;

   procedure deferRelease
     (fs : in out Filesystem; blocks : Release_List; status : out Write_Status)
   is
      flushed : Flush_Status;
   begin
      status := Write_Complete;
      if not fs.journal.Active then
         if not Is_Volatile (fs.device.description) then
            Ext2.Journal.Flush (fs, flushed);
            if flushed /= Flush_Complete then
               status := Write_Device_Error;
               return;
            end if;
         end if;
         releaseBlocks (fs, blocks, status);
         return;
      end if;
      if fs.pendingCount + blocks'Length > Maximum_Pending_Releases then
         commitTransaction (fs, status);
         if status /= Write_Complete then
            return;
         end if;
      end if;
      for block of blocks loop
         fs.pendingCount := fs.pendingCount + 1;
         fs.pending (fs.pendingCount) := block;
      end loop;
   end deferRelease;

   procedure allocateInode
     (fs       : in out Filesystem;
      inodeNum : out Unsigned_32;
      status   : out Write_Status;
      directory : Boolean := False)
   is
      layout : constant Geometry.Geometry := Shape (fs);
      bgd : BlockGroupDescriptor;
      updatedBGD : BlockGroupDescriptor;
      bitmapBuf : Bitmap_Buffer with Alignment => 8;
      readSize : Natural;
      groupCount : Count_32;
      group : Group_Number;
      firstByte, lastByte : Natural;
      --  Bitmap bytes read at a time while searching.
      BITMAP_CHUNK : constant := 64;
      groupFirst : Count_32;
      validInodes : Count_32;
      candidate : Ext2_Bitmaps.Bit_Index;
      candidateInode : Inode_Number;
      firstUsable : Inode_Number;
      readStatus : Read_Status;
      writeStatus : Write_Status;
      sawAdvertisedSpace : Boolean := False;
      blankIno : constant Inode := NULL_INODE;
   begin
      inodeNum := 0;
      status := Write_No_Space;
      if fs.writeQuarantined then
         status := Write_Recovery_Required;
         return;
      elsif Is_Read_Only (fs.device.description)
      then
         status := Write_Read_Only;
         return;
      end if;

      if fs.sb.inodesPerBlockGroup = 0 or else fs.sb.inodeCount = 0 then
         status := Write_Device_Error;
         return;
      end if;

      if fs.sb.freeInodes = 0 then
         return;
      end if;

      groupCount := Geometry.Inode_Group_Count (layout);
      firstUsable := Geometry.First_Usable_Inode
        (fs.sb.majorVersion, Count_32 (fs.sb.firstNonReservedInode));
      readSize := Natural'Min (Natural (Geometry.Inode_Bitmap_Bytes (layout)),
                               bitmapBuf'Length);

      --  From the hint's group and byte to the end of the bitmaps, then
      --  from the start around to the hint (pass groupCount: the hint's
      --  group below its byte). Bitmaps are read in chunks as searched.
      if Count_32 (fs.inodeHintGroup) >= groupCount or else fs.inodeHintByte >= readSize then
         fs.inodeHintGroup := 0;
         fs.inodeHintByte := 0;
      end if;
      for pass in 0 .. groupCount loop
         group := Group_Number
           ((Long_Long_Integer (fs.inodeHintGroup) + Long_Long_Integer (pass)) mod
              Long_Long_Integer (groupCount));
         firstByte := (if pass = 0 then fs.inodeHintByte else 0);
         lastByte := (if pass = groupCount then fs.inodeHintByte else readSize);
         exit when pass = groupCount and then fs.inodeHintByte = 0;
         readBGD (fs, group, bgd, readStatus);
         if readStatus /= Read_Complete then
            status :=
              (if readStatus = Read_Out_Of_Range then Write_Out_Of_Range
               else Write_Device_Error);
            return;
         end if;

         if bgd.numFreeInodes /= 0 then
            sawAdvertisedSpace := True;
            groupFirst := Count_32 (group) * layout.Inodes_Per_Group;
            validInodes := Count_32'Min
              (layout.Inodes_Per_Group, layout.Inode_Count - groupFirst);

            for byteIdx in firstByte .. lastByte - 1 loop
               if byteIdx = firstByte or else byteIdx mod BITMAP_CHUNK = 0 then
                  declare
                     chunkEnd : constant Natural := Natural'Min
                       ((byteIdx / BITMAP_CHUNK + 1) * BITMAP_CHUNK, lastByte);
                  begin
                     readBytes
                       (fs,
                        bitmapOffset (fs, bgd.inodeBitmapAddr) + Storage_Offset (byteIdx),
                        bitmapBuf (byteIdx)'Address,
                        Storage_Count (chunkEnd - byteIdx),
                        readStatus);
                  end;
                  if readStatus /= Read_Complete then
                     status :=
                       (if readStatus = Read_Out_Of_Range then Write_Out_Of_Range
                        else Write_Device_Error);
                     return;
                  end if;
               end if;
               if bitmapBuf (byteIdx) /= Unsigned_8'Last then
                  for bitIdx in 0 .. Bits_Per_Byte - 1 loop
                     candidate := byteIdx * Bits_Per_Byte + bitIdx;
                     if Count_32 (candidate) < validInodes and then
                       not Bitmaps.Is_Set (bitmapBuf, candidate)
                     then
                        candidateInode := Inode_Number (groupFirst) + Inode_Number (candidate) + 1;
                        if candidateInode < firstUsable then
                           goto Continue_Inode_Bit;
                        end if;

                        Bitmaps.Set (bitmapBuf, candidate);
                        writeBytes
                          (fs,
                           bitmapOffset (fs, bgd.inodeBitmapAddr) + Storage_Offset (byteIdx),
                           bitmapBuf (byteIdx)'Address, 1,
                           writeStatus, Allocation_Metadata);
                        if writeStatus /= Write_Complete then
                           --  Error replies do not prove a metadata write
                           --  left storage unchanged. Do not attempt blind
                           --  rollback or hand out an uncertain reservation.
                           fs.writeQuarantined := True;
                           status := Write_Recovery_Required;
                           return;
                        end if;

                        updatedBGD := bgd;
                        updatedBGD.numFreeInodes :=
                          updatedBGD.numFreeInodes - 1;
                        if directory then
                           updatedBGD.numDirectories :=
                             updatedBGD.numDirectories + 1;
                        end if;
                        writeBGD (fs, group, updatedBGD, writeStatus);
                        if writeStatus /= Write_Complete then
                           fs.writeQuarantined := True;
                           status := Write_Recovery_Required;
                           return;
                        end if;

                        fs.sb.freeInodes := fs.sb.freeInodes - 1;
                        writeSuperblock (fs, writeStatus);
                        if writeStatus = Write_Complete then
                           writeInode
                             (fs, Unsigned_32 (candidateInode), blankIno, writeStatus,
                              Initialize_New_Inode);
                        end if;

                        if writeStatus /= Write_Complete then
                           fs.writeQuarantined := True;
                           status := Write_Recovery_Required;
                           return;
                        end if;

                        inodeNum := Unsigned_32 (candidateInode);
                        fs.inodeHintGroup := group;
                        fs.inodeHintByte := byteIdx;
                        status := Write_Complete;
                        return;
                     end if;

                     <<Continue_Inode_Bit>>
                  end loop;
               end if;
            end loop;
         end if;
      end loop;

      if sawAdvertisedSpace then
         status := Write_Device_Error;
      end if;
   end allocateInode;

end Ext2.Allocation;
