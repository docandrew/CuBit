------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  See the spec.
------------------------------------------------------------------------------
pragma Ada_2022;
with Ext2_Support; use type Ext2_Support.File_Admission;
with Triple_Mappings;
with Double_Mappings;
with Inode_Mappings;
with Sector_Accounting;
with CuBit.Block_Devices; use CuBit.Block_Devices;
with System.Storage_Elements; use System.Storage_Elements;
with Ext2_Format; use Ext2_Format;
with Ext2.Allocation; use Ext2.Allocation;
with Ext2.Journal; use Ext2.Journal;
with Ext2.Mapping; use Ext2.Mapping;
with Ext2.Metadata; use Ext2.Metadata;

package body Ext2.Files is

   use type System.Address;

   procedure readData
     (fs     : Filesystem;
      ino    : Inode;
      offset : Unsigned_64;
      buf    : System.Address;
      count  : Unsigned_64;
      bytesRead : out Unsigned_64;
      status    : out Read_Status)
   is
      size : constant Unsigned_64 := fileSize (ino);
      remaining : Unsigned_64;
      pos       : Unsigned_64 := offset;
      completed : Unsigned_64 := 0;
      terminalStatus : Read_Status := Read_Complete;
      maximumLogicalBlocks : constant Block_Paths.Logical_Block_Count :=
        Block_Paths.Block_Limit (Sector_Accounting.Block_Sectors (fs.blkSize / 512));

      --  Maximum contiguous payload accepted by this block session.
      maxContigBytes : constant Unsigned_64 :=
        Unsigned_64 (fs.device.grantBytes);
   begin
      bytesRead := 0;
      status := Read_Complete;
      if Ext2_Support.Check_File (ino, Unlinked_Allowed => True) /= Ext2_Support.File_Allowed then
         status := Read_Object_Unsupported;
         return;
      end if;
      if offset >= size then
         return;
      end if;

      remaining := size - offset;
      if remaining > count then
         remaining := count;
      end if;

      --  Note: no cache invalidation needed here. The indirect block cache
      --  is keyed by physical block number, which is unique across all
      --  inodes in ext2. Different files miss naturally; same file hits.

      while remaining > 0 loop
         declare
            logicalIndex : constant Unsigned_64 :=
              pos / Unsigned_64 (fs.blkSize);
            blockOffset : constant Unsigned_32 :=
              Unsigned_32 (pos mod Unsigned_64 (fs.blkSize));
            physBlock   : Unsigned_32;
            canRead     : Unsigned_64;
         begin
            if logicalIndex >= maximumLogicalBlocks then
               terminalStatus := Read_File_Range_Unsupported;
               exit;
            end if;

            declare
               logBlock : constant Unsigned_32 := Unsigned_32 (logicalIndex);
            begin
               getDataBlock (fs, ino, logBlock, physBlock, terminalStatus);
               exit when terminalStatus /= Read_Complete;

            if physBlock = 0 then
               --  Sparse block (hole) — fill with zeros
               canRead := Unsigned_64 (fs.blkSize - blockOffset);
               if canRead > remaining then
                  canRead := remaining;
               end if;
               declare
                  dst : String (1 .. Natural (canRead))
                    with Import, Address => buf + Storage_Offset (completed);
               begin
                  for i in dst'Range loop
                     dst (i) := Character'Val (0);
                  end loop;
               end;
            else
               --  Scan ahead for contiguous physical blocks to batch
               --  into a single readBytes call.
               declare
                  contigBlocks : Unsigned_32 := 1;
                  maxBlocks    : Unsigned_32;
                  nextPhys     : Unsigned_32;
               begin
                  if blockOffset = 0 then
                     --  Only batch from block-aligned positions
                     maxBlocks := Unsigned_32
                       (maxContigBytes / Unsigned_64 (fs.blkSize));
                     if maxBlocks = 0 then
                        maxBlocks := 1;
                     end if;

                     while contigBlocks < maxBlocks loop
                        --  Don't read past file or request
                        exit when Unsigned_64 (contigBlocks) *
                          Unsigned_64 (fs.blkSize) >= remaining;

                        exit when logicalIndex + Unsigned_64 (contigBlocks) >=
                          maximumLogicalBlocks;

                        getDataBlock
                          (fs, ino, logBlock + contigBlocks, nextPhys,
                           terminalStatus);
                        exit when terminalStatus /= Read_Complete;

                        --  Must be consecutive physical blocks
                        exit when nextPhys /= physBlock + contigBlocks;

                        contigBlocks := contigBlocks + 1;
                     end loop;
                  end if;

                  --  Do not hide a failed speculative lookup by retrying it,
                  --  or report this not-yet-read batch as a completed prefix.
                  exit when terminalStatus /= Read_Complete;
                  canRead := Unsigned_64 (contigBlocks) *
                    Unsigned_64 (fs.blkSize) -
                    Unsigned_64 (blockOffset);
                  if canRead > remaining then
                     canRead := remaining;
                  end if;

                  declare
                     readStatus : Read_Status;
                  begin
                     readPayload
                       (fs,
                        Storage_Offset (physBlock) *
                          Storage_Offset (fs.blkSize) +
                          Storage_Offset (blockOffset),
                        buf + Storage_Offset (completed),
                        Storage_Count (canRead),
                        readStatus);
                     if readStatus /= Read_Complete then
                        terminalStatus := readStatus;
                        exit;
                     end if;
                  end;
               end;
            end if;
            end;

            completed := completed + canRead;
            pos       := pos + canRead;
            remaining := remaining - canRead;
         end;
      end loop;

      bytesRead := completed;
      status := terminalStatus;
   end readData;

   procedure appendRun
     (fs : in out Filesystem; ino : in out Inode; pos : Unsigned_64;
      source : System.Address; available : Unsigned_64;
      consumed : out Unsigned_64; status : out Write_Status;
      payloadClass : Block_Class := File_Data)
   is
      sectors : constant Sector_Accounting.Block_Sectors :=
        Sector_Accounting.Block_Sectors (fs.blkSize / 512);
      blockBytes : constant Unsigned_64 := Unsigned_64 (fs.blkSize);
      pointers : constant Unsigned_64 := Block_Paths.Pointer_Count (sectors);
      first : constant Unsigned_64 := pos / blockBytes;
      offset : constant Unsigned_64 := pos mod blockBytes;
      path : constant Block_Paths.Block_Path := Block_Paths.Decode (first, sectors);
      containerEnd : Unsigned_64 := 0;
      runBlocks : Unsigned_64;
      rootBuf, middleBuf, leafBuf : Pointer_Block := [others => 0];
      root, middle, leaf : Unsigned_32 := 0;
      newRoot, newMiddle, newLeaf : Boolean := False;
      leafSlot : Block_Paths.Pointer_Index := 0;
      pointerCount : Reserved_Count;
      plan : Reservation;
      candidate, next : Inode;
      accepted : Boolean;
      readStatus : Read_Status;
      goal : Unsigned_32 := 0;
      bytes : Unsigned_64;
      type Payload_Bytes is array (Natural range <>) of Unsigned_8;
      payload : Payload_Bytes (0 .. Maximum_Batch_Bytes - 1) with Alignment => 8;

      procedure Load (number : Unsigned_32; contents : out Pointer_Block) is
      begin
         readBytes (fs, Storage_Offset (number) * Storage_Offset (fs.blkSize),
                    contents'Address, Storage_Count (fs.blkSize), readStatus);
         if readStatus /= Read_Complete then
            status := Write_Device_Error;
         end if;
      end Load;

      procedure Publish
        (number : Unsigned_32; contents : Pointer_Block; class : Block_Class) is
      begin
         if status = Write_Complete then
            writeBytes (fs, Storage_Offset (number) * Storage_Offset (fs.blkSize),
                        contents'Address, Storage_Count (fs.blkSize), status, class);
         end if;
      end Publish;

      function Data_Block (index : Unsigned_64) return Unsigned_32 is
        (Unsigned_32 (plan.Blocks (pointerCount + 1 + Natural (index))));
   begin
      consumed := 0;
      status := Write_Complete;
      case path.Kind is
         when Block_Paths.Direct =>
            containerEnd := NUM_DIRECT_BLOCKS;
         when Block_Paths.Single_Indirect =>
            containerEnd := Block_Paths.First_Double (sectors);
            leafSlot := path.Single_Slot;
            leaf := ino.singleIndirectBlock;
            newLeaf := leaf = 0;
            if not newLeaf then Load (leaf, leafBuf); end if;
         when Block_Paths.Double_Indirect =>
            containerEnd := Block_Paths.First_Double (sectors) +
              (Unsigned_64 (path.Root_Slot) + 1) * pointers;
            leafSlot := path.Leaf_Slot;
            root := ino.doubleIndirectBlock;
            newRoot := root = 0;
            if not newRoot then
               Load (root, rootBuf);
               leaf := rootBuf (path.Root_Slot);
            end if;
            newLeaf := leaf = 0;
            if status = Write_Complete and then not newLeaf then
               Load (leaf, leafBuf);
            end if;
         when Block_Paths.Triple_Indirect =>
            containerEnd := Block_Paths.First_Triple (sectors) +
              Unsigned_64 (path.Top_Slot) * Block_Paths.Middle_Span (sectors) +
              (Unsigned_64 (path.Middle_Slot) + 1) * pointers;
            leafSlot := path.Bottom_Slot;
            root := ino.tripleIndirectBlock;
            newRoot := root = 0;
            if not newRoot then
               Load (root, rootBuf);
               middle := rootBuf (path.Top_Slot);
            end if;
            newMiddle := middle = 0;
            if status = Write_Complete and then not newMiddle then
               Load (middle, middleBuf);
               leaf := middleBuf (path.Middle_Slot);
            end if;
            newLeaf := leaf = 0;
            if status = Write_Complete and then not newLeaf then
               Load (leaf, leafBuf);
            end if;
         when Block_Paths.Unsupported =>
            status := Write_File_Range_Unsupported;
      end case;
      if status /= Write_Complete then
         return;
      end if;
      pointerCount :=
        Boolean'Pos (newRoot) + Boolean'Pos (newMiddle) + Boolean'Pos (newLeaf);

      --  Blocks the request reaches, bounded by the leaf and the staging
      --  buffer; every one must still be unmapped.
      runBlocks := Unsigned_64'Min
        (Unsigned_64'Min (containerEnd - first, Maximum_Batch_Bytes / blockBytes),
         (offset + Unsigned_64'Min (available, Maximum_Batch_Bytes) +
            blockBytes - 1) / blockBytes);
      for index in 0 .. runBlocks - 1 loop
         if (if path.Kind = Block_Paths.Direct
             then ino.directBlocks (Natural (first + index)) /= 0
             else leafBuf (leafSlot + Natural (index)) /= 0)
         then
            if index = 0 then
               status := Write_Out_Of_Range;
               return;
            end if;
            runBlocks := index;
            exit;
         end if;
      end loop;
      --  Never let the allocation count wrap: shorten the run to what fits.
      declare
         room : constant Unsigned_64 :=
           (Unsigned_64 (Unsigned_32'Last) - Unsigned_64 (ino.numDiskSectors)) /
             Unsigned_64 (sectors);
      begin
         if room < Unsigned_64 (pointerCount) + 1 then
            status := Write_Out_Of_Range;
            return;
         end if;
         runBlocks := Unsigned_64'Min (runBlocks, room - Unsigned_64 (pointerCount));
      end;

      --  Prefer the block after the file's previous one, so appends coalesce.
      if first > 0 then
         declare
            previous, previousLeaf, previousMiddle : Unsigned_32;
            lookupStatus : Read_Status;
         begin
            resolveBlock (fs, ino, Unsigned_32 (first - 1), previous, previousLeaf,
                          previousMiddle, lookupStatus);
            if lookupStatus = Read_Complete and then previous /= 0 then
               goal := previous + 1;
            end if;
         end;
      end if;
      reserveBlocks (fs, Block_Number (goal), pointerCount + Natural (runBlocks), pointerCount + 1,
                     plan, status);
      if status /= Write_Complete then
         return;
      end if;
      runBlocks := Unsigned_64 (plan.Count - pointerCount);
      declare
         next_Pointer : Reserved_Index := Reserved_Index'First;
      begin
         if newRoot then
            root := Unsigned_32 (plan.Blocks (next_Pointer));
            next_Pointer := next_Pointer + 1;
         end if;
         if newMiddle then
            middle := Unsigned_32 (plan.Blocks (next_Pointer));
            next_Pointer := next_Pointer + 1;
         end if;
         if newLeaf then
            leaf := Unsigned_32 (plan.Blocks (next_Pointer));
         end if;
      end;

      --  The proved per-attachment transformations, applied in order. Only
      --  the first attachment adds new pointer blocks.
      candidate := ino;
      for index in 0 .. runBlocks - 1 loop
         case path.Kind is
            when Block_Paths.Direct =>
               Inode_Mappings.Prepare_Attachment
                 (candidate, Unsigned_32 (first + index), Data_Block (index), 0,
                  sectors, next, accepted);
            when Block_Paths.Single_Indirect =>
               Inode_Mappings.Prepare_Attachment
                 (candidate, Unsigned_32 (first + index), Data_Block (index), leaf,
                  sectors, next, accepted);
            when Block_Paths.Double_Indirect =>
               Double_Mappings.Prepare
                 (candidate, root, leaf, Data_Block (index), newLeaf and index = 0,
                  sectors, next, accepted);
            when Block_Paths.Triple_Indirect =>
               Triple_Mappings.Prepare
                 (candidate, root, middle, leaf, Data_Block (index),
                  newMiddle and index = 0, newLeaf and index = 0,
                  sectors, next, accepted);
            when Block_Paths.Unsupported =>
               accepted := False;
         end case;
         if not accepted then
            status := Write_Out_Of_Range; -- only an in-memory plan is dropped
            return;
         end if;
         candidate := next;
         if path.Kind /= Block_Paths.Direct then
            leafBuf (leafSlot + Natural (index)) := Data_Block (index);
         end if;
      end loop;
      if newLeaf then
         case path.Kind is
            when Block_Paths.Double_Indirect => rootBuf (path.Root_Slot) := leaf;
            when Block_Paths.Triple_Indirect => middleBuf (path.Middle_Slot) := leaf;
            when others => null;
         end case;
      end if;
      if newMiddle then
         rootBuf (path.Top_Slot) := middle;
      end if;

      commitReservation (fs, plan, status);
      if status /= Write_Complete then
         return;
      end if;

      --  Payload: whole fresh blocks, coalesced over physically contiguous
      --  reservations. Reservation metadata is already committed: after a
      --  transport error, stop; speculative rollback could compound it.
      bytes := Unsigned_64'Min (available, runBlocks * blockBytes - offset);
      payload (0 .. Natural (runBlocks * blockBytes) - 1) := [others => 0];
      declare
         data : Payload_Bytes (0 .. Natural (bytes) - 1)
           with Import, Address => source;
      begin
         payload (Natural (offset) .. Natural (offset + bytes) - 1) := data;
      end;
      declare
         start : Unsigned_64 := 0;
         length : Unsigned_64;
      begin
         while start < runBlocks loop
            length := 1;
            while start + length < runBlocks and then
              Unsigned_64 (Data_Block (start + length)) =
                Unsigned_64 (Data_Block (start)) + length
            loop
               length := length + 1;
            end loop;
            writeBytes
              (fs, Storage_Offset (Data_Block (start)) * Storage_Offset (fs.blkSize),
               payload (Natural (start * blockBytes))'Address,
               Storage_Count (length * blockBytes), status, payloadClass);
            if status /= Write_Complete then
               fs.writeQuarantined := True;
               status := Write_Recovery_Required;
               return;
            end if;
            start := start + length;
         end loop;
      end;

      --  Pointer blocks, each initialized before its parent points to it.
      --  A failed publication may already have reached storage: neither the
      --  pointer block nor its targets may return to the free pool.
      if path.Kind /= Block_Paths.Direct then
         Publish (leaf, leafBuf, Leaf_Pointers);
         if newLeaf and then path.Kind = Block_Paths.Double_Indirect then
            Publish (root, rootBuf, Middle_Pointers);
         elsif newLeaf and then path.Kind = Block_Paths.Triple_Indirect then
            Publish (middle, middleBuf, Middle_Pointers);
         end if;
         if newMiddle then
            Publish (root, rootBuf, Root_Pointers);
         end if;
         if status /= Write_Complete then
            fs.writeQuarantined := True;
            status := Write_Recovery_Required;
            invalidateBlockCache;
            return;
         end if;
      end if;
      ino := candidate;
      --  Keep the lookup caches warm with the acknowledged pointer blocks.
      --  This is not deferred write-back: a failure above invalidates them.
      selectCacheIdentity (fs);
      invalidateBlockCache;
      case path.Kind is
         when Block_Paths.Single_Indirect =>
            Single_Cache := (Block_Number => leaf, Data => leafBuf);
         when Block_Paths.Double_Indirect =>
            Double_Root_Cache := (Block_Number => root, Data => rootBuf);
            Double_Leaf_Cache := (Block_Number => leaf, Data => leafBuf);
         when Block_Paths.Triple_Indirect =>
            Triple_Root_Cache := (Block_Number => root, Data => rootBuf);
            Triple_Middle_Cache := (Block_Number => middle, Data => middleBuf);
            Triple_Leaf_Cache := (Block_Number => leaf, Data => leafBuf);
         when others => null;
      end case;
      consumed := bytes;
   end appendRun;

   procedure zeroExposedRange
     (fs : in out Filesystem; ino : Inode; first, limit : Unsigned_64;
      status : out Write_Status)
   is
      pos : Unsigned_64 := first;
      blockBytes : constant Unsigned_64 := Unsigned_64 (fs.blkSize);
      zeroes : constant String (1 .. Natural (fs.blkSize)) :=
        [others => Character'Val (0)];
      physical, pointerLeaf, pointerMiddle : Unsigned_32;
      readStatus : Read_Status;
   begin
      status := Write_Complete;
      while pos < limit loop
         declare
            withinBlock : constant Unsigned_64 := pos mod blockBytes;
            length : Unsigned_64 :=
              Unsigned_64'Min (blockBytes - withinBlock, limit - pos);
         begin
            resolveBlock
              (fs, ino, Unsigned_32 (pos / blockBytes), physical, pointerLeaf,
               pointerMiddle, readStatus);
            if readStatus /= Read_Complete then
               status := Write_Device_Error;
               return;
            end if;
            if physical /= 0 then
               writeBytes
                 (fs, Storage_Offset (physical) * Storage_Offset (fs.blkSize) +
                    Storage_Offset (withinBlock),
                  zeroes'Address, Storage_Count (length), status, File_Data);
               if status /= Write_Complete then
                  fs.writeQuarantined := True;
                  status := Write_Recovery_Required;
                  return;
               end if;
            end if;
            if physical = 0 then
               --  Skip an absent subtree, not each of its sparse data slots.
               --  Lookup succeeded above: an I/O error is never a hole.
               declare
                  sectors : constant Sector_Accounting.Block_Sectors :=
                    Sector_Accounting.Block_Sectors (fs.blkSize / 512);
                  path : constant Block_Paths.Block_Path :=
                    Block_Paths.Decode (pos / blockBytes, sectors);
                  pointers : constant Unsigned_64 := Block_Paths.Pointer_Count (sectors);
                  nextBlock : Unsigned_64 := pos / blockBytes + 1;
               begin
                  case path.Kind is
                     when Block_Paths.Single_Indirect =>
                        if ino.singleIndirectBlock = 0 then
                           nextBlock := NUM_DIRECT_BLOCKS + pointers;
                        end if;
                     when Block_Paths.Double_Indirect =>
                        if ino.doubleIndirectBlock = 0 then
                           nextBlock := Block_Paths.First_Triple (sectors);
                        elsif pointerLeaf = 0 then
                           nextBlock := Block_Paths.First_Double (sectors) +
                             (Unsigned_64 (path.Root_Slot) + 1) * pointers;
                        end if;
                     when Block_Paths.Triple_Indirect =>
                        if ino.tripleIndirectBlock = 0 then
                           nextBlock := Block_Paths.Block_Limit (sectors);
                        elsif pointerMiddle = 0 then
                           nextBlock := Block_Paths.First_Triple (sectors) +
                             (Unsigned_64 (path.Top_Slot) + 1) *
                               Block_Paths.Middle_Span (sectors);
                        elsif pointerLeaf = 0 then
                           nextBlock := Block_Paths.First_Triple (sectors) +
                             Unsigned_64 (path.Top_Slot) *
                               Block_Paths.Middle_Span (sectors) +
                             (Unsigned_64 (path.Middle_Slot) + 1) * pointers;
                        end if;
                     when others => null;
                  end case;
                  length := Unsigned_64'Min (nextBlock * blockBytes, limit) - pos;
               end;
            end if;
            pos := pos + length;
         end;
      end loop;
   end zeroExposedRange;

   procedure writeData
     (fs       : in out Filesystem;
      inodeNum : Unsigned_32;
      ino      : in out Inode;
      offset   : Unsigned_64;
      buf      : System.Address;
      count    : Unsigned_64;
      bytesWritten : out Unsigned_64;
      status       : out Write_Status)
   is
      remaining : Unsigned_64 := count;
      pos       : Unsigned_64 := offset;
      written   : Unsigned_64 := 0;
      terminalStatus : Write_Status := Write_Complete;
      originalInode : constant Inode := ino;
      gapCleared : Boolean := offset <= fileSize (ino);
      inodeCached : Boolean := False;
      sectors : constant Sector_Accounting.Block_Sectors :=
        Sector_Accounting.Block_Sectors (fs.blkSize / 512);
   begin
      bytesWritten := 0;
      status := Write_Complete;
      if Ext2_Support.Check_File (ino, Unlinked_Allowed => True) /= Ext2_Support.File_Allowed then
         status := Write_Object_Unsupported;
         return;
      end if;
      if fs.writeQuarantined then
         status := Write_Recovery_Required;
         return;
      end if;

      --  Keep all subsequent position arithmetic inside Unsigned_64.
      if count > 0 and then Is_Read_Only (fs.device.description) then
         status := Write_Read_Only;
         return;
      end if;
      if count > Unsigned_64'Last - offset then
         status := Write_Out_Of_Range;
         return;
      end if;
      if count > 0 and then offset + count > fileSize (ino) and then
        not Ext2_Support.Size_Admitted (offset + count, fs.sb.readOnlyFeatures)
      then
         status := Write_File_Range_Unsupported;
         return;
      end if;

      if count > 0 and then offset > fileSize (ino) then
         --  Bound the gap walk before converting logical indices or issuing
         --  any I/O. The write loop still reports partial supported prefixes.
         if offset >= Unsigned_64 (fs.blkSize) * Block_Paths.Block_Limit (sectors)
         then
            status := Write_File_Range_Unsupported;
            return;
         end if;
      end if;

      --  Each allocation run or overwrite batch is one journal operation.
      startHandle (fs, Write_Credits);
      while remaining > 0 loop
         declare
            logicalIndex : constant Unsigned_64 :=
              pos / Unsigned_64 (fs.blkSize);
            blockOffset : constant Unsigned_32 :=
              Unsigned_32 (pos mod Unsigned_64 (fs.blkSize));
            physBlock   : Unsigned_32;
            pointerLeaf, pointerMiddle : Unsigned_32;
            lookupStatus : Read_Status;
            dataStatus    : Write_Status;
            canWrite    : Unsigned_64 :=
              Unsigned_64 (fs.blkSize - blockOffset);
            path : constant Block_Paths.Block_Path :=
              Block_Paths.Decode (logicalIndex, sectors);
         begin
            if path.Kind = Block_Paths.Unsupported then
               terminalStatus := Write_File_Range_Unsupported;
               exit;
            end if;

            declare
               logBlock : constant Unsigned_32 :=
                 Unsigned_32 (logicalIndex);
            begin
               resolveBlock
                 (fs, ino, logBlock, physBlock, pointerLeaf, pointerMiddle, lookupStatus);
               if lookupStatus /= Read_Complete then
                  terminalStatus :=
                    (case lookupStatus is
                       when Read_Out_Of_Range => Write_Out_Of_Range,
                       when Read_File_Range_Unsupported =>
                          Write_File_Range_Unsupported,
                       when others => Write_Device_Error);
                  exit;
               end if;

               if canWrite > remaining then
                  canWrite := remaining;
               end if;
               --  Reject unrepresentable accounting before even zeroing a
               --  growth gap. Neither step may precede the allocation plan.
               if physBlock = 0 and then
                 (case path.Kind is
                    when Block_Paths.Double_Indirect =>
                      not Double_Mappings.Fits (ino, pointerLeaf = 0, sectors),
                    when Block_Paths.Triple_Indirect =>
                      not Triple_Mappings.Fits
                        (ino, pointerMiddle = 0, pointerLeaf = 0, sectors),
                    when others => not Inode_Mappings.Fits (ino, logBlock, sectors))
               then
                  terminalStatus := Write_Out_Of_Range;
                  exit;
               end if;
               if physBlock = 0 and then not inodeCached then
                  --  Read the inode's table block before any allocation, so
                  --  publishing the new mapping needs no read that could
                  --  fail after the reservation is on disk.
                  declare
                     current : Inode;
                     readStatus : Read_Status;
                  begin
                     Ext2.Metadata.readInode (fs, inodeNum, current, readStatus);
                     if readStatus /= Read_Complete then
                        terminalStatus :=
                          (if readStatus = Read_Out_Of_Range then Write_Out_Of_Range
                           else Write_Device_Error);
                        exit;
                     end if;
                     inodeCached := True;
                  end;
               end if;
               if not gapCleared then
                  zeroExposedRange (fs, ino, fileSize (ino), offset, dataStatus);
                  if dataStatus /= Write_Complete then
                     terminalStatus := dataStatus;
                     exit;
                  end if;
                  gapCleared := True;
               end if;

               if physBlock = 0 then
                  appendRun
                    (fs, ino, pos, buf + Storage_Offset (written), remaining,
                     canWrite, dataStatus);
                  if dataStatus /= Write_Complete then
                     terminalStatus := dataStatus;
                     exit;
                  end if;
                  if fs.journal.Active then
                     --  Journaled: the run's inode goes into its operation,
                     --  so a commit after it never leaves the run unlinked.
                     declare
                        published : Inode := ino;
                        runEnd : constant Unsigned_64 := pos + canWrite;
                     begin
                        if runEnd > fileSize (published) then
                           published.sizeLo := Unsigned_32 (runEnd and 16#FFFF_FFFF#);
                           published.sizeHi_DirACL :=
                             Unsigned_32 (Shift_Right (runEnd, 32));
                        end if;
                        writeInode (fs, inodeNum, published, dataStatus);
                        if dataStatus /= Write_Complete then
                           terminalStatus := dataStatus;
                           exit;
                        end if;
                     end;
                  end if;
               else
                  --  Batch only complete, already allocated filesystem blocks
                  --  inside the published file extent. Allocation, EOF growth
                  --  and partial-sector writes retain their checked paths.
                  --  Bound a batch by BOTH the grant and provider limits so a
                  --  failed completion never hides an earlier command inside it.
                  if blockOffset = 0 and then
                    fs.blkSize >= fs.device.description.logicalBlockSize and then
                    pos < fileSize (originalInode)
                  then
                     declare
                        blockBytes : constant Unsigned_64 := Unsigned_64 (fs.blkSize);
                        transferBytes : constant Unsigned_64 := Unsigned_64'Min
                          (Unsigned_64 (fs.device.grantBytes),
                           Unsigned_64 (fs.device.description.maxTransferBlocks) *
                             Unsigned_64 (fs.device.description.logicalBlockSize));
                        maxBlocks : constant Unsigned_64 := Unsigned_64'Min
                          (Unsigned_64'Min (transferBytes / blockBytes,
                                            remaining / blockBytes),
                           Unsigned_64'Min
                             ((fileSize (originalInode) - pos) / blockBytes,
                              Block_Paths.Block_Limit (sectors) - logicalIndex));
                        contiguous : Unsigned_64 := 1;
                        nextPhysical : Unsigned_32;
                     begin
                        while contiguous < maxBlocks loop
                           getDataBlock
                             (fs, ino, logBlock + Unsigned_32 (contiguous),
                              nextPhysical, lookupStatus);
                           exit when lookupStatus /= Read_Complete;
                           exit when Unsigned_64 (nextPhysical) /=
                             Unsigned_64 (physBlock) + contiguous;
                           contiguous := contiguous + 1;
                        end loop;
                        if lookupStatus /= Read_Complete then
                           --  A failed speculative lookup must not be retried,
                           --  skipped, or counted as a completed data batch.
                           terminalStatus :=
                             (case lookupStatus is
                                when Read_Out_Of_Range => Write_Out_Of_Range,
                                when Read_File_Range_Unsupported =>
                                  Write_File_Range_Unsupported,
                                when others => Write_Device_Error);
                           exit;
                        end if;
                        if contiguous > 1 then
                           canWrite := contiguous * blockBytes;
                        end if;
                     end;
                  end if;
                  writeBytes
                    (fs,
                     Storage_Offset (physBlock) *
                       Storage_Offset (fs.blkSize) +
                       Storage_Offset (blockOffset),
                     buf + Storage_Offset (written),
                     Storage_Count (canWrite),
                     dataStatus, File_Data);
                  if dataStatus /= Write_Complete then
                     terminalStatus := dataStatus;
                     exit;
                  end if;
               end if;
            end;

            written   := written + canWrite;
            pos       := pos + canWrite;
            remaining := remaining - canWrite;
         end;
         if remaining > 0 then
            stopHandle (fs);
            startHandle (fs, Write_Credits);
         end if;
      end loop;

      --  Update inode size if we wrote past EOF
      declare
         newEnd : constant Unsigned_64 := offset + written;
         oldSize : constant Unsigned_64 := fileSize (ino);
      begin
         if written > 0 and then newEnd > oldSize then
            ino.sizeLo := Unsigned_32 (newEnd and 16#FFFF_FFFF#);
            ino.sizeHi_DirACL :=
              Unsigned_32 (Shift_Right (newEnd, 32));
         end if;
      end;

      --  An earlier completed attachment/extension still needs publication.
      --  Do not issue that metadata I/O after a later transport failure.
      if terminalStatus in Write_Device_Error | Write_Recovery_Required and then
        ino /= originalInode
      then
         fs.writeQuarantined := True;
      end if;

      --  Only a write that changed bytes changes the times; a failed one
      --  must not, and must not publish an inode for its sake.
      if written > 0 and then not fs.writeQuarantined then
         Stamp (ino, Content_Changed);
      end if;
      if written > 0 and then not fs.writeQuarantined and then
        ino /= originalInode
      then
         --  Data-only overwrites within the same second do not alter the
         --  inode. Avoid a block-group read and inode-sector RMW in that
         --  common path. Compare the whole
         --  typed record so future timestamp/accounting fields cannot silently
         --  bypass publication by forgetting to set a separate dirty flag.
         --  Publish new pointers and size only after their data blocks have
         --  completed.  A metadata write failure supersedes the earlier
         --  terminal state because the committed extent is then uncertain.
         declare
            inodeStatus : Write_Status;
         begin
            writeInode (fs, inodeNum, ino, inodeStatus);
            if inodeStatus /= Write_Complete then
               fs.writeQuarantined := True;
            end if;
         end;
      end if;
      stopHandle (fs);

      if fs.writeQuarantined then
         --  No reliable published prefix can be promised. The caller must
         --  retire shared inode aliases rather than publish the candidate.
         bytesWritten := 0;
         status := Write_Recovery_Required;
      else
         bytesWritten := written;
         status := terminalStatus;
      end if;
   end writeData;

end Ext2.Files;
