------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Ext2 filesystem operations for userspace ramdisk server.
--  Reads directly from a memory-mapped ramdisk image (no disk I/O).
------------------------------------------------------------------------------
pragma Ada_2022;
with System.Storage_Elements; use System.Storage_Elements;

with CuBit.Messages; use CuBit.Messages;
with CuBit.Block_Devices; use CuBit.Block_Devices;
with CuBit.Filesystems;
with CuBit.Directory_Paths;
with CuBit.File_Access;
with Directory_Blocks;
with Directory_Commit;

package body Ext2 is

   use type System.Address;

   --  Indirect block cache (avoids re-reading same block per getDataBlock call)
   --  Sized for max 4KB ext2 blocks (1024 ptrs); 1KB blocks use first 256.
   cachedIndBlockNum : Unsigned_32 := 0;
   cachedIndBuf      : array (0 .. 1023) of Unsigned_32;

   cachedDIndL1Num   : Unsigned_32 := 0;
   cachedDIndL1Buf   : array (0 .. 1023) of Unsigned_32;

   cachedDIndL2Num   : Unsigned_32 := 0;
   cachedDIndL2Buf   : array (0 .. 1023) of Unsigned_32;

   cacheIdentityValid : Boolean := False;
   cachedBackend       : BlockBackend := MEMORY;
   cachedBase          : System.Address := System.Null_Address;
   cachedCapSlot       : Unsigned_64 := 0;

   NULL_INODE : constant Inode :=
     (typeAndPermissions => 0,
      uid => 0, sizeLo => 0,
      accessedTime => 0, creationTime => 0,
      modifiedTime => 0, deletedTime => 0,
      gid => 0, numHardLinks => 0,
      numDiskSectors => 0, flags => 0,
      osSpecific1 => 0,
      directBlocks => [others => 0],
      singleIndirectBlock => 0,
      doubleIndirectBlock => 0,
      tripleIndirectBlock => 0,
      generationNumber => 0,
      fileACL => 0, sizeHi_DirACL => 0,
      fragmentBlockAddr => 0,
      osSpecific2A => 0, osSpecific2B => 0,
      osSpecific2C => 0);

   procedure invalidateBlockCache is
   begin
      cachedIndBlockNum := 0;
      cachedDIndL1Num   := 0;
      cachedDIndL2Num   := 0;
   end invalidateBlockCache;

   procedure selectCacheIdentity (fs : Filesystem) is
   begin
      if not cacheIdentityValid or else
         cachedBackend /= fs.backend or else
         cachedBase /= fs.base or else
         cachedCapSlot /= fs.device.endpointSlot
      then
         invalidateBlockCache;
         cachedBackend := fs.backend;
         cachedBase := fs.base;
         cachedCapSlot := fs.device.endpointSlot;
         cacheIdentityValid := True;
      end if;
   end selectCacheIdentity;

   --  Read bytes from the filesystem at a byte offset.
   --  Dispatches on fs.backend: MEMORY does a bounded direct copy; the other
   --  path uses a generic Block.Device.V1 session.
   procedure readBytes
     (fs     : Filesystem;
      offset : Storage_Offset;
      dest   : System.Address;
      len    : Storage_Count;
      status : out Read_Status)
   is
   begin
      status := Read_Device_Error;
      case fs.backend is
         when MEMORY =>
            if offset < 0 or else
               Unsigned_64 (offset) > fs.imageSize or else
               Unsigned_64 (len) > fs.imageSize - Unsigned_64 (offset)
            then
               status := Read_Out_Of_Range;
               return;
            end if;

            declare
               src : String (1 .. Natural (len))
                 with Import, Address => fs.base + offset;
               dst : String (1 .. Natural (len))
                 with Import, Address => dest;
            begin
               dst := src;
            end;
            status := Read_Complete;

         when BLOCK_DEVICE =>
            if offset < 0 then
               status := Read_Out_Of_Range;
               return;
            end if;

            --  Convert byte offset/len to multi-sector reads via IPC.
            --  Read up to the session transfer bound per IPC call into the
            --  transitional grant buffer, then copy to dest.
            declare
               byteOff       : Unsigned_64 := Unsigned_64 (offset);
               remaining     : Unsigned_64 := Unsigned_64 (len);
               dstOff        : Storage_Offset := 0;
               lba           : Unsigned_64;
               secOff        : Unsigned_64;
               copyLen       : Unsigned_64;
               sectorsNeeded : Unsigned_64;
               deviceBlockSize : constant Unsigned_64 :=
                 Unsigned_64 (fs.device.description.logicalBlockSize);
               maxSectors    : constant Unsigned_64 := Unsigned_64'Min
                 (Unsigned_64 (fs.device.grantBytes) / deviceBlockSize,
                  Unsigned_64 (fs.device.description.maxTransferBlocks));
               msg           : Message;
               ignore        : MessageTag;
            begin
               while remaining > 0 loop
                  lba    := byteOff / deviceBlockSize;
                  secOff := byteOff mod deviceBlockSize;

                  if lba >= fs.device.description.blockCount then
                     status := Read_Out_Of_Range;
                     return;
                  end if;

                  --  Calculate how many sectors to read in this batch
                  sectorsNeeded :=
                    (remaining + secOff + deviceBlockSize - 1) /
                    deviceBlockSize;
                  if sectorsNeeded > maxSectors then
                     sectorsNeeded := maxSectors;
                  end if;

                  if sectorsNeeded = 0 or else
                     sectorsNeeded > fs.device.description.blockCount - lba
                  then
                     status := Read_Out_Of_Range;
                     return;
                  end if;

                  --  Read multiple sectors from driver
                  msg.tag := (label  => OP_READ_BLOCKS,
                              length => 4,
                              flags  => 0,
                              badge  => 0);
                  msg.capBadge := 0;
                  msg.words := [0 => lba,
                                1 => fs.device.grant.slot,
                                2 => sectorsNeeded,
                                3 => fs.device.grant.generation];

                  ignore := capCall (fs.device.endpointSlot, msg);

                  if msg.tag.label /= REPLY_OK or else
                     msg.tag.length /= 1 or else
                     msg.words (0) /= sectorsNeeded * deviceBlockSize
                  then
                     debugPrint ("Ext2: read reply not OK." & ASCII.LF);
                     return;
                  end if;

                  --  Copy relevant portion from grant buffer to dest
                  copyLen :=
                    sectorsNeeded * deviceBlockSize - secOff;
                  if copyLen > remaining then
                     copyLen := remaining;
                  end if;

                  declare
                     srcSlice : String (1 .. Natural (copyLen))
                       with Import,
                            Address => fs.device.grantBuffer +
                              Storage_Offset (secOff);
                     dstSlice : String (1 .. Natural (copyLen))
                       with Import,
                            Address => dest + dstOff;
                  begin
                     dstSlice := srcSlice;
                  end;

                  byteOff   := byteOff + copyLen;
                  dstOff    := dstOff + Storage_Offset (copyLen);
                  remaining := remaining - copyLen;
               end loop;
               status := Read_Complete;
            end;
      end case;
   end readBytes;

   --  Metadata readers still use a value-only API.  Fail closed by clearing
   --  the whole destination if transport validation fails; file-data reads
   --  use the checked overload directly and surface the failure to clients.
   procedure readBytes
     (fs     : Filesystem;
      offset : Storage_Offset;
      dest   : System.Address;
      len    : Storage_Count)
   is
      readStatus : Read_Status;
   begin
      readBytes (fs, offset, dest, len, readStatus);
      if readStatus /= Read_Complete then
         declare
            dst : String (1 .. Natural (len))
              with Import, Address => dest;
         begin
            dst := [others => Character'Val (0)];
         end;
      end if;
   end readBytes;

   --  Read a full block from the ramdisk
   procedure readBlock
     (fs       : Filesystem;
      blockNum : Unsigned_32;
      dest     : System.Address)
   is
      offset : constant Storage_Offset :=
        Storage_Offset (blockNum) * Storage_Offset (fs.blkSize);
   begin
      readBytes (fs, offset, dest, Storage_Count (fs.blkSize));
   end readBlock;

   procedure readBlock
     (fs       : Filesystem;
      blockNum : Unsigned_32;
      dest     : System.Address;
      status   : out Read_Status)
   is
   begin
      if blockNum >= fs.sb.blockCount then
         status := Read_Out_Of_Range;
         return;
      end if;

      readBytes
        (fs,
         Storage_Offset (blockNum) * Storage_Offset (fs.blkSize),
         dest, Storage_Count (fs.blkSize), status);
   end readBlock;

   function blockSize (sb : Superblock) return Unsigned_32 is
   begin
      return Shift_Left (Unsigned_32'(1024), Natural (sb.blockShift));
   end blockSize;

   --  Validate the geometry assumptions used by the bounded ext2
   --  implementation.  Doing this once at mount keeps malformed on-disk
   --  values from becoming divisors, array bounds, or bitmap indices in the
   --  I/O path.
   function supportedSuperblock (sb : Superblock) return Boolean is
      size : Unsigned_32;
      inodeBytes : Unsigned_32;
      allocatableBlocks : Unsigned_32;
   begin
      if sb.signature /= EXT2_SIGNATURE or else
         sb.blockShift > 2 or else
         sb.blockCount = 0 or else
         sb.blockCount <= sb.firstDataBlock or else
         sb.inodeCount = 0 or else
         sb.blocksPerBlockGroup = 0 or else
         sb.inodesPerBlockGroup = 0
      then
         return False;
      end if;

      size := blockSize (sb);
      inodeBytes :=
        (if sb.majorVersion >= 1 then Unsigned_32 (sb.inodeSize) else 128);
      allocatableBlocks := sb.blockCount - sb.firstDataBlock;

      return inodeBytes >= Inode'Size / 8 and then
        inodeBytes <= size and then
        inodeBytes mod 4 = 0 and then
        sb.blocksPerBlockGroup <= size * 8 and then
        sb.inodesPerBlockGroup <= size * 8 and then
        sb.freeBlocks <= allocatableBlocks and then
        sb.freeInodes <= sb.inodeCount;
   end supportedSuperblock;

   function inodeType (ino : Inode) return Unsigned_8 is
   begin
      return Unsigned_8 (Shift_Right (ino.typeAndPermissions, 12) and 16#F#);
   end inodeType;

   function fileSize (ino : Inode) return Unsigned_64 is
   begin
      return Unsigned_64 (ino.sizeHi_DirACL) * 16#1_0000_0000# +
             Unsigned_64 (ino.sizeLo);
   end fileSize;

   procedure readInode
     (fs : Filesystem; inodeNum : Unsigned_32;
      ino : out Inode; status : out Read_Status)
   is
      blockGroup, inodeIndex : Unsigned_32;
      bgdtOffset, inodeTableByteOffset : Storage_Offset;
      bgd : BlockGroupDescriptor;
      inoSize : Unsigned_32;
   begin
      ino := NULL_INODE;
      status := Read_Out_Of_Range;
      if inodeNum = 0 or else inodeNum > fs.sb.inodeCount then
         return;
      end if;
      blockGroup := (inodeNum - 1) / fs.sb.inodesPerBlockGroup;
      inodeIndex := (inodeNum - 1) mod fs.sb.inodesPerBlockGroup;
      --  Widen before multiplying filesystem block numbers into byte offsets.
      bgdtOffset :=
        (Storage_Offset (fs.sb.firstDataBlock) + 1) *
          Storage_Offset (fs.blkSize) +
        Storage_Offset (blockGroup) * (BlockGroupDescriptor'Size / 8);
      readBytes
        (fs, bgdtOffset, bgd'Address, BlockGroupDescriptor'Size / 8, status);
      if status /= Read_Complete then
         return;
      end if;
      if bgd.inodeTableAddr = 0 or else
        bgd.inodeTableAddr >= fs.sb.blockCount
      then
         status := Read_Out_Of_Range;
         return;
      end if;
      inoSize := (if fs.sb.majorVersion >= 1 then
                    Unsigned_32 (fs.sb.inodeSize) else 128);
      inodeTableByteOffset :=
        Storage_Offset (bgd.inodeTableAddr) * Storage_Offset (fs.blkSize) +
        Storage_Offset (inodeIndex) * Storage_Offset (inoSize);
      if inodeTableByteOffset >
        Storage_Offset (fs.sb.blockCount) * Storage_Offset (fs.blkSize) -
          Inode'Size / 8
      then
         status := Read_Out_Of_Range;
         return;
      end if;
      readBytes
        (fs, inodeTableByteOffset, ino'Address, Inode'Size / 8, status);
      if status /= Read_Complete then
         ino := NULL_INODE;
      end if;
   end readInode;

   procedure readInode
     (fs : Filesystem; inodeNum : Unsigned_32; ino : out Inode)
   is
      status : Read_Status;
   begin
      readInode (fs, inodeNum, ino, status);
   end readInode;

   procedure lookupInDir
     (fs      : Filesystem;
      dirIno  : Inode;
      name : String;
      inodeNum : out Unsigned_32;
      status : out Directory_Lookup_Status)
   is
      entries   : CuBit.Filesystems.Directory_Entries;
      cursor    : Unsigned_64 := 0;
      nextCursor : Unsigned_64;
      count     : Natural;
      pageStatus : Directory_Read_Status;
   begin
      inodeNum := 0;
      status := Lookup_Not_Found;
      loop
         readDirectoryPage
           (fs, dirIno, cursor, entries, count, nextCursor, pageStatus);

         if pageStatus not in Directory_Page_Complete | Directory_End then
            status := (case pageStatus is
              when Directory_Malformed => Lookup_Malformed,
              when Directory_Device_Error => Lookup_Device_Error,
              when Directory_Out_Of_Range => Lookup_Out_Of_Range,
              when Directory_Range_Unsupported => Lookup_Range_Unsupported,
              when others => Lookup_Not_Found);
            return;
         end if;

         if count > 0 then
            for index in 0 .. count - 1 loop
               declare
                  matches : Boolean :=
                    Natural (entries (index).nameLength) = name'Length;
               begin
                  if matches then
                     for characterIndex in 1 .. name'Length loop
                        if Character'Val
                          (entries (index).name (characterIndex)) /=
                          name (name'First + characterIndex - 1)
                        then
                           matches := False;
                           exit;
                        end if;
                     end loop;
                  end if;

                  if matches then
                     inodeNum := Unsigned_32 (entries (index).objectHint);
                     status := Lookup_Found;
                     return;
                  end if;
               end;
            end loop;
         end if;

         exit when pageStatus = Directory_End;
         if nextCursor <= cursor then
            status := Lookup_Malformed;
            return;
         end if;
         cursor := nextCursor;
      end loop;

      status := Lookup_Not_Found;
   end lookupInDir;


   function lookupInDir
     (fs : Filesystem; dirIno : Inode; name : String) return Unsigned_32
   is
      inodeNum : Unsigned_32;
      status : Directory_Lookup_Status;
   begin
      lookupInDir (fs, dirIno, name, inodeNum, status);
      return inodeNum;
   end lookupInDir;

   function resolvePath
     (fs   : Filesystem;
      path : String) return Unsigned_32
   is
      currentInode : Unsigned_32 := ROOT_INODE;
      ino : Inode;
      nameStart : Natural := path'First;
      nameEnd   : Natural;
   begin
      --  Skip leading '/'
      while nameStart <= path'Last and then path (nameStart) = '/' loop
         nameStart := nameStart + 1;
      end loop;

      --  Empty path = root
      if nameStart > path'Last then
         return ROOT_INODE;
      end if;

      --  Walk each path component
      while nameStart <= path'Last loop
         --  Find end of current component
         nameEnd := nameStart;
         while nameEnd <= path'Last and then path (nameEnd) /= '/' loop
            nameEnd := nameEnd + 1;
         end loop;

         --  Read current directory inode
         readInode (fs, currentInode, ino);

         --  Look up the component
         currentInode := lookupInDir
           (fs, ino, path (nameStart .. nameEnd - 1));

         if currentInode = 0 then
            return 0;  --  Not found
         end if;

         --  Skip trailing '/'
         nameStart := nameEnd + 1;
         while nameStart <= path'Last and then path (nameStart) = '/' loop
            nameStart := nameStart + 1;
         end loop;
      end loop;

      return currentInode;
   end resolvePath;

   procedure resolvePath
     (fs : Filesystem; path : String; inodeNum : out Unsigned_32;
      status : out Directory_Lookup_Status)
   is
      current : Unsigned_32 := ROOT_INODE;
      ino : Inode;
      readStatus : Read_Status;
      first : Natural := path'First;
      last : Natural;
   begin
      inodeNum := 0;
      status := Lookup_Malformed;
      if path'Length > CuBit.Directory_Paths.Maximum_Bytes or else
        not CuBit.File_Access.Valid_Path (path)
      then
         return;
      end if;
      while first <= path'Last loop
         if path (first) = '/' then
            first := first + 1;
         else
            last := first;
            while last < path'Last and then path (last + 1) /= '/' loop
               last := last + 1;
            end loop;
            if not CuBit.Directory_Paths.Valid_Child_Name (path (first .. last)) then
               status := Lookup_Malformed;
               return;
            end if;
            readInode (fs, current, ino, readStatus);
            if readStatus /= Read_Complete then
               status := (if readStatus = Read_Out_Of_Range then
                            Lookup_Out_Of_Range else Lookup_Device_Error);
               return;
            end if;
            lookupInDir (fs, ino, path (first .. last), current, status);
            if status /= Lookup_Found then
               return;
            end if;
            first := last + 1;
         end if;
      end loop;
      inodeNum := current;
      status := Lookup_Found;
   end resolvePath;

   --  Get the block number for a given logical block index in a file.
   --  Handles direct blocks and single indirect blocks.
   function getDataBlock
     (fs       : Filesystem;
      ino      : Inode;
      logBlock : Unsigned_32) return Unsigned_32
   is
      ptrsPerBlock : constant Unsigned_32 := fs.blkSize / 4;
   begin
      selectCacheIdentity (fs);

      --  Direct blocks (0..11)
      if logBlock < NUM_DIRECT_BLOCKS then
         return ino.directBlocks (Natural (logBlock));
      end if;

      --  Single indirect (12 .. 12+ptrsPerBlock-1)
      declare
         indirectIdx : constant Unsigned_32 :=
           logBlock - Unsigned_32 (NUM_DIRECT_BLOCKS);
      begin
         if indirectIdx < ptrsPerBlock then
            if ino.singleIndirectBlock = 0 then
               return 0;
            end if;

            --  Read indirect block only if not cached
            if ino.singleIndirectBlock /= cachedIndBlockNum then
               readBlock (fs, ino.singleIndirectBlock,
                          cachedIndBuf'Address);
               cachedIndBlockNum := ino.singleIndirectBlock;
            end if;
            return cachedIndBuf (Natural (indirectIdx));
         end if;
      end;

      --  Double indirect (for larger files)
      declare
         diIdx : constant Unsigned_32 :=
           logBlock - Unsigned_32 (NUM_DIRECT_BLOCKS) - ptrsPerBlock;
         l1Idx : constant Unsigned_32 := diIdx / ptrsPerBlock;
         l2Idx : constant Unsigned_32 := diIdx mod ptrsPerBlock;
      begin
         if l1Idx < ptrsPerBlock then
            if ino.doubleIndirectBlock = 0 then
               return 0;
            end if;

            --  Cache L1 (top-level double-indirect) block
            if ino.doubleIndirectBlock /= cachedDIndL1Num then
               readBlock (fs, ino.doubleIndirectBlock,
                          cachedDIndL1Buf'Address);
               cachedDIndL1Num := ino.doubleIndirectBlock;
            end if;

            if cachedDIndL1Buf (Natural (l1Idx)) = 0 then
               return 0;
            end if;

            --  Cache L2 (second-level double-indirect) block
            if cachedDIndL1Buf (Natural (l1Idx)) /= cachedDIndL2Num then
               readBlock (fs, cachedDIndL1Buf (Natural (l1Idx)),
                          cachedDIndL2Buf'Address);
               cachedDIndL2Num := cachedDIndL1Buf (Natural (l1Idx));
            end if;

            return cachedDIndL2Buf (Natural (l2Idx));
         end if;
      end;

      return 0;  --  Beyond supported range
   end getDataBlock;

   procedure readDirectoryPage
     (fs          : Filesystem;
      dirIno      : Inode;
      cursor      : Unsigned_64;
      entries     : out CuBit.Filesystems.Directory_Entries;
      entryCount  : out Natural;
      nextCursor  : out Unsigned_64;
      status      : out Directory_Read_Status)
   is
      use CuBit.Filesystems;
      size : constant Unsigned_64 := fileSize (dirIno);
      blockBuf : String (1 .. Natural (fs.blkSize))
        with Alignment => 8;
      scanCursor : Unsigned_64 := cursor;
      loadedLogicalBlock : Unsigned_64 := Unsigned_64'Last;

      procedure locateDirectoryBlock
        (logicalBlock : Unsigned_64;
         physicalBlock : out Unsigned_32;
         result : out Directory_Read_Status)
      is
         pointersPerBlock : constant Unsigned_32 := fs.blkSize / 4;
         pointerStatus : Read_Status;
         pointerBlock : array (0 .. 1023) of Unsigned_32
           with Alignment => 8;
         secondPointerBlock : array (0 .. 1023) of Unsigned_32
           with Alignment => 8;
         logical32 : Unsigned_32;
      begin
         physicalBlock := 0;
         result := Directory_Malformed;
         if logicalBlock > Unsigned_64 (Unsigned_32'Last) then
            result := Directory_Range_Unsupported;
            return;
         end if;
         logical32 := Unsigned_32 (logicalBlock);

         if logical32 < Unsigned_32 (NUM_DIRECT_BLOCKS) then
            physicalBlock := dirIno.directBlocks (Natural (logical32));
         elsif logical32 - Unsigned_32 (NUM_DIRECT_BLOCKS) <
           pointersPerBlock
         then
            if dirIno.singleIndirectBlock = 0 then
               return;
            end if;
            readBlock
              (fs, dirIno.singleIndirectBlock, pointerBlock'Address,
               pointerStatus);
            if pointerStatus /= Read_Complete then
               result :=
                 (if pointerStatus = Read_Out_Of_Range then
                     Directory_Out_Of_Range else Directory_Device_Error);
               return;
            end if;
            physicalBlock := pointerBlock
              (Natural (logical32 - Unsigned_32 (NUM_DIRECT_BLOCKS)));
         else
            declare
               doubleIndex : constant Unsigned_32 :=
                 logical32 - Unsigned_32 (NUM_DIRECT_BLOCKS) -
                 pointersPerBlock;
               firstIndex : constant Unsigned_32 :=
                 doubleIndex / pointersPerBlock;
               secondIndex : constant Unsigned_32 :=
                 doubleIndex mod pointersPerBlock;
            begin
               if firstIndex >= pointersPerBlock then
                  result := Directory_Range_Unsupported;
                  return;
               end if;
               if dirIno.doubleIndirectBlock = 0 then
                  return;
               end if;
               readBlock
                 (fs, dirIno.doubleIndirectBlock, pointerBlock'Address,
                  pointerStatus);
               if pointerStatus /= Read_Complete then
                  result :=
                    (if pointerStatus = Read_Out_Of_Range then
                        Directory_Out_Of_Range else Directory_Device_Error);
                  return;
               end if;
               if pointerBlock (Natural (firstIndex)) = 0 then
                  return;
               end if;
               readBlock
                 (fs, pointerBlock (Natural (firstIndex)),
                  secondPointerBlock'Address, pointerStatus);
               if pointerStatus /= Read_Complete then
                  result :=
                    (if pointerStatus = Read_Out_Of_Range then
                        Directory_Out_Of_Range else Directory_Device_Error);
                  return;
               end if;
               physicalBlock := secondPointerBlock (Natural (secondIndex));
            end;
         end if;

         if physicalBlock = 0 or else physicalBlock >= fs.sb.blockCount then
            result := Directory_Malformed;
         else
            result := Directory_Page_Complete;
         end if;
      end locateDirectoryBlock;
   begin
      entries := [others =>
        (objectHint => 0, sizeBytes => 0, nameLength => 0,
         kind => DIRECTORY_KIND_UNKNOWN, flags => 0, reserved => 0,
         name => [others => 0])];
      entryCount := 0;
      nextCursor := cursor;
      status := Directory_Malformed;

      if inodeType (dirIno) /= INODE_DIRECTORY or else cursor > size then
         return;
      end if;

      while scanCursor < size loop
         declare
            logicalBlock : constant Unsigned_64 :=
              scanCursor / Unsigned_64 (fs.blkSize);
            blockOffset : constant Unsigned_64 :=
              scanCursor mod Unsigned_64 (fs.blkSize);
            blockRemaining : constant Unsigned_64 :=
              Unsigned_64 (fs.blkSize) - blockOffset;
            fileRemaining : constant Unsigned_64 := size - scanCursor;
            physicalBlock : Unsigned_32;
            locateStatus : Directory_Read_Status;
         begin
            if blockRemaining < DirectoryEntry'Size / 8 or else
               fileRemaining < DirectoryEntry'Size / 8
            then
               status := Directory_Malformed;
               return;
            end if;

            if loadedLogicalBlock /= logicalBlock then
               locateDirectoryBlock
                 (logicalBlock, physicalBlock, locateStatus);
               if locateStatus /= Directory_Page_Complete then
                  status := locateStatus;
                  return;
               end if;
               declare
                  blockStatus : Read_Status;
               begin
                  readBlock
                    (fs, physicalBlock, blockBuf'Address, blockStatus);
                  if blockStatus /= Read_Complete then
                     status :=
                       (if blockStatus = Read_Out_Of_Range then
                           Directory_Out_Of_Range else
                           Directory_Device_Error);
                     return;
                  end if;
               end;
               loadedLogicalBlock := logicalBlock;
            end if;

            declare
               dent : DirectoryEntry
                 with Import,
                      Address => blockBuf'Address +
                        Storage_Offset (blockOffset);
               recordLength : constant Unsigned_64 :=
                 Unsigned_64 (dent.length);
               nameLength : constant Natural := Natural (dent.nameLength);
            begin
               if recordLength < DirectoryEntry'Size / 8 or else
                  recordLength mod 4 /= 0 or else
                  recordLength > blockRemaining or else
                  recordLength > fileRemaining or else
                  Unsigned_64 (nameLength) >
                    recordLength - DirectoryEntry'Size / 8 or else
                  dent.inode > fs.sb.inodeCount
               then
                  status := Directory_Malformed;
                  return;
               end if;

               if dent.inode /= 0 and then nameLength > 0 then
                  declare
                     entryName : String (1 .. nameLength)
                       with Import,
                            Address => blockBuf'Address +
                              Storage_Offset (blockOffset) +
                              (DirectoryEntry'Size / 8);
                     isDot : constant Boolean :=
                       (nameLength = 1 and then entryName (1) = '.') or else
                       (nameLength = 2 and then entryName (1) = '.' and then
                        entryName (2) = '.');
                  begin
                     if not isDot then
                        if entryCount = MAXIMUM_DIRECTORY_PAGE_ENTRIES then
                           nextCursor := scanCursor;
                           status := Directory_Page_Complete;
                           return;
                        end if;

                        entries (entryCount).objectHint :=
                          Unsigned_64 (dent.inode);
                        entries (entryCount).nameLength :=
                          Unsigned_16 (nameLength);
                        entries (entryCount).kind :=
                          (case dent.fileType is
                              when FILETYPE_REGULAR => DIRECTORY_KIND_FILE,
                              when FILETYPE_DIRECTORY =>
                                DIRECTORY_KIND_DIRECTORY,
                              when 7 => DIRECTORY_KIND_SYMLINK,
                              when others => DIRECTORY_KIND_UNKNOWN);
                        for index in 1 .. nameLength loop
                           entries (entryCount).name (index) :=
                             Unsigned_8 (Character'Pos (entryName (index)));
                        end loop;
                        entryCount := entryCount + 1;
                     end if;
                  end;
               end if;

               scanCursor := scanCursor + recordLength;
            end;
         end;
      end loop;

      nextCursor := scanCursor;
      status := Directory_End;
   end readDirectoryPage;

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
      ptrsPerBlock : constant Unsigned_64 := Unsigned_64 (fs.blkSize / 4);
      maximumLogicalBlocks : constant Unsigned_64 :=
        Unsigned_64 (NUM_DIRECT_BLOCKS) + ptrsPerBlock +
          ptrsPerBlock * ptrsPerBlock;

      --  Maximum contiguous payload accepted by this block session.
      maxContigBytes : constant Unsigned_64 :=
        (if fs.backend = MEMORY then Unsigned_64 (fs.blkSize)
         else Unsigned_64 (fs.device.grantBytes));
   begin
      bytesRead := 0;
      status := Read_Complete;
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
               physBlock := getDataBlock (fs, ino, logBlock);

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

                        nextPhys := getDataBlock
                          (fs, ino, logBlock + contigBlocks);

                        --  Must be consecutive physical blocks
                        exit when nextPhys /= physBlock + contigBlocks;

                        contigBlocks := contigBlocks + 1;
                     end loop;
                  end if;

                  canRead := Unsigned_64 (contigBlocks) *
                    Unsigned_64 (fs.blkSize) -
                    Unsigned_64 (blockOffset);
                  if canRead > remaining then
                     canRead := remaining;
                  end if;

                  declare
                     readStatus : Read_Status;
                  begin
                     readBytes
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

   --  writeBytes uses the session's described logical block and transfer bound.

   --  Write bytes to the filesystem at a raw byte offset.
   --  Handles non-aligned writes via read-modify-write of partial sectors.
   procedure writeBytes
     (fs     : Filesystem;
      offset : Storage_Offset;
      src    : System.Address;
      len    : Storage_Count;
      status : out Write_Status)
   is
   begin
      status := Write_Device_Error;

      if fs.writeQuarantined then
         status := Write_Read_Only;
         return;
      end if;

      if fs.backend = BLOCK_DEVICE and then
         Is_Read_Only (fs.device.description)
      then
         status := Write_Read_Only;
         return;
      end if;

      case fs.backend is
         when MEMORY =>
            if offset < 0 or else
               Unsigned_64 (offset) > fs.imageSize or else
               Unsigned_64 (len) > fs.imageSize - Unsigned_64 (offset)
            then
               status := Write_Out_Of_Range;
               return;
            end if;

            declare
               dst : String (1 .. Natural (len))
                 with Import, Address => fs.base + offset;
               source : String (1 .. Natural (len))
                 with Import, Address => src;
            begin
               dst := source;
            end;
            status := Write_Complete;

         when BLOCK_DEVICE =>
            if offset < 0 then
               status := Write_Out_Of_Range;
               return;
            end if;

            declare
               byteOff   : Unsigned_64 := Unsigned_64 (offset);
               remaining : Unsigned_64 := Unsigned_64 (len);
               srcOff    : Storage_Offset := 0;
               lba       : Unsigned_64;
               secOff    : Unsigned_64;
               copyLen   : Unsigned_64;
               deviceBlockSize : constant Unsigned_64 :=
                 Unsigned_64 (fs.device.description.logicalBlockSize);
               msg       : Message;
               ignore    : MessageTag;
            begin
               while remaining > 0 loop
                  lba    := byteOff / deviceBlockSize;
                  secOff := byteOff mod deviceBlockSize;

                  if lba >= fs.device.description.blockCount then
                     status := Write_Out_Of_Range;
                     return;
                  end if;

                  if secOff /= 0 or remaining < deviceBlockSize then
                     --  Partial sector: read-modify-write
                     copyLen := deviceBlockSize - secOff;
                     if copyLen > remaining then
                        copyLen := remaining;
                     end if;

                     --  Read the sector into grant buffer
                     msg.tag := (label  => OP_READ_BLOCKS,
                                 length => 4,
                                 flags  => 0,
                                 badge  => 0);
                     msg.capBadge := 0;
                     msg.words := [0 => lba,
                                   1 => fs.device.grant.slot,
                                   2 => 1,
                                   3 => fs.device.grant.generation];
                     ignore := capCall (fs.device.endpointSlot, msg);

                     if msg.tag.label /= REPLY_OK or else
                        msg.tag.length /= 1 or else
                        msg.words (0) /= deviceBlockSize
                     then
                        debugPrint
                          ("Ext2: write RMW read failed." & ASCII.LF);
                        return;
                     end if;

                     --  Overlay our data onto the grant buffer
                     declare
                        dstSlice : String (1 .. Natural (copyLen))
                          with Import,
                               Address => fs.device.grantBuffer +
                                 Storage_Offset (secOff);
                        srcSlice : String (1 .. Natural (copyLen))
                          with Import,
                               Address => src + srcOff;
                     begin
                        dstSlice := srcSlice;
                     end;

                     --  Write the modified sector back
                     msg.tag := (label  => OP_WRITE_BLOCKS,
                                 length => 4,
                                 flags  => 0,
                                 badge  => 0);
                     msg.capBadge := 0;
                     msg.words := [0 => lba,
                                   1 => fs.device.grant.slot,
                                   2 => 1,
                                   3 => fs.device.grant.generation];
                     ignore := capCall (fs.device.endpointSlot, msg);

                     if msg.tag.label /= REPLY_OK or else
                        msg.tag.length /= 1 or else
                        msg.words (0) /= deviceBlockSize
                     then
                        debugPrint
                          ("Ext2: write RMW write failed." & ASCII.LF);
                        return;
                     end if;
                  else
                     --  Sector-aligned: batch write full sectors
                     declare
                        maxWriteSectors : constant Unsigned_64 :=
                          Unsigned_64'Min
                            (Unsigned_64 (fs.device.grantBytes) /
                               deviceBlockSize,
                             Unsigned_64
                               (fs.device.description.maxTransferBlocks));
                        sectorsNeeded : Unsigned_64 :=
                          remaining / deviceBlockSize;
                     begin
                        if sectorsNeeded > maxWriteSectors then
                           sectorsNeeded := maxWriteSectors;
                        end if;

                        if sectorsNeeded = 0 or else
                           sectorsNeeded >
                             fs.device.description.blockCount - lba
                        then
                           status := Write_Out_Of_Range;
                           return;
                        end if;

                        copyLen :=
                          sectorsNeeded * deviceBlockSize;

                        --  Copy source data into grant buffer
                        declare
                           dstSlice : String (1 .. Natural (copyLen))
                             with Import, Address => fs.device.grantBuffer;
                           srcSlice : String (1 .. Natural (copyLen))
                             with Import, Address => src + srcOff;
                        begin
                           dstSlice := srcSlice;
                        end;

                        --  Write sectors
                        msg.tag := (label  => OP_WRITE_BLOCKS,
                                    length => 4,
                                    flags  => 0,
                                    badge  => 0);
                        msg.capBadge := 0;
                        msg.words := [0 => lba,
                                      1 => fs.device.grant.slot,
                                      2 => sectorsNeeded,
                                      3 => fs.device.grant.generation];
                        ignore := capCall (fs.device.endpointSlot, msg);

                        if msg.tag.label /= REPLY_OK or else
                           msg.tag.length /= 1 or else
                           msg.words (0) /= copyLen
                        then
                           debugPrint
                             ("Ext2: batch write failed." & ASCII.LF);
                           return;
                        end if;
                     end;
                  end if;

                  byteOff   := byteOff + copyLen;
                  srcOff    := srcOff + Storage_Offset (copyLen);
                  remaining := remaining - copyLen;
               end loop;
               status := Write_Complete;
            end;
      end case;
   end writeBytes;

   --  Transitional wrapper for metadata operations whose public APIs do not
   --  yet expose an I/O result.  File-data writes use the checked overload
   --  below and never report an unchecked transport completion as success.
   procedure writeBytes
     (fs     : Filesystem;
      offset : Storage_Offset;
      src    : System.Address;
      len    : Storage_Count)
   is
      ignoredStatus : Write_Status;
   begin
      writeBytes (fs, offset, src, len, ignoredStatus);
   end writeBytes;

   --  Write an inode back to disk (mirror of readInode)
   procedure writeInode
     (fs       : Filesystem;
      inodeNum : Unsigned_32;
      ino      : Inode;
      status   : out Write_Status)
   is
      blockGroup : constant Unsigned_32 :=
        (inodeNum - 1) / fs.sb.inodesPerBlockGroup;

      inodeIndex : constant Unsigned_32 :=
        (inodeNum - 1) mod fs.sb.inodesPerBlockGroup;

      bgdtOffset : constant Storage_Offset :=
        Storage_Offset ((fs.sb.firstDataBlock + 1) * fs.blkSize) +
        Storage_Offset (blockGroup) * (BlockGroupDescriptor'Size / 8);

      bgd : BlockGroupDescriptor;

      inodeTableByteOffset : Storage_Offset;
      inoSize : Unsigned_32;
      readStatus : Read_Status;
   begin
      readBytes
        (fs, bgdtOffset, bgd'Address, BlockGroupDescriptor'Size / 8,
         readStatus);
      if readStatus /= Read_Complete then
         status :=
           (if readStatus = Read_Out_Of_Range then Write_Out_Of_Range
            else Write_Device_Error);
         return;
      end if;

      if fs.sb.majorVersion >= 1 then
         inoSize := Unsigned_32 (fs.sb.inodeSize);
      else
         inoSize := 128;
      end if;

      inodeTableByteOffset :=
        Storage_Offset (bgd.inodeTableAddr) * Storage_Offset (fs.blkSize) +
        Storage_Offset (inodeIndex) * Storage_Offset (inoSize);

      writeBytes
        (fs, inodeTableByteOffset, ino'Address, Inode'Size / 8, status);
   end writeInode;

   procedure writeInode
     (fs       : Filesystem;
      inodeNum : Unsigned_32;
      ino      : Inode)
   is
      ignoredStatus : Write_Status;
   begin
      writeInode (fs, inodeNum, ino, ignoredStatus);
   end writeInode;

   --  Write the superblock back to disk
   procedure writeSuperblock
     (fs : Filesystem;
      status : out Write_Status)
   is
   begin
      writeBytes (fs, SUPERBLOCK_OFFSET, fs.sb'Address,
                  Superblock'Size / 8, status);
   end writeSuperblock;

   procedure writeSuperblock (fs : Filesystem) is
      ignoredStatus : Write_Status;
   begin
      writeSuperblock (fs, ignoredStatus);
   end writeSuperblock;

   --  Write a block group descriptor back to disk
   procedure writeBGD
     (fs         : Filesystem;
      blockGroup : Unsigned_32;
      bgd        : BlockGroupDescriptor;
      status     : out Write_Status)
   is
      bgdtOffset : constant Storage_Offset :=
        Storage_Offset ((fs.sb.firstDataBlock + 1) * fs.blkSize) +
        Storage_Offset (blockGroup) * (BlockGroupDescriptor'Size / 8);
   begin
      writeBytes (fs, bgdtOffset, bgd'Address,
                  BlockGroupDescriptor'Size / 8, status);
   end writeBGD;

   procedure writeBGD
     (fs         : Filesystem;
      blockGroup : Unsigned_32;
      bgd        : BlockGroupDescriptor)
   is
      ignoredStatus : Write_Status;
   begin
      writeBGD (fs, blockGroup, bgd, ignoredStatus);
   end writeBGD;

   --  Read block group descriptor for a given block group
   procedure readBGD
     (fs         : Filesystem;
      blockGroup : Unsigned_32;
      bgd        : out BlockGroupDescriptor;
      status     : out Read_Status)
   is
      bgdtOffset : constant Storage_Offset :=
        Storage_Offset ((fs.sb.firstDataBlock + 1) * fs.blkSize) +
        Storage_Offset (blockGroup) * (BlockGroupDescriptor'Size / 8);
   begin
      readBytes (fs, bgdtOffset, bgd'Address,
                 BlockGroupDescriptor'Size / 8, status);
   end readBGD;

   procedure readBGD
     (fs         : Filesystem;
      blockGroup : Unsigned_32;
      bgd        : out BlockGroupDescriptor)
   is
      ignoredStatus : Read_Status;
   begin
      readBGD (fs, blockGroup, bgd, ignoredStatus);
      if ignoredStatus /= Read_Complete then
         bgd :=
           (blockBitmapAddr => 0,
            inodeBitmapAddr => 0,
            inodeTableAddr  => 0,
            numFreeBlocks   => 0,
            numFreeInodes   => 0,
            numDirectories  => 0,
            padding         => 0,
            reserved        => 0);
      end if;
   end readBGD;

   --  Allocate a free block from the first group which has one.  Block bitmap
   --  bits are relative to their group, not to firstDataBlock globally.
   procedure allocateBlock
     (fs       : in out Filesystem;
      blockNum : out Unsigned_32;
      status   : out Write_Status)
   is
      bgd : BlockGroupDescriptor;
      updatedBGD : BlockGroupDescriptor;
      bitmapBuf : array (0 .. 4095) of Unsigned_8 with Alignment => 8;
      bitmapBytes : constant Unsigned_32 :=
        (fs.sb.blocksPerBlockGroup + 7) / 8;
      readSize : Unsigned_32;
      groupCount : Unsigned_32;
      allocatableBlocks : Unsigned_32;
      groupFirst : Unsigned_32;
      validBlocks : Unsigned_32;
      candidate : Unsigned_32;
      originalByte : Unsigned_8;
      readStatus : Read_Status;
      writeStatus : Write_Status;
      rollbackStatus : Write_Status;
      sawAdvertisedSpace : Boolean := False;
   begin
      blockNum := 0;
      status := Write_No_Space;

      if fs.sb.blocksPerBlockGroup = 0 or else
         fs.sb.blockCount <= fs.sb.firstDataBlock
      then
         status := Write_Device_Error;
         return;
      end if;

      if fs.sb.freeBlocks = 0 then
         return;
      end if;

      allocatableBlocks := fs.sb.blockCount - fs.sb.firstDataBlock;
      groupCount := 1 +
        (allocatableBlocks - 1) / fs.sb.blocksPerBlockGroup;

      readSize := bitmapBytes;
      if readSize > bitmapBuf'Length then
         readSize := Unsigned_32 (bitmapBuf'Length);
      end if;

      if readSize = 0 then
         return;
      end if;

      for group in Unsigned_32 range 0 .. groupCount - 1 loop
         readBGD (fs, group, bgd, readStatus);
         if readStatus /= Read_Complete then
            status :=
              (if readStatus = Read_Out_Of_Range then Write_Out_Of_Range
               else Write_Device_Error);
            return;
         end if;

         if bgd.numFreeBlocks /= 0 then
            sawAdvertisedSpace := True;
            groupFirst := fs.sb.firstDataBlock +
              group * fs.sb.blocksPerBlockGroup;
            validBlocks := Unsigned_32'Min
              (fs.sb.blocksPerBlockGroup,
               fs.sb.blockCount - groupFirst);

            readBytes
              (fs,
               Storage_Offset (bgd.blockBitmapAddr) *
                 Storage_Offset (fs.blkSize),
               bitmapBuf'Address,
               Storage_Count (readSize),
               readStatus);
            if readStatus /= Read_Complete then
               status :=
                 (if readStatus = Read_Out_Of_Range then Write_Out_Of_Range
                  else Write_Device_Error);
               return;
            end if;

            for byteIdx in 0 .. Natural (readSize) - 1 loop
               if bitmapBuf (byteIdx) /= 16#FF# then
                  for bitIdx in 0 .. 7 loop
                     candidate := Unsigned_32 (byteIdx * 8 + bitIdx);
                     if candidate < validBlocks and then
                        (bitmapBuf (byteIdx) and
                         Shift_Left (Unsigned_8'(1), bitIdx)) = 0
                     then
                        originalByte := bitmapBuf (byteIdx);
                        bitmapBuf (byteIdx) := bitmapBuf (byteIdx) or
                          Shift_Left (Unsigned_8'(1), bitIdx);

                        writeBytes
                          (fs,
                           Storage_Offset (bgd.blockBitmapAddr) *
                           Storage_Offset (fs.blkSize),
                           bitmapBuf'Address,
                           Storage_Count (readSize),
                           writeStatus);
                        if writeStatus /= Write_Complete then
                           status := writeStatus;
                           return;
                        end if;

                        updatedBGD := bgd;
                        updatedBGD.numFreeBlocks :=
                          updatedBGD.numFreeBlocks - 1;
                        writeBGD (fs, group, updatedBGD, writeStatus);
                        if writeStatus /= Write_Complete then
                           --  Best-effort rollback of the bitmap reservation.
                           bitmapBuf (byteIdx) := originalByte;
                           writeBytes
                             (fs,
                              Storage_Offset (bgd.blockBitmapAddr) *
                                Storage_Offset (fs.blkSize),
                              bitmapBuf'Address,
                              Storage_Count (readSize),
                              rollbackStatus);
                           status := writeStatus;
                           return;
                        end if;

                        fs.sb.freeBlocks := fs.sb.freeBlocks - 1;
                        writeSuperblock (fs, writeStatus);
                        if writeStatus /= Write_Complete then
                           --  Restore the in-memory count and attempt to put
                           --  both earlier metadata writes back as well.
                           fs.sb.freeBlocks := fs.sb.freeBlocks + 1;
                           writeBGD (fs, group, bgd, rollbackStatus);
                           bitmapBuf (byteIdx) := originalByte;
                           writeBytes
                             (fs,
                              Storage_Offset (bgd.blockBitmapAddr) *
                                Storage_Offset (fs.blkSize),
                              bitmapBuf'Address,
                              Storage_Count (readSize),
                              rollbackStatus);
                           status := writeStatus;
                           return;
                        end if;

                        blockNum := groupFirst + candidate;
                        status := Write_Complete;
                        return;
                     end if;
                  end loop;
               end if;
            end loop;
         end if;
      end loop;

      --  Free-space counters that advertise an unavailable block indicate
      --  inconsistent metadata, not a normal no-space condition.
      if sawAdvertisedSpace then
         status := Write_Device_Error;
      end if;
   end allocateBlock;

   procedure allocateBlock
     (fs       : in out Filesystem;
      blockNum : out Unsigned_32;
      ok       : out Boolean)
   is
      status : Write_Status;
   begin
      allocateBlock (fs, blockNum, status);
      ok := status = Write_Complete;
   end allocateBlock;

   --  Free a previously allocated block
   procedure freeBlock
     (fs       : in out Filesystem;
      blockNum : Unsigned_32)
   is
      bgd : BlockGroupDescriptor;
      relBlock : Unsigned_32;
      blockGroup : Unsigned_32;
      groupRelativeBlock : Unsigned_32;
      byteIdx  : Natural;
      bitIdx   : Natural;
      bitmapBuf : array (0 .. 4095) of Unsigned_8 with Alignment => 8;
      bitmapBytes : constant Unsigned_32 :=
        (fs.sb.blocksPerBlockGroup + 7) / 8;
      readSize : Unsigned_32;
      groupFirst : Unsigned_32;
      validBlocks : Unsigned_32;
      allocatableBlocks : Unsigned_32;
   begin
      if fs.sb.blocksPerBlockGroup = 0 or else
         blockNum < fs.sb.firstDataBlock or else
         blockNum >= fs.sb.blockCount
      then
         return;
      end if;

      relBlock := blockNum - fs.sb.firstDataBlock;
      blockGroup := relBlock / fs.sb.blocksPerBlockGroup;
      groupRelativeBlock := relBlock mod fs.sb.blocksPerBlockGroup;
      byteIdx := Natural (groupRelativeBlock / 8);
      bitIdx := Natural (groupRelativeBlock mod 8);

      readBGD (fs, blockGroup, bgd);

      groupFirst := fs.sb.firstDataBlock +
        blockGroup * fs.sb.blocksPerBlockGroup;
      validBlocks := Unsigned_32'Min
        (fs.sb.blocksPerBlockGroup, fs.sb.blockCount - groupFirst);
      allocatableBlocks := fs.sb.blockCount - fs.sb.firstDataBlock;

      if Unsigned_32 (bgd.numFreeBlocks) >= validBlocks or else
         fs.sb.freeBlocks >= allocatableBlocks
      then
         return;
      end if;

      readSize := bitmapBytes;
      if readSize > Unsigned_32 (bitmapBuf'Length) then
         readSize := Unsigned_32 (bitmapBuf'Length);
      end if;

      if byteIdx >= Natural (readSize) then
         return;
      end if;

      readBytes (fs,
                 Storage_Offset (bgd.blockBitmapAddr) *
                   Storage_Offset (fs.blkSize),
                 bitmapBuf'Address,
                 Storage_Count (readSize));

      --  A duplicate or corrupt free must not inflate allocator counts.
      if (bitmapBuf (byteIdx) and
          Shift_Left (Unsigned_8'(1), bitIdx)) = 0
      then
         return;
      end if;

      bitmapBuf (byteIdx) := bitmapBuf (byteIdx) and
        not Shift_Left (Unsigned_8'(1), bitIdx);

      writeBytes (fs,
                  Storage_Offset (bgd.blockBitmapAddr) *
                    Storage_Offset (fs.blkSize),
                  bitmapBuf'Address,
                  Storage_Count (readSize));

      bgd.numFreeBlocks := bgd.numFreeBlocks + 1;
      writeBGD (fs, blockGroup, bgd);

      fs.sb.freeBlocks := fs.sb.freeBlocks + 1;
      writeSuperblock (fs);
   end freeBlock;

   --  Allocate a free inode from any block group.
   procedure allocateInode
     (fs       : in out Filesystem;
      inodeNum : out Unsigned_32;
      status   : out Write_Status)
   is
      bgd : BlockGroupDescriptor;
      updatedBGD : BlockGroupDescriptor;
      bitmapBuf : array (0 .. 4095) of Unsigned_8 with Alignment => 8;
      bitmapBytes : constant Unsigned_32 :=
        (fs.sb.inodesPerBlockGroup + 7) / 8;
      readSize : Unsigned_32;
      groupCount : Unsigned_32;
      groupFirst : Unsigned_32;
      validInodes : Unsigned_32;
      candidate : Unsigned_32;
      candidateInode : Unsigned_32;
      firstUsable : Unsigned_32;
      originalByte : Unsigned_8;
      readStatus : Read_Status;
      writeStatus : Write_Status;
      rollbackStatus : Write_Status;
      sawAdvertisedSpace : Boolean := False;
      blankIno : constant Inode :=
        (typeAndPermissions => 0,
         uid => 0, sizeLo => 0,
         accessedTime => 0, creationTime => 0,
         modifiedTime => 0, deletedTime => 0,
         gid => 0, numHardLinks => 0,
         numDiskSectors => 0, flags => 0,
         osSpecific1 => 0,
         directBlocks => [others => 0],
         singleIndirectBlock => 0,
         doubleIndirectBlock => 0,
         tripleIndirectBlock => 0,
         generationNumber => 0,
         fileACL => 0, sizeHi_DirACL => 0,
         fragmentBlockAddr => 0,
         osSpecific2A => 0, osSpecific2B => 0,
         osSpecific2C => 0);
   begin
      inodeNum := 0;
      status := Write_No_Space;

      if fs.sb.inodesPerBlockGroup = 0 or else fs.sb.inodeCount = 0 then
         status := Write_Device_Error;
         return;
      end if;

      if fs.sb.freeInodes = 0 then
         return;
      end if;

      groupCount := 1 +
        (fs.sb.inodeCount - 1) / fs.sb.inodesPerBlockGroup;
      firstUsable :=
        (if fs.sb.majorVersion >= 1 and then
            fs.sb.firstNonReservedInode > 0
         then fs.sb.firstNonReservedInode
         else 11);

      readSize := bitmapBytes;
      if readSize > Unsigned_32 (bitmapBuf'Length) then
         readSize := Unsigned_32 (bitmapBuf'Length);
      end if;

      for group in Unsigned_32 range 0 .. groupCount - 1 loop
         readBGD (fs, group, bgd, readStatus);
         if readStatus /= Read_Complete then
            status :=
              (if readStatus = Read_Out_Of_Range then Write_Out_Of_Range
               else Write_Device_Error);
            return;
         end if;

         if bgd.numFreeInodes /= 0 then
            sawAdvertisedSpace := True;
            groupFirst := group * fs.sb.inodesPerBlockGroup;
            validInodes := Unsigned_32'Min
              (fs.sb.inodesPerBlockGroup,
               fs.sb.inodeCount - groupFirst);

            readBytes
              (fs,
               Storage_Offset (bgd.inodeBitmapAddr) *
                 Storage_Offset (fs.blkSize),
               bitmapBuf'Address,
               Storage_Count (readSize),
               readStatus);
            if readStatus /= Read_Complete then
               status :=
                 (if readStatus = Read_Out_Of_Range then Write_Out_Of_Range
                  else Write_Device_Error);
               return;
            end if;

            for byteIdx in 0 .. Natural (readSize) - 1 loop
               if bitmapBuf (byteIdx) /= 16#FF# then
                  for bitIdx in 0 .. 7 loop
                     candidate := Unsigned_32 (byteIdx * 8 + bitIdx);
                     if candidate < validInodes and then
                        (bitmapBuf (byteIdx) and
                         Shift_Left (Unsigned_8'(1), bitIdx)) = 0
                     then
                        candidateInode := groupFirst + candidate + 1;
                        if candidateInode < firstUsable then
                           goto Continue_Inode_Bit;
                        end if;

                        originalByte := bitmapBuf (byteIdx);
                        bitmapBuf (byteIdx) := bitmapBuf (byteIdx) or
                          Shift_Left (Unsigned_8'(1), bitIdx);
                        writeBytes
                          (fs,
                           Storage_Offset (bgd.inodeBitmapAddr) *
                             Storage_Offset (fs.blkSize),
                           bitmapBuf'Address,
                           Storage_Count (readSize),
                           writeStatus);
                        if writeStatus /= Write_Complete then
                           status := writeStatus;
                           return;
                        end if;

                        updatedBGD := bgd;
                        updatedBGD.numFreeInodes :=
                          updatedBGD.numFreeInodes - 1;
                        writeBGD (fs, group, updatedBGD, writeStatus);
                        if writeStatus /= Write_Complete then
                           bitmapBuf (byteIdx) := originalByte;
                           writeBytes
                             (fs,
                              Storage_Offset (bgd.inodeBitmapAddr) *
                                Storage_Offset (fs.blkSize),
                              bitmapBuf'Address,
                              Storage_Count (readSize),
                              rollbackStatus);
                           status := writeStatus;
                           return;
                        end if;

                        fs.sb.freeInodes := fs.sb.freeInodes - 1;
                        writeSuperblock (fs, writeStatus);
                        if writeStatus = Write_Complete then
                           writeInode
                             (fs, candidateInode, blankIno, writeStatus);
                        end if;

                        if writeStatus /= Write_Complete then
                           fs.sb.freeInodes := fs.sb.freeInodes + 1;
                           writeSuperblock (fs, rollbackStatus);
                           writeBGD (fs, group, bgd, rollbackStatus);
                           bitmapBuf (byteIdx) := originalByte;
                           writeBytes
                             (fs,
                              Storage_Offset (bgd.inodeBitmapAddr) *
                                Storage_Offset (fs.blkSize),
                              bitmapBuf'Address,
                              Storage_Count (readSize),
                              rollbackStatus);
                           status := writeStatus;
                           return;
                        end if;

                        inodeNum := candidateInode;
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

   procedure allocateInode
     (fs       : in out Filesystem;
      inodeNum : out Unsigned_32;
      ok       : out Boolean)
   is
      status : Write_Status;
   begin
      allocateInode (fs, inodeNum, status);
      ok := status = Write_Complete;
   end allocateInode;

   --  Free a previously allocated inode
   procedure freeInode
     (fs       : in out Filesystem;
      inodeNum : Unsigned_32)
   is
      bgd : BlockGroupDescriptor;
      updatedBGD : BlockGroupDescriptor;
      relInode : Unsigned_32;
      blockGroup : Unsigned_32;
      groupRelativeInode : Unsigned_32;
      byteIdx  : Natural;
      bitIdx   : Natural;
      bitmapBuf : array (0 .. 4095) of Unsigned_8 with Alignment => 8;
      bitmapBytes : constant Unsigned_32 :=
         (fs.sb.inodesPerBlockGroup + 7) / 8;
      readSize : Unsigned_32;
      validInodes : Unsigned_32;
      groupFirst : Unsigned_32;
      originalByte : Unsigned_8;
      readStatus : Read_Status;
      writeStatus : Write_Status;
      rollbackStatus : Write_Status;
   begin
      if fs.sb.inodesPerBlockGroup = 0 or else
         inodeNum = 0 or else inodeNum > fs.sb.inodeCount
      then
         return;
      end if;

      relInode := inodeNum - 1;
      blockGroup := relInode / fs.sb.inodesPerBlockGroup;
      groupRelativeInode := relInode mod fs.sb.inodesPerBlockGroup;
      byteIdx := Natural (groupRelativeInode / 8);
      bitIdx := Natural (groupRelativeInode mod 8);

      readBGD (fs, blockGroup, bgd, readStatus);
      if readStatus /= Read_Complete then
         return;
      end if;

      groupFirst := blockGroup * fs.sb.inodesPerBlockGroup;
      validInodes := Unsigned_32'Min
        (fs.sb.inodesPerBlockGroup, fs.sb.inodeCount - groupFirst);
      if Unsigned_32 (bgd.numFreeInodes) >= validInodes or else
         fs.sb.freeInodes >= fs.sb.inodeCount
      then
         return;
      end if;

      readSize := bitmapBytes;
      if readSize > Unsigned_32 (bitmapBuf'Length) then
         readSize := Unsigned_32 (bitmapBuf'Length);
      end if;

      if byteIdx >= Natural (readSize) then
         return;
      end if;

      readBytes
        (fs,
         Storage_Offset (bgd.inodeBitmapAddr) * Storage_Offset (fs.blkSize),
         bitmapBuf'Address,
         Storage_Count (readSize),
         readStatus);
      if readStatus /= Read_Complete then
         return;
      end if;

      --  A duplicate free must not inflate either free-inode counter.
      if (bitmapBuf (byteIdx) and
          Shift_Left (Unsigned_8'(1), bitIdx)) = 0
      then
         return;
      end if;

      originalByte := bitmapBuf (byteIdx);
      bitmapBuf (byteIdx) := bitmapBuf (byteIdx) and
        not Shift_Left (Unsigned_8'(1), bitIdx);

      writeBytes
        (fs,
         Storage_Offset (bgd.inodeBitmapAddr) * Storage_Offset (fs.blkSize),
         bitmapBuf'Address,
         Storage_Count (readSize),
         writeStatus);
      if writeStatus /= Write_Complete then
         return;
      end if;

      updatedBGD := bgd;
      updatedBGD.numFreeInodes := updatedBGD.numFreeInodes + 1;
      writeBGD (fs, blockGroup, updatedBGD, writeStatus);
      if writeStatus /= Write_Complete then
         bitmapBuf (byteIdx) := originalByte;
         writeBytes
           (fs,
            Storage_Offset (bgd.inodeBitmapAddr) *
              Storage_Offset (fs.blkSize),
            bitmapBuf'Address,
            Storage_Count (readSize),
            rollbackStatus);
         return;
      end if;

      fs.sb.freeInodes := fs.sb.freeInodes + 1;
      writeSuperblock (fs, writeStatus);
      if writeStatus /= Write_Complete then
         fs.sb.freeInodes := fs.sb.freeInodes - 1;
         writeBGD (fs, blockGroup, bgd, rollbackStatus);
         bitmapBuf (byteIdx) := originalByte;
         writeBytes
           (fs,
            Storage_Offset (bgd.inodeBitmapAddr) *
              Storage_Offset (fs.blkSize),
            bitmapBuf'Address,
            Storage_Count (readSize),
            rollbackStatus);
      end if;
   end freeInode;

   --  Set a block pointer in an inode (direct or single indirect).
   --  Updates ino in place and writes the indirect block if needed.
   procedure setBlockPointer
     (fs       : in out Filesystem;
      ino      : in out Inode;
      logBlock : Unsigned_32;
      physBlk  : Unsigned_32;
      status   : out Write_Status)
   is
      ptrsPerBlock : constant Unsigned_32 := fs.blkSize / 4;
   begin
      status := Write_File_Range_Unsupported;
      if logBlock < NUM_DIRECT_BLOCKS then
         ino.directBlocks (Natural (logBlock)) := physBlk;
         status := Write_Complete;
      elsif logBlock - Unsigned_32 (NUM_DIRECT_BLOCKS) < ptrsPerBlock then
         --  Single indirect
         declare
            indirectIdx : constant Unsigned_32 :=
              logBlock - Unsigned_32 (NUM_DIRECT_BLOCKS);
            indBuf : array (0 .. 1023) of Unsigned_32 with Alignment => 8;
            createdIndirect : Boolean := False;
         begin
            if ino.singleIndirectBlock = 0 then
               --  Allocate the indirect block itself
               declare
                  newBlk : Unsigned_32;
               begin
                  allocateBlock (fs, newBlk, status);
                  if status /= Write_Complete then
                     return;
                  end if;
                  ino.singleIndirectBlock := newBlk;
                  createdIndirect := True;
                  --  Zero-fill the new indirect block
                  indBuf := [others => 0];
               end;
            else
               readBlock (fs, ino.singleIndirectBlock, indBuf'Address);
            end if;

            indBuf (Natural (indirectIdx)) := physBlk;
            declare
               blkOffset : constant Storage_Offset :=
                 Storage_Offset (ino.singleIndirectBlock) *
                   Storage_Offset (fs.blkSize);
            begin
               writeBytes (fs, blkOffset, indBuf'Address,
                           Storage_Count (fs.blkSize), status);
            end;
            if status /= Write_Complete then
               if createdIndirect then
                  freeBlock (fs, ino.singleIndirectBlock);
                  ino.singleIndirectBlock := 0;
               end if;
               return;
            end if;
            invalidateBlockCache;
         end;
      end if;
      --  Double/triple indirect allocation not implemented
   end setBlockPointer;

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
   is
      remaining : Unsigned_64 := count;
      pos       : Unsigned_64 := offset;
      written   : Unsigned_64 := 0;
      terminalStatus : Write_Status := Write_Complete;
   begin
      bytesWritten := 0;
      status := Write_Complete;

      --  Keep all subsequent position arithmetic inside Unsigned_64.
      if count > Unsigned_64'Last - offset then
         status := Write_Out_Of_Range;
         return;
      end if;

      while remaining > 0 loop
         declare
            logicalIndex : constant Unsigned_64 :=
              pos / Unsigned_64 (fs.blkSize);
            blockOffset : constant Unsigned_32 :=
              Unsigned_32 (pos mod Unsigned_64 (fs.blkSize));
            physBlock   : Unsigned_32;
            pointerStatus : Write_Status;
            dataStatus    : Write_Status;
            canWrite    : Unsigned_64 :=
              Unsigned_64 (fs.blkSize - blockOffset);
         begin
            if logicalIndex >=
              Unsigned_64 (NUM_DIRECT_BLOCKS) +
                Unsigned_64 (fs.blkSize / 4)
            then
               terminalStatus := Write_File_Range_Unsupported;
               exit;
            end if;

            declare
               logBlock : constant Unsigned_32 :=
                 Unsigned_32 (logicalIndex);
            begin
               physBlock := getDataBlock (fs, ino, logBlock);

               if canWrite > remaining then
                  canWrite := remaining;
               end if;

               if physBlock = 0 then
                  --  Initialize a fresh data block before publishing its
                  --  inode pointer.  Besides failure ordering, this prevents
                  --  bytes from a previously freed block becoming visible
                  --  through a partial first write.
                  declare
                     blockBuf : String (1 .. Natural (fs.blkSize)) :=
                       (others => Character'Val (0));
                     source   : String (1 .. Natural (canWrite))
                       with Import,
                            Address => buf + Storage_Offset (written);
                     firstByte : constant Natural :=
                       Natural (blockOffset) + 1;
                     lastByte  : constant Natural :=
                       Natural (blockOffset) + Natural (canWrite);
                  begin
                     allocateBlock (fs, physBlock, dataStatus);
                     if dataStatus /= Write_Complete then
                        terminalStatus := dataStatus;
                        exit;
                     end if;

                     blockBuf (firstByte .. lastByte) := source;
                     writeBytes
                       (fs,
                        Storage_Offset (physBlock) *
                          Storage_Offset (fs.blkSize),
                        blockBuf'Address,
                        Storage_Count (fs.blkSize),
                        dataStatus);
                     if dataStatus /= Write_Complete then
                        freeBlock (fs, physBlock);
                        terminalStatus := dataStatus;
                        exit;
                     end if;

                     setBlockPointer
                       (fs, ino, logBlock, physBlock, pointerStatus);
                     if pointerStatus /= Write_Complete then
                        freeBlock (fs, physBlock);
                        terminalStatus := pointerStatus;
                        exit;
                     end if;
                  end;
               else
                  writeBytes
                    (fs,
                     Storage_Offset (physBlock) *
                       Storage_Offset (fs.blkSize) +
                       Storage_Offset (blockOffset),
                     buf + Storage_Offset (written),
                     Storage_Count (canWrite),
                     dataStatus);
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
      end loop;

      --  Update inode size if we wrote past EOF
      declare
         newEnd : constant Unsigned_64 := offset + written;
         oldSize : constant Unsigned_64 := fileSize (ino);
      begin
         if newEnd > oldSize then
            ino.sizeLo := Unsigned_32 (newEnd and 16#FFFF_FFFF#);
            ino.sizeHi_DirACL :=
              Unsigned_32 (Shift_Right (newEnd, 32));

            --  Update disk sector count (512-byte sectors)
            ino.numDiskSectors :=
              Unsigned_32 ((newEnd + 511) / 512);
         end if;
      end;

      if written > 0 then
         --  Publish new pointers and size only after their data blocks have
         --  completed.  A metadata write failure supersedes the earlier
         --  terminal state because the committed extent is then uncertain.
         declare
            inodeStatus : Write_Status;
         begin
            writeInode (fs, inodeNum, ino, inodeStatus);
            if inodeStatus /= Write_Complete then
               terminalStatus := inodeStatus;
            end if;
         end;
      end if;

      bytesWritten := written;
      status := terminalStatus;
   end writeData;

   --  Add a directory entry pointing to an existing inode.
   function addDirectoryEntry
     (fs          : in out Filesystem;
      dirInodeNum : Unsigned_32;
      inodeNum    : Unsigned_32;
      name        : String;
      fileType    : Unsigned_8) return Boolean
   is
      dirIno   : Inode;
      blockBuf : String (1 .. Natural (fs.blkSize))
        with Alignment => 8;
   begin
      if name'Length = 0 or name'Length > 255 then
         return False;
      end if;

      readInode (fs, dirInodeNum, dirIno);

      declare
         size : constant Unsigned_64 := fileSize (dirIno);
         bytesScanned : Unsigned_64 := 0;
         blockIdx : Natural := 0;
         rawSize : constant Unsigned_32 :=
           Unsigned_32 (DirectoryEntry'Size / 8 + name'Length + 3);
         entrySize : constant Unsigned_16 :=
           Unsigned_16 (rawSize and not Unsigned_32'(3));
         inserted : Boolean := False;
      begin
         while bytesScanned < size and
               blockIdx < NUM_DIRECT_BLOCKS and
               not inserted
         loop
            declare
               blkNum : constant Unsigned_32 :=
                 dirIno.directBlocks (blockIdx);
               scanOff : Storage_Offset := 0;
            begin
               if blkNum = 0 then
                  exit;
               end if;

               readBlock (fs, blkNum, blockBuf'Address);

               while scanOff < Storage_Offset (fs.blkSize) and
                     bytesScanned < size and not inserted
               loop
                  declare
                     dent : DirectoryEntry
                       with Import,
                            Address => blockBuf'Address + scanOff;
                     rawReal : constant Unsigned_32 :=
                       Unsigned_32 (DirectoryEntry'Size / 8 +
                         Natural (dent.nameLength) + 3);
                     realSize : constant Unsigned_16 :=
                       Unsigned_16 (rawReal and not Unsigned_32'(3));
                     slack : Unsigned_16;
                  begin
                     if dent.length = 0 then
                        exit;
                     end if;

                     slack := dent.length - realSize;

                     if slack >= entrySize then
                        dent.length := realSize;

                        declare
                           newOff : constant Storage_Offset :=
                             scanOff + Storage_Offset (realSize);
                           newDent : DirectoryEntry
                             with Import,
                                  Address => blockBuf'Address + newOff;
                        begin
                           newDent.inode := inodeNum;
                           newDent.length := slack;
                           newDent.nameLength :=
                             Unsigned_8 (name'Length);
                           newDent.fileType := fileType;

                           declare
                              entName :
                                String (1 .. name'Length)
                                  with Import,
                                       Address => blockBuf'Address +
                                         newOff +
                                         (DirectoryEntry'Size / 8);
                           begin
                              for i in 1 .. name'Length loop
                                 entName (i) :=
                                   name (name'First + i - 1);
                              end loop;
                           end;
                        end;

                        declare
                           blkOffset : constant Storage_Offset :=
                             Storage_Offset (blkNum) *
                               Storage_Offset (fs.blkSize);
                        begin
                           writeBytes (fs, blkOffset,
                                       blockBuf'Address,
                                       Storage_Count (fs.blkSize));
                        end;
                        inserted := True;
                     end if;

                     bytesScanned := bytesScanned +
                       Unsigned_64 (dent.length);
                     scanOff := scanOff +
                       Storage_Offset (dent.length);
                  end;
               end loop;
            end;
            blockIdx := blockIdx + 1;
         end loop;

         if not inserted then
            declare
               newBlk : Unsigned_32;
               blkOk  : Boolean;
               dent   : DirectoryEntry
                 with Import, Address => blockBuf'Address;
            begin
               allocateBlock (fs, newBlk, blkOk);
               if not blkOk then
                  return False;
               end if;

               for i in blockBuf'Range loop
                  blockBuf (i) := Character'Val (0);
               end loop;

               dent.inode := inodeNum;
               dent.length := Unsigned_16 (fs.blkSize);
               dent.nameLength := Unsigned_8 (name'Length);
               dent.fileType := fileType;

               declare
                  entName : String (1 .. name'Length)
                    with Import,
                         Address => blockBuf'Address +
                           (DirectoryEntry'Size / 8);
               begin
                  for i in 1 .. name'Length loop
                     entName (i) := name (name'First + i - 1);
                  end loop;
               end;

               declare
                  blkOffset : constant Storage_Offset :=
                    Storage_Offset (newBlk) *
                      Storage_Offset (fs.blkSize);
               begin
                  writeBytes (fs, blkOffset, blockBuf'Address,
                              Storage_Count (fs.blkSize));
               end;

               declare
                  pointerStatus : Write_Status;
               begin
                  setBlockPointer
                    (fs, dirIno, Unsigned_32 (blockIdx), newBlk,
                     pointerStatus);
                  if pointerStatus /= Write_Complete then
                     freeBlock (fs, newBlk);
                     return False;
                  end if;
               end;
               dirIno.sizeLo := dirIno.sizeLo + fs.blkSize;
               dirIno.numDiskSectors := dirIno.numDiskSectors +
                 fs.blkSize / 512;
               writeInode (fs, dirInodeNum, dirIno);
            end;
         end if;
      end;

      return True;
   end addDirectoryEntry;

   --  Remove a directory entry by name.
   --  Returns the inode number of the removed entry, or 0 on failure.
   function removeDirectoryEntry
     (fs          : in out Filesystem;
      dirInodeNum : Unsigned_32;
      name        : String) return Unsigned_32
   is
      dirIno   : Inode;
      blockBuf : String (1 .. Natural (fs.blkSize))
        with Alignment => 8;
   begin
      if name'Length = 0 or name'Length > 255 then
         return 0;
      end if;

      readInode (fs, dirInodeNum, dirIno);

      declare
         size : constant Unsigned_64 := fileSize (dirIno);
         bytesScanned : Unsigned_64 := 0;
         blockIdx : Natural := 0;
      begin
         while bytesScanned < size and
               blockIdx < NUM_DIRECT_BLOCKS
         loop
            declare
               blkNum  : constant Unsigned_32 :=
                 dirIno.directBlocks (blockIdx);
               scanOff : Storage_Offset := 0;
               prevOff : Storage_Offset := 0;
               isFirst : Boolean := True;
            begin
               if blkNum = 0 then
                  exit;
               end if;

               readBlock (fs, blkNum, blockBuf'Address);

               while scanOff < Storage_Offset (fs.blkSize) and
                     bytesScanned < size
               loop
                  declare
                     dent : DirectoryEntry
                       with Import,
                            Address => blockBuf'Address + scanOff;
                  begin
                     if dent.length = 0 then
                        exit;
                     end if;

                     if Natural (dent.nameLength) = name'Length and
                        dent.inode /= 0
                     then
                        declare
                           entName : String (1 .. Natural (dent.nameLength))
                             with Import,
                                  Address => blockBuf'Address + scanOff +
                                    (DirectoryEntry'Size / 8);
                           match : Boolean := True;
                           removedInode : Unsigned_32;
                        begin
                           for i in 1 .. name'Length loop
                              if entName (i) /=
                                 name (name'First + i - 1)
                              then
                                 match := False;
                                 exit;
                              end if;
                           end loop;

                           if match then
                              removedInode := dent.inode;

                              if isFirst then
                                 --  First entry in block: zero inode
                                 dent.inode := 0;
                              else
                                 --  Merge into previous entry
                                 declare
                                    prevDent : DirectoryEntry
                                      with Import,
                                           Address => blockBuf'Address +
                                             prevOff;
                                 begin
                                    prevDent.length :=
                                      prevDent.length + dent.length;
                                 end;
                              end if;

                              declare
                                 blkOffset : constant Storage_Offset :=
                                   Storage_Offset (blkNum) *
                                     Storage_Offset (fs.blkSize);
                              begin
                                 writeBytes (fs, blkOffset,
                                             blockBuf'Address,
                                             Storage_Count (fs.blkSize));
                              end;

                              return removedInode;
                           end if;
                        end;
                     end if;

                     bytesScanned := bytesScanned +
                       Unsigned_64 (dent.length);
                     prevOff := scanOff;
                     isFirst := False;
                     scanOff := scanOff +
                       Storage_Offset (dent.length);
                  end;
               end loop;
            end;
            blockIdx := blockIdx + 1;
         end loop;
      end;

      return 0;
   end removeDirectoryEntry;

   procedure renameEntry
     (fs : in out Filesystem; dirInodeNum : Unsigned_32;
      oldName, newName : String; status : out Rename_Status)
   is
      dirIno : Inode;
      readStatus : Read_Status;
      lookupStatus : Directory_Lookup_Status;
      identity : Unsigned_32;
      original : Directory_Blocks.Block_Data := [others => 0];
      candidate : Directory_Blocks.Block_Data;
      prepareStatus : Directory_Blocks.Prepare_Result;
      blockNumber : Unsigned_32 := 0;

      function Lookup_Failure
        (result : Directory_Lookup_Status) return Rename_Status is
      begin
         case result is
            when Lookup_Malformed => return Rename_Malformed;
            when Lookup_Device_Error => return Rename_IO_Error;
            when Lookup_Out_Of_Range => return Rename_Out_Of_Range;
            when Lookup_Range_Unsupported => return Rename_Range_Unsupported;
            when others => return Rename_Source_Not_Found;
         end case;
      end Lookup_Failure;

      procedure Write_Block
        (data : Directory_Blocks.Block_Data;
         size : Directory_Blocks.Block_Length; success : out Boolean)
      is
         writeStatus : Write_Status;
      begin
         writeBytes
           (fs, Storage_Offset (blockNumber) * Storage_Offset (fs.blkSize),
            data'Address, Storage_Count (size), writeStatus);
         success := writeStatus = Write_Complete;
      end Write_Block;
      package Committer is new Directory_Commit (Write_Block);
      result : Committer.Commit_Result;
   begin
      status := Rename_Invalid_Name;
      if not CuBit.Directory_Paths.Valid_Child_Name (oldName) or else
        not CuBit.Directory_Paths.Valid_Child_Name (newName)
      then
         return;
      end if;
      if fs.writeQuarantined or else
        (fs.backend = BLOCK_DEVICE and then Is_Read_Only (fs.device.description))
      then
         status := Rename_Read_Only;
         return;
      end if;
      readInode (fs, dirInodeNum, dirIno, readStatus);
      if readStatus /= Read_Complete then
         status := (if readStatus = Read_Out_Of_Range then
                       Rename_Out_Of_Range else Rename_IO_Error);
         return;
      elsif inodeType (dirIno) /= INODE_DIRECTORY then
         status := Rename_Malformed;
         return;
      elsif dirIno.flags /= 0 then
         --  Only plain directory records are supported for mutation. In
         --  particular, changing a name without updating a hash index would
         --  make that name unreachable to an index-aware filesystem reader.
         --  Other inode flags may also require semantics we do not implement.
         status := Rename_Range_Unsupported;
         return;
      end if;

      --  Preflight the whole directory, not just the source block: another
      --  block may already contain the requested destination.
      lookupInDir (fs, dirIno, oldName, identity, lookupStatus);
      if lookupStatus /= Lookup_Found then
         status := Lookup_Failure (lookupStatus);
         return;
      end if;
      if oldName = newName then
         status := Rename_Complete;
         return;
      end if;
      lookupInDir (fs, dirIno, newName, identity, lookupStatus);
      if lookupStatus = Lookup_Found then
         status := Rename_Destination_Exists;
         return;
      elsif lookupStatus /= Lookup_Not_Found then
         status := Lookup_Failure (lookupStatus);
         return;
      end if;
      if dirIno.sizeLo = 0 or else dirIno.sizeLo mod fs.blkSize /= 0 then
         status := Rename_Malformed;
         return;
      elsif Unsigned_64 (dirIno.sizeLo) >
        Unsigned_64 (NUM_DIRECT_BLOCKS) * Unsigned_64 (fs.blkSize)
      then
         status := Rename_Range_Unsupported;
         return;
      end if;

      for index in 0 .. Natural (dirIno.sizeLo / fs.blkSize) - 1 loop
         blockNumber := dirIno.directBlocks (index);
         if blockNumber = 0 then
            status := Rename_Malformed;
            return;
         end if;
         readBlock (fs, blockNumber, original'Address, readStatus);
         if readStatus /= Read_Complete then
            status := (if readStatus = Read_Out_Of_Range then
                          Rename_Out_Of_Range else Rename_IO_Error);
            return;
         end if;
         candidate := original;
         Directory_Blocks.Prepare_Rename
           (candidate, Directory_Blocks.Block_Length (fs.blkSize),
            fs.sb.inodeCount, oldName, newName, prepareStatus);
         case prepareStatus is
            when Directory_Blocks.Prepared =>
               Committer.Commit
                 (original, candidate,
                  Directory_Blocks.Block_Length (fs.blkSize), result);
               case result is
                  when Committer.Committed => status := Rename_Complete;
                  when Committer.Original_Restored => status := Rename_IO_Error;
                  when Committer.Recovery_Required =>
                     fs.writeQuarantined := True;
                     status := Rename_Recovery_Required;
               end case;
               return;
            when Directory_Blocks.Source_Not_Found => null;
            when Directory_Blocks.Destination_Exists =>
               status := Rename_Destination_Exists;
               return;
            when Directory_Blocks.Insufficient_Space =>
               status := Rename_Range_Unsupported;
               return;
            when Directory_Blocks.Invalid_Name =>
               status := Rename_Invalid_Name;
               return;
            when Directory_Blocks.Malformed_Block =>
               status := Rename_Malformed;
               return;
            when Directory_Blocks.Unchanged =>
               status := Rename_Complete;
               return;
         end case;
      end loop;
      status := Rename_Source_Not_Found;
   end renameEntry;

   procedure renamePath
     (fs : in out Filesystem; oldPath, newPath : String;
      status : out Rename_Status)
   is
      function Leaf_Start (path : String) return Integer is
      begin
         for index in reverse path'Range loop
            if path (index) = '/' then
               return index + 1;
            end if;
         end loop;
         return path'First;
      end Leaf_Start;
      directory : Unsigned_32 := ROOT_INODE;
   begin
      status := Rename_Invalid_Name;
      if not CuBit.File_Access.Valid_Path (oldPath) or else
        not CuBit.File_Access.Valid_Path (newPath) or else
        oldPath'Length = 0 or else newPath'Length = 0 or else
        oldPath (oldPath'Last) = '/' or else newPath (newPath'Last) = '/'
      then
         return;
      end if;
      declare
         oldStart : constant Integer := Leaf_Start (oldPath);
         newStart : constant Integer := Leaf_Start (newPath);
         oldParent : constant String :=
           (if oldStart = oldPath'First then ""
            else oldPath (oldPath'First .. oldStart - 2));
         newParent : constant String :=
           (if newStart = newPath'First then ""
            else newPath (newPath'First .. newStart - 2));
      begin
         if oldParent /= newParent then
            status := Rename_Range_Unsupported;
            return;
         end if;
         if oldParent'Length > 0 then
            directory := resolvePath (fs, oldParent);
            if directory = 0 then
               status := Rename_Source_Not_Found;
               return;
            end if;
         end if;
         renameEntry
           (fs, directory, oldPath (oldStart .. oldPath'Last),
            newPath (newStart .. newPath'Last), status);
      end;
   end renamePath;

   --  Create a new file in a directory
   function createFile
     (fs          : in out Filesystem;
      dirInodeNum : Unsigned_32;
      name        : String;
      fileType    : Unsigned_8) return Unsigned_32
   is
      newInodeNum : Unsigned_32;
      allocOk     : Boolean;
      newIno      : Inode;
   begin
      if name'Length = 0 or name'Length > 255 then
         return 0;
      end if;

      --  Allocate inode
      allocateInode (fs, newInodeNum, allocOk);
      if not allocOk then
         return 0;
      end if;

      --  Initialize new inode
      readInode (fs, newInodeNum, newIno);
      if fileType = FILETYPE_DIRECTORY then
         newIno.typeAndPermissions := 16#41FF#;  -- drwxrwxrwx
      else
         newIno.typeAndPermissions := 16#81A4#;  -- -rw-r--r--
      end if;
      newIno.numHardLinks := 1;
      newIno.sizeLo := 0;
      newIno.sizeHi_DirACL := 0;
      writeInode (fs, newInodeNum, newIno);

      --  Add directory entry to parent
      if not addDirectoryEntry (fs, dirInodeNum, newInodeNum,
                                name, fileType)
      then
         freeInode (fs, newInodeNum);
         return 0;
      end if;

      return newInodeNum;
   end createFile;

   --  Truncate a file to newSize bytes
   procedure truncateFile
     (fs       : in out Filesystem;
      inodeNum : Unsigned_32;
      newSize  : Unsigned_64)
   is
      ino : Inode;
      oldSize    : Unsigned_64;
      firstFree  : Unsigned_32;
      ptrsPerBlk : Unsigned_32;
   begin
      readInode (fs, inodeNum, ino);
      oldSize := fileSize (ino);

      if newSize >= oldSize then
         return;
      end if;

      --  First logical block to free (round up)
      firstFree := Unsigned_32 (
        (newSize + Unsigned_64 (fs.blkSize) - 1) /
        Unsigned_64 (fs.blkSize));
      ptrsPerBlk := fs.blkSize / 4;

      --  Free direct blocks beyond newSize
      for i in Natural (firstFree) .. NUM_DIRECT_BLOCKS - 1 loop
         if ino.directBlocks (i) /= 0 then
            freeBlock (fs, ino.directBlocks (i));
            ino.directBlocks (i) := 0;
         end if;
      end loop;

      --  Free single indirect block entries
      if ino.singleIndirectBlock /= 0 then
         declare
            indBuf : array (0 .. 1023) of Unsigned_32
              with Alignment => 8;
            startIdx : Unsigned_32 := 0;
            anyLeft  : Boolean := False;
         begin
            readBlock (fs, ino.singleIndirectBlock, indBuf'Address);

            if firstFree > Unsigned_32 (NUM_DIRECT_BLOCKS) then
               startIdx := firstFree -
                 Unsigned_32 (NUM_DIRECT_BLOCKS);
            end if;

            for i in Natural (startIdx) ..
                     Natural (ptrsPerBlk) - 1
            loop
               if indBuf (i) /= 0 then
                  freeBlock (fs, indBuf (i));
                  indBuf (i) := 0;
               end if;
            end loop;

            --  Check if any entries remain
            for i in 0 .. Natural (startIdx) - 1 loop
               if indBuf (i) /= 0 then
                  anyLeft := True;
                  exit;
               end if;
            end loop;

            if anyLeft then
               --  Write modified indirect block
               declare
                  blkOffset : constant Storage_Offset :=
                    Storage_Offset (ino.singleIndirectBlock) *
                      Storage_Offset (fs.blkSize);
               begin
                  writeBytes (fs, blkOffset, indBuf'Address,
                              Storage_Count (fs.blkSize));
               end;
            else
               freeBlock (fs, ino.singleIndirectBlock);
               ino.singleIndirectBlock := 0;
            end if;

            invalidateBlockCache;
         end;
      end if;

      --  Update inode size
      ino.sizeLo := Unsigned_32 (newSize and 16#FFFF_FFFF#);
      ino.sizeHi_DirACL := Unsigned_32 (Shift_Right (newSize, 32));
      ino.numDiskSectors := Unsigned_32 ((newSize + 511) / 512);

      writeInode (fs, inodeNum, ino);
   end truncateFile;

   procedure initBlockDevice
     (fs         : out Filesystem;
      capSlot    : Unsigned_64;
      grant      : CuBit.Memory_Grants.Grant_Reference;
      grantBuf   : System.Address;
      grantBytes : Unsigned_32;
      ok         : out Boolean)
   is
      sb : Superblock;
      description : Device_Description;
      describeMsg : Message;
      ignore      : MessageTag;
      requiredBytes  : Unsigned_64;
      requiredBlocks : Unsigned_64;
      tmpFs : Filesystem;
   begin
      ok := False;
      fs.writeQuarantined := False;
      if grantBuf = System.Null_Address or else grantBytes = 0 then
         return;
      end if;

      describeMsg :=
        (tag      => (label => OP_DESCRIBE_DEVICE, length => 0,
                      flags => 0, badge => 0),
         capBadge => 0,
         words    => [others => 0]);
      ignore := capCall (capSlot, describeMsg);
      if describeMsg.tag.label /= REPLY_OK or else
         describeMsg.tag.length /= 4 or else
         not Decode_Description
           (describeMsg.words (0), describeMsg.words (1),
            describeMsg.words (2), describeMsg.words (3), description) or else
         grantBytes < Unsigned_32 (description.logicalBlockSize)
      then
         debugPrint ("Ext2: invalid block-device description." & ASCII.LF);
         return;
      end if;

      tmpFs.base      := System.Null_Address;
      tmpFs.imageSize := 0;
      tmpFs.blkSize   := 0;
      tmpFs.backend   := BLOCK_DEVICE;
      tmpFs.device :=
        (endpointSlot => capSlot,
         grant        => grant,
         grantBuffer  => grantBuf,
         grantBytes   => grantBytes,
         description  => description);

      --  A newly installed grant may need one retry while remote translation
      --  invalidation completes.
      for attempt in 1 .. 2 loop
         readBytes (tmpFs, SUPERBLOCK_OFFSET, sb'Address,
                    Superblock'Size / 8);
         exit when sb.signature = EXT2_SIGNATURE;
      end loop;

      if not supportedSuperblock (sb) then
         debugPrint ("Ext2: unsupported or invalid geometry." & ASCII.LF);
         return;
      end if;

      fs := tmpFs;
      fs.sb      := sb;
      fs.blkSize := blockSize (sb);
      requiredBytes := Unsigned_64 (sb.blockCount) * Unsigned_64 (fs.blkSize);
      requiredBlocks :=
        (requiredBytes + Unsigned_64 (description.logicalBlockSize) - 1) /
        Unsigned_64 (description.logicalBlockSize);
      if sb.blockCount = 0 or else requiredBlocks > description.blockCount then
         debugPrint ("Ext2: filesystem exceeds block session." & ASCII.LF);
         return;
      end if;

      ok         := True;
   end initBlockDevice;

   procedure initMemory
     (fs         : out Filesystem;
      base       : System.Address;
      imageSize  : Unsigned_64;
      ok         : out Boolean)
   is
      sb : Superblock;
      size : Unsigned_32;
   begin
      ok := False;
      fs.writeQuarantined := False;

      if base = System.Null_Address or else
         imageSize < SUPERBLOCK_OFFSET + Superblock'Size / 8
      then
         return;
      end if;

      declare
         source : Superblock
           with Import, Address => base + SUPERBLOCK_OFFSET;
      begin
         sb := source;
      end;

      --  Ext2 supports additional layouts, but the bounded implementation
      --  intentionally accepts only the geometry validated above.
      if not supportedSuperblock (sb) then
         return;
      end if;

      size := blockSize (sb);
      if sb.blockCount = 0 or else
         Unsigned_64 (sb.blockCount) > imageSize / Unsigned_64 (size)
      then
         return;
      end if;

      fs.base := base;
      fs.imageSize := imageSize;
      fs.sb := sb;
      fs.blkSize := size;
      fs.backend := MEMORY;
      fs.device := (others => <>);
      invalidateBlockCache;
      cacheIdentityValid := False;
      ok := True;
   end initMemory;

end Ext2;
