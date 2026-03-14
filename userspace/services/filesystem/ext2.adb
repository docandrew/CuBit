------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Ext2 filesystem operations for userspace ramdisk server.
--  Reads directly from a memory-mapped ramdisk image (no disk I/O).
------------------------------------------------------------------------------
with System.Storage_Elements; use System.Storage_Elements;

with CuBit.Messages; use CuBit.Messages;

package body Ext2 is

   --  ATA sector size
   ATA_SECTOR_SIZE : constant := 512;

   --  IPC labels for block device operations
   OP_READ_BLOCK  : constant Unsigned_32 := 16#0210#;
   OP_WRITE_BLOCK : constant Unsigned_32 := 16#0211#;
   REPLY_OK       : constant Unsigned_32 := 16#F000#;

   --  Indirect block cache (avoids re-reading same block per getDataBlock call)
   --  Sized for max 4KB ext2 blocks (1024 ptrs); 1KB blocks use first 256.
   cachedIndBlockNum : Unsigned_32 := 0;
   cachedIndBuf      : array (0 .. 1023) of Unsigned_32;

   cachedDIndL1Num   : Unsigned_32 := 0;
   cachedDIndL1Buf   : array (0 .. 1023) of Unsigned_32;

   cachedDIndL2Num   : Unsigned_32 := 0;
   cachedDIndL2Buf   : array (0 .. 1023) of Unsigned_32;

   procedure invalidateBlockCache is
   begin
      cachedIndBlockNum := 0;
      cachedDIndL1Num   := 0;
      cachedDIndL2Num   := 0;
   end invalidateBlockCache;

   --  Read bytes from the filesystem at a byte offset.
   --  Dispatches on fs.backend: RAMDISK does direct memory copy,
   --  ATA sends OP_READ_BLOCK IPC to the ATA driver.
   procedure readBytes
     (fs     : Filesystem;
      offset : Storage_Offset;
      dest   : System.Address;
      len    : Storage_Count)
   is
   begin
      case fs.backend is
         when ATA | NVME =>
            --  Convert byte offset/len to multi-sector reads via IPC.
            --  Read up to grantBufSize/sectorSize sectors per IPC call
            --  into the grant buffer, then copy to dest.
            declare
               byteOff       : Unsigned_64 := Unsigned_64 (offset);
               remaining     : Unsigned_64 := Unsigned_64 (len);
               dstOff        : Storage_Offset := 0;
               lba           : Unsigned_64;
               secOff        : Unsigned_64;
               copyLen       : Unsigned_64;
               sectorsNeeded : Unsigned_64;
               maxSectors    : constant Unsigned_64 :=
                 Unsigned_64 (fs.grantBufSize) / ATA_SECTOR_SIZE;
               msg           : Message;
               ignore        : MessageTag;
            begin
               while remaining > 0 loop
                  lba    := byteOff / ATA_SECTOR_SIZE;
                  secOff := byteOff mod ATA_SECTOR_SIZE;

                  --  Calculate how many sectors to read in this batch
                  sectorsNeeded :=
                    (remaining + secOff + ATA_SECTOR_SIZE - 1) /
                    ATA_SECTOR_SIZE;
                  if sectorsNeeded > maxSectors then
                     sectorsNeeded := maxSectors;
                  end if;

                  --  Read multiple sectors from driver
                  msg.tag := (label  => OP_READ_BLOCK,
                              length => 3,
                              flags  => 0,
                              badge  => 0);
                  msg.capBadge := 0;
                  msg.words := (0 => lba,
                                1 => fs.ataGrantId,
                                2 => sectorsNeeded,
                                3 => 0);

                  ignore := capCall (fs.ataCapSlot, msg);

                  if msg.tag.label /= REPLY_OK then
                     debugPrint ("Ext2: read reply not OK." & ASCII.LF);
                     --  Read failed; zero-fill remaining
                     declare
                        dstBuf : String (1 .. Natural (len))
                          with Import, Address => dest;
                     begin
                        for i in Natural (dstOff) + 1 .. Natural (len) loop
                           dstBuf (i) := Character'Val (0);
                        end loop;
                     end;
                     return;
                  end if;

                  --  Copy relevant portion from grant buffer to dest
                  copyLen :=
                    sectorsNeeded * ATA_SECTOR_SIZE - secOff;
                  if copyLen > remaining then
                     copyLen := remaining;
                  end if;

                  declare
                     srcSlice : String (1 .. Natural (copyLen))
                       with Import,
                            Address => fs.ataGrantBuf +
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
            end;
      end case;
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

   function blockSize (sb : Superblock) return Unsigned_32 is
   begin
      return Shift_Left (Unsigned_32'(1024), Natural (sb.blockShift));
   end blockSize;

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
     (fs       : Filesystem;
      inodeNum : Unsigned_32;
      ino      : out Inode)
   is
      --  Determine which block group this inode is in
      blockGroup : constant Unsigned_32 :=
        (inodeNum - 1) / fs.sb.inodesPerBlockGroup;

      --  Index within that block group's inode table
      inodeIndex : constant Unsigned_32 :=
        (inodeNum - 1) mod fs.sb.inodesPerBlockGroup;

      --  Read the block group descriptor
      bgdtOffset : constant Storage_Offset :=
        Storage_Offset ((fs.sb.firstDataBlock + 1) * fs.blkSize) +
        Storage_Offset (blockGroup) * (BlockGroupDescriptor'Size / 8);

      bgd : BlockGroupDescriptor;

      --  Compute byte offset of the inode within the inode table
      inodeTableByteOffset : Storage_Offset;
      inoSize : Unsigned_32;
   begin
      readBytes (fs, bgdtOffset, bgd'Address, BlockGroupDescriptor'Size / 8);

      --  Use inodeSize from superblock if major version >= 1
      if fs.sb.majorVersion >= 1 then
         inoSize := Unsigned_32 (fs.sb.inodeSize);
      else
         inoSize := 128;
      end if;

      inodeTableByteOffset :=
        Storage_Offset (bgd.inodeTableAddr) * Storage_Offset (fs.blkSize) +
        Storage_Offset (inodeIndex) * Storage_Offset (inoSize);

      readBytes (fs, inodeTableByteOffset, ino'Address, Inode'Size / 8);
   end readInode;

   function lookupInDir
     (fs      : Filesystem;
      dirIno  : Inode;
      name    : String) return Unsigned_32
   is
      size : constant Unsigned_64 := fileSize (dirIno);
      bytesRead : Unsigned_64 := 0;
      --  Use a stack buffer for one block
      blockBuf : String (1 .. Natural (fs.blkSize))
        with Alignment => 8;
      blockIdx : Natural := 0;
   begin
      --  Walk through each data block of the directory
      while bytesRead < size and blockIdx < NUM_DIRECT_BLOCKS loop
         declare
            blkNum : constant Unsigned_32 :=
              dirIno.directBlocks (blockIdx);
            offset : Storage_Offset := 0;
         begin
            if blkNum = 0 then
               exit;
            end if;

            readBlock (fs, blkNum, blockBuf'Address);

            --  Parse directory entries within this block
            while offset < Storage_Offset (fs.blkSize) and
                  bytesRead < size
            loop
               declare
                  dent : DirectoryEntry
                    with Import,
                         Address => blockBuf'Address + offset;
               begin
                  if dent.inode /= 0 and
                     Natural (dent.nameLength) = name'Length
                  then
                     declare
                        entryName : String (1 .. Natural (dent.nameLength))
                          with Import,
                               Address => blockBuf'Address + offset +
                                          (DirectoryEntry'Size / 8);
                        match : Boolean := True;
                     begin
                        for i in 1 .. name'Length loop
                           if entryName (i) /=
                              name (name'First + i - 1)
                           then
                              match := False;
                              exit;
                           end if;
                        end loop;

                        if match then
                           return dent.inode;
                        end if;
                     end;
                  end if;

                  bytesRead := bytesRead + Unsigned_64 (dent.length);
                  offset := offset + Storage_Offset (dent.length);

                  --  Guard against malformed entry
                  exit when dent.length = 0;
               end;
            end loop;
         end;

         blockIdx := blockIdx + 1;
      end loop;

      return 0;  --  Not found
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

   --  Get the block number for a given logical block index in a file.
   --  Handles direct blocks and single indirect blocks.
   function getDataBlock
     (fs       : Filesystem;
      ino      : Inode;
      logBlock : Unsigned_32) return Unsigned_32
   is
      ptrsPerBlock : constant Unsigned_32 := fs.blkSize / 4;
   begin
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

   function readDir
     (fs       : Filesystem;
      dirIno   : Inode;
      dest     : System.Address;
      destSize : Unsigned_64) return Unsigned_64
   is
      size : constant Unsigned_64 := fileSize (dirIno);
      bytesScanned : Unsigned_64 := 0;
      written      : Unsigned_64 := 0;
      blockBuf : String (1 .. Natural (fs.blkSize))
        with Alignment => 8;
      blockIdx : Natural := 0;

      outBuf : String (1 .. Natural (destSize))
        with Import, Address => dest;
   begin
      while bytesScanned < size and blockIdx < NUM_DIRECT_BLOCKS loop
         declare
            blkNum : constant Unsigned_32 :=
              dirIno.directBlocks (blockIdx);
            offset : Storage_Offset := 0;
         begin
            if blkNum = 0 then
               exit;
            end if;

            readBlock (fs, blkNum, blockBuf'Address);

            while offset < Storage_Offset (fs.blkSize) and
                  bytesScanned < size
            loop
               declare
                  dent : DirectoryEntry
                    with Import,
                         Address => blockBuf'Address + offset;
               begin
                  if dent.inode /= 0 and dent.nameLength > 0 then
                     declare
                        nameLen : constant Natural :=
                          Natural (dent.nameLength);
                        entryName : String (1 .. nameLen)
                          with Import,
                               Address => blockBuf'Address + offset +
                                          (DirectoryEntry'Size / 8);
                     begin
                        --  Skip "." and ".." entries
                        if not (nameLen = 1 and then
                                entryName (1) = '.') and then
                           not (nameLen = 2 and then
                                entryName (1) = '.' and then
                                entryName (2) = '.')
                        then
                           --  Check room for name + newline
                           if written + Unsigned_64 (nameLen) + 1 <=
                              destSize
                           then
                              for i in 1 .. nameLen loop
                                 outBuf (Natural (written) + i) :=
                                   entryName (i);
                              end loop;
                              written := written + Unsigned_64 (nameLen);
                              outBuf (Natural (written) + 1) := ASCII.LF;
                              written := written + 1;
                           else
                              --  No more room
                              return written;
                           end if;
                        end if;
                     end;
                  end if;

                  bytesScanned := bytesScanned +
                    Unsigned_64 (dent.length);
                  offset := offset + Storage_Offset (dent.length);
                  exit when dent.length = 0;
               end;
            end loop;
         end;
         blockIdx := blockIdx + 1;
      end loop;

      return written;
   end readDir;

   function readData
     (fs     : Filesystem;
      ino    : Inode;
      offset : Unsigned_64;
      buf    : System.Address;
      count  : Unsigned_64) return Unsigned_64
   is
      size : constant Unsigned_64 := fileSize (ino);
      remaining : Unsigned_64;
      pos       : Unsigned_64 := offset;
      bytesRead : Unsigned_64 := 0;

      --  Max contiguous bytes per batch (matches grant buffer size)
      maxContigBytes : constant Unsigned_64 := Unsigned_64 (fs.grantBufSize);
   begin
      if offset >= size then
         return 0;
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
            logBlock    : constant Unsigned_32 :=
              Unsigned_32 (pos / Unsigned_64 (fs.blkSize));
            blockOffset : constant Unsigned_32 :=
              Unsigned_32 (pos mod Unsigned_64 (fs.blkSize));
            physBlock   : constant Unsigned_32 :=
              getDataBlock (fs, ino, logBlock);
            canRead     : Unsigned_64;
         begin
            if physBlock = 0 then
               --  Sparse block (hole) — fill with zeros
               canRead := Unsigned_64 (fs.blkSize - blockOffset);
               if canRead > remaining then
                  canRead := remaining;
               end if;
               declare
                  dst : String (1 .. Natural (canRead))
                    with Import, Address => buf + Storage_Offset (bytesRead);
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

                  readBytes (fs,
                             Storage_Offset (physBlock) *
                               Storage_Offset (fs.blkSize) +
                               Storage_Offset (blockOffset),
                             buf + Storage_Offset (bytesRead),
                             Storage_Count (canRead));
               end;
            end if;

            bytesRead := bytesRead + canRead;
            pos       := pos + canRead;
            remaining := remaining - canRead;
         end;
      end loop;

      return bytesRead;
   end readData;

   --  (writeBytes uses fs.grantBufSize for sector limit)

   --  Write bytes to the filesystem at a raw byte offset.
   --  Handles non-aligned writes via read-modify-write of partial sectors.
   procedure writeBytes
     (fs     : Filesystem;
      offset : Storage_Offset;
      src    : System.Address;
      len    : Storage_Count)
   is
   begin
      case fs.backend is
         when ATA | NVME =>
            declare
               byteOff   : Unsigned_64 := Unsigned_64 (offset);
               remaining : Unsigned_64 := Unsigned_64 (len);
               srcOff    : Storage_Offset := 0;
               lba       : Unsigned_64;
               secOff    : Unsigned_64;
               copyLen   : Unsigned_64;
               msg       : Message;
               ignore    : MessageTag;
            begin
               while remaining > 0 loop
                  lba    := byteOff / ATA_SECTOR_SIZE;
                  secOff := byteOff mod ATA_SECTOR_SIZE;

                  if secOff /= 0 or remaining < ATA_SECTOR_SIZE then
                     --  Partial sector: read-modify-write
                     copyLen := ATA_SECTOR_SIZE - secOff;
                     if copyLen > remaining then
                        copyLen := remaining;
                     end if;

                     --  Read the sector into grant buffer
                     msg.tag := (label  => OP_READ_BLOCK,
                                 length => 3,
                                 flags  => 0,
                                 badge  => 0);
                     msg.capBadge := 0;
                     msg.words := (0 => lba,
                                   1 => fs.ataGrantId,
                                   2 => 1,
                                   3 => 0);
                     ignore := capCall (fs.ataCapSlot, msg);

                     if msg.tag.label /= REPLY_OK then
                        debugPrint
                          ("Ext2: write RMW read failed." & ASCII.LF);
                        return;
                     end if;

                     --  Overlay our data onto the grant buffer
                     declare
                        dstSlice : String (1 .. Natural (copyLen))
                          with Import,
                               Address => fs.ataGrantBuf +
                                 Storage_Offset (secOff);
                        srcSlice : String (1 .. Natural (copyLen))
                          with Import,
                               Address => src + srcOff;
                     begin
                        dstSlice := srcSlice;
                     end;

                     --  Write the modified sector back
                     msg.tag := (label  => OP_WRITE_BLOCK,
                                 length => 3,
                                 flags  => 0,
                                 badge  => 0);
                     msg.capBadge := 0;
                     msg.words := (0 => lba,
                                   1 => fs.ataGrantId,
                                   2 => 1,
                                   3 => 0);
                     ignore := capCall (fs.ataCapSlot, msg);

                     if msg.tag.label /= REPLY_OK then
                        debugPrint
                          ("Ext2: write RMW write failed." & ASCII.LF);
                        return;
                     end if;
                  else
                     --  Sector-aligned: batch write full sectors
                     declare
                        maxWriteSectors : constant Unsigned_64 :=
                          Unsigned_64 (fs.grantBufSize) / ATA_SECTOR_SIZE;
                        sectorsNeeded : Unsigned_64 :=
                          remaining / ATA_SECTOR_SIZE;
                     begin
                        if sectorsNeeded > maxWriteSectors then
                           sectorsNeeded := maxWriteSectors;
                        end if;

                        copyLen :=
                          sectorsNeeded * ATA_SECTOR_SIZE;

                        --  Copy source data into grant buffer
                        declare
                           dstSlice : String (1 .. Natural (copyLen))
                             with Import, Address => fs.ataGrantBuf;
                           srcSlice : String (1 .. Natural (copyLen))
                             with Import, Address => src + srcOff;
                        begin
                           dstSlice := srcSlice;
                        end;

                        --  Write sectors
                        msg.tag := (label  => OP_WRITE_BLOCK,
                                    length => 3,
                                    flags  => 0,
                                    badge  => 0);
                        msg.capBadge := 0;
                        msg.words := (0 => lba,
                                      1 => fs.ataGrantId,
                                      2 => sectorsNeeded,
                                      3 => 0);
                        ignore := capCall (fs.ataCapSlot, msg);

                        if msg.tag.label /= REPLY_OK then
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
            end;
      end case;
   end writeBytes;

   --  Write an inode back to disk (mirror of readInode)
   procedure writeInode
     (fs       : Filesystem;
      inodeNum : Unsigned_32;
      ino      : Inode)
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
   begin
      readBytes (fs, bgdtOffset, bgd'Address, BlockGroupDescriptor'Size / 8);

      if fs.sb.majorVersion >= 1 then
         inoSize := Unsigned_32 (fs.sb.inodeSize);
      else
         inoSize := 128;
      end if;

      inodeTableByteOffset :=
        Storage_Offset (bgd.inodeTableAddr) * Storage_Offset (fs.blkSize) +
        Storage_Offset (inodeIndex) * Storage_Offset (inoSize);

      writeBytes (fs, inodeTableByteOffset, ino'Address, Inode'Size / 8);
   end writeInode;

   --  Write the superblock back to disk
   procedure writeSuperblock (fs : Filesystem) is
   begin
      writeBytes (fs, SUPERBLOCK_OFFSET, fs.sb'Address,
                  Superblock'Size / 8);
   end writeSuperblock;

   --  Write a block group descriptor back to disk
   procedure writeBGD
     (fs         : Filesystem;
      blockGroup : Unsigned_32;
      bgd        : BlockGroupDescriptor)
   is
      bgdtOffset : constant Storage_Offset :=
        Storage_Offset ((fs.sb.firstDataBlock + 1) * fs.blkSize) +
        Storage_Offset (blockGroup) * (BlockGroupDescriptor'Size / 8);
   begin
      writeBytes (fs, bgdtOffset, bgd'Address,
                  BlockGroupDescriptor'Size / 8);
   end writeBGD;

   --  Read block group descriptor for a given block group
   procedure readBGD
     (fs         : Filesystem;
      blockGroup : Unsigned_32;
      bgd        : out BlockGroupDescriptor)
   is
      bgdtOffset : constant Storage_Offset :=
        Storage_Offset ((fs.sb.firstDataBlock + 1) * fs.blkSize) +
        Storage_Offset (blockGroup) * (BlockGroupDescriptor'Size / 8);
   begin
      readBytes (fs, bgdtOffset, bgd'Address,
                 BlockGroupDescriptor'Size / 8);
   end readBGD;

   --  Allocate a free block from block group 0
   procedure allocateBlock
     (fs       : in out Filesystem;
      blockNum : out Unsigned_32;
      ok       : out Boolean)
   is
      bgd : BlockGroupDescriptor;
      bitmapBuf : array (0 .. 4095) of Unsigned_8 with Alignment => 8;
      bitmapBytes : constant Unsigned_32 :=
        (fs.sb.blocksPerBlockGroup + 7) / 8;
      readSize : Unsigned_32;
   begin
      blockNum := 0;
      ok := False;

      readBGD (fs, 0, bgd);

      if bgd.numFreeBlocks = 0 then
         return;
      end if;

      readSize := bitmapBytes;
      if readSize > bitmapBuf'Length then
         readSize := Unsigned_32 (bitmapBuf'Length);
      end if;

      readBytes (fs,
                 Storage_Offset (bgd.blockBitmapAddr) *
                   Storage_Offset (fs.blkSize),
                 bitmapBuf'Address,
                 Storage_Count (readSize));

      --  Scan for first free bit
      for byteIdx in 0 .. Natural (readSize) - 1 loop
         if bitmapBuf (byteIdx) /= 16#FF# then
            for bitIdx in 0 .. 7 loop
               if (bitmapBuf (byteIdx) and
                   Shift_Left (Unsigned_8'(1), bitIdx)) = 0
               then
                  blockNum := Unsigned_32 (byteIdx * 8 + bitIdx) +
                    fs.sb.firstDataBlock;

                  --  Set the bit
                  bitmapBuf (byteIdx) := bitmapBuf (byteIdx) or
                    Shift_Left (Unsigned_8'(1), bitIdx);

                  --  Write bitmap back
                  writeBytes (fs,
                              Storage_Offset (bgd.blockBitmapAddr) *
                                Storage_Offset (fs.blkSize),
                              bitmapBuf'Address,
                              Storage_Count (readSize));

                  --  Update counts
                  bgd.numFreeBlocks := bgd.numFreeBlocks - 1;
                  writeBGD (fs, 0, bgd);

                  fs.sb.freeBlocks := fs.sb.freeBlocks - 1;
                  writeSuperblock (fs);

                  ok := True;
                  return;
               end if;
            end loop;
         end if;
      end loop;
   end allocateBlock;

   --  Free a previously allocated block
   procedure freeBlock
     (fs       : in out Filesystem;
      blockNum : Unsigned_32)
   is
      bgd : BlockGroupDescriptor;
      relBlock : constant Unsigned_32 := blockNum - fs.sb.firstDataBlock;
      byteIdx  : constant Natural := Natural (relBlock / 8);
      bitIdx   : constant Natural := Natural (relBlock mod 8);
      bitmapBuf : array (0 .. 4095) of Unsigned_8 with Alignment => 8;
      bitmapBytes : constant Unsigned_32 :=
        (fs.sb.blocksPerBlockGroup + 7) / 8;
      readSize : Unsigned_32;
   begin
      readBGD (fs, 0, bgd);

      readSize := bitmapBytes;
      if readSize > Unsigned_32 (bitmapBuf'Length) then
         readSize := Unsigned_32 (bitmapBuf'Length);
      end if;

      readBytes (fs,
                 Storage_Offset (bgd.blockBitmapAddr) *
                   Storage_Offset (fs.blkSize),
                 bitmapBuf'Address,
                 Storage_Count (readSize));

      --  Clear the bit
      bitmapBuf (byteIdx) := bitmapBuf (byteIdx) and
        not Shift_Left (Unsigned_8'(1), bitIdx);

      writeBytes (fs,
                  Storage_Offset (bgd.blockBitmapAddr) *
                    Storage_Offset (fs.blkSize),
                  bitmapBuf'Address,
                  Storage_Count (readSize));

      bgd.numFreeBlocks := bgd.numFreeBlocks + 1;
      writeBGD (fs, 0, bgd);

      fs.sb.freeBlocks := fs.sb.freeBlocks + 1;
      writeSuperblock (fs);
   end freeBlock;

   --  Allocate a free inode from block group 0
   procedure allocateInode
     (fs       : in out Filesystem;
      inodeNum : out Unsigned_32;
      ok       : out Boolean)
   is
      bgd : BlockGroupDescriptor;
      bitmapBuf : array (0 .. 4095) of Unsigned_8 with Alignment => 8;
      bitmapBytes : constant Unsigned_32 :=
        (fs.sb.inodesPerBlockGroup + 7) / 8;
      readSize : Unsigned_32;
   begin
      inodeNum := 0;
      ok := False;

      readBGD (fs, 0, bgd);

      if bgd.numFreeInodes = 0 then
         return;
      end if;

      readSize := bitmapBytes;
      if readSize > Unsigned_32 (bitmapBuf'Length) then
         readSize := Unsigned_32 (bitmapBuf'Length);
      end if;

      readBytes (fs,
                 Storage_Offset (bgd.inodeBitmapAddr) *
                   Storage_Offset (fs.blkSize),
                 bitmapBuf'Address,
                 Storage_Count (readSize));

      for byteIdx in 0 .. Natural (readSize) - 1 loop
         if bitmapBuf (byteIdx) /= 16#FF# then
            for bitIdx in 0 .. 7 loop
               if (bitmapBuf (byteIdx) and
                   Shift_Left (Unsigned_8'(1), bitIdx)) = 0
               then
                  --  Inode numbers are 1-based
                  inodeNum := Unsigned_32 (byteIdx * 8 + bitIdx) + 1;

                  --  Skip reserved inodes (1..10 in standard ext2)
                  if inodeNum < 11 then
                     goto Continue_Scan;
                  end if;

                  bitmapBuf (byteIdx) := bitmapBuf (byteIdx) or
                    Shift_Left (Unsigned_8'(1), bitIdx);

                  writeBytes (fs,
                              Storage_Offset (bgd.inodeBitmapAddr) *
                                Storage_Offset (fs.blkSize),
                              bitmapBuf'Address,
                              Storage_Count (readSize));

                  bgd.numFreeInodes := bgd.numFreeInodes - 1;
                  writeBGD (fs, 0, bgd);

                  fs.sb.freeInodes := fs.sb.freeInodes - 1;
                  writeSuperblock (fs);

                  --  Initialize blank inode on disk
                  declare
                     blankIno : Inode;
                  begin
                     blankIno := (typeAndPermissions => 0,
                                  uid => 0, sizeLo => 0,
                                  accessedTime => 0, creationTime => 0,
                                  modifiedTime => 0, deletedTime => 0,
                                  gid => 0, numHardLinks => 0,
                                  numDiskSectors => 0, flags => 0,
                                  osSpecific1 => 0,
                                  directBlocks => (others => 0),
                                  singleIndirectBlock => 0,
                                  doubleIndirectBlock => 0,
                                  tripleIndirectBlock => 0,
                                  generationNumber => 0,
                                  fileACL => 0, sizeHi_DirACL => 0,
                                  fragmentBlockAddr => 0,
                                  osSpecific2A => 0, osSpecific2B => 0,
                                  osSpecific2C => 0);
                     writeInode (fs, inodeNum, blankIno);
                  end;

                  ok := True;
                  return;

                  <<Continue_Scan>>
               end if;
            end loop;
         end if;
      end loop;
   end allocateInode;

   --  Free a previously allocated inode
   procedure freeInode
     (fs       : in out Filesystem;
      inodeNum : Unsigned_32)
   is
      bgd : BlockGroupDescriptor;
      relInode : constant Unsigned_32 := inodeNum - 1;
      byteIdx  : constant Natural := Natural (relInode / 8);
      bitIdx   : constant Natural := Natural (relInode mod 8);
      bitmapBuf : array (0 .. 4095) of Unsigned_8 with Alignment => 8;
      bitmapBytes : constant Unsigned_32 :=
        (fs.sb.inodesPerBlockGroup + 7) / 8;
      readSize : Unsigned_32;
   begin
      readBGD (fs, 0, bgd);

      readSize := bitmapBytes;
      if readSize > Unsigned_32 (bitmapBuf'Length) then
         readSize := Unsigned_32 (bitmapBuf'Length);
      end if;

      readBytes (fs,
                 Storage_Offset (bgd.inodeBitmapAddr) *
                   Storage_Offset (fs.blkSize),
                 bitmapBuf'Address,
                 Storage_Count (readSize));

      bitmapBuf (byteIdx) := bitmapBuf (byteIdx) and
        not Shift_Left (Unsigned_8'(1), bitIdx);

      writeBytes (fs,
                  Storage_Offset (bgd.inodeBitmapAddr) *
                    Storage_Offset (fs.blkSize),
                  bitmapBuf'Address,
                  Storage_Count (readSize));

      bgd.numFreeInodes := bgd.numFreeInodes + 1;
      writeBGD (fs, 0, bgd);

      fs.sb.freeInodes := fs.sb.freeInodes + 1;
      writeSuperblock (fs);
   end freeInode;

   --  Set a block pointer in an inode (direct or single indirect).
   --  Updates ino in place and writes the indirect block if needed.
   procedure setBlockPointer
     (fs       : in out Filesystem;
      ino      : in out Inode;
      logBlock : Unsigned_32;
      physBlk  : Unsigned_32)
   is
      ptrsPerBlock : constant Unsigned_32 := fs.blkSize / 4;
   begin
      if logBlock < NUM_DIRECT_BLOCKS then
         ino.directBlocks (Natural (logBlock)) := physBlk;
      elsif logBlock - Unsigned_32 (NUM_DIRECT_BLOCKS) < ptrsPerBlock then
         --  Single indirect
         declare
            indirectIdx : constant Unsigned_32 :=
              logBlock - Unsigned_32 (NUM_DIRECT_BLOCKS);
            indBuf : array (0 .. 1023) of Unsigned_32 with Alignment => 8;
         begin
            if ino.singleIndirectBlock = 0 then
               --  Allocate the indirect block itself
               declare
                  newBlk : Unsigned_32;
                  allocOk : Boolean;
               begin
                  allocateBlock (fs, newBlk, allocOk);
                  if not allocOk then
                     return;
                  end if;
                  ino.singleIndirectBlock := newBlk;
                  --  Zero-fill the new indirect block
                  indBuf := (others => 0);
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
                           Storage_Count (fs.blkSize));
            end;
            invalidateBlockCache;
         end;
      end if;
      --  Double/triple indirect allocation not implemented
   end setBlockPointer;

   --  Write file data to an inode starting at the given offset.
   --  Supports file growth via block allocation.
   function writeData
     (fs       : in out Filesystem;
      inodeNum : Unsigned_32;
      ino      : in out Inode;
      offset   : Unsigned_64;
      buf      : System.Address;
      count    : Unsigned_64) return Unsigned_64
   is
      remaining : Unsigned_64 := count;
      pos       : Unsigned_64 := offset;
      written   : Unsigned_64 := 0;
   begin
      while remaining > 0 loop
         declare
            logBlock    : constant Unsigned_32 :=
              Unsigned_32 (pos / Unsigned_64 (fs.blkSize));
            blockOffset : constant Unsigned_32 :=
              Unsigned_32 (pos mod Unsigned_64 (fs.blkSize));
            physBlock   : Unsigned_32 :=
              getDataBlock (fs, ino, logBlock);
            canWrite    : Unsigned_64 :=
              Unsigned_64 (fs.blkSize - blockOffset);
         begin
            if physBlock = 0 then
               --  Need to allocate a new block
               declare
                  allocOk : Boolean;
               begin
                  allocateBlock (fs, physBlock, allocOk);
                  if not allocOk then
                     --  Out of space
                     exit;
                  end if;
                  setBlockPointer (fs, ino, logBlock, physBlock);
               end;
            end if;

            if canWrite > remaining then
               canWrite := remaining;
            end if;

            writeBytes (fs,
                        Storage_Offset (physBlock) *
                          Storage_Offset (fs.blkSize) +
                          Storage_Offset (blockOffset),
                        buf + Storage_Offset (written),
                        Storage_Count (canWrite));

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

      --  Write updated inode back to disk
      writeInode (fs, inodeNum, ino);

      return written;
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

               setBlockPointer (fs, dirIno, Unsigned_32 (blockIdx),
                                newBlk);
               dirIno.sizeLo := dirIno.sizeLo + fs.blkSize;
               dirIno.numDiskSectors := dirIno.numDiskSectors +
                 Unsigned_32 (fs.blkSize / 512);
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

   --  Rename a file within the same directory.
   function renameEntry
     (fs          : in out Filesystem;
      dirInodeNum : Unsigned_32;
      oldName     : String;
      newName     : String) return Boolean
   is
      removedInode : Unsigned_32;
      oldIno       : Inode;
      ft           : Unsigned_8;
      inoType      : Unsigned_8;
   begin
      --  Look up the old entry to get its inode and file type
      removedInode := removeDirectoryEntry (fs, dirInodeNum, oldName);
      if removedInode = 0 then
         return False;
      end if;

      --  Determine file type from the inode, convert to dir entry type.
      --  inodeType returns upper nibble (0x4=dir, 0x8=regular, 0xA=symlink)
      --  but dir entries use different codes (2=dir, 1=regular, 7=symlink).
      readInode (fs, removedInode, oldIno);
      inoType := inodeType (oldIno);
      case inoType is
         when 16#4# => ft := FILETYPE_DIRECTORY;
         when others => ft := FILETYPE_REGULAR;
      end case;

      --  Add new entry pointing to the same inode
      return addDirectoryEntry (fs, dirInodeNum, removedInode, newName, ft);
   end renameEntry;

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

   procedure initATA
     (fs         : out Filesystem;
      capSlot    : Unsigned_64;
      grantId    : Unsigned_64;
      grantBuf   : System.Address;
      ok         : out Boolean)
   is
      sb : Superblock;

      --  Temporary filesystem for reading superblock via ATA
      tmpFs : Filesystem;
   begin
      tmpFs.base        := System.Null_Address;
      tmpFs.blkSize     := 0;
      tmpFs.backend     := ATA;
      tmpFs.ataCapSlot  := capSlot;
      tmpFs.ataGrantId  := grantId;
      tmpFs.ataGrantBuf := grantBuf;

      --  Read superblock via ATA.  The first read may return stale data
      --  if the grant mapping hasn't fully propagated yet, so retry once.
      for attempt in 1 .. 2 loop
         readBytes (tmpFs, SUPERBLOCK_OFFSET, sb'Address,
                    Superblock'Size / 8);
         exit when sb.signature = EXT2_SIGNATURE;
      end loop;

      if sb.signature /= EXT2_SIGNATURE then
         debugPrint ("Ext2.initATA: bad ext2 signature." & ASCII.LF);
         ok := False;
         return;
      end if;

      fs := tmpFs;
      fs.sb      := sb;
      fs.blkSize := blockSize (sb);
      ok         := True;
   end initATA;

end Ext2;
