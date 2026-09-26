------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Ext2 operations over capability-bound block devices.
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
with Sector_Accounting;
with Inode_Mappings;
with Block_Paths;
with Block_Inventory;
with Double_Mappings;
with Ext2_Support; use type Ext2_Support.File_Admission;

package body Ext2 is

   use type System.Address;
   use type Block_Paths.Path_Kind;
   use Volume_Admission;

   procedure Flush (fs : Filesystem; status : out Flush_Status) is
      msg : Message := NULL_MESSAGE;
   begin
      if fs.writeQuarantined then
         status := Flush_Recovery_Required;
      elsif Is_Volatile (fs.device.description) or else
        (fs.device.description.features and FEATURE_FLUSH) = 0
      then
         status := Flush_Unsupported;
      else
         --  Ext2 has no write-back cache: completed write paths have already
         --  submitted both data and metadata. The device flush is the barrier.
         msg.tag := (label => OP_FLUSH_DEVICE, length => 0,
                     flags => 0, reserved => 0);
         msg.tag := capCall (fs.device.endpointSlot, msg);
         if msg.tag.label = CuBit.Block_Devices.REPLY_OK and then
           msg.tag.length = 1 and then msg.tag.flags = 0 and then
           msg.tag.reserved = 0 and then msg.words (0) = 0
         then
            status := Flush_Complete;
         else
            status := Flush_IO_Error;
         end if;
      end if;
   end Flush;

   --  Indirect block cache (avoids re-reading same block per getDataBlock call)
   --  Sized for max 4KB ext2 blocks (1024 ptrs); 1KB blocks use first 256.
   type Pointer_Block is array (0 .. 1023) of Unsigned_32;
   type Pointer_Cache is record
      Block_Number : Unsigned_32 := 0; -- zero means invalid, never a cache hit
      Data : Pointer_Block;
   end record;
   Single_Cache, Double_Root_Cache, Double_Leaf_Cache : Pointer_Cache;

   cacheIdentityValid : Boolean := False;
   cachedCapSlot       : Unsigned_64 := 0;

   procedure invalidateBlockCache is
   begin
      Single_Cache.Block_Number := 0;
      Double_Root_Cache.Block_Number := 0;
      Double_Leaf_Cache.Block_Number := 0;
   end invalidateBlockCache;

   procedure selectCacheIdentity (fs : Filesystem) is
   begin
      if not cacheIdentityValid or else
         cachedCapSlot /= fs.device.endpointSlot
      then
         invalidateBlockCache;
         cachedCapSlot := fs.device.endpointSlot;
         cacheIdentityValid := True;
      end if;
   end selectCacheIdentity;

   --  Read bytes from the filesystem at a byte offset.
   --  All storage, including RAM, uses a Block.Device.V1 session.
   procedure readBytes
     (fs     : Filesystem;
      offset : Storage_Offset;
      dest   : System.Address;
      len    : Storage_Count;
      status : out Read_Status)
   is
   begin
      status := Read_Device_Error;
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
                        reserved  => 0);
            msg.authorityTag := 0;
            msg.words := [0 => lba,
                          1 => fs.device.grant.slot,
                          2 => sectorsNeeded,
                          3 => fs.device.grant.generation];

            ignore := capCall (fs.device.endpointSlot, msg);

            if msg.tag.label /= REPLY_OK or else
               msg.tag.length /= 1 or else
               msg.tag.flags /= 0 or else msg.tag.reserved /= 0 or else
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
   end readBytes;

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
   --  implementation. Doing this once at volume admission keeps malformed on-disk
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
        (inodeBytes and (inodeBytes - 1)) = 0 and then
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
   --  Checked direct, single-indirect and double-indirect resolution.
   procedure resolveBlock
     (fs : Filesystem; ino : Inode; logBlock : Unsigned_32;
      physical, pointerLeaf : out Unsigned_32; status : out Read_Status)
   is
      path : constant Block_Paths.Block_Path := Block_Paths.Decode
        (Unsigned_64 (logBlock), Sector_Accounting.Block_Sectors (fs.blkSize / 512));

      procedure Load (blockNum : Unsigned_32; cache : in out Pointer_Cache) is
      begin
         if blockNum = 0 or else blockNum < fs.sb.firstDataBlock or else
           blockNum >= fs.sb.blockCount
         then
            status := Read_Out_Of_Range;
            return;
         end if;
         if cache.Block_Number /= blockNum then
            --  Invalidate BEFORE touching the buffer. A multi-transfer read
            --  may overwrite only a prefix before failing; neither the old
            --  key nor the new key may then expose this buffer.
            cache.Block_Number := 0;
            readBlock (fs, blockNum, cache.Data'Address, status);
            if status /= Read_Complete then
               return;
            end if;
            cache.Block_Number := blockNum;
         end if;
         status := Read_Complete;
      end Load;
   begin
      physical := 0;
      pointerLeaf := 0;
      status := Read_Complete;
      selectCacheIdentity (fs);
      case path.Kind is
         when Block_Paths.Direct =>
            physical := ino.directBlocks (path.Direct_Slot);
         when Block_Paths.Single_Indirect =>
            if ino.singleIndirectBlock = 0 then
               return; -- absent tree: sparse data, not an I/O error
            end if;
            Load (ino.singleIndirectBlock, Single_Cache);
            if status /= Read_Complete then
               return;
            end if;
            physical := Single_Cache.Data (path.Single_Slot);
            pointerLeaf := ino.singleIndirectBlock;
         when Block_Paths.Double_Indirect =>
            if ino.doubleIndirectBlock = 0 then
               return;
            end if;
            Load (ino.doubleIndirectBlock, Double_Root_Cache);
            if status /= Read_Complete then
               return;
            end if;
            declare
               leaf : constant Unsigned_32 := Double_Root_Cache.Data (path.Root_Slot);
            begin
               if leaf = 0 then
                  return;
               end if;
               Load (leaf, Double_Leaf_Cache);
               if status /= Read_Complete then
                  return;
               end if;
               physical := Double_Leaf_Cache.Data (path.Leaf_Slot);
               pointerLeaf := leaf;
            end;
         when Block_Paths.Unsupported =>
            status := Read_File_Range_Unsupported;
            return;
      end case;
      if physical /= 0 and then
        (physical < fs.sb.firstDataBlock or else physical >= fs.sb.blockCount)
      then
         physical := 0;
         status := Read_Out_Of_Range;
      end if;
   end resolveBlock;

   procedure getDataBlock
     (fs : Filesystem; ino : Inode; logBlock : Unsigned_32;
      physical : out Unsigned_32; status : out Read_Status)
   is
      pointerLeaf : Unsigned_32;
   begin
      resolveBlock (fs, ino, logBlock, physical, pointerLeaf, status);
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
        Unsigned_64 (fs.device.grantBytes);
   begin
      bytesRead := 0;
      status := Read_Complete;
      if Ext2_Support.Check_File (ino) /= Ext2_Support.File_Allowed then
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

      if Is_Read_Only (fs.device.description)
      then
         status := Write_Read_Only;
         return;
      end if;

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
                           reserved  => 0);
               msg.authorityTag := 0;
               msg.words := [0 => lba,
                             1 => fs.device.grant.slot,
                             2 => 1,
                             3 => fs.device.grant.generation];
               ignore := capCall (fs.device.endpointSlot, msg);

               if msg.tag.label /= REPLY_OK or else
                  msg.tag.length /= 1 or else
                  msg.tag.flags /= 0 or else msg.tag.reserved /= 0 or else
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
                           reserved  => 0);
               msg.authorityTag := 0;
               msg.words := [0 => lba,
                             1 => fs.device.grant.slot,
                             2 => 1,
                             3 => fs.device.grant.generation];
               ignore := capCall (fs.device.endpointSlot, msg);

               if msg.tag.label /= REPLY_OK or else
                  msg.tag.length /= 1 or else
                  msg.tag.flags /= 0 or else msg.tag.reserved /= 0 or else
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
                              reserved  => 0);
                  msg.authorityTag := 0;
                  msg.words := [0 => lba,
                                1 => fs.device.grant.slot,
                                2 => sectorsNeeded,
                                3 => fs.device.grant.generation];
                  ignore := capCall (fs.device.endpointSlot, msg);

                  if msg.tag.label /= REPLY_OK or else
                     msg.tag.length /= 1 or else
                     msg.tag.flags /= 0 or else msg.tag.reserved /= 0 or else
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
   end writeBytes;

   type Inode_Write_Mode is (Update_Existing_Inode, Initialize_New_Inode);

   --  Existing inodes preserve their extended metadata. A newly reserved slot
   --  must be initialized in full before any directory entry can expose it.
   procedure writeInode
     (fs       : Filesystem;
      inodeNum : Unsigned_32;
      ino      : Inode;
      status   : out Write_Status;
      mode     : Inode_Write_Mode := Update_Existing_Inode)
   is
      blockGroup : constant Unsigned_32 :=
        (inodeNum - 1) / fs.sb.inodesPerBlockGroup;

      inodeIndex : constant Unsigned_32 :=
        (inodeNum - 1) mod fs.sb.inodesPerBlockGroup;

      bgdtOffset : constant Storage_Offset :=
        (Storage_Offset (fs.sb.firstDataBlock) + 1) * Storage_Offset (fs.blkSize) +
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

      case mode is
         when Update_Existing_Inode =>
            writeBytes
              (fs, inodeTableByteOffset, ino'Address, Inode'Size / 8, status);
         when Initialize_New_Inode =>
            declare
               --  Admission bounds the power-of-two inode slot by blkSize.
               slot : String (1 .. Natural (inoSize)) := [others => Character'Val (0)]
                 with Alignment => Unsigned_32'Alignment;
               header : Inode with Import, Address => slot'Address;
            begin
               header := ino;
               writeBytes
                 (fs, inodeTableByteOffset, slot'Address, Storage_Count (inoSize), status);
            end;
      end case;
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
      readStatus : Read_Status;
      writeStatus : Write_Status;
      sawAdvertisedSpace : Boolean := False;
      --  A scan-local sector snapshot, NOT a persistent metadata cache. Every
      --  descriptor read used to reread this same sector (16 records on a
      --  512-byte provider). The search performs no writes until it finds a
      --  candidate, and every mutation path then returns from allocateBlock.
      --  Therefore no cached descriptor survives a metadata mutation.
      Descriptor_Read_Bytes : constant := Logical_Block_Size'First;
      Descriptors_Per_Read : constant Unsigned_32 :=
        Descriptor_Read_Bytes / (BlockGroupDescriptor'Size / 8);
      subtype Descriptor_Index is Unsigned_32 range 0 .. Descriptors_Per_Read - 1;
      type Descriptor_Buffer is array (Descriptor_Index) of BlockGroupDescriptor;
      descriptors : Descriptor_Buffer;
      pragma Compile_Time_Error
        (Descriptor_Buffer'Size /= Descriptor_Read_Bytes * 8,
         "Ext2 descriptor sector layout changed");
   begin
      blockNum := 0;
      status := Write_No_Space;
      if fs.writeQuarantined then
         status := Write_Recovery_Required;
         return;
      elsif Is_Read_Only (fs.device.description)
      then
         status := Write_Read_Only;
         return;
      end if;

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
         if group mod Descriptors_Per_Read = 0 then
            readBytes
              (fs,
               (Storage_Offset (fs.sb.firstDataBlock) + 1) *
                 Storage_Offset (fs.blkSize) +
                 Storage_Offset (group) * (BlockGroupDescriptor'Size / 8),
               descriptors'Address, Descriptor_Read_Bytes, readStatus);
            if readStatus /= Read_Complete then
               status :=
                 (if readStatus = Read_Out_Of_Range then Write_Out_Of_Range
                  else Write_Device_Error);
               return;
            end if;
         end if;
         bgd := descriptors (group mod Descriptors_Per_Read);

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
                           --  Error replies do not prove a metadata write
                           --  left storage unchanged. Do not attempt blind
                           --  rollback or hand out an uncertain reservation.
                           fs.writeQuarantined := True;
                           status := Write_Recovery_Required;
                           return;
                        end if;

                        updatedBGD := bgd;
                        updatedBGD.numFreeBlocks :=
                          updatedBGD.numFreeBlocks - 1;
                        writeBGD (fs, group, updatedBGD, writeStatus);
                        if writeStatus /= Write_Complete then
                           --  Error replies do not prove a metadata write
                           --  left storage unchanged. Do not attempt blind
                           --  rollback or hand out an uncertain reservation.
                           fs.writeQuarantined := True;
                           status := Write_Recovery_Required;
                           return;
                        end if;

                        fs.sb.freeBlocks := fs.sb.freeBlocks - 1;
                        writeSuperblock (fs, writeStatus);
                        if writeStatus /= Write_Complete then
                           --  Error replies do not prove a metadata write
                           --  left storage unchanged. Do not attempt blind
                           --  rollback or hand out an uncertain reservation.
                           fs.writeQuarantined := True;
                           status := Write_Recovery_Required;
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

   --  Free a previously allocated block
   procedure freeBlock
     (fs       : in out Filesystem;
      blockNum : Unsigned_32;
      status : out Write_Status)
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
      readStatus : Read_Status;
   begin
      status := Write_Out_Of_Range;
      if fs.writeQuarantined then
         status := Write_Read_Only;
         return;
      end if;
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

      readBGD (fs, blockGroup, bgd, readStatus);
      if readStatus /= Read_Complete then
         status := Write_Device_Error;
         return;
      end if;

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
                 Storage_Count (readSize), readStatus);
      if readStatus /= Read_Complete then
         status := Write_Device_Error;
         return;
      end if;

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
                  Storage_Count (readSize), status);
      if status /= Write_Complete then
         fs.writeQuarantined := True;
         return;
      end if;

      bgd.numFreeBlocks := bgd.numFreeBlocks + 1;
      writeBGD (fs, blockGroup, bgd, status);
      if status /= Write_Complete then
         fs.writeQuarantined := True;
         return;
      end if;

      fs.sb.freeBlocks := fs.sb.freeBlocks + 1;
      writeSuperblock (fs, status);
      if status /= Write_Complete then
         fs.writeQuarantined := True;
      end if;
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
                        writeBGD (fs, group, updatedBGD, writeStatus);
                        if writeStatus /= Write_Complete then
                           --  Error replies do not prove a metadata write
                           --  left storage unchanged. Do not attempt blind
                           --  rollback or hand out an uncertain reservation.
                           fs.writeQuarantined := True;
                           status := Write_Recovery_Required;
                           return;
                        end if;

                        fs.sb.freeInodes := fs.sb.freeInodes - 1;
                        writeSuperblock (fs, writeStatus);
                        if writeStatus = Write_Complete then
                           writeInode
                             (fs, candidateInode, blankIno, writeStatus,
                              Initialize_New_Inode);
                        end if;

                        if writeStatus /= Write_Complete then
                           --  Error replies do not prove a metadata write
                           --  left storage unchanged. Do not attempt blind
                           --  rollback or hand out an uncertain reservation.
                           fs.writeQuarantined := True;
                           status := Write_Recovery_Required;
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

   --  Attach a direct, single-indirect or double-indirect data block.
   --  Updates ino in place and writes the indirect block if needed.
   procedure setBlockPointer
     (fs       : in out Filesystem;
      ino      : in out Inode;
      logBlock : Unsigned_32;
      physBlk  : Unsigned_32;
      status   : out Write_Status)
   is
      ptrsPerBlock : constant Unsigned_32 := fs.blkSize / 4;
      candidate : Inode;
      accepted : Boolean;
      sectors : constant Sector_Accounting.Block_Sectors :=
        Sector_Accounting.Block_Sectors (fs.blkSize / 512);
   begin
      status := Write_File_Range_Unsupported;
      if logBlock < NUM_DIRECT_BLOCKS then
         Inode_Mappings.Prepare_Attachment
           (ino, logBlock, physBlk, 0, sectors, candidate, accepted);
         if not accepted then
            status := Write_Out_Of_Range;
            return;
         end if;
         ino := candidate;
         status := Write_Complete;
      elsif logBlock - Unsigned_32 (NUM_DIRECT_BLOCKS) < ptrsPerBlock then
         --  Single indirect
         declare
            indirectIdx : constant Unsigned_32 :=
              logBlock - Unsigned_32 (NUM_DIRECT_BLOCKS);
            indBuf : Pointer_Block := [others => 0];
            readStatus : Read_Status;
            indirectBlock : Unsigned_32 := ino.singleIndirectBlock;
         begin
            if indirectBlock = 0 then
               --  Allocate the indirect block itself
               declare
                  newBlk : Unsigned_32;
               begin
                  allocateBlock (fs, newBlk, status);
                  if status /= Write_Complete then
                     return;
                  end if;
                  indirectBlock := newBlk;
                  --  Zero-fill the new indirect block
                  indBuf := [others => 0];
               end;
            else
               readBytes
                 (fs, Storage_Offset (indirectBlock) *
                    Storage_Offset (fs.blkSize),
                  indBuf'Address, Storage_Count (fs.blkSize), readStatus);
               if readStatus /= Read_Complete then
                  status := Write_Device_Error;
                  return;
               end if;
            end if;

            --  Prepare both fields together, but do not expose the candidate
            --  until its pointer block has completed successfully.
            Inode_Mappings.Prepare_Attachment
              (ino, logBlock, physBlk, indirectBlock, sectors,
               candidate, accepted);
            if not accepted or else indBuf (Natural (indirectIdx)) /= 0 then
               status := Write_Out_Of_Range;
               return;
            end if;
            indBuf (Natural (indirectIdx)) := physBlk;
            declare
               blkOffset : constant Storage_Offset :=
                 Storage_Offset (indirectBlock) *
                   Storage_Offset (fs.blkSize);
            begin
               writeBytes (fs, blkOffset, indBuf'Address,
                           Storage_Count (fs.blkSize), status);
            end;
            if status /= Write_Complete then
               --  The pointer may already have reached storage. Neither the
               --  indirect block nor its target may be returned to the free
               --  pool after an ambiguous publication.
               fs.writeQuarantined := True;
               status := Write_Recovery_Required;
               invalidateBlockCache;
               return;
            end if;
            ino := candidate;
            --  Publish only an entirely acknowledged pointer block. Keep the
            --  existing read cache warm instead of rereading the block we just
            --  wrote at the next lookup. This is not deferred write-back;
            --  failure above still invalidates every cached pointer block.
            selectCacheIdentity (fs);
            invalidateBlockCache;
            Single_Cache := (Block_Number => indirectBlock, Data => indBuf);
         end;
      else
         declare
            path : constant Block_Paths.Block_Path :=
              Block_Paths.Decode (Unsigned_64 (logBlock), sectors);
            rootBuf, leafBuf : Pointer_Block := [others => 0];
            root : Unsigned_32 := ino.doubleIndirectBlock;
            leaf : Unsigned_32 := 0;
            newRoot : constant Boolean := root = 0;
            newLeaf : Boolean;
            readStatus : Read_Status;
            cleanupStatus : Write_Status;
         begin
            if path.Kind /= Block_Paths.Double_Indirect then return; end if;
            if not newRoot then
               readBytes (fs, Storage_Offset (root) * Storage_Offset (fs.blkSize),
                          rootBuf'Address, Storage_Count (fs.blkSize), readStatus);
               if readStatus /= Read_Complete then
                  status := Write_Device_Error;
                  return;
               end if;
               leaf := rootBuf (path.Root_Slot);
            end if;
            newLeaf := leaf = 0;
            if not newLeaf then
               readBytes (fs, Storage_Offset (leaf) * Storage_Offset (fs.blkSize),
                          leafBuf'Address, Storage_Count (fs.blkSize), readStatus);
               if readStatus /= Read_Complete then
                  status := Write_Device_Error;
                  return;
               end if;
            end if;
            if leafBuf (path.Leaf_Slot) /= 0 or else
              not Double_Mappings.Fits (ino, newLeaf, sectors)
            then
               status := Write_Out_Of_Range;
               return;
            end if;
            if newRoot then
               allocateBlock (fs, root, status);
               if status /= Write_Complete then return; end if;
            end if;
            if newLeaf then
               allocateBlock (fs, leaf, status);
               if status /= Write_Complete then
                  --  Only a definite no-space result permits undoing the
                  --  unpublished root reservation. Never follow failed I/O
                  --  with speculative cleanup writes.
                  if status = Write_No_Space and then newRoot then
                     freeBlock (fs, root, cleanupStatus);
                     if cleanupStatus /= Write_Complete then
                        fs.writeQuarantined := True;
                        status := Write_Recovery_Required;
                     end if;
                  end if;
                  return;
               end if;
            end if;
            Double_Mappings.Prepare
              (ino, root, leaf, physBlk, newLeaf, sectors, candidate, accepted);
            if not accepted then
               status := Write_Out_Of_Range;
               return;
            end if;
            --  Initialize each child before publishing its parent pointer.
            --  This is checked completion ordering, not a power-loss journal.
            leafBuf (path.Leaf_Slot) := physBlk;
            writeBytes (fs, Storage_Offset (leaf) * Storage_Offset (fs.blkSize),
                        leafBuf'Address, Storage_Count (fs.blkSize), status);
            if status = Write_Complete and then newLeaf then
               rootBuf (path.Root_Slot) := leaf;
               writeBytes (fs, Storage_Offset (root) * Storage_Offset (fs.blkSize),
                           rootBuf'Address, Storage_Count (fs.blkSize), status);
            end if;
            if status /= Write_Complete then
               fs.writeQuarantined := True;
               status := Write_Recovery_Required;
               invalidateBlockCache;
               return;
            end if;
            ino := candidate;
            selectCacheIdentity (fs);
            invalidateBlockCache;
            Double_Root_Cache := (Block_Number => root, Data => rootBuf);
            Double_Leaf_Cache := (Block_Number => leaf, Data => leafBuf);
         end;
      end if;
   end setBlockPointer;

   --  Clear only mapped storage in a newly exposed byte range. Holes already
   --  read as zero and must not cause allocation. This is shared by explicit
   --  resize and positioned writes past EOF, including a retained partial block
   --  after shrink. Never publish the larger size before this completes.
   procedure zeroExposedRange
     (fs : in out Filesystem; ino : Inode; first, limit : Unsigned_64;
      status : out Write_Status)
   is
      pos : Unsigned_64 := first;
      blockBytes : constant Unsigned_64 := Unsigned_64 (fs.blkSize);
      zeroes : constant String (1 .. Natural (fs.blkSize)) :=
        [others => Character'Val (0)];
      physical, pointerLeaf : Unsigned_32;
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
              (fs, ino, Unsigned_32 (pos / blockBytes), physical, pointerLeaf, readStatus);
            if readStatus /= Read_Complete then
               status := Write_Device_Error;
               return;
            end if;
            if physical /= 0 then
               writeBytes
                 (fs, Storage_Offset (physical) * Storage_Offset (fs.blkSize) +
                    Storage_Offset (withinBlock),
                  zeroes'Address, Storage_Count (length), status);
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
                           nextBlock := Block_Paths.Block_Limit (sectors);
                        elsif pointerLeaf = 0 then
                           nextBlock := NUM_DIRECT_BLOCKS + pointers +
                             (Unsigned_64 (path.Root_Slot) + 1) * pointers;
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
      originalInode : constant Inode := ino;
      gapCleared : Boolean := offset <= fileSize (ino);
      sectors : constant Sector_Accounting.Block_Sectors :=
        Sector_Accounting.Block_Sectors (fs.blkSize / 512);
   begin
      bytesWritten := 0;
      status := Write_Complete;
      if Ext2_Support.Check_File (ino) /= Ext2_Support.File_Allowed then
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

      if count > 0 and then offset + count > 16#7FFF_FFFF# and then
        offset + count > fileSize (ino) and then
        (fs.sb.readOnlyFeatures and 2) = 0
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

      while remaining > 0 loop
         declare
            logicalIndex : constant Unsigned_64 :=
              pos / Unsigned_64 (fs.blkSize);
            blockOffset : constant Unsigned_32 :=
              Unsigned_32 (pos mod Unsigned_64 (fs.blkSize));
            physBlock   : Unsigned_32;
            pointerLeaf : Unsigned_32;
            lookupStatus : Read_Status;
            pointerStatus : Write_Status;
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
               resolveBlock (fs, ino, logBlock, physBlock, pointerLeaf, lookupStatus);
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
                 (if path.Kind = Block_Paths.Double_Indirect then
                    not Double_Mappings.Fits (ino, pointerLeaf = 0, sectors)
                  else not Inode_Mappings.Fits (ino, logBlock, sectors))
               then
                  terminalStatus := Write_Out_Of_Range;
                  exit;
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
                        --  Reservation metadata is already committed. Stop
                        --  after a transport error; speculative rollback can
                        --  compound an ambiguous device state.
                        fs.writeQuarantined := True;
                        terminalStatus := Write_Recovery_Required;
                        exit;
                     end if;

                     setBlockPointer
                       (fs, ino, logBlock, physBlock, pointerStatus);
                     if pointerStatus /= Write_Complete then
                        if pointerStatus = Write_No_Space then
                           --  A definite lack of space for the indirect root
                           --  published nothing. Reclaim only in this case.
                           freeBlock (fs, physBlock, dataStatus);
                           if dataStatus /= Write_Complete then
                              fs.writeQuarantined := True;
                           end if;
                           terminalStatus := Write_No_Space;
                        else
                           fs.writeQuarantined := True;
                           terminalStatus := Write_Recovery_Required;
                        end if;
                        exit;
                     end if;
                  end;
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

      if written > 0 and then not fs.writeQuarantined and then
        ino /= originalInode
      then
         --  Data-only overwrites do not alter the inode. Avoid a block-group
         --  read and inode-sector RMW in that common path. Compare the whole
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
        Is_Read_Only (fs.device.description)
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
      lookupStatus : Directory_Lookup_Status;
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
            resolvePath (fs, oldParent, directory, lookupStatus);
            if lookupStatus /= Lookup_Found then
               status := (case lookupStatus is
                 when Lookup_Not_Found => Rename_Source_Not_Found,
                 when Lookup_Malformed => Rename_Malformed,
                 when Lookup_Out_Of_Range => Rename_Out_Of_Range,
                 when Lookup_Range_Unsupported => Rename_Range_Unsupported,
                 when others => Rename_IO_Error);
               return;
            end if;
         end if;
         renameEntry
           (fs, directory, oldPath (oldStart .. oldPath'Last),
            newPath (newStart .. newPath'Last), status);
      end;
   end renamePath;

   --  Create a new file in a directory
   --  Create an empty regular file. Prepare the directory insertion before
   --  reserving anything; publish its initialized inode before its name.
   procedure createFile
     (fs : in out Filesystem; dirInodeNum : Unsigned_32; name : String;
      inodeNum : out Unsigned_32; status : out Write_Status)
   is
      parent : Inode;
      fresh : Inode := NULL_INODE;
      readStatus : Read_Status;
      lookupStatus : Directory_Lookup_Status;
      existing, reservedInode, targetBlock : Unsigned_32 := 0;
      parentBlocks : Natural;
      buffer : String (1 .. Natural (fs.blkSize)) := [others => Character'Val (0)]
        with Alignment => 8;
      entryOffset : Storage_Offset := 0;
      entrySpan : Unsigned_16 := 0;
      found : Boolean := False;
      grow : Boolean;
      needed : Unsigned_32;
      cleanupStatus : Write_Status;
      grownParent : Inode;
      accepted : Boolean;

      procedure Uncertain is
      begin
         fs.writeQuarantined := True;
         status := Write_Recovery_Required;
      end Uncertain;
   begin
      inodeNum := 0;
      status := Write_Out_Of_Range;
      if not CuBit.Directory_Paths.Valid_Child_Name (name) then
         return;
      elsif fs.writeQuarantined then
         status := Write_Recovery_Required;
         return;
      elsif Is_Read_Only (fs.device.description)
      then
         status := Write_Read_Only;
         return;
      end if;
      needed := ((8 + Unsigned_32 (name'Length) + 3) / 4) * 4;
      readInode (fs, dirInodeNum, parent, readStatus);
      if readStatus /= Read_Complete then
         status := Write_Device_Error;
         return;
      elsif inodeType (parent) /= INODE_DIRECTORY then
         return;
      elsif parent.flags /= 0 or else
        fileSize (parent) > Unsigned_64 (NUM_DIRECT_BLOCKS) *
          Unsigned_64 (fs.blkSize) or else
        parent.sizeLo mod fs.blkSize /= 0 or else
        parent.singleIndirectBlock /= 0 or else
        parent.doubleIndirectBlock /= 0 or else parent.tripleIndirectBlock /= 0
      then
         status := Write_File_Range_Unsupported;
         return;
      end if;
      lookupInDir (fs, parent, name, existing, lookupStatus);
      if lookupStatus = Lookup_Found then
         status := Write_Already_Exists;
         return;
      elsif lookupStatus /= Lookup_Not_Found then
         status := Write_Device_Error;
         return;
      end if;

      parentBlocks := Natural (parent.sizeLo / fs.blkSize);
      for index in 0 .. parentBlocks - 1 loop
         targetBlock := parent.directBlocks (index);
         readBlock (fs, targetBlock, buffer'Address, readStatus);
         if readStatus /= Read_Complete then
            status := Write_Device_Error;
            return;
         end if;
         declare
            position : Storage_Offset := 0;
         begin
            while position < Storage_Offset (fs.blkSize) loop
               --  Validate serialized record bounds before overlay/access.
               if Storage_Offset (fs.blkSize) - position < 8 then
                  status := Write_Device_Error;
                  return;
               end if;
               declare
                  dent : DirectoryEntry
                    with Import, Address => buffer'Address + position;
                  used : Unsigned_16;
               begin
                  if dent.length < 8 or else dent.length mod 4 /= 0 or else
                    Storage_Offset (dent.length) >
                      Storage_Offset (fs.blkSize) - position or else
                    Unsigned_16 (dent.nameLength) > dent.length - 8
                  then
                     status := Write_Device_Error;
                     return;
                  end if;
                  used := (if dent.inode = 0 then 0 else
                    Unsigned_16 (((8 + Natural (dent.nameLength) + 3) / 4) * 4));
                  if Unsigned_32 (dent.length - used) >= needed then
                     entryOffset := position + Storage_Offset (used);
                     entrySpan := dent.length - used;
                     if used /= 0 then
                        dent.length := used;
                     end if;
                     found := True;
                     exit;
                  end if;
                  position := position + Storage_Offset (dent.length);
               end;
            end loop;
         end;
         exit when found;
      end loop;

      grow := not found;
      if grow then
         if parentBlocks = NUM_DIRECT_BLOCKS then
            status := Write_File_Range_Unsupported;
            return;
         elsif parent.directBlocks (parentBlocks) /= 0 then
            --  Do not overwrite an unexplained pointer beyond directory EOF.
            status := Write_Device_Error;
            return;
         end if;
         buffer := [others => Character'Val (0)];
         entryOffset := 0;
         entrySpan := Unsigned_16 (fs.blkSize);
         if not Inode_Mappings.Fits
           (parent, Unsigned_32 (parentBlocks),
            Sector_Accounting.Block_Sectors (fs.blkSize / 512))
         then
            status := Write_Out_Of_Range;
            return;
         end if;
         --  Reserve directory space first. No inode is consumed on ENOSPC.
         allocateBlock (fs, targetBlock, status);
         if status /= Write_Complete then
            return;
         end if;
         Inode_Mappings.Prepare_Attachment
           (parent, Unsigned_32 (parentBlocks), targetBlock, 0,
            Sector_Accounting.Block_Sectors (fs.blkSize / 512),
            grownParent, accepted);
         if not accepted then
            Uncertain;
            return;
         end if;
      end if;

      allocateInode (fs, reservedInode, status);
      if status /= Write_Complete then
         if grow and then status = Write_No_Space then
            --  This block has never been published. Only a definite no-space
            --  rejection permits cleanup; transport uncertainty must stop.
            freeBlock (fs, targetBlock, cleanupStatus);
            if cleanupStatus /= Write_Complete then
               Uncertain;
            end if;
         elsif grow then
            Uncertain;
         end if;
         return;
      end if;

      fresh.typeAndPermissions := 16#81A4#;
      fresh.numHardLinks := 1;
      writeInode (fs, reservedInode, fresh, status);
      if status /= Write_Complete then
         Uncertain;
         return;
      end if;
      declare
         dent : DirectoryEntry
           with Import, Address => buffer'Address + entryOffset;
         entryName : String (1 .. name'Length)
           with Import, Address => buffer'Address + entryOffset + 8;
      begin
         dent := (inode => reservedInode, length => entrySpan,
                   nameLength => Unsigned_8 (name'Length),
                   fileType => FILETYPE_REGULAR);
         entryName := name;
      end;
      writeBytes
        (fs, Storage_Offset (targetBlock) * Storage_Offset (fs.blkSize),
         buffer'Address, Storage_Count (fs.blkSize), status);
      if status /= Write_Complete then
         --  The name may now reference reservedInode. Never free it.
         Uncertain;
         return;
      end if;
      if grow then
         parent := grownParent;
         parent.sizeLo := parent.sizeLo + fs.blkSize;
         writeInode (fs, dirInodeNum, parent, status);
         if status /= Write_Complete then
            Uncertain;
            return;
         end if;
      end if;
      inodeNum := reservedInode;
      status := Write_Complete;
   end createFile;

   --  Validate ownership within this inode BEFORE any mutation. The inventory
   --  is sized by an admitted allocation count, not by logical file size.
   --  Sorting is O(n log n), replacing the old quadratic duplicate walk.
   procedure validateBlockTree
     (fs : Filesystem; ino : Inode; status : out Truncate_Status)
   is
      sectors : constant Unsigned_32 := fs.blkSize / 512;
      ptrCount : constant Unsigned_32 := fs.blkSize / 4;
      claimed : constant Unsigned_32 := ino.numDiskSectors / sectors;
      geometryLimit : constant Unsigned_64 :=
        Block_Paths.Block_Limit (Sector_Accounting.Block_Sectors (sectors)) +
          Unsigned_64 (ptrCount) + 2;
   begin
      status := Truncate_Invalid;
      if ino.numDiskSectors mod sectors /= 0 or else
        Unsigned_64 (claimed) > geometryLimit or else claimed > fs.sb.blockCount
      then
         return;
      end if;
      declare
         blocks : Block_Inventory.Block_Array (1 .. Natural (claimed));
         count : Block_Inventory.Block_Count := 0;
         root, leaf : Pointer_Block;
         valid, unique : Boolean := True;
         readStatus : Read_Status;

         procedure Remember (number : Unsigned_32) is
         begin
            if number = 0 then
               return;
            elsif number < fs.sb.firstDataBlock or else number >= fs.sb.blockCount or else
              count = blocks'Length
            then
               valid := False;
               return;
            end if;
            count := count + 1;
            blocks (count) := number;
         end Remember;

         procedure Load (number : Unsigned_32; contents : out Pointer_Block) is
         begin
            Remember (number);
            if not valid then
               return;
            end if;
            readBytes (fs, Storage_Offset (number) * Storage_Offset (fs.blkSize),
                       contents'Address, Storage_Count (fs.blkSize), readStatus);
            if readStatus /= Read_Complete then
               status := Truncate_IO_Error;
               valid := False;
            end if;
         end Load;

         procedure Data_Pointers (contents : Pointer_Block) is
         begin
            for I in 0 .. Natural (ptrCount) - 1 loop
               Remember (contents (I));
               exit when not valid;
            end loop;
         end Data_Pointers;
      begin
         for number of ino.directBlocks loop
            Remember (number);
            exit when not valid;
         end loop;
         if not valid then return; end if;
         if ino.singleIndirectBlock /= 0 then
            Load (ino.singleIndirectBlock, leaf);
            if not valid then return; end if;
            Data_Pointers (leaf);
            if not valid then return; end if;
         end if;
         if ino.doubleIndirectBlock /= 0 then
            Load (ino.doubleIndirectBlock, root);
            if not valid then return; end if;
            for I in 0 .. Natural (ptrCount) - 1 loop
               if root (I) /= 0 then
                  Load (root (I), leaf);
                  if not valid then return; end if;
                  Data_Pointers (leaf);
                  if not valid then return; end if;
               end if;
            end loop;
         end if;
         if Unsigned_32 (count) /= claimed then
            return;
         end if;
         Block_Inventory.Sort_And_Check (blocks, unique);
         if unique then
            status := Truncate_Complete;
         end if;
      end;
   end validateBlockTree;

   --  Whole-tree preflight, then bounded leaf-sized detach/flush/reclaim.
   --  No mutable pointer tree is revisited after its storage has been freed.
   procedure resizeFile
     (fs : in out Filesystem; inodeNum : Unsigned_32;
      newSize : Unsigned_64; resizedInode : out Inode;
      status : out Truncate_Status)
   is
      ino : Inode;
      readStatus : Read_Status;
      writeStatus : Write_Status;
      flushStatus : Flush_Status;
      root, leaf : Pointer_Block := [others => 0];
      retired : array (1 .. Sector_Accounting.Retired_Blocks'Last) of Unsigned_32;
      count : Sector_Accounting.Retired_Blocks := 0;
      keepBlocks : Unsigned_64;
      mutated : Boolean := False;
      failed : Boolean := False;
      ptrCount : Unsigned_32;

      procedure Fail (reason : Truncate_Status) is
      begin
         failed := True;
         if mutated or else fs.writeQuarantined then
            fs.writeQuarantined := True;
            invalidateBlockCache;
            status := Truncate_Recovery_Required;
         else
            status := reason;
         end if;
      end Fail;

      procedure Retire (number : Unsigned_32) is
      begin
         if number /= 0 then
            count := count + 1;
            retired (count) := number;
         end if;
      end Retire;

      procedure Load (number : Unsigned_32; contents : out Pointer_Block) is
      begin
         readBytes (fs, Storage_Offset (number) * Storage_Offset (fs.blkSize),
                    contents'Address, Storage_Count (fs.blkSize), readStatus);
         if readStatus /= Read_Complete then Fail (Truncate_IO_Error); end if;
      end Load;

      procedure Publish (number : Unsigned_32; contents : Pointer_Block) is
      begin
         mutated := True;
         writeBytes (fs, Storage_Offset (number) * Storage_Offset (fs.blkSize),
                     contents'Address, Storage_Count (fs.blkSize), writeStatus);
         if writeStatus /= Write_Complete then Fail (Truncate_IO_Error); end if;
      end Publish;

      procedure Commit_Batch is
         fits : Boolean;
         newSectors : Unsigned_32;
      begin
         Sector_Accounting.Plan_Removal
           (ino.numDiskSectors, Sector_Accounting.Block_Sectors (fs.blkSize / 512),
            count, newSectors, fits);
         if not fits then
            Fail (Truncate_Invalid);
            return;
         end if;
         ino.numDiskSectors := newSectors;
         ino.sizeLo := Unsigned_32 (newSize and 16#FFFF_FFFF#);
         ino.sizeHi_DirACL := Unsigned_32 (Shift_Right (newSize, 32));
         mutated := True;
         writeInode (fs, inodeNum, ino, writeStatus);
         if writeStatus /= Write_Complete then
            Fail (Truncate_IO_Error);
            return;
         end if;
         invalidateBlockCache;
         if not Is_Volatile (fs.device.description) then
            Flush (fs, flushStatus);
            if flushStatus /= Flush_Complete then
               Fail (Truncate_IO_Error);
               return;
            end if;
         end if;
         --  Both inode and surviving parent pointers are durably detached.
         for I in 1 .. count loop
            freeBlock (fs, retired (I), writeStatus);
            if writeStatus /= Write_Complete then
               Fail (Truncate_IO_Error);
               return;
            end if;
         end loop;
         count := 0;
      end Commit_Batch;

      procedure Trim_Leaf
        (number : Unsigned_32; firstLogical : Unsigned_64;
         inDouble : Boolean; rootSlot : Natural := 0)
      is
         remains, changed : Boolean := False;
      begin
         Load (number, leaf);
         if failed then return; end if;
         for I in 0 .. Natural (ptrCount) - 1 loop
            if firstLogical + Unsigned_64 (I) >= keepBlocks then
               if leaf (I) /= 0 then
                  Retire (leaf (I));
                  leaf (I) := 0;
                  changed := True;
               end if;
            elsif leaf (I) /= 0 then
               remains := True;
            end if;
         end loop;
         if remains then
            if not changed then return; end if;
            Publish (number, leaf);
         else
            Retire (number);
            if inDouble then
               root (rootSlot) := 0;
               if (for all I in 0 .. Natural (ptrCount) - 1 => root (I) = 0) then
                  Retire (ino.doubleIndirectBlock);
                  ino.doubleIndirectBlock := 0;
               else
                  Publish (ino.doubleIndirectBlock, root);
               end if;
            else
               ino.singleIndirectBlock := 0;
            end if;
         end if;
         if failed then return; end if;
         Commit_Batch;
      end Trim_Leaf;
   begin
      resizedInode := NULL_INODE;
      status := Truncate_Invalid;
      if fs.writeQuarantined then
         status := Truncate_Recovery_Required;
         return;
      elsif Is_Read_Only (fs.device.description) then
         status := Truncate_Read_Only;
         return;
      elsif not Is_Volatile (fs.device.description) and then
        (fs.device.description.features and FEATURE_FLUSH) = 0
      then
         status := Truncate_Durability_Unsupported;
         return;
      end if;
      readInode (fs, inodeNum, ino, readStatus);
      if readStatus /= Read_Complete then
         status := Truncate_IO_Error;
         return;
      elsif Ext2_Support.Check_File (ino) /= Ext2_Support.File_Allowed or else
        ino.tripleIndirectBlock /= 0 or else ino.fileACL /= 0 or else
        fs.blkSize not in 1024 | 2048 | 4096
      then
         status := Truncate_Unsupported;
         return;
      end if;
      ptrCount := fs.blkSize / 4;
      if newSize > Unsigned_64 (fs.blkSize) *
        Block_Paths.Block_Limit (Sector_Accounting.Block_Sectors (fs.blkSize / 512)) or else
        (newSize > 16#7FFF_FFFF# and then (fs.sb.readOnlyFeatures and 2) = 0)
      then
         status := Truncate_Unsupported;
         return;
      end if;
      validateBlockTree (fs, ino, status);
      if status /= Truncate_Complete then return; end if;
      keepBlocks := newSize / Unsigned_64 (fs.blkSize) +
        (if newSize mod Unsigned_64 (fs.blkSize) = 0 then 0 else 1);
      if newSize > fileSize (ino) then
         zeroExposedRange (fs, ino, fileSize (ino), newSize, writeStatus);
         if writeStatus /= Write_Complete then
            Fail (Truncate_IO_Error);
            return;
         end if;
         Commit_Batch;
      elsif newSize < fileSize (ino) then
         if ino.doubleIndirectBlock /= 0 then
            Load (ino.doubleIndirectBlock, root);
            if failed then return; end if;
            for I in reverse 0 .. Natural (ptrCount) - 1 loop
               if root (I) /= 0 then
                  Trim_Leaf (root (I),
                    Unsigned_64 (NUM_DIRECT_BLOCKS) + Unsigned_64 (ptrCount) +
                      Unsigned_64 (I) * Unsigned_64 (ptrCount), True, I);
                  if failed then return; end if;
               end if;
            end loop;
            --  Also retire a valid but initially empty double root.
            if ino.doubleIndirectBlock /= 0 and then
              (for all I in 0 .. Natural (ptrCount) - 1 => root (I) = 0)
            then
               Retire (ino.doubleIndirectBlock);
               ino.doubleIndirectBlock := 0;
               Commit_Batch;
               if failed then return; end if;
            end if;
         end if;
         if ino.singleIndirectBlock /= 0 then
            Trim_Leaf (ino.singleIndirectBlock, NUM_DIRECT_BLOCKS, False);
            if failed then return; end if;
         end if;
         for I in ino.directBlocks'Range loop
            if Unsigned_64 (I) >= keepBlocks then
               Retire (ino.directBlocks (I));
               ino.directBlocks (I) := 0;
            end if;
         end loop;
         if count /= 0 or else fileSize (ino) /= newSize then
            Commit_Batch;
         end if;
      end if;
      if not failed then
         resizedInode := ino;
         status := Truncate_Complete;
      end if;
   end resizeFile;

   --  OPEN_TRUNCATE uses the same mutation implementation as general resize.
   procedure truncateToEmpty
     (fs : in out Filesystem; inodeNum : Unsigned_32;
      emptyInode : out Inode; status : out Truncate_Status)
   is
   begin
      resizeFile (fs, inodeNum, 0, emptyInode, status);
   end truncateToEmpty;

   procedure initBlockDevice
     (fs         : out Filesystem;
      capSlot    : Unsigned_64;
      grant      : CuBit.Memory_Grants.Grant_Reference;
      grantBuf   : System.Address;
      grantBytes : Unsigned_32;
      result     : out Admission_Result)
   is
      sb : Superblock;
      description : Device_Description;
      describeMsg : Message;
      requiredBytes  : Unsigned_64;
      requiredBlocks : Unsigned_64;
      readStatus : Read_Status;
      tmpFs : Filesystem;
   begin
      --  No partial session is published on rejection, even when this output
      --  previously held an admitted filesystem.
      fs := (sb => (mountCount | maxMountCount | signature | state |
                    errorBehaviour | minorVersion | reservedBlocksUID |
                    reservedBlocksGID | inodeSize | blockGroupNumber => 0,
                    others => 0),
             blkSize => 0, device => <>,
             writeQuarantined => False);
      result := Invalid_Session;
      if capSlot not in 1 .. 62 or else
         grantBuf = System.Null_Address or else grantBytes = 0
      then
         return;
      end if;

      describeMsg :=
        (tag      => (label => OP_DESCRIBE_DEVICE, length => 0,
                      flags => 0, reserved => 0),
         authorityTag => 0,
         words    => [others => 0]);
      describeMsg.tag := capCall (capSlot, describeMsg);
      if describeMsg.tag.label = REPLY_NO_DEVICE then
         result :=
           (if describeMsg.tag.length = 1 and then
               describeMsg.tag.flags = 0 and then describeMsg.tag.reserved = 0 and then
               describeMsg.words (0) = 0
            then No_Device else Invalid_Description);
         return;
      elsif describeMsg.tag.label /= REPLY_OK then
         result := Device_Error;
         return;
      end if;
      result := Invalid_Description;
      if describeMsg.tag.length /= 4 or else
         describeMsg.tag.flags /= 0 or else describeMsg.tag.reserved /= 0 or else
         not Decode_Description
           (describeMsg.words (0), describeMsg.words (1),
            describeMsg.words (2), describeMsg.words (3), description)
      then
         debugPrint ("Ext2: invalid block-device description." & ASCII.LF);
         return;
      end if;
      if grantBytes < Unsigned_32 (description.logicalBlockSize) then
         result := Invalid_Session;
         return;
      end if;

      tmpFs.blkSize   := 0;
      tmpFs.device :=
        (endpointSlot => capSlot,
         grant        => grant,
         grantBuffer  => grantBuf,
         grantBytes   => grantBytes,
         description  => description);

      --  Completion must mean the grant is usable. Do not hide a failed read
      --  (or malformed reply) with a speculative second admission attempt.
      readBytes (tmpFs, SUPERBLOCK_OFFSET, sb'Address,
                 Superblock'Size / 8, readStatus);
      if readStatus /= Read_Complete then
         result := (if readStatus = Read_Out_Of_Range then Invalid_Description
                    else Device_Error);
         return;
      elsif sb.signature /= EXT2_SIGNATURE or else sb.blockShift > 2 or else
        not Ext2_Support.Supported_Volume
          (sb.majorVersion, sb.creatorOS, sb.compatibleFeatures,
           sb.incompatibleFeatures, sb.readOnlyFeatures)
      then
         result := Unsupported_Filesystem;
         return;
      elsif not supportedSuperblock (sb) then
         result := Invalid_Filesystem;
         debugPrint ("Ext2: unsupported or invalid geometry." & ASCII.LF);
         return;
      end if;

      requiredBytes := Unsigned_64 (sb.blockCount) * Unsigned_64 (blockSize (sb));
      requiredBlocks :=
        (requiredBytes + Unsigned_64 (description.logicalBlockSize) - 1) /
        Unsigned_64 (description.logicalBlockSize);
      if sb.blockCount = 0 or else requiredBlocks > description.blockCount then
         result := Invalid_Filesystem;
         debugPrint ("Ext2: filesystem exceeds block session." & ASCII.LF);
         return;
      end if;

      --  A volume reopening may reuse the same endpoint/grant addresses. Old cached
      --  pointer blocks do not belong to the newly admitted filesystem.
      invalidateBlockCache;
      cacheIdentityValid := False;
      tmpFs.sb := sb;
      tmpFs.blkSize := blockSize (sb);
      fs := tmpFs;
      result := Admitted;
   end initBlockDevice;


end Ext2;
