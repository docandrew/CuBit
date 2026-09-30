------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Ext2 operations over capability-bound block devices.
------------------------------------------------------------------------------
pragma Ada_2022;
with System.Storage_Elements; use System.Storage_Elements;
with Ada.Unchecked_Conversion;

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
with Block_Cache_Index;
with Dentry_Cache;
with Double_Mappings;
with Jbd2_Recovery;
with Triple_Mappings;
with Ext2_Support; use type Ext2_Support.File_Admission;

package body Ext2 is

   --  New inodes: rw-r--r-- regular files and rwxr-xr-x directories.
   Regular_Mode   : constant Unsigned_16 := 16#81A4#;
   Directory_Mode : constant Unsigned_16 := 16#41ED#;

   use type System.Address;
   use type Block_Paths.Path_Kind;
   use type Directory_Blocks.Prepare_Result;
   use type Block_Cache_Index.Block_Key;
   use Volume_Admission;


   --  Indirect block cache (avoids re-reading same block per getDataBlock call)
   --  Sized for max 4KB ext2 blocks (1024 ptrs); 1KB blocks use first 256.
   type Pointer_Block is array (0 .. 1023) of Unsigned_32;
   type Pointer_Cache is record
      Block_Number : Unsigned_32 := 0; -- zero means invalid, never a cache hit
      Data : Pointer_Block;
   end record;
   --  One cache per tree level, so a sequential walk rereads no pointer block.
   Single_Cache, Double_Root_Cache, Double_Leaf_Cache : Pointer_Cache;
   Triple_Root_Cache, Triple_Middle_Cache, Triple_Leaf_Cache : Pointer_Cache;

   cacheIdentityValid : Boolean := False;
   cachedCapSlot       : Unsigned_64 := 0;

   --  Device requests issued (and barriers among them), for profiling
   --  the I/O each operation costs (deviceRequests).
   deviceCalls, deviceFlushes : Unsigned_64 := 0;

   procedure deviceRequests (requests, flushes : out Unsigned_64) is
   begin
      requests := deviceCalls;
      flushes := deviceFlushes;
   end deviceRequests;

   procedure invalidateBlockCache is
   begin
      Single_Cache.Block_Number := 0;
      Double_Root_Cache.Block_Number := 0;
      Double_Leaf_Cache.Block_Number := 0;
      Triple_Root_Cache.Block_Number := 0;
      Triple_Middle_Cache.Block_Number := 0;
      Triple_Leaf_Cache.Block_Number := 0;
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

   --  Read bytes from the device at a byte offset, bypassing the block cache.
   --  All storage, including RAM, uses a Block.Device.V1 session.
   procedure deviceRead
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

            deviceCalls := deviceCalls + 1;
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
   end deviceRead;

   --  Write-through block cache for metadata: inode tables, directories,
   --  bitmaps, group descriptors and pointer blocks. Cached reads fill it;
   --  every acknowledged write patches any cached copy of the blocks it
   --  covered, and whole metadata blocks are inserted once written; a failed
   --  write drops the volume's entries. A cached block therefore never holds
   --  bytes the device has not acknowledged, and none is ever dirty. Every
   --  write names its dependency class and the index keeps dirty/class tags
   --  and ordered write-back selection: the hooks a journal needs to make
   --  this cache write-back later. It relies on this service owning each
   --  volume exclusively, as the pointer caches do.
   subtype Cache_Slot is Block_Cache_Index.Slot_Index;
   subtype Block_Class is Block_Cache_Index.Block_Class;
   use all type Block_Cache_Index.Block_Class;
   Cached_Block_Bytes : constant := 4096; -- largest admitted ext2 block
   type Cached_Block is array (0 .. Cached_Block_Bytes - 1) of Unsigned_8
     with Alignment => 8;
   --  One owned-memory allocation is at most 16 MiB.
   Maximum_Chunk_Bytes : constant := 16 * 1024 * 1024;
   Cache_Index : Block_Cache_Index.Index;
   Cache_Ways : Block_Cache_Index.Way_Count :=
     Block_Cache_Index.Way_Count (Default_Cache_Megabytes / Megabytes_Per_Way);

   --  Journal spill: metadata an open operation dirties while every way of
   --  its cache set is dirty. It waits here, beside the cache, for the next
   --  commit instead of forcing a commit in mid-operation. startHandle
   --  commits first unless the operation's credits fit (as JBD2 handles
   --  reserve log credits).
   Spill_Capacity : constant := 64;
   subtype Spill_Count is Natural range 0 .. Spill_Capacity;
   subtype Spill_Index is Natural range 0 .. Spill_Capacity - 1;
   First_Spill_Slot : constant := Block_Cache_Index.Capacity;
   --  Cache ways, then spill entries: every buffer a lookup can return.
   subtype Buffer_Slot is Natural range 0 .. First_Spill_Slot + Spill_Capacity - 1;
   Spill_Keys : array (Spill_Index) of Block_Cache_Index.Block_Key;
   Spill_Classes : array (Spill_Index) of Block_Cache_Index.Block_Class;
   Spill_Used : Spill_Count := 0;

   --  Block memory for the active ways and the spill, in owned-memory
   --  chunks allocated on first use (a large cache costs nothing in the
   --  image or before a volume is used).
   type Block_Access is access all Cached_Block;
   function toBlock is new Ada.Unchecked_Conversion (System.Address, Block_Access);
   Chunk_Blocks : constant := Maximum_Chunk_Bytes / Cached_Block_Bytes;
   Maximum_Chunks : constant :=
     (Block_Cache_Index.Capacity + Spill_Capacity + Chunk_Blocks - 1) / Chunk_Blocks;
   subtype Chunk_Index is Natural range 0 .. Maximum_Chunks - 1;
   Chunk_Bases : array (Chunk_Index) of System.Address :=
     [others => System.Null_Address];
   Store_Ready : Boolean := False;

   function storeBlocks return Natural is
     (Block_Cache_Index.Sets * Cache_Ways + Spill_Capacity);

   --  Allocate the store; on failure try fewer ways (at least one).
   procedure ensureStore is
      chunks : Natural;
      base : Unsigned_64;
      failed : Boolean;
   begin
      if Store_Ready then
         return;
      end if;
      loop
         chunks := (storeBlocks + Chunk_Blocks - 1) / Chunk_Blocks;
         failed := False;
         for index in 0 .. chunks - 1 loop
            base := syscall (SYSCALL_ALLOCATE_OWNED_MEMORY,
                             Unsigned_64 (Natural'Min (Chunk_Blocks,
                               storeBlocks - index * Chunk_Blocks)) * Cached_Block_Bytes);
            if base = 0 or else base = Unsigned_64'Last then
               failed := True;
               exit;
            end if;
            Chunk_Bases (index) := System.Storage_Elements.To_Address
              (System.Storage_Elements.Integer_Address (base));
         end loop;
         exit when not failed or else Cache_Ways = 1;
         --  Chunks already obtained stay owned (bounded, startup only).
         Cache_Ways := Cache_Ways - 1;
         Block_Cache_Index.Clear (Cache_Index, Cache_Ways);
      end loop;
      Store_Ready := not failed;
      if failed then
         raise Program_Error; --  no memory for even one way: cannot run
      end if;
   end ensureStore;

   --  The block buffer of an index slot (active ways only) or spill slot.
   function Cache_Blocks (slot : Natural) return Block_Access is
      index : constant Natural :=
        (if slot < First_Spill_Slot
         then (slot / Block_Cache_Index.Ways) * Cache_Ways + slot mod Block_Cache_Index.Ways
         else Block_Cache_Index.Sets * Cache_Ways + (slot - First_Spill_Slot));
   begin
      if not Store_Ready then
         ensureStore;
      end if;
      return toBlock (Chunk_Bases (index / Chunk_Blocks) +
                      Storage_Offset ((index mod Chunk_Blocks) * Cached_Block_Bytes));
   end Cache_Blocks;
   --  Commits forced inside an open operation (spill full: its credits
   --  were understated). Diagnostic; zero in every test.
   Split_Commits : Natural := 0;
   --  Blocks an open operation placed beside a full cache set (spilled
   --  metadata, data sent home at once). Diagnostic for tests.
   Pressure_Blocks : Natural := 0;

   function splitCommits return Natural is (Split_Commits);
   function pressureBlocks return Natural is (Pressure_Blocks);

   --  The spill buffer holding key, or -1.
   function spillSlot (key : Block_Cache_Index.Block_Key) return Integer is
   begin
      for index in 0 .. Spill_Used - 1 loop
         if Spill_Keys (index) = key then
            return First_Spill_Slot + index;
         end if;
      end loop;
      return -1;
   end spillSlot;

   --  Drop a volume's spill entries (committed, or the volume discarded).
   procedure dropSpill (volume : Unsigned_64) is
      kept : Spill_Count := 0;
   begin
      for index in 0 .. Spill_Used - 1 loop
         if Spill_Keys (index).Volume /= volume then
            Spill_Keys (kept) := Spill_Keys (index);
            Spill_Classes (kept) := Spill_Classes (index);
            Cache_Blocks (First_Spill_Slot + kept).all :=
              Cache_Blocks (First_Spill_Slot + index).all;
            kept := kept + 1;
         end if;
      end loop;
      Spill_Used := kept;
   end dropSpill;

   --  File transfers from this size bypass the block cache: large writes
   --  go home directly (journaled volumes too) and large reads fill
   --  nothing, so bulk I/O cannot evict metadata. Smaller ones are cached
   --  like metadata (small files are read again soon, as Linux's page
   --  cache assumes).
   Direct_Data_Bytes : constant := 64 * 1024;

   --  Bulk file payload bypasses the cache so it cannot evict metadata.
   type Cache_Policy is (Cache_Fill, Cache_Bypass);

   function cacheKey (fs : Filesystem; number : Unsigned_32)
      return Block_Cache_Index.Block_Key is
     ((Volume => fs.device.endpointSlot, Block => number));

   function cacheable (fs : Filesystem) return Boolean is
     (fs.blkSize in 1024 | 2048 | 4096);

   --  Nothing is ever dirty in a write-through cache, so dropping a volume's
   --  entries loses nothing the device lacks.
   --  The name cache (see Dentry_Cache).
   Names : Dentry_Cache.Table;

   --  A name about to change in a directory is no longer cached.
   procedure rememberName
     (fs : Filesystem; directory : Unsigned_32; name : String; inode : Unsigned_32)
   is
      displaced : Boolean;
      from : Dentry_Cache.Directory_Id;
   begin
      if Dentry_Cache.Cacheable (name) then
         Dentry_Cache.Insert
           (Names, Dentry_Cache.Make_Key (fs.device.endpointSlot, directory, name),
            inode, displaced, from);
      end if;
   end rememberName;

   --  A change to directory that may not have completed: names not cached
   --  there can no longer be taken as absent.
   procedure uncertainDirectory (fs : Filesystem; directory : Unsigned_32) is
   begin
      Dentry_Cache.Mark_Incomplete (Names, fs.device.endpointSlot, directory);
   end uncertainDirectory;

   function completeDirectory (fs : Filesystem; directory : Unsigned_32) return Boolean is
     (Dentry_Cache.Is_Complete (Names, fs.device.endpointSlot, directory));

   procedure forgetName (fs : Filesystem; directory : Unsigned_32; name : String) is
   begin
      if Dentry_Cache.Cacheable (name) then
         Dentry_Cache.Forget
           (Names, Dentry_Cache.Make_Key (fs.device.endpointSlot, directory, name));
      end if;
   end forgetName;

   procedure forgetVolume (fs : Filesystem) is
   begin
      Dentry_Cache.Discard_Volume (Names, fs.device.endpointSlot);
      dropSpill (fs.device.endpointSlot);
      Block_Cache_Index.Discard_Volume (Cache_Index, fs.device.endpointSlot);
   end forgetVolume;

   --  The slot holding a block, read on a miss. A failed read leaves no
   --  entry. found is False only if every way of the block's set were dirty,
   --  which a write-through cache never is; callers then read directly.
   procedure cachedBlock
     (fs : Filesystem; number : Unsigned_32; slot : out Buffer_Slot;
      found : out Boolean; status : out Read_Status)
   is
      key : constant Block_Cache_Index.Block_Key := cacheKey (fs, number);
      way : Cache_Slot;
   begin
      status := Read_Complete;
      slot := 0;
      if Spill_Used > 0 and then spillSlot (key) >= 0 then
         slot := spillSlot (key);
         found := True;
         return;
      end if;
      Block_Cache_Index.Find (Cache_Index, key, found, way);
      if found then
         slot := way;
         return;
      end if;
      Block_Cache_Index.Claim (Cache_Index, key, found, way);
      if not found then
         return;
      end if;
      slot := way;
      deviceRead
        (fs, Storage_Offset (number) * Storage_Offset (fs.blkSize),
         Cache_Blocks (slot).all'Address, Storage_Count (fs.blkSize), status);
      if status /= Read_Complete then
         Block_Cache_Index.Forget (Cache_Index, key);
         found := False;
      end if;
   end cachedBlock;

   --  Read bytes through the block cache (whole blocks are cached) unless the
   --  caller bypasses it or the volume is not yet admitted.
   procedure readBytes
     (fs     : Filesystem;
      offset : Storage_Offset;
      dest   : System.Address;
      len    : Storage_Count;
      status : out Read_Status;
      policy : Cache_Policy := Cache_Fill)
   is
      blockBytes : constant Storage_Offset := Storage_Offset (fs.blkSize);
      position : Storage_Offset := offset;
      done : Storage_Offset := 0;
      slot : Buffer_Slot;
      found : Boolean;
   begin
      if policy = Cache_Bypass or else not cacheable (fs) or else offset < 0 then
         deviceRead (fs, offset, dest, len, status);
         return;
      end if;
      status := Read_Complete;
      while done < len loop
         declare
            number : constant Storage_Offset := position / blockBytes;
            within : constant Storage_Offset := position mod blockBytes;
            part : constant Storage_Offset :=
              Storage_Offset'Min (blockBytes - within, len - done);
         begin
            if number >= Storage_Offset (fs.sb.blockCount) then
               status := Read_Out_Of_Range;
               return;
            end if;
            cachedBlock (fs, Unsigned_32 (number), slot, found, status);
            if status /= Read_Complete then
               return;
            elsif found then
               declare
                  source : String (1 .. Natural (part))
                    with Import, Address => Cache_Blocks (slot) (Natural (within))'Address;
                  target : String (1 .. Natural (part))
                    with Import, Address => dest + done;
               begin
                  target := source;
               end;
            else
               deviceRead (fs, position, dest + done, part, status);
               if status /= Read_Complete then
                  return;
               end if;
            end if;
            position := position + part;
            done := done + part;
         end;
      end loop;
   end readBytes;

   --  Copy device sector lba into the grant buffer from the filesystem
   --  blocks it overlaps, filling the cache with any that are missing: small
   --  metadata writes then need no read once their block is cached. found is
   --  False when the sector lies outside the volume's blocks (the caller
   --  reads it directly); failed reports a read that must end the write.
   procedure cachedSector
     (fs : Filesystem; lba, sectorBytes : Unsigned_64;
      found, failed : out Boolean)
   is
      blockBytes : constant Unsigned_64 := Unsigned_64 (fs.blkSize);
      first : constant Unsigned_64 := lba * sectorBytes;
      position : Unsigned_64 := first;
      slot : Buffer_Slot;
      cached : Boolean;
      readStatus : Read_Status;
   begin
      found := False;
      failed := False;
      if not cacheable (fs) then
         return;
      end if;
      while position < first + sectorBytes loop
         declare
            number : constant Unsigned_64 := position / blockBytes;
            within : constant Unsigned_64 := position mod blockBytes;
            part : constant Unsigned_64 :=
              Unsigned_64'Min (blockBytes - within, first + sectorBytes - position);
         begin
            if number >= Unsigned_64 (fs.sb.blockCount) then
               return;
            end if;
            cachedBlock (fs, Unsigned_32 (number), slot, cached, readStatus);
            if readStatus /= Read_Complete then
               failed := True;
               return;
            elsif not cached then
               return;
            end if;
            declare
               source : String (1 .. Natural (part))
                 with Import, Address => Cache_Blocks (slot) (Natural (within))'Address;
               target : String (1 .. Natural (part))
                 with Import, Address =>
                   fs.device.grantBuffer + Storage_Offset (position - first);
            begin
               target := source;
            end;
            position := position + part;
         end;
      end loop;
      found := True;
   end cachedSector;

   --  After the device acknowledged a write, patch any cached copy. A whole
   --  metadata block not yet cached is inserted: it will be read again
   --  (inode tables, directories, bitmaps, pointer blocks). File payload is
   --  only patched, so bulk writes cannot evict metadata.
   procedure writeThrough
     (fs : Filesystem; offset : Storage_Offset; src : System.Address;
      len : Storage_Count; class : Block_Class)
   is
      blockBytes : constant Storage_Offset := Storage_Offset (fs.blkSize);
      position : Storage_Offset := offset;
      done : Storage_Offset := 0;
      slot : Cache_Slot;
      hit : Boolean;
   begin
      if not cacheable (fs) then
         return;
      end if;
      while done < len loop
         declare
            number : constant Storage_Offset := position / blockBytes;
            within : constant Storage_Offset := position mod blockBytes;
            part : constant Storage_Offset :=
              Storage_Offset'Min (blockBytes - within, len - done);
         begin
            exit when number >= Storage_Offset (fs.sb.blockCount);
            Block_Cache_Index.Find
              (Cache_Index, cacheKey (fs, Unsigned_32 (number)), hit, slot);
            if not hit and then part = blockBytes and then
              (class /= File_Data or else len < Direct_Data_Bytes)
            then
               Block_Cache_Index.Claim
                 (Cache_Index, cacheKey (fs, Unsigned_32 (number)), hit, slot);
            end if;
            if hit then
               declare
                  source : String (1 .. Natural (part))
                    with Import, Address => src + done;
                  target : String (1 .. Natural (part))
                    with Import, Address => Cache_Blocks (slot) (Natural (within))'Address;
               begin
                  target := source;
               end;
            end if;
            position := position + part;
            done := done + part;
         end;
      end loop;
   end writeThrough;

   --  File payload: cached (possibly dirty) blocks are copied; runs of
   --  uncached blocks are read from the device directly, without filling the
   --  cache, so bulk reads cannot evict metadata.
   procedure readPayload
     (fs : Filesystem; offset : Storage_Offset; dest : System.Address;
      len : Storage_Count; status : out Read_Status)
   is
      blockBytes : constant Storage_Offset := Storage_Offset (fs.blkSize);
      position : Storage_Offset := offset;
      done : Storage_Offset := 0;
      missStart : Storage_Offset := 0;
      missLength : Storage_Offset := 0;
      slot : Buffer_Slot;
      way : Cache_Slot;
      found : Boolean;

      procedure Read_Misses is
      begin
         if missLength > 0 then
            deviceRead (fs, missStart, dest + (missStart - offset), missLength, status);
            missLength := 0;
         end if;
      end Read_Misses;
   begin
      status := Read_Complete;
      if not cacheable (fs) then
         deviceRead (fs, offset, dest, len, status);
         return;
      elsif len < Direct_Data_Bytes then
         readBytes (fs, offset, dest, len, status);
         return;
      end if;
      while done < len loop
         declare
            number : constant Storage_Offset := position / blockBytes;
            within : constant Storage_Offset := position mod blockBytes;
            part : constant Storage_Offset :=
              Storage_Offset'Min (blockBytes - within, len - done);
         begin
            if Spill_Used > 0 and then
              spillSlot (cacheKey (fs, Unsigned_32 (number))) >= 0
            then
               found := True;
               slot := spillSlot (cacheKey (fs, Unsigned_32 (number)));
            else
               Block_Cache_Index.Find
                 (Cache_Index, cacheKey (fs, Unsigned_32 (number)), found, way);
               slot := way;
            end if;
            if found then
               Read_Misses;
               if status /= Read_Complete then
                  return;
               end if;
               declare
                  source : String (1 .. Natural (part))
                    with Import, Address => Cache_Blocks (slot) (Natural (within))'Address;
                  target : String (1 .. Natural (part))
                    with Import, Address => dest + done;
               begin
                  target := source;
               end;
            else
               if missLength = 0 then
                  missStart := position;
               end if;
               missLength := missLength + part;
            end if;
            position := position + part;
            done := done + part;
         end;
      end loop;
      Read_Misses;
   end readPayload;

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

   --  The fast path of a lookup: a plain directory's blocks scanned in the
   --  block cache, comparing lengths before bytes. With clean, the answer
   --  (and, if found, the index of the directory block holding the name);
   --  without, a record did not validate or a block was not cached, and
   --  the general, fully checked path must answer.
   procedure scanCachedDirectory
     (fs : Filesystem; dirIno : Inode; name : String;
      inodeNum : out Unsigned_32; blockIndex : out Natural;
      status : out Directory_Lookup_Status; clean : out Boolean)
   is
   begin
      inodeNum := 0;
      blockIndex := 0;
      status := Lookup_Not_Found;
      clean := True;
      if inodeType (dirIno) = INODE_DIRECTORY and then dirIno.flags = 0 and then
        cacheable (fs) and then dirIno.sizeLo /= 0 and then
        dirIno.sizeLo mod fs.blkSize = 0 and then
        Unsigned_64 (dirIno.sizeLo) <=
          Unsigned_64 (NUM_DIRECT_BLOCKS) * Unsigned_64 (fs.blkSize) and then
        dirIno.sizeHi_DirACL = 0 and then name'Length in 1 .. 255
      then
         declare
            blockBytes : constant Natural := Natural (fs.blkSize);
            slot : Buffer_Slot;
            cached : Boolean := True;
            readStatus : Read_Status;
         begin
            for index in 0 .. Natural (dirIno.sizeLo / fs.blkSize) - 1 loop
               if dirIno.directBlocks (index) = 0 or else
                 dirIno.directBlocks (index) >= fs.sb.blockCount
               then
                  clean := False;
                  exit;
               end if;
               cachedBlock (fs, dirIno.directBlocks (index), slot, cached, readStatus);
               if readStatus /= Read_Complete then
                  --  A failed read is not retried by the general path.
                  status := (if readStatus = Read_Out_Of_Range then Lookup_Out_Of_Range
                             else Lookup_Device_Error);
                  return;
               elsif not cached then
                  clean := False;
                  exit;
               end if;
               declare
                  data : Cached_Block renames Cache_Blocks (slot).all;
                  position : Natural := 0;
                  span, length : Natural;
                  number : Unsigned_32;
               begin
                  while position < blockBytes loop
                     if blockBytes - position < 8 then
                        clean := False;
                        exit;
                     end if;
                     span := Natural (data (position + 4)) + 256 * Natural (data (position + 5));
                     length := Natural (data (position + 6));
                     number := Unsigned_32 (data (position)) or
                       Shift_Left (Unsigned_32 (data (position + 1)), 8) or
                       Shift_Left (Unsigned_32 (data (position + 2)), 16) or
                       Shift_Left (Unsigned_32 (data (position + 3)), 24);
                     if span < 8 or else span mod 4 /= 0 or else
                       span > blockBytes - position or else length > span - 8 or else
                       number > fs.sb.inodeCount or else (number /= 0 and then length = 0)
                     then
                        clean := False;
                        exit;
                     end if;
                     if number /= 0 and then length = name'Length then
                        declare
                           matches : Boolean := True;
                        begin
                           for k in 0 .. length - 1 loop
                              if Character'Val (data (position + 8 + k)) /=
                                name (name'First + k)
                              then
                                 matches := False;
                                 exit;
                              end if;
                           end loop;
                           if matches then
                              inodeNum := number;
                              blockIndex := index;
                              status := Lookup_Found;
                              return;
                           end if;
                        end;
                     end if;
                     position := position + span;
                  end loop;
               end;
               exit when not clean;
            end loop;
         end;
      else
         clean := False;
      end if;
   end scanCachedDirectory;

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
      blockIndex : Natural;
      clean : Boolean;
   begin
      inodeNum := 0;
      status := Lookup_Not_Found;
      scanCachedDirectory (fs, dirIno, name, inodeNum, blockIndex, status, clean);
      if clean or else status not in Lookup_Found | Lookup_Not_Found then
         return;
      end if;
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




   --  Cache every name of a plain directory and mark it complete, when all
   --  its records validate and every name fits the cache (and none of its
   --  entries was displaced meanwhile). Otherwise nothing is claimed.
   procedure indexDirectory
     (fs : Filesystem; number : Unsigned_32; dir : Inode; readStatus : out Read_Status)
   is
      blockBytes : constant Natural := Natural (fs.blkSize);
      slot : Buffer_Slot;
      cached : Boolean;
      clean : Boolean := True;
      displaced : Boolean;
      from : Dentry_Cache.Directory_Id;
   begin
      readStatus := Read_Complete;
      if inodeType (dir) /= INODE_DIRECTORY or else dir.flags /= 0 or else
        not cacheable (fs) or else dir.sizeLo = 0 or else
        dir.sizeLo mod fs.blkSize /= 0 or else dir.sizeHi_DirACL /= 0 or else
        Unsigned_64 (dir.sizeLo) >
          Unsigned_64 (NUM_DIRECT_BLOCKS) * Unsigned_64 (fs.blkSize)
      then
         return;
      end if;
      for index in 0 .. Natural (dir.sizeLo / fs.blkSize) - 1 loop
         if dir.directBlocks (index) = 0 or else
           dir.directBlocks (index) >= fs.sb.blockCount
         then
            return;
         end if;
         cachedBlock (fs, dir.directBlocks (index), slot, cached, readStatus);
         if readStatus /= Read_Complete or else not cached then
            return;
         end if;
         declare
            data : Cached_Block renames Cache_Blocks (slot).all;
            position : Natural := 0;
            span, length : Natural;
            entryInode : Unsigned_32;
         begin
            while position < blockBytes loop
               if blockBytes - position < 8 then
                  return;
               end if;
               span := Natural (data (position + 4)) + 256 * Natural (data (position + 5));
               length := Natural (data (position + 6));
               entryInode := Unsigned_32 (data (position)) or
                 Shift_Left (Unsigned_32 (data (position + 1)), 8) or
                 Shift_Left (Unsigned_32 (data (position + 2)), 16) or
                 Shift_Left (Unsigned_32 (data (position + 3)), 24);
               if span < 8 or else span mod 4 /= 0 or else
                 span > blockBytes - position or else length > span - 8 or else
                 entryInode > fs.sb.inodeCount or else
                 (entryInode /= 0 and then length = 0)
               then
                  return;
               end if;
               if entryInode /= 0 then
                  declare
                     name : String (1 .. length);
                  begin
                     for k in 1 .. length loop
                        name (k) := Character'Val (data (position + 7 + k));
                     end loop;
                     if name /= "." and then name /= ".." then
                        if not Dentry_Cache.Cacheable (name) then
                           clean := False;
                        else
                           Dentry_Cache.Insert
                             (Names,
                              Dentry_Cache.Make_Key (fs.device.endpointSlot, number, name),
                              entryInode, displaced, from);
                           if displaced and then from.Parent = number and then
                             from.Volume = fs.device.endpointSlot
                           then
                              clean := False;
                           end if;
                        end if;
                     end if;
                  end;
               end if;
               position := position + span;
            end loop;
         end;
      end loop;
      if clean then
         Dentry_Cache.Mark_Complete (Names, fs.device.endpointSlot, number);
      end if;
   end indexDirectory;

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
            declare
               name : String renames path (first .. last);
               cached : Boolean := False;
               found : Unsigned_32;
               parent : constant Unsigned_32 := current;
            begin
               if Dentry_Cache.Cacheable (name) then
                  Dentry_Cache.Find
                    (Names, Dentry_Cache.Make_Key (fs.device.endpointSlot, parent, name),
                     cached, found);
               end if;
               if cached and then found = Dentry_Cache.No_Inode then
                  status := Lookup_Not_Found;
                  return;
               elsif cached then
                  current := found;
               else
                  readInode (fs, current, ino, readStatus);
                  if readStatus /= Read_Complete then
                     status := (if readStatus = Read_Out_Of_Range then
                                  Lookup_Out_Of_Range else Lookup_Device_Error);
                     return;
                  end if;
                  --  A complete directory has every name cached: this one
                  --  is absent. Otherwise index the directory once, so later
                  --  lookups in it are hash probes, and look again.
                  if Dentry_Cache.Cacheable (name) and then
                    not completeDirectory (fs, parent)
                  then
                     indexDirectory (fs, parent, ino, readStatus);
                     if readStatus /= Read_Complete then
                        --  A failed read is reported, not retried.
                        status := (if readStatus = Read_Out_Of_Range then
                                     Lookup_Out_Of_Range else Lookup_Device_Error);
                        return;
                     end if;
                  end if;
                  if Dentry_Cache.Cacheable (name) and then completeDirectory (fs, parent) then
                     Dentry_Cache.Find
                       (Names, Dentry_Cache.Make_Key (fs.device.endpointSlot, parent, name),
                        cached, found);
                     if cached and then found /= Dentry_Cache.No_Inode then
                        current := found;
                        status := Lookup_Found;
                     else
                        status := Lookup_Not_Found;
                     end if;
                  else
                     lookupInDir (fs, ino, name, current, status);
                  end if;
                  --  A complete directory needs no negative entries (and they
                  --  could displace its names).
                  if status = Lookup_Found or else
                    (status = Lookup_Not_Found and then not completeDirectory (fs, parent))
                  then
                     rememberName
                       (fs, parent, name,
                        (if status = Lookup_Found then current else Dentry_Cache.No_Inode));
                  end if;
                  if status /= Lookup_Found then
                     return;
                  end if;
               end if;
            end;
            first := last + 1;
         end if;
      end loop;
      inodeNum := current;
      status := Lookup_Found;
   end resolvePath;

   --  Get the block number for a given logical block index in a file.
   --  Checked direct, single-, double- and triple-indirect resolution.
   --  pointerLeaf is the pointer block holding the data slot; pointerMiddle
   --  is the triple-tree middle block holding that leaf. Zero means absent.
   procedure resolveBlock
     (fs : Filesystem; ino : Inode; logBlock : Unsigned_32;
      physical, pointerLeaf, pointerMiddle : out Unsigned_32;
      status : out Read_Status)
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
      pointerMiddle := 0;
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
         when Block_Paths.Triple_Indirect =>
            if ino.tripleIndirectBlock = 0 then
               return;
            end if;
            Load (ino.tripleIndirectBlock, Triple_Root_Cache);
            if status /= Read_Complete then
               return;
            end if;
            declare
               middle : constant Unsigned_32 :=
                 Triple_Root_Cache.Data (path.Top_Slot);
            begin
               if middle = 0 then
                  return;
               end if;
               Load (middle, Triple_Middle_Cache);
               if status /= Read_Complete then
                  return;
               end if;
               pointerMiddle := middle;
               declare
                  leaf : constant Unsigned_32 :=
                    Triple_Middle_Cache.Data (path.Middle_Slot);
               begin
                  if leaf = 0 then
                     return;
                  end if;
                  Load (leaf, Triple_Leaf_Cache);
                  if status /= Read_Complete then
                     return;
                  end if;
                  physical := Triple_Leaf_Cache.Data (path.Bottom_Slot);
                  pointerLeaf := leaf;
               end;
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
      pointerLeaf, pointerMiddle : Unsigned_32;
   begin
      resolveBlock (fs, ino, logBlock, physical, pointerLeaf, pointerMiddle, status);
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

   --  writeBytes uses the session's described logical block and transfer bound.

   --  Write bytes to the device at a raw byte offset. Non-aligned writes
   --  read-modify-write partial sectors, taking the other bytes from cached
   --  blocks when present instead of reading the sector.
   procedure deviceWrite
     (fs     : Filesystem;
      offset : Storage_Offset;
      src    : System.Address;
      len    : Storage_Count;
      status : out Write_Status;
      fua    : Boolean := False)
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
         fromCache, cacheReadFailed : Boolean;
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

               --  The sector's other bytes: cached blocks equal the
               --  acknowledged disk state. Outside the volume's blocks (or
               --  before admission), read the sector itself.
               cachedSector (fs, lba, deviceBlockSize, fromCache, cacheReadFailed);
               if cacheReadFailed then
                  debugPrint ("Ext2: write RMW read failed." & ASCII.LF);
                  return;
               elsif not fromCache then
                  msg.tag := (label  => OP_READ_BLOCKS,
                              length => 4,
                              flags  => 0,
                              reserved  => 0);
                  msg.authorityTag := 0;
                  msg.words := [0 => lba,
                                1 => fs.device.grant.slot,
                                2 => 1,
                                3 => fs.device.grant.generation];
            deviceCalls := deviceCalls + 1;
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
            deviceCalls := deviceCalls + 1;
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
                              flags  => (if fua then WRITE_FLAG_FUA else 0),
                              reserved  => 0);
                  msg.authorityTag := 0;
                  msg.words := [0 => lba,
                                1 => fs.device.grant.slot,
                                2 => sectorsNeeded,
                                3 => fs.device.grant.generation];
            deviceCalls := deviceCalls + 1;
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
   end deviceWrite;


   Journal_Start_Offset : constant := 16#1C#;
   Journal_Sequence_Offset : constant := 16#18#;

   --  One device transfer of up to this many bytes (the staging buffer).
   Maximum_Transfer_Bytes : constant := 512 * 1024;
   subtype Staging_Index is Natural range 0 .. Maximum_Transfer_Bytes - 1;
   type Staging_Buffer is array (Staging_Index) of Unsigned_8 with Alignment => 8;

   --  Completed writes are durable: nothing to do without a volatile cache;
   --  otherwise the device's FLUSH. A failure quarantines the volume.
   --  Checkpoint writes issued since the last barrier (see Journal_State).
   Home_Writes_Pending : Boolean := False;

   procedure barrier (fs : in out Filesystem; ok : out Boolean) is
      msg : Message := NULL_MESSAGE;
   begin
      ok := True;
      if not Has_Volatile_Cache (fs.device.description) then
         Home_Writes_Pending := False;
         return;
      end if;
      msg.tag := (label => OP_FLUSH_DEVICE, length => 0, flags => 0, reserved => 0);
      deviceCalls := deviceCalls + 1;
      deviceFlushes := deviceFlushes + 1;
      msg.tag := capCall (fs.device.endpointSlot, msg);
      ok := msg.tag.label = CuBit.Block_Devices.REPLY_OK and then
        msg.tag.length = 1 and then msg.tag.flags = 0 and then
        msg.tag.reserved = 0 and then msg.words (0) = 0;
      if ok then
         Home_Writes_Pending := False;
      else
         fs.writeQuarantined := True;
      end if;
   end barrier;

   --  One block durable on its own: FUA where offered, else write + barrier.
   procedure writeDurable
     (fs : in out Filesystem; number : Unsigned_32; source : System.Address;
      ok : out Boolean)
   is
      status : Write_Status;
      useFua : constant Boolean :=
        Has_Volatile_Cache (fs.device.description) and then
        Supports_FUA (fs.device.description);
   begin
      deviceWrite (fs, Storage_Offset (number) * Storage_Offset (fs.blkSize), source,
                   Storage_Count (fs.blkSize), status, fua => useFua);
      ok := status = Write_Complete;
      if ok and then not useFua then
         barrier (fs, ok);
      end if;
      if not ok then
         fs.writeQuarantined := True;
      end if;
   end writeDurable;

   function dirtyBlocks (fs : Filesystem) return Boolean is
     (Block_Cache_Index.Has_Dirty (Cache_Index, fs.device.endpointSlot) or else
      fs.pendingCount > 0);

   --  Commit the running transaction of a journaled volume, JBD2 style and
   --  data=ordered: (1) dirty file data home; (2) barrier; (3) the log at the
   --  journal's first block: descriptor blocks naming each dirty metadata
   --  block's home, followed by its copy (escaped if it begins with the
   --  journal magic), with the journal's checksums; (4) barrier, then the
   --  commit block, durable; (5) checkpoint: each metadata block home, then
   --  a barrier; (6) the journal superblock's next sequence, durable. Until
   --  (6) a crash replays the transaction; replay writes the same blocks the
   --  checkpoint does. The log is always empty again after (6), so no revoke
   --  record is ever needed. A transaction larger than the log is split into
   --  several, each whole. Any failure quarantines: the device state is then
   --  uncertain and dirty blocks stay dirty, never silently dropped.
   procedure applyReleases (fs : in out Filesystem; status : out Write_Status);

   --  Set by commitTransaction: its last run committed a transaction, and
   --  so ended with every earlier write durable.
   Commit_Was_Barrier : Boolean := False;

   procedure commitTransaction (fs : in out Filesystem; status : out Write_Status) is
      volume : constant Unsigned_64 := fs.device.endpointSlot;
      blockBytes : constant Unsigned_64 := Unsigned_64 (fs.blkSize);
      size : constant Jbd2_Format.Block_Bytes := Jbd2_Format.Block_Bytes (fs.blkSize);
      incompat : constant Unsigned_32 := fs.journal.Incompat;
      checksums : constant Boolean := Jbd2_Format.Checksummed (incompat);
      v3 : constant Boolean := Jbd2_Format.Has (incompat, Jbd2_Format.Incompat_Csum_V3);
      wide : constant Boolean := Jbd2_Format.Has (incompat, Jbd2_Format.Incompat_64bit);
      transactionSums : constant Boolean :=
        not checksums and then
        Jbd2_Format.Has (fs.journal.Compat, Jbd2_Format.Compat_Checksum);
      seed : constant Unsigned_32 :=
        (if checksums then Jbd2_Format.Crc32c_UUID (16#FFFF_FFFF#, fs.journal.Identity)
         else 0);
      tagBytes : constant Natural := Jbd2_Format.Tag_Bytes (incompat);
      --  The first tag carries the journal UUID; later ones SAME_UUID.
      tagsPerDescriptor : constant Natural :=
        (Jbd2_Format.Usable_Bytes (size, incompat) - Jbd2_Format.Header_Bytes -
           Jbd2_Format.UUID_Bytes) / tagBytes;
      logBlocks : constant Unsigned_32 := fs.journal.Max_Length - fs.journal.First;
      slotBits : constant := 14;
      pragma Compile_Time_Error
        (2 ** slotBits <= Buffer_Slot'Last, "slot encoding too narrow");
      type Pending_Array is array (1 .. Buffer_Slot'Last + 1) of Unsigned_64;
      data, meta : Pending_Array;
      dataCount, metaCount : Natural := 0;
      staging : Staging_Buffer;
      runStart : Unsigned_32 := 0;
      runBlocks : Natural := 0;
      ok : Boolean := True;
      releasing : Boolean := False;

      function Block_Of (item : Unsigned_64) return Unsigned_32 is
        (Unsigned_32 (Shift_Right (item, slotBits)));
      function Slot_Of (item : Unsigned_64) return Buffer_Slot is
        (Buffer_Slot (item and (2 ** slotBits - 1)));

      --  Spill entries leave the spill as a whole after the commit.
      procedure Clean (slot : Buffer_Slot) is
      begin
         if slot < First_Spill_Slot then
            Block_Cache_Index.Mark_Clean (Cache_Index, slot);
         end if;
      end Clean;

      procedure Sort (list : in out Pending_Array; count : Natural) is
         procedure Sift (start, last : Positive) is
            parent : Positive := start;
            child : Positive;
            saved : Unsigned_64;
         begin
            while parent <= last / 2 loop
               child := parent * 2;
               if child < last and then list (child) < list (child + 1) then
                  child := child + 1;
               end if;
               exit when list (parent) >= list (child);
               saved := list (parent);
               list (parent) := list (child);
               list (child) := saved;
               parent := child;
            end loop;
         end Sift;
         saved : Unsigned_64;
      begin
         for start in reverse 1 .. count / 2 loop
            Sift (start, count);
         end loop;
         for last in reverse 2 .. count loop
            saved := list (1);
            list (1) := list (last);
            list (last) := saved;
            Sift (1, last - 1);
         end loop;
      end Sort;

      procedure Flush_Run is
         result : Write_Status;
      begin
         if runBlocks > 0 and then ok then
            deviceWrite (fs, Storage_Offset (runStart) * Storage_Offset (blockBytes),
                         staging'Address,
                         Storage_Count (Unsigned_64 (runBlocks) * blockBytes), result);
            ok := result = Write_Complete;
         end if;
         runBlocks := 0;
      end Flush_Run;

      --  Queue one block for its physical home, coalescing consecutive ones.
      procedure Append (physical : Unsigned_32; source : System.Address) is
      begin
         if runBlocks > 0 and then
           (Unsigned_64 (physical) /= Unsigned_64 (runStart) + Unsigned_64 (runBlocks) or else
            Unsigned_64 (runBlocks + 1) * blockBytes > Maximum_Transfer_Bytes)
         then
            Flush_Run;
         end if;
         if runBlocks = 0 then
            runStart := physical;
         end if;
         declare
            from : String (1 .. Natural (blockBytes)) with Import, Address => source;
            to : String (1 .. Natural (blockBytes))
              with Import, Address => staging (runBlocks * Natural (blockBytes))'Address;
         begin
            to := from;
         end;
         runBlocks := runBlocks + 1;
      end Append;

      function Log_Home (logical : Unsigned_32) return Unsigned_32 is
         physical, leaf, middle : Unsigned_32;
         result : Read_Status;
      begin
         resolveBlock (fs, fs.journal.Journal_Inode, logical, physical, leaf, middle, result);
         if result /= Read_Complete or else physical = 0 then
            ok := False;
            return 0;
         end if;
         return physical;
      end Log_Home;

      procedure Put_Be32 (block : in out Jbd2_Format.Block; offset : Natural;
                          value : Unsigned_32) is
      begin
         block (offset) := Unsigned_8 (Shift_Right (value, 24));
         block (offset + 1) := Unsigned_8 (Shift_Right (value, 16) and 16#FF#);
         block (offset + 2) := Unsigned_8 (Shift_Right (value, 8) and 16#FF#);
         block (offset + 3) := Unsigned_8 (value and 16#FF#);
      end Put_Be32;

      procedure Put_Be16 (block : in out Jbd2_Format.Block; offset : Natural;
                          value : Unsigned_32) is
      begin
         block (offset) := Unsigned_8 (Shift_Right (value, 8) and 16#FF#);
         block (offset + 1) := Unsigned_8 (value and 16#FF#);
      end Put_Be16;

      procedure Header (block : in out Jbd2_Format.Block; kind, sequence : Unsigned_32) is
      begin
         block := [others => 0];
         Put_Be32 (block, 0, Jbd2_Format.Magic);
         Put_Be32 (block, 4, kind);
         Put_Be32 (block, 8, sequence);
      end Header;

      --  One whole transaction over meta (first .. last).
      function Next_Position (position : Unsigned_32) return Unsigned_32 is
        (if position + 1 >= fs.journal.Max_Length then fs.journal.First
         else position + 1);

      --  Move the log tail to the head: every committed transaction's home
      --  writes durable (a barrier if any is pending), then the journal
      --  superblock names the head and the next sequence, durably.
      procedure Move_Tail is
         super : Jbd2_Format.Block := [others => 0];
         result : Read_Status;
      begin
         if Home_Writes_Pending then
            barrier (fs, ok);
            if not ok then
               return;
            end if;
         end if;
         deviceRead (fs, Storage_Offset (fs.journal.Super_Home) *
                       Storage_Offset (blockBytes),
                     super'Address, Storage_Count (blockBytes), result);
         ok := result = Read_Complete;
         if not ok then
            return;
         end if;
         Put_Be32 (super, Journal_Sequence_Offset, fs.journal.Sequence);
         Put_Be32 (super, Journal_Start_Offset, fs.journal.Head);
         if checksums then
            Put_Be32 (super, Jbd2_Format.Superblock_Checksum_Offset, 0);
            Put_Be32 (super, Jbd2_Format.Superblock_Checksum_Offset,
                      Jbd2_Format.Crc32c (16#FFFF_FFFF#, super, 0,
                                          Jbd2_Format.Superblock_Bytes));
         end if;
         writeDurable (fs, fs.journal.Super_Home, super'Address, ok);
         if ok then
            fs.journal.Tail := fs.journal.Head;
            fs.journal.Tail_Sequence := fs.journal.Sequence;
            fs.journal.Live_Blocks := 0;
         end if;
      end Move_Tail;

      procedure Commit_Chunk (first, last : Positive) is
         sequence : constant Unsigned_32 := fs.journal.Sequence;
         items : constant Unsigned_32 := Unsigned_32 (last - first + 1);
         --  Copies, a descriptor per group of tags, the commit block.
         needed : constant Unsigned_32 := items +
           (items + Unsigned_32 (tagsPerDescriptor) - 1) /
             Unsigned_32 (tagsPerDescriptor) + 1;
         position : Unsigned_32;
         descriptor, copy, commit : Jbd2_Format.Block;
         runningSum : Unsigned_32 := 16#FFFF_FFFF#;
         index : Positive := first;
      begin
         --  Room at the head, or the tail moves (one block always stays
         --  free, so the head never meets the tail).
         if fs.journal.Live_Blocks + needed >= logBlocks then
            Move_Tail;
            if not ok then
               return;
            end if;
         end if;
         position := fs.journal.Head;
         --  (3) descriptors and copies, in log order.
         while index <= last and then ok loop
            declare
               groupLast : constant Positive :=
                 Positive'Min (last, index + tagsPerDescriptor - 1);
               offset : Natural := Jbd2_Format.Header_Bytes;
               descriptorPosition : constant Unsigned_32 := position;
            begin
               Header (descriptor, Jbd2_Format.Descriptor_Kind, sequence);
               for item in index .. groupLast loop
                  declare
                     slot : constant Buffer_Slot := Slot_Of (meta (item));
                     home : constant Unsigned_32 := Block_Of (meta (item));
                     flags : Unsigned_32 := 0;
                     tagChecksum : Unsigned_32 := 0;
                  begin
                     copy := [others => 0];
                     for b in 0 .. Natural (blockBytes) - 1 loop
                        copy (b) := Cache_Blocks (slot) (b);
                     end loop;
                     if Jbd2_Format.Be32 (copy, 0) = Jbd2_Format.Magic then
                        flags := flags or Jbd2_Format.Tag_Escape;
                        copy (0 .. 3) := [others => 0];
                     end if;
                     if item /= index then
                        flags := flags or Jbd2_Format.Tag_Same_UUID;
                     end if;
                     if item = groupLast then
                        flags := flags or Jbd2_Format.Tag_Last;
                     end if;
                     if checksums then
                        tagChecksum := Jbd2_Format.Crc32c
                          (Jbd2_Format.Crc32c_Be32 (seed, sequence), copy, 0, size);
                     end if;
                     Put_Be32 (descriptor, offset, home);
                     if v3 then
                        Put_Be32 (descriptor, offset + 4, flags);
                        Put_Be32 (descriptor, offset + 8, 0);
                        Put_Be32 (descriptor, offset + 12, tagChecksum);
                     else
                        Put_Be16 (descriptor, offset + 4, tagChecksum and 16#FFFF#);
                        Put_Be16 (descriptor, offset + 6, flags);
                        if wide then
                           Put_Be32 (descriptor, offset + 8, 0);
                        end if;
                     end if;
                     offset := offset + tagBytes;
                     if item = index then
                        for u in Jbd2_Format.UUID'Range loop
                           descriptor (offset + u) := fs.journal.Identity (u);
                        end loop;
                        offset := offset + Jbd2_Format.UUID_Bytes;
                     end if;
                     --  The copy follows the descriptor, in tag order.
                     position := Next_Position (position);
                     Append (Log_Home (position), copy'Address);
                  end;
               end loop;
               if checksums then
                  Put_Be32 (descriptor, size - Jbd2_Format.Tail_Bytes,
                            Jbd2_Format.Crc32c (seed, descriptor, 0, size));
               end if;
               --  The descriptor precedes its copies in the log. Its sum is
               --  jbd2's order too: descriptor, then each copy.
               if transactionSums then
                  runningSum := Jbd2_Format.Crc32_Be (runningSum, descriptor, 0, size);
                  for item in index .. groupLast loop
                     declare
                        slot : constant Buffer_Slot := Slot_Of (meta (item));
                     begin
                        copy := [others => 0];
                        for b in 0 .. Natural (blockBytes) - 1 loop
                           copy (b) := Cache_Blocks (slot) (b);
                        end loop;
                        if Jbd2_Format.Be32 (copy, 0) = Jbd2_Format.Magic then
                           copy (0 .. 3) := [others => 0];
                        end if;
                        runningSum := Jbd2_Format.Crc32_Be (runningSum, copy, 0, size);
                     end;
                  end loop;
               end if;
               Flush_Run;
               declare
                  home : constant Unsigned_32 := Log_Home (descriptorPosition);
                  result : Write_Status;
               begin
                  if ok then
                     deviceWrite (fs, Storage_Offset (home) * Storage_Offset (blockBytes),
                                  descriptor'Address, Storage_Count (blockBytes), result);
                     ok := result = Write_Complete;
                  end if;
               end;
               position := Next_Position (position);
               index := groupLast + 1;
            end;
         end loop;
         if not ok then
            return;
         end if;
         --  (4) the commit block, after its transaction is durable.
         barrier (fs, ok);
         if not ok then
            return;
         end if;
         Header (commit, Jbd2_Format.Commit_Kind, sequence);
         if transactionSums then
            commit (Jbd2_Format.Commit_Checksum_Type_Offset) :=
              Jbd2_Format.Checksum_Type_Crc32;
            commit (Jbd2_Format.Commit_Checksum_Size_Offset) :=
              Jbd2_Format.Crc32_Checksum_Bytes;
            Put_Be32 (commit, Jbd2_Format.Commit_Checksum_Offset, runningSum);
         elsif checksums then
            Put_Be32 (commit, Jbd2_Format.Commit_Checksum_Offset,
                      Jbd2_Format.Crc32c (seed, commit, 0, size));
         end if;
         writeDurable (fs, Log_Home (position), commit'Address, ok);
         if not ok then
            return;
         end if;
         --  The transaction is durable: it joins the live log.
         fs.journal.Head := Next_Position (position);
         fs.journal.Live_Blocks := fs.journal.Live_Blocks + needed;
         fs.journal.Sequence := sequence + 1;
         --  (5) checkpoint: metadata home, in block order, without waiting:
         --  the next barrier (the next commit's, or the tail's move) makes
         --  it durable. Until the tail moves, recovery replays it anyway.
         for item in first .. last loop
            Append (Block_Of (meta (item)), Cache_Blocks (Slot_Of (meta (item))).all'Address);
         end loop;
         Flush_Run;
         if not ok then
            return;
         end if;
         Home_Writes_Pending := True;
         for item in first .. last loop
            Clean (Slot_Of (meta (item)));
         end loop;
      end Commit_Chunk;
   begin
      status := Write_Complete;
      if fs.writeQuarantined then
         status := Write_Recovery_Required;
         return;
      end if;
      if fs.journal.Handle_Depth > 0 then
         Split_Commits := Split_Commits + 1;
      end if;
      --  Released blocks are freed inside the transaction being committed,
      --  with the detaches that released them; no running operation could
      --  allocate them before.
      releasing := fs.pendingCount > 0;
      if fs.pendingCount > 0 then
         fs.journal.Handle_Depth := fs.journal.Handle_Depth + 1;
         applyReleases (fs, status);
         fs.journal.Handle_Depth := fs.journal.Handle_Depth - 1;
         if status /= Write_Complete then
            fs.writeQuarantined := True;
            invalidateBlockCache;
            status := Write_Recovery_Required;
            return;
         end if;
      end if;
      for index in 0 .. Spill_Used - 1 loop
         if Spill_Keys (index).Volume = volume then
            metaCount := metaCount + 1;
            meta (metaCount) :=
              Shift_Left (Unsigned_64 (Spill_Keys (index).Block), slotBits) or
              Unsigned_64 (First_Spill_Slot + index);
         end if;
      end loop;
      for slot in Cache_Slot loop
         if Cache_Index.Used (slot) and then Cache_Index.Dirty (slot) and then
           Cache_Index.Keys (slot).Volume = volume
         then
            declare
               item : constant Unsigned_64 :=
                 Shift_Left (Unsigned_64 (Cache_Index.Keys (slot).Block), slotBits) or
                 Unsigned_64 (slot);
            begin
               if Cache_Index.Classes (slot) = File_Data then
                  dataCount := dataCount + 1;
                  data (dataCount) := item;
               else
                  metaCount := metaCount + 1;
                  meta (metaCount) := item;
               end if;
            end;
         end if;
      end loop;
      --  (1) ordered data, home first.
      Sort (data, dataCount);
      for item in 1 .. dataCount loop
         Append (Block_Of (data (item)), Cache_Blocks (Slot_Of (data (item))).all'Address);
      end loop;
      Flush_Run;
      if ok then
         for item in 1 .. dataCount loop
            Clean (Slot_Of (data (item)));
         end loop;
      end if;
      if ok and then metaCount > 0 then
         --  (2) The data a commit refers to must be durable before it: the
         --  barrier before each commit block (4) covers the data sent home
         --  above as well as the log copies, as jbd2's one pre-flush does.
         Sort (meta, metaCount);
         declare
            --  Blocks per transaction: copies + descriptors + commit fit the log.
            perTransaction : constant Positive := Positive
              (Unsigned_64'Max
                 (1, (Unsigned_64 (logBlocks) - 1) * Unsigned_64 (tagsPerDescriptor) /
                     Unsigned_64 (tagsPerDescriptor + 1)));
            first : Positive := 1;
         begin
            while first <= metaCount and then ok loop
               Commit_Chunk (first, Positive'Min (metaCount, first + perTransaction - 1));
               first := first + perTransaction;
            end loop;
         end;
      end if;
      --  Blocks this transaction freed may be reused as file data, which
      --  is not journaled: no live transaction may still hold an older
      --  copy of them for replay to write back (jbd2 uses revoke records).
      --  Such a commit moves the tail past everything, itself included.
      if ok and then metaCount > 0 and then releasing then
         Move_Tail;
      end if;
      --  A commit leaves everything submitted before it durable (data by
      --  its barrier, metadata in the log): a flush needs no other.
      Commit_Was_Barrier := ok and then metaCount > 0;
      if ok then
         fs.journal.Dirty_Metadata := 0;
         dropSpill (volume);
      else
         fs.writeQuarantined := True;
         invalidateBlockCache;
         status := Write_Recovery_Required;
      end if;
   end commitTransaction;

   procedure Flush (fs : in out Filesystem; status : out Flush_Status) is
      committed : Write_Status;
      ok : Boolean;
   begin
      if fs.writeQuarantined then
         status := Flush_Recovery_Required;
         return;
      end if;
      Commit_Was_Barrier := False;
      if fs.journal.Active then
         commitTransaction (fs, committed);
         if committed /= Write_Complete then
            status := Flush_IO_Error;
            return;
         end if;
      end if;
      if not Can_Persist (fs.device.description) then
         status := Flush_Unsupported;
         return;
      end if;
      --  Ext2 write paths have submitted everything else already: the
      --  device barrier makes it durable (unless a commit just did).
      if Commit_Was_Barrier then
         status := Flush_Complete;
         return;
      end if;
      barrier (fs, ok);
      status := (if ok then Flush_Complete else Flush_IO_Error);
   end Flush;

   --  Journaled volumes: write into the cache and mark each block dirty in
   --  its class; commitTransaction later sends file data home and metadata
   --  through the journal. A partially written block is read first. With
   --  every way of a set dirty, or the running transaction at half the log,
   --  the transaction commits first. Only an uncertain commit quarantines.
   procedure writeCached
     (fs     : in out Filesystem;
      offset : Storage_Offset;
      src    : System.Address;
      len    : Storage_Count;
      status : out Write_Status;
      class  : Block_Class)
   is
      blockBytes : constant Storage_Offset := Storage_Offset (fs.blkSize);
      position : Storage_Offset := offset;
      done : Storage_Offset := 0;
      slot : Cache_Slot;
      found : Boolean;
      readStatus : Read_Status;
   begin
      status := Write_Complete;
      if fs.writeQuarantined or else Is_Read_Only (fs.device.description) then
         status := Write_Read_Only;
         return;
      elsif offset < 0 then
         status := Write_Out_Of_Range;
         return;
      end if;
      while done < len loop
         declare
            number : constant Storage_Offset := position / blockBytes;
            within : constant Storage_Offset := position mod blockBytes;
            part : constant Storage_Offset :=
              Storage_Offset'Min (blockBytes - within, len - done);
            key : constant Block_Cache_Index.Block_Key :=
              cacheKey (fs, Unsigned_32 (number));
         begin
            if number >= Storage_Offset (fs.sb.blockCount) then
               status := Write_Out_Of_Range;
               return;
            end if;
            if Spill_Used > 0 and then spillSlot (key) >= 0 then
               --  Already spilled in this transaction: patch that copy.
               declare
                  source : String (1 .. Natural (part)) with Import, Address => src + done;
                  target : String (1 .. Natural (part))
                    with Import, Address =>
                      Cache_Blocks (spillSlot (key)) (Natural (within))'Address;
               begin
                  target := source;
               end;
               goto Next_Block;
            end if;
            Block_Cache_Index.Find (Cache_Index, key, found, slot);
            if not found then
               Block_Cache_Index.Claim (Cache_Index, key, found, slot);
               if not found and then fs.journal.Handle_Depth > 0 and then
                 class = File_Data
               then
                  --  Inside an operation, file data may go home at once
                  --  (data=ordered only needs it there before the commit).
                  deviceWrite (fs, position, src + done, Storage_Count (part), status);
                  if status /= Write_Complete then
                     return;
                  end if;
                  Pressure_Blocks := Pressure_Blocks + 1;
                  goto Next_Block;
               elsif not found and then fs.journal.Handle_Depth > 0 and then
                 Spill_Used < Spill_Capacity
               then
                  --  Inside an operation, metadata waits in the spill.
                  declare
                     spilled : constant Buffer_Slot := First_Spill_Slot + Spill_Used;
                  begin
                     if part < blockBytes then
                        deviceRead (fs, number * blockBytes,
                                    Cache_Blocks (spilled).all'Address,
                                    Storage_Count (blockBytes), readStatus);
                        if readStatus /= Read_Complete then
                           status := (if readStatus = Read_Out_Of_Range then
                                        Write_Out_Of_Range else Write_Device_Error);
                           return;
                        end if;
                     end if;
                     declare
                        source : String (1 .. Natural (part))
                          with Import, Address => src + done;
                        target : String (1 .. Natural (part))
                          with Import, Address =>
                            Cache_Blocks (spilled) (Natural (within))'Address;
                     begin
                        target := source;
                     end;
                     Spill_Keys (Spill_Used) := key;
                     Spill_Classes (Spill_Used) := class;
                     Spill_Used := Spill_Used + 1;
                     Pressure_Blocks := Pressure_Blocks + 1;
                     fs.journal.Dirty_Metadata := fs.journal.Dirty_Metadata + 1;
                  end;
                  goto Next_Block;
               elsif not found then
                  --  Memory pressure: commit, which cleans this volume.
                  commitTransaction (fs, status);
                  if status /= Write_Complete then
                     return;
                  end if;
                  Block_Cache_Index.Claim (Cache_Index, key, found, slot);
                  if not found then
                     --  Every way holds another journaled volume's dirty
                     --  blocks: never bypass this volume's journal.
                     fs.writeQuarantined := True;
                     status := Write_Recovery_Required;
                     return;
                  end if;
               end if;
               if part < blockBytes then
                  deviceRead (fs, number * blockBytes, Cache_Blocks (slot).all'Address,
                              Storage_Count (blockBytes), readStatus);
                  if readStatus /= Read_Complete then
                     Block_Cache_Index.Forget (Cache_Index, key);
                     status := (if readStatus = Read_Out_Of_Range then
                                  Write_Out_Of_Range else Write_Device_Error);
                     return;
                  end if;
               end if;
            end if;
            declare
               source : String (1 .. Natural (part)) with Import, Address => src + done;
               target : String (1 .. Natural (part))
                 with Import, Address => Cache_Blocks (slot) (Natural (within))'Address;
            begin
               target := source;
            end;
            if class /= File_Data and then not Cache_Index.Dirty (slot) then
               fs.journal.Dirty_Metadata := fs.journal.Dirty_Metadata + 1;
            end if;
            Block_Cache_Index.Mark_Dirty
              (Cache_Index, slot,
               --  A block keeps its metadata class once journaled in this
               --  transaction, even if file data is written into it later.
               (if Cache_Index.Dirty (slot) and then Cache_Index.Classes (slot) /= File_Data
                then Cache_Index.Classes (slot) else class));
            <<Next_Block>>
            position := position + part;
            done := done + part;
         end;
      end loop;
      --  Keep every transaction comfortably inside the log; an open
      --  operation commits at its end (stopHandle).
      if fs.journal.Handle_Depth = 0 and then
        Natural (fs.journal.Max_Length - fs.journal.First) / 2 <=
          fs.journal.Dirty_Metadata
      then
         commitTransaction (fs, status);
      end if;
   end writeCached;

   --  Operation credits: an upper bound on the distinct metadata blocks
   --  one operation dirties (JBD2 handle credits).
   --  An allocation run: inode, three pointer levels, a bitmap and a
   --  descriptor block per reserved group, the superblock.
   Write_Credits : constant := 1 + 3 + 2 * 4 + 1;
   --  create/mkdir: parent and new inode blocks, the name's directory block
   --  and a new one, inode bitmap and descriptor, two single-block
   --  allocations (bitmap + descriptor each), the superblock.
   Create_Credits : constant := 2 + 2 + 2 + 2 * 2 + 1;
   --  unlink/rmdir/inode release: directory block, inode and parent inode
   --  blocks, inode bitmap and descriptor, the superblock, and for rmdir
   --  a bitmap and descriptor per directory block.
   Remove_Credits : constant := 3 + 2 + 1 + 2 * 12;
   Rename_Credits : constant := 1;
   --  Orphan list: the superblock, the inode and its predecessor.
   Orphan_Credits : constant := 3;

   --  Open an operation. Outermost only: commit first unless its credits
   --  fit in half the log and in the spill, so nothing in it forces a
   --  commit before stopHandle. A failed commit quarantines the volume,
   --  which the operation's own writes then report.
   procedure startHandle (fs : in out Filesystem; credits : Natural) is
      committed : Write_Status;
   begin
      if not fs.journal.Active then
         return;
      end if;
      if fs.journal.Handle_Depth = 0 and then
        (fs.journal.Dirty_Metadata + credits >
           Natural (fs.journal.Max_Length - fs.journal.First) / 2 or else
         Spill_Used + credits > Spill_Capacity or else
         --  Room for one more batch of releases, which a full queue would
         --  otherwise commit in mid-operation.
         fs.pendingCount >
           Maximum_Pending_Releases - Sector_Accounting.Retired_Blocks'Last)
      then
         commitTransaction (fs, committed);
      end if;
      fs.journal.Handle_Depth := fs.journal.Handle_Depth + 1;
   end startHandle;

   procedure stopHandle (fs : in out Filesystem) is
      committed : Write_Status;
   begin
      if not fs.journal.Active or else fs.journal.Handle_Depth = 0 then
         return;
      end if;
      fs.journal.Handle_Depth := fs.journal.Handle_Depth - 1;
      if fs.journal.Handle_Depth = 0 and then
        Natural (fs.journal.Max_Length - fs.journal.First) / 2 <=
          fs.journal.Dirty_Metadata
      then
         commitTransaction (fs, committed);
      end if;
   end stopHandle;

   --  Write through the block cache: patch cached copies only after the
   --  device acknowledged; after any failure the device state of the range
   --  is uncertain, so drop the volume's cached blocks.
   procedure writeBytes
     (fs     : in out Filesystem;
      offset : Storage_Offset;
      src    : System.Address;
      len    : Storage_Count;
      status : out Write_Status;
      class  : Block_Class)
   is
   begin
      --  Journaled metadata goes through the cache and the journal, as do
      --  small file writes (written back by the next commit, data first).
      --  Large file writes go home at once, in the caller's transfers:
      --  data=ordered needs them there only before the commit (whose
      --  barrier covers them), and they would only churn the cache.
      if fs.journal.Active and then cacheable (fs) and then
        (class /= File_Data or else len < Direct_Data_Bytes)
      then
         writeCached (fs, offset, src, len, status, class);
         return;
      end if;
      deviceWrite (fs, offset, src, len, status);
      if status = Write_Complete then
         writeThrough (fs, offset, src, len, class);
      elsif fs.journal.Active then
         --  The cache holds uncommitted metadata: keep it, stop writing.
         fs.writeQuarantined := True;
      else
         forgetVolume (fs);
      end if;
   end writeBytes;

   --  Exact_Inode writes every field as given; Update_Existing_Inode keeps
   --  the on-disk dtime of an unlinked inode of a journaled volume, which
   --  is its ext3 orphan-list link: only the orphan-list code sets it, and
   --  the copies open handles hold may be older.
   type Inode_Write_Mode is (Update_Existing_Inode, Initialize_New_Inode, Exact_Inode);

   --  Existing inodes preserve their extended metadata. A newly reserved slot
   --  must be initialized in full before any directory entry can expose it.
   procedure writeInode
     (fs       : in out Filesystem;
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
         when Update_Existing_Inode | Exact_Inode =>
            declare
               written : Inode := ino;
               current : Inode;
            begin
               if mode = Update_Existing_Inode and then fs.journal.Active and then
                 ino.numHardLinks = 0
               then
                  readBytes (fs, inodeTableByteOffset, current'Address,
                             Inode'Size / 8, readStatus);
                  if readStatus /= Read_Complete then
                     status := Write_Device_Error;
                     return;
                  end if;
                  written.deletedTime := current.deletedTime;
               end if;
               writeBytes
                 (fs, inodeTableByteOffset, written'Address, Inode'Size / 8, status,
                  Inode_Table);
            end;
         when Initialize_New_Inode =>
            declare
               --  Admission bounds the power-of-two inode slot by blkSize.
               slot : String (1 .. Natural (inoSize)) := [others => Character'Val (0)]
                 with Alignment => Unsigned_32'Alignment;
               header : Inode with Import, Address => slot'Address;
            begin
               header := ino;
               writeBytes
                 (fs, inodeTableByteOffset, slot'Address, Storage_Count (inoSize), status,
                  Inode_Table);
            end;
      end case;
   end writeInode;


   --  Write the superblock back to disk
   procedure writeSuperblock
     (fs : in out Filesystem;
      status : out Write_Status)
   is
   begin
      writeBytes (fs, SUPERBLOCK_OFFSET, fs.sb'Address,
                  Superblock'Size / 8, status, Allocation_Metadata);
   end writeSuperblock;


   --  Write a block group descriptor back to disk
   procedure writeBGD
     (fs         : in out Filesystem;
      blockGroup : Unsigned_32;
      bgd        : BlockGroupDescriptor;
      status     : out Write_Status)
   is
      bgdtOffset : constant Storage_Offset :=
        Storage_Offset ((fs.sb.firstDataBlock + 1) * fs.blkSize) +
        Storage_Offset (blockGroup) * (BlockGroupDescriptor'Size / 8);
   begin
      writeBytes (fs, bgdtOffset, bgd'Address,
                  BlockGroupDescriptor'Size / 8, status, Allocation_Metadata);
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

   --  Blocks reserved in memory for one request batch, with each touched
   --  group's updated bitmap and descriptor. Nothing reaches the disk before
   --  commitReservation, so discarding an uncommitted plan needs no I/O.
   Maximum_Reserved_Groups : constant := 4;
   Maximum_Bitmap_Bytes : constant := 4096; -- one 4 KiB bitmap block
   --  Payload staged per batch: the per-volume transfer buffer's size.
   Maximum_Batch_Bytes : constant := 512 * 1024;
   Minimum_Block_Bytes : constant := 1024;
   Maximum_Batch_Data_Blocks : constant := Maximum_Batch_Bytes / Minimum_Block_Bytes;
   --  A batch stays under one leaf: at most a new leaf, middle and root.
   Maximum_Batch_Pointer_Blocks : constant := 3;
   Maximum_Reserved_Blocks : constant :=
     Maximum_Batch_Data_Blocks + Maximum_Batch_Pointer_Blocks;
   subtype Reserved_Count is Natural range 0 .. Maximum_Reserved_Blocks;
   subtype Reserved_Index is Positive range 1 .. Maximum_Reserved_Blocks;
   subtype Group_Change_Count is Natural range 0 .. Maximum_Reserved_Groups;
   subtype Group_Change_Index is Positive range 1 .. Maximum_Reserved_Groups;
   subtype Bitmap_Index is Natural range 0 .. Maximum_Bitmap_Bytes - 1;
   type Bitmap_Buffer is array (Bitmap_Index) of Unsigned_8 with Alignment => 8;
   type Group_Change is record
      Group : Unsigned_32 := 0;
      Descriptor : BlockGroupDescriptor;
      Bitmap : Bitmap_Buffer;
      Taken : Reserved_Count := 0;
   end record;
   type Group_Changes is array (Group_Change_Index) of Group_Change;
   type Reserved_Blocks is array (Reserved_Index) of Unsigned_32;
   type Reservation is record
      Blocks : Reserved_Blocks := [others => 0];
      Count : Reserved_Count := 0;
      Groups : Group_Changes;
      Group_Total : Group_Change_Count := 0;
   end record;

   function bitmapBytes (fs : Filesystem) return Unsigned_32 is
     (Unsigned_32'Min ((fs.sb.blocksPerBlockGroup + 7) / 8, Maximum_Bitmap_Bytes));

   --  Reserve up to wanted free blocks in memory, at least minimum or none,
   --  scanning from goal and then onward through the groups. Bitmap bits
   --  are relative to their group, not to firstDataBlock globally. Free-space
   --  counters advertising space that no bitmap bit backs are inconsistent
   --  metadata, not a normal no-space condition.
   procedure reserveBlocks
     (fs : Filesystem; goal : Unsigned_32; wanted, minimum : Reserved_Count;
      plan : out Reservation; status : out Write_Status)
   is
      readSize : constant Unsigned_32 := bitmapBytes (fs);
      blockBytes : constant Storage_Offset := Storage_Offset (fs.blkSize);
      allocatableBlocks, groupCount : Unsigned_32;
      startGroup, startBit : Unsigned_32 := 0;
      readStatus : Read_Status;
      sawAdvertisedSpace : Boolean := False;

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
      elsif wanted = 0 or else fs.sb.freeBlocks = 0 or else readSize = 0 then
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
      allocatableBlocks := fs.sb.blockCount - fs.sb.firstDataBlock;
      groupCount := 1 + (allocatableBlocks - 1) / fs.sb.blocksPerBlockGroup;
      declare
         start : constant Unsigned_32 :=
           (if goal /= 0 then goal else fs.allocationHint);
      begin
         if start >= fs.sb.firstDataBlock and then start < fs.sb.blockCount then
            startGroup := (start - fs.sb.firstDataBlock) / fs.sb.blocksPerBlockGroup;
            startBit := (start - fs.sb.firstDataBlock) mod fs.sb.blocksPerBlockGroup;
         end if;
      end;

      for step in 0 .. Unsigned_64 (groupCount) - 1 loop
         exit when plan.Count = wanted or else
           plan.Group_Total = Maximum_Reserved_Groups or else
           Unsigned_32 (plan.Count) = fs.sb.freeBlocks;
         declare
            group : constant Unsigned_32 := Unsigned_32
              ((Unsigned_64 (startGroup) + step) mod Unsigned_64 (groupCount));
            change : Group_Change renames plan.Groups (plan.Group_Total + 1);
            groupFirst, validBlocks, scanBits, bit : Unsigned_32;
         begin
            readBGD (fs, group, change.Descriptor, readStatus);
            if readStatus /= Read_Complete then
               Discard (if readStatus = Read_Out_Of_Range then Write_Out_Of_Range
                        else Write_Device_Error);
               return;
            end if;
            if change.Descriptor.numFreeBlocks /= 0 then
               sawAdvertisedSpace := True;
               groupFirst := fs.sb.firstDataBlock + group * fs.sb.blocksPerBlockGroup;
               validBlocks := Unsigned_32'Min
                 (fs.sb.blocksPerBlockGroup, fs.sb.blockCount - groupFirst);
               readBytes
                 (fs, Storage_Offset (change.Descriptor.blockBitmapAddr) * blockBytes,
                  change.Bitmap'Address, Storage_Count (readSize), readStatus);
               if readStatus /= Read_Complete then
                  Discard (if readStatus = Read_Out_Of_Range then Write_Out_Of_Range
                           else Write_Device_Error);
                  return;
               end if;
               scanBits := Unsigned_32'Min (validBlocks, readSize * 8);
               change.Group := group;
               change.Taken := 0;
               for scanned in 0 .. scanBits - 1 loop
                  exit when plan.Count = wanted or else
                    Unsigned_32 (change.Taken) =
                      Unsigned_32 (change.Descriptor.numFreeBlocks) or else
                    Unsigned_32 (plan.Count) = fs.sb.freeBlocks;
                  bit := (if step = 0 then (startBit + scanned) mod scanBits
                          else scanned);
                  declare
                     mask : constant Unsigned_8 :=
                       Shift_Left (Unsigned_8'(1), Natural (bit mod 8));
                     byte : Unsigned_8 renames change.Bitmap (Natural (bit / 8));
                  begin
                     if (byte and mask) = 0 then
                        byte := byte or mask;
                        plan.Count := plan.Count + 1;
                        plan.Blocks (plan.Count) := groupFirst + bit;
                        change.Taken := change.Taken + 1;
                     end if;
                  end;
               end loop;
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

   --  Publish a reservation: every touched bitmap, then every descriptor,
   --  then the superblock, each written once. Error replies do not prove a
   --  metadata write left storage unchanged: never attempt blind rollback or
   --  hand out an uncertain reservation.
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
           (fs, Storage_Offset (plan.Groups (index).Descriptor.blockBitmapAddr) *
              Storage_Offset (fs.blkSize),
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

   --  Allocate one free block. Returns a block only after all reservation
   --  metadata writes complete.
   procedure allocateBlock
     (fs       : in out Filesystem;
      blockNum : out Unsigned_32;
      status   : out Write_Status)
   is
      plan : Reservation;
   begin
      blockNum := 0;
      reserveBlocks (fs, 0, 1, 1, plan, status);
      if status /= Write_Complete then
         return;
      end if;
      commitReservation (fs, plan, status);
      if status = Write_Complete then
         blockNum := plan.Blocks (Reserved_Index'First);
      end if;
   end allocateBlock;

   type Release_List is array (Positive range <>) of Unsigned_32;

   --  Return blocks to their groups' free pools: each touched group's bitmap
   --  and descriptor are written once, then the superblock once. An out-of-
   --  range block, or one already free (a duplicate or corrupt free must not
   --  inflate allocator counts), is rejected before its group's writes. Any
   --  failed write quarantines further mutation.
   procedure releaseBlocks
     (fs : in out Filesystem; blocks : Release_List; status : out Write_Status)
   is
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
        (for some number of blocks =>
           number < fs.sb.firstDataBlock or else number >= fs.sb.blockCount)
      then
         return;
      end if;
      allocatableBlocks := fs.sb.blockCount - fs.sb.firstDataBlock;
      for first in blocks'Range loop
         if not done (first) then
            declare
               group : constant Unsigned_32 :=
                 (blocks (first) - fs.sb.firstDataBlock) / fs.sb.blocksPerBlockGroup;
               groupFirst : constant Unsigned_32 :=
                 fs.sb.firstDataBlock + group * fs.sb.blocksPerBlockGroup;
               validBlocks : constant Unsigned_32 := Unsigned_32'Min
                 (fs.sb.blocksPerBlockGroup, fs.sb.blockCount - groupFirst);
               cleared : Unsigned_32 := 0;
            begin
               readBGD (fs, group, bgd, readStatus);
               if readStatus /= Read_Complete then
                  status := Write_Device_Error;
                  return;
               end if;
               readBytes
                 (fs, Storage_Offset (bgd.blockBitmapAddr) * Storage_Offset (fs.blkSize),
                  bitmap'Address, Storage_Count (readSize), readStatus);
               if readStatus /= Read_Complete then
                  status := Write_Device_Error;
                  return;
               end if;
               for index in first .. blocks'Last loop
                  if not done (index) and then
                    (blocks (index) - fs.sb.firstDataBlock) / fs.sb.blocksPerBlockGroup = group
                  then
                     declare
                        bit : constant Unsigned_32 := blocks (index) - groupFirst;
                        mask : constant Unsigned_8 :=
                          Shift_Left (Unsigned_8'(1), Natural (bit mod 8));
                     begin
                        if bit / 8 >= readSize or else
                          (bitmap (Natural (bit / 8)) and mask) = 0
                        then
                           return;
                        end if;
                        bitmap (Natural (bit / 8)) := bitmap (Natural (bit / 8)) and not mask;
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
                 (fs, Storage_Offset (bgd.blockBitmapAddr) * Storage_Offset (fs.blkSize),
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

   --  Free the pending released blocks. The list is emptied first, so a
   --  commit the release itself might force does not free them twice.
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

   --  Release blocks a detach freed. Journaled: queued (see
   --  Pending_Blocks) for the commit of the running transaction; a full
   --  queue commits first. Write-through: after a flush makes the detach
   --  durable (a volatile device has nothing to order), at once.
   procedure deferRelease
     (fs : in out Filesystem; blocks : Release_List; status : out Write_Status)
   is
      flushed : Flush_Status;
   begin
      status := Write_Complete;
      if not fs.journal.Active then
         if not Is_Volatile (fs.device.description) then
            Flush (fs, flushed);
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

   --  Allocate a free inode from any block group.
   procedure allocateInode
     (fs       : in out Filesystem;
      inodeNum : out Unsigned_32;
      status   : out Write_Status;
      directory : Boolean := False)
   is
      bgd : BlockGroupDescriptor;
      updatedBGD : BlockGroupDescriptor;
      bitmapBuf : array (0 .. 4095) of Unsigned_8 with Alignment => 8;
      bitmapBytes : constant Unsigned_32 :=
        (fs.sb.inodesPerBlockGroup + 7) / 8;
      readSize : Unsigned_32;
      groupCount : Unsigned_32;
      group, firstByte, lastByte : Unsigned_32;
      --  Bitmap bytes read at a time while searching.
      BITMAP_CHUNK : constant := 64;
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

      --  From the hint's group and byte to the end of the bitmaps, then
      --  from the start around to the hint (pass groupCount: the hint's
      --  group below its byte). Bitmaps are read in chunks as searched.
      if fs.inodeHintGroup >= groupCount or else fs.inodeHintByte >= readSize then
         fs.inodeHintGroup := 0;
         fs.inodeHintByte := 0;
      end if;
      for pass in Unsigned_32 range 0 .. groupCount loop
         group := (fs.inodeHintGroup + pass) mod groupCount;
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
            groupFirst := group * fs.sb.inodesPerBlockGroup;
            validInodes := Unsigned_32'Min
              (fs.sb.inodesPerBlockGroup,
               fs.sb.inodeCount - groupFirst);

            for byteIdx in Natural (firstByte) .. Natural (lastByte) - 1 loop
               if byteIdx = Natural (firstByte) or else byteIdx mod BITMAP_CHUNK = 0 then
                  declare
                     chunkEnd : constant Natural := Natural'Min
                       ((byteIdx / BITMAP_CHUNK + 1) * BITMAP_CHUNK, Natural (lastByte));
                  begin
                     readBytes
                       (fs,
                        Storage_Offset (bgd.inodeBitmapAddr) *
                          Storage_Offset (fs.blkSize) + Storage_Offset (byteIdx),
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
                             Storage_Offset (fs.blkSize) + Storage_Offset (byteIdx),
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
                        fs.inodeHintGroup := group;
                        fs.inodeHintByte := Unsigned_32 (byteIdx);
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

   --  Allocate, attach and write a run of unmapped logical blocks starting at
   --  byte pos, all under one pointer leaf (or the inode's direct slots), for
   --  up to available bytes from source. The run's allocation metadata is
   --  published once; then the payload in coalesced transfers (fresh blocks
   --  are zero-filled around it, so no previously freed bytes can appear);
   --  then each modified pointer block, children before parents. Each block
   --  is thus on disk before any pointer to it, and allocation metadata
   --  before the pointers that reference its blocks. The caller publishes the
   --  inode. A failure before commit has written nothing; any failure after
   --  it quarantines the volume. consumed is zero unless status is Complete.
   procedure appendRun
     (fs : in out Filesystem; ino : in out Inode; pos : Unsigned_64;
      source : System.Address; available : Unsigned_64;
      consumed : out Unsigned_64; status : out Write_Status)
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
        (plan.Blocks (pointerCount + 1 + Natural (index)));
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
      reserveBlocks (fs, goal, pointerCount + Natural (runBlocks), pointerCount + 1,
                     plan, status);
      if status /= Write_Complete then
         return;
      end if;
      runBlocks := Unsigned_64 (plan.Count - pointerCount);
      declare
         next_Pointer : Reserved_Index := Reserved_Index'First;
      begin
         if newRoot then
            root := plan.Blocks (next_Pointer);
            next_Pointer := next_Pointer + 1;
         end if;
         if newMiddle then
            middle := plan.Blocks (next_Pointer);
            next_Pointer := next_Pointer + 1;
         end if;
         if newLeaf then
            leaf := plan.Blocks (next_Pointer);
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
               Storage_Count (length * blockBytes), status, File_Data);
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
                     readInode (fs, inodeNum, current, readStatus);
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

   procedure renameInner
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
            data'Address, Storage_Count (size), writeStatus, Directory_Data);
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
      forgetName (fs, dirInodeNum, oldName);
      forgetName (fs, dirInodeNum, newName);
      --  The new name is not cached: the directory is no longer complete.
      uncertainDirectory (fs, dirInodeNum);
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
   end renameInner;

   procedure renameEntry
     (fs : in out Filesystem; dirInodeNum : Unsigned_32;
      oldName, newName : String; status : out Rename_Status)
   is
   begin
      startHandle (fs, Rename_Credits);
      renameInner (fs, dirInodeNum, oldName, newName, status);
      stopHandle (fs);
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

   --  Create an empty regular file or directory. Prepare the directory
   --  insertion before reserving anything; publish the initialized inode
   --  (and a directory's "."/".." block before it) before its name. A new
   --  directory's parent gains its ".." link before anything refers to it
   --  (an over-count is harmless); a grown parent's new block is attached
   --  after it holds the name.
   procedure createNodeInner
     (fs : in out Filesystem; dirInodeNum : Unsigned_32; name : String;
      directory : Boolean; inodeNum : out Unsigned_32; status : out Write_Status)
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
      knownAbsent : Boolean := False;
      grownParent : Inode;
      accepted : Boolean;
      ownBlock : Unsigned_32 := 0;
      sectors : constant Unsigned_32 := fs.blkSize / 512;

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
      --  A cached negative entry already proves the name absent.
      if Dentry_Cache.Cacheable (name) then
         declare
            cached : Boolean;
            found : Unsigned_32;
         begin
            Dentry_Cache.Find
              (Names, Dentry_Cache.Make_Key (fs.device.endpointSlot, dirInodeNum, name),
               cached, found);
            knownAbsent := (cached and then found = Dentry_Cache.No_Inode) or else
              (not cached and then completeDirectory (fs, dirInodeNum));
         end;
      end if;
      forgetName (fs, dirInodeNum, name);
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
      elsif directory and then
        (parent.numHardLinks >= Maximum_Links or else
         fs.blkSize < Directory_Blocks.Minimum_Directory_Block)
      then
         status := Write_File_Range_Unsupported;
         return;
      end if;
      if knownAbsent then
         lookupStatus := Lookup_Not_Found;
      else
         lookupInDir (fs, parent, name, existing, lookupStatus);
      end if;
      if lookupStatus = Lookup_Found then
         status := Write_Already_Exists;
         return;
      elsif lookupStatus /= Lookup_Not_Found then
         status := Write_Device_Error;
         return;
      end if;

      parentBlocks := Natural (parent.sizeLo / fs.blkSize);
      --  Last block first: names are appended, so free space is usually
      --  at the end (any block with room will do).
      for index in reverse 0 .. parentBlocks - 1 loop
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

      if directory then
         allocateBlock (fs, ownBlock, status);
         if status /= Write_Complete then
            if grow and then status = Write_No_Space then
               releaseBlocks (fs, [1 => targetBlock], cleanupStatus);
               if cleanupStatus /= Write_Complete then
                  Uncertain;
               end if;
            elsif grow then
               Uncertain;
            end if;
            return;
         end if;
      end if;

      allocateInode (fs, reservedInode, status, directory);
      if status /= Write_Complete then
         if status = Write_No_Space and then (grow or else directory) then
            --  These blocks were never published. Only a definite no-space
            --  rejection permits cleanup; transport uncertainty must stop.
            if grow and then directory then
               releaseBlocks (fs, [targetBlock, ownBlock], cleanupStatus);
            elsif grow then
               releaseBlocks (fs, [1 => targetBlock], cleanupStatus);
            else
               releaseBlocks (fs, [1 => ownBlock], cleanupStatus);
            end if;
            if cleanupStatus /= Write_Complete then
               Uncertain;
            end if;
         elsif grow or else directory then
            Uncertain;
         end if;
         return;
      end if;

      if directory then
         --  The parent's ".." link first: until the name exists it is an
         --  over-count, which is harmless.
         parent.numHardLinks := parent.numHardLinks + 1;
         grownParent.numHardLinks := parent.numHardLinks;
         writeInode (fs, dirInodeNum, parent, status);
         if status /= Write_Complete then
            Uncertain;
            return;
         end if;
         declare
            contents : Directory_Blocks.Block_Data;
         begin
            Directory_Blocks.Initial_Block
              (contents, Directory_Blocks.Block_Length (fs.blkSize),
               reservedInode, dirInodeNum, FILETYPE_DIRECTORY);
            writeBytes
              (fs, Storage_Offset (ownBlock) * Storage_Offset (fs.blkSize),
               contents'Address, Storage_Count (fs.blkSize), status,
               Directory_Data);
         end;
         if status /= Write_Complete then
            Uncertain;
            return;
         end if;
         fresh.typeAndPermissions := Directory_Mode;
         fresh.numHardLinks := 2; -- its name and its own "."
         fresh.sizeLo := fs.blkSize;
         fresh.directBlocks (0) := ownBlock;
         fresh.numDiskSectors := sectors;
      else
         fresh.typeAndPermissions := Regular_Mode;
         fresh.numHardLinks := 1;
      end if;
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
                   fileType => (if directory then FILETYPE_DIRECTORY
                                else FILETYPE_REGULAR));
         entryName := name;
      end;
      writeBytes
        (fs, Storage_Offset (targetBlock) * Storage_Offset (fs.blkSize),
         buffer'Address, Storage_Count (fs.blkSize), status, Directory_Data);
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
      rememberName (fs, dirInodeNum, name, reservedInode);
   end createNodeInner;

   procedure createNode
     (fs : in out Filesystem; dirInodeNum : Unsigned_32; name : String;
      directory : Boolean; inodeNum : out Unsigned_32; status : out Write_Status)
   is
   begin
      startHandle (fs, Create_Credits);
      createNodeInner (fs, dirInodeNum, name, directory, inodeNum, status);
      stopHandle (fs);
      if status /= Write_Complete then
         uncertainDirectory (fs, dirInodeNum);
      end if;
   end createNode;

   procedure createFile
     (fs : in out Filesystem; dirInodeNum : Unsigned_32; name : String;
      inodeNum : out Unsigned_32; status : out Write_Status)
   is
   begin
      createNode (fs, dirInodeNum, name, False, inodeNum, status);
   end createFile;

   procedure makeDirectory
     (fs : in out Filesystem; dirInodeNum : Unsigned_32; name : String;
      inodeNum : out Unsigned_32; status : out Write_Status)
   is
   begin
      createNode (fs, dirInodeNum, name, True, inodeNum, status);
   end makeDirectory;

   --  Validate ownership within this inode BEFORE any mutation. The inventory
   --  is sized by an admitted allocation count, not by logical file size.
   --  Sorting is O(n log n), replacing the old quadratic duplicate walk.
   --  Allocations beyond the scratch inventory capacity (only reachable with
   --  triple-indirect trees) are rejected as unsupported, never partially
   --  checked.
   procedure validateBlockTree
     (fs : Filesystem; ino : Inode; status : out Truncate_Status)
   is
      sectors : constant Unsigned_32 := fs.blkSize / 512;
      ptrCount : constant Unsigned_32 := fs.blkSize / 4;
      claimed : constant Unsigned_32 := ino.numDiskSectors / sectors;
      --  Data blocks plus the single, double (root + leaves) and triple
      --  (root + middles + leaves) pointer blocks of a complete tree.
      pointerBlockLimit : constant Unsigned_64 :=
        1 + (1 + Unsigned_64 (ptrCount)) +
        (1 + Unsigned_64 (ptrCount) + Unsigned_64 (ptrCount) * Unsigned_64 (ptrCount));
      geometryLimit : constant Unsigned_64 :=
        Block_Paths.Block_Limit (Sector_Accounting.Block_Sectors (sectors)) +
          pointerBlockLimit;
   begin
      status := Truncate_Invalid;
      if ino.numDiskSectors mod sectors /= 0 or else
        Unsigned_64 (claimed) > geometryLimit or else claimed > fs.sb.blockCount
      then
         return;
      elsif Unsigned_64 (claimed) > Block_Inventory.Maximum_Blocks then
         status := Truncate_Unsupported;
         return;
      end if;
      declare
         blocks : Block_Inventory.Block_Array (1 .. Natural (claimed));
         count : Block_Inventory.Block_Count := 0;
         top, middle, root, leaf : Pointer_Block;
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
         if ino.tripleIndirectBlock /= 0 then
            Load (ino.tripleIndirectBlock, top);
            if not valid then return; end if;
            for I in 0 .. Natural (ptrCount) - 1 loop
               if top (I) /= 0 then
                  Load (top (I), middle);
                  if not valid then return; end if;
                  for J in 0 .. Natural (ptrCount) - 1 loop
                     if middle (J) /= 0 then
                        Load (middle (J), leaf);
                        if not valid then return; end if;
                        Data_Pointers (leaf);
                        if not valid then return; end if;
                     end if;
                  end loop;
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
   --  Metadata one resize batch dirties: three pointer levels, the inode,
   --  the superblock, a bitmap and a descriptor per group its blocks lie in.
   function resizeCredits (fs : Filesystem) return Natural is
     (5 + 2 * Natural (Unsigned_32'Min
        (Unsigned_32 (Sector_Accounting.Retired_Blocks'Last),
         (if fs.sb.blocksPerBlockGroup = 0 then 1
          else 1 + (fs.sb.blockCount - fs.sb.firstDataBlock) /
                     fs.sb.blocksPerBlockGroup))));

   procedure resizeInner
     (fs : in out Filesystem; inodeNum : Unsigned_32;
      newSize : Unsigned_64; resizedInode : out Inode;
      status : out Truncate_Status)
   is
      ino : Inode;
      readStatus : Read_Status;
      writeStatus : Write_Status;
      flushStatus : Flush_Status;
      top, middle, root, leaf : Pointer_Block := [others => 0];
      --  One batch retires at most one leaf's data plus that leaf and its
      --  emptied middle and triple root: P + 3 <= Retired_Blocks'Last.
      retired : Release_List (1 .. Sector_Accounting.Retired_Blocks'Last);
      count : Sector_Accounting.Retired_Blocks := 0;
      keepBlocks : Unsigned_64;
      mutated : Boolean := False;
      failed : Boolean := False;
      ptrCount : Unsigned_32;
      type Leaf_Parent is (Single_Parent, Double_Parent, Triple_Parent);

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

      procedure Publish
        (number : Unsigned_32; contents : Pointer_Block; class : Block_Class) is
      begin
         mutated := True;
         writeBytes (fs, Storage_Offset (number) * Storage_Offset (fs.blkSize),
                     contents'Address, Storage_Count (fs.blkSize), writeStatus, class);
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
         mutated := True;
         if fs.journal.Active then
            --  Journaled: the release is applied by the commit of this
            --  batch's transaction; the size shrinks (the blocks beyond it
            --  are already gone) only in the final batch.
            writeInode (fs, inodeNum, ino, writeStatus);
            if writeStatus = Write_Complete and then count > 0 then
               deferRelease (fs, retired (1 .. count), writeStatus);
            end if;
            if writeStatus /= Write_Complete then
               Fail (Truncate_IO_Error);
               return;
            end if;
            invalidateBlockCache;
            count := 0;
            --  A batch is one operation: a commit may follow it, never
            --  split it (the file then has holes up to its old size).
            stopHandle (fs);
            startHandle (fs, resizeCredits (fs));
            return;
         end if;
         ino.sizeLo := Unsigned_32 (newSize and 16#FFFF_FFFF#);
         ino.sizeHi_DirACL := Unsigned_32 (Shift_Right (newSize, 32));
         writeInode (fs, inodeNum, ino, writeStatus);
         if writeStatus /= Write_Complete then
            Fail (Truncate_IO_Error);
            return;
         end if;
         invalidateBlockCache;
         --  Freed once the inode and the surviving parent pointers are
         --  durably detached.
         if count > 0 then
            deferRelease (fs, retired (1 .. count), writeStatus);
            if writeStatus /= Write_Complete then
               Fail (Truncate_IO_Error);
               return;
            end if;
         end if;
         count := 0;
      end Commit_Batch;

      function Empty (contents : Pointer_Block) return Boolean is
        (for all I in 0 .. Natural (ptrCount) - 1 => contents (I) = 0);

      --  Remove triple root slot topSlot, whose middle block is now empty,
      --  retiring the root too when that was its last middle.
      procedure Detach_Middle (topSlot : Natural) is
      begin
         Retire (top (topSlot));
         top (topSlot) := 0;
         if Empty (top) then
            Retire (ino.tripleIndirectBlock);
            ino.tripleIndirectBlock := 0;
         else
            Publish (ino.tripleIndirectBlock, top, Root_Pointers);
         end if;
      end Detach_Middle;

      procedure Trim_Leaf
        (number : Unsigned_32; firstLogical : Unsigned_64;
         parent : Leaf_Parent; outerSlot, middleSlot : Natural := 0)
      is
         remains, changed : Boolean := False;
      begin
         --  Validation already read every pointer block. A leaf lying wholly
         --  below the retained extent needs neither I/O nor publication.
         if firstLogical + Unsigned_64 (ptrCount) <= keepBlocks then return; end if;
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
            Publish (number, leaf, Leaf_Pointers);
         else
            Retire (number);
            --  Publish only the nearest surviving ancestor; emptied parents
            --  are retired in the same batch after the inode is flushed.
            case parent is
               when Single_Parent =>
                  ino.singleIndirectBlock := 0;
               when Double_Parent =>
                  root (outerSlot) := 0;
                  if Empty (root) then
                     Retire (ino.doubleIndirectBlock);
                     ino.doubleIndirectBlock := 0;
                  else
                     Publish (ino.doubleIndirectBlock, root, Middle_Pointers);
                  end if;
               when Triple_Parent =>
                  middle (middleSlot) := 0;
                  if Empty (middle) then
                     Detach_Middle (outerSlot);
                  else
                     Publish (top (outerSlot), middle, Middle_Pointers);
                  end if;
            end case;
         end if;
         if failed then return; end if;
         Commit_Batch;
      end Trim_Leaf;

      --  Trim one middle block's leaves from the highest logical block down.
      --  A middle wholly below the retained extent is skipped without I/O.
      procedure Trim_Middle (topSlot : Natural) is
         sectors : constant Sector_Accounting.Block_Sectors :=
           Sector_Accounting.Block_Sectors (fs.blkSize / 512);
         firstLogical : constant Unsigned_64 :=
           Block_Paths.First_Triple (sectors) +
             Unsigned_64 (topSlot) * Block_Paths.Middle_Span (sectors);
      begin
         if firstLogical + Block_Paths.Middle_Span (sectors) <= keepBlocks then
            return;
         end if;
         Load (top (topSlot), middle);
         if failed then return; end if;
         for J in reverse 0 .. Natural (ptrCount) - 1 loop
            if middle (J) /= 0 then
               Trim_Leaf (middle (J),
                 firstLogical + Unsigned_64 (J) * Unsigned_64 (ptrCount),
                 Triple_Parent, topSlot, J);
               if failed then return; end if;
               --  The last leaf's removal also retired this middle block.
               exit when top (topSlot) = 0;
            end if;
         end loop;
         --  Also retire a valid but initially empty middle block.
         if top (topSlot) /= 0 and then Empty (middle) then
            Detach_Middle (topSlot);
            if failed then return; end if;
            Commit_Batch;
         end if;
      end Trim_Middle;
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
        not Can_Persist (fs.device.description)
      then
         status := Truncate_Durability_Unsupported;
         return;
      end if;
      readInode (fs, inodeNum, ino, readStatus);
      if readStatus /= Read_Complete then
         status := Truncate_IO_Error;
         return;
      elsif Ext2_Support.Check_File (ino, Unlinked_Allowed => True) /= Ext2_Support.File_Allowed or else
        ino.fileACL /= 0 or else fs.blkSize not in 1024 | 2048 | 4096
      then
         status := Truncate_Unsupported;
         return;
      end if;
      ptrCount := fs.blkSize / 4;
      if newSize > Unsigned_64 (fs.blkSize) *
        Block_Paths.Block_Limit (Sector_Accounting.Block_Sectors (fs.blkSize / 512)) or else
        not Ext2_Support.Size_Admitted (newSize, fs.sb.readOnlyFeatures)
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
         --  Highest logical blocks first: triple, double, single, direct.
         if ino.tripleIndirectBlock /= 0 then
            Load (ino.tripleIndirectBlock, top);
            if failed then return; end if;
            for I in reverse 0 .. Natural (ptrCount) - 1 loop
               if top (I) /= 0 then
                  Trim_Middle (I);
                  if failed then return; end if;
               end if;
               exit when ino.tripleIndirectBlock = 0;
            end loop;
            --  Also retire a valid but initially empty triple root.
            if ino.tripleIndirectBlock /= 0 and then Empty (top) then
               Retire (ino.tripleIndirectBlock);
               ino.tripleIndirectBlock := 0;
               Commit_Batch;
               if failed then return; end if;
            end if;
         end if;
         if ino.doubleIndirectBlock /= 0 then
            Load (ino.doubleIndirectBlock, root);
            if failed then return; end if;
            for I in reverse 0 .. Natural (ptrCount) - 1 loop
               if root (I) /= 0 then
                  Trim_Leaf (root (I),
                    Unsigned_64 (NUM_DIRECT_BLOCKS) + Unsigned_64 (ptrCount) +
                      Unsigned_64 (I) * Unsigned_64 (ptrCount), Double_Parent, I);
                  if failed then return; end if;
               end if;
            end loop;
            --  Also retire a valid but initially empty double root.
            if ino.doubleIndirectBlock /= 0 and then Empty (root) then
               Retire (ino.doubleIndirectBlock);
               ino.doubleIndirectBlock := 0;
               Commit_Batch;
               if failed then return; end if;
            end if;
         end if;
         if ino.singleIndirectBlock /= 0 then
            Trim_Leaf (ino.singleIndirectBlock, NUM_DIRECT_BLOCKS, Single_Parent);
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
      if not failed and then fs.journal.Active then
         --  The new size, then one commit for the whole resize.
         ino.sizeLo := Unsigned_32 (newSize and 16#FFFF_FFFF#);
         ino.sizeHi_DirACL := Unsigned_32 (Shift_Right (newSize, 32));
         mutated := True;
         writeInode (fs, inodeNum, ino, writeStatus);
         if writeStatus /= Write_Complete then
            Fail (Truncate_IO_Error);
         end if;
      end if;
      if not failed then
         resizedInode := ino;
         status := Truncate_Complete;
      end if;
   end resizeInner;

   procedure resizeInode
     (fs : in out Filesystem; inodeNum : Unsigned_32;
      newSize : Unsigned_64; resizedInode : out Inode;
      status : out Truncate_Status)
   is
   begin
      startHandle (fs, resizeCredits (fs));
      resizeInner (fs, inodeNum, newSize, resizedInode, status);
      stopHandle (fs);
   end resizeInode;

   procedure resizeFile
     (fs : in out Filesystem; inodeNum : Unsigned_32;
      newSize : Unsigned_64; resizedInode : out Inode;
      status : out Truncate_Status)
   is
   begin
      resizeInode (fs, inodeNum, newSize, resizedInode, status);
   end resizeFile;

   --  OPEN_TRUNCATE uses the same mutation implementation as general resize.
   procedure truncateToEmpty
     (fs : in out Filesystem; inodeNum : Unsigned_32;
      emptyInode : out Inode; status : out Truncate_Status)
   is
   begin
      resizeFile (fs, inodeNum, 0, emptyInode, status);
   end truncateToEmpty;

   ---------------------------------------------------------------------------
   --  Name removal and directory creation by path
   ---------------------------------------------------------------------------

   --  The parent directory of path and where its leaf name starts.
   procedure resolveParent
     (fs : Filesystem; path : String; parent : out Unsigned_32;
      leafFirst : out Positive; status : out Directory_Lookup_Status)
   is
   begin
      parent := ROOT_INODE;
      leafFirst := path'First;
      status := Lookup_Found;
      for index in reverse path'Range loop
         if path (index) = '/' then
            leafFirst := index + 1;
            exit;
         end if;
      end loop;
      if leafFirst > path'First + 1 then
         resolvePath (fs, path (path'First .. leafFirst - 2), parent, status);
      end if;
      if status /= Lookup_Found then
         parent := 0;
      end if;
   end resolveParent;

   --  A directory whose records this service may rewrite: unindexed, whole
   --  blocks, direct pointers only.
   function plainDirectory (fs : Filesystem; dir : Inode) return Boolean is
     (inodeType (dir) = INODE_DIRECTORY and then dir.flags = 0 and then
      dir.sizeLo /= 0 and then dir.sizeLo mod fs.blkSize = 0 and then
      Unsigned_64 (dir.sizeLo) <=
        Unsigned_64 (NUM_DIRECT_BLOCKS) * Unsigned_64 (fs.blkSize) and then
      dir.singleIndirectBlock = 0 and then dir.doubleIndirectBlock = 0 and then
      dir.tripleIndirectBlock = 0);

   function removeLookupFailure
     (status : Directory_Lookup_Status) return Remove_Status is
     (case status is
        when Lookup_Malformed => Remove_Malformed,
        when Lookup_Device_Error => Remove_IO_Error,
        when Lookup_Out_Of_Range => Remove_Out_Of_Range,
        when Lookup_Range_Unsupported => Remove_Unsupported,
        when others => Remove_Not_Found);

   function removeReadFailure (status : Read_Status) return Remove_Status is
     (if status = Read_Out_Of_Range then Remove_Out_Of_Range
      else Remove_IO_Error);

   --  The dtime of a freed inode: ext2 requires it nonzero. Without a clock
   --  the volume's last write time serves.
   function deletionTime (fs : Filesystem) return Unsigned_32 is
     (Unsigned_32'Max (1, fs.sb.lastWriteTime));

   --  A file with no data, no blocks and no block pointers.
   function emptyFile (ino : Inode) return Boolean is
     (ino.sizeLo = 0 and then ino.sizeHi_DirACL = 0 and then
      ino.numDiskSectors = 0 and then
      (for all b of ino.directBlocks => b = 0) and then
      ino.singleIndirectBlock = 0 and then ino.doubleIndirectBlock = 0 and then
      ino.tripleIndirectBlock = 0);

   --  Remove name's record (which must name expected) from a plain
   --  directory: one block rewritten, restored if that write fails.
   procedure removeEntry
     (fs : in out Filesystem; dir : Inode; name : String;
      expected : Unsigned_32; status : out Remove_Status)
   is
      original : Directory_Blocks.Block_Data := [others => 0];
      candidate : Directory_Blocks.Block_Data;
      prepared : Directory_Blocks.Prepare_Result;
      removed : Unsigned_32;
      kind : Unsigned_8;
      blockNumber : Unsigned_32 := 0;
      readStatus : Read_Status;

      procedure Write_Block
        (data : Directory_Blocks.Block_Data;
         size : Directory_Blocks.Block_Length; success : out Boolean)
      is
         writeStatus : Write_Status;
      begin
         writeBytes
           (fs, Storage_Offset (blockNumber) * Storage_Offset (fs.blkSize),
            data'Address, Storage_Count (size), writeStatus, Directory_Data);
         success := writeStatus = Write_Complete;
      end Write_Block;
      package Committer is new Directory_Commit (Write_Block);
      result : Committer.Commit_Result;
      --  The block holding the name, if the cached scan finds it: only it
      --  is prepared (the others hold no such record). Anything else scans
      --  every block, fully checked.
      found : Unsigned_32;
      holder : Natural;
      lookup : Directory_Lookup_Status;
      clean : Boolean;
      first, last : Natural;
   begin
      first := 0;
      last := Natural (dir.sizeLo / fs.blkSize);
      scanCachedDirectory (fs, dir, name, found, holder, lookup, clean);
      if clean and then lookup = Lookup_Found and then found = expected and then
        holder < last
      then
         --  The hot path: that block alone, the record removed in place
         --  (proved: at most four bytes change), and only they written.
         blockNumber := dir.directBlocks (holder);
         if blockNumber = 0 then
            status := Remove_Malformed;
            return;
         end if;
         readBlock (fs, blockNumber, original'Address, readStatus);
         if readStatus /= Read_Complete then
            status := removeReadFailure (readStatus);
            return;
         end if;
         candidate := original;
         declare
            changedFirst, changedLast : Positive;
            writeStatus : Write_Status;
            blockAt : constant Storage_Offset :=
              Storage_Offset (blockNumber) * Storage_Offset (fs.blkSize);
         begin
            Directory_Blocks.Remove_In_Place
              (candidate, Directory_Blocks.Block_Length (fs.blkSize),
               fs.sb.inodeCount, name, removed, kind, changedFirst, changedLast,
               prepared);
            if prepared = Directory_Blocks.Prepared then
               if removed /= expected then
                  status := Remove_Malformed;
                  return;
               end if;
               writeBytes
                 (fs, blockAt + Storage_Offset (changedFirst - 1),
                  candidate (changedFirst)'Address,
                  Storage_Count (changedLast - changedFirst + 1), writeStatus,
                  Directory_Data);
               if writeStatus = Write_Complete then
                  status := Remove_Complete;
                  return;
               end if;
               writeBytes
                 (fs, blockAt + Storage_Offset (changedFirst - 1),
                  original (changedFirst)'Address,
                  Storage_Count (changedLast - changedFirst + 1), writeStatus,
                  Directory_Data);
               if writeStatus = Write_Complete then
                  status := Remove_IO_Error;
               else
                  fs.writeQuarantined := True;
                  status := Remove_Recovery_Required;
               end if;
               return;
            end if;
         end;
         --  Anything else: every block, fully checked, below.
      end if;
      for index in first .. last - 1 loop
         blockNumber := dir.directBlocks (index);
         if blockNumber = 0 then
            status := Remove_Malformed;
            return;
         end if;
         readBlock (fs, blockNumber, original'Address, readStatus);
         if readStatus /= Read_Complete then
            status := removeReadFailure (readStatus);
            return;
         end if;
         candidate := original;
         Directory_Blocks.Prepare_Remove
           (candidate, Directory_Blocks.Block_Length (fs.blkSize),
            fs.sb.inodeCount, name, removed, kind, prepared);
         case prepared is
            when Directory_Blocks.Prepared =>
               if removed /= expected then
                  status := Remove_Malformed;
                  return;
               end if;
               Committer.Commit
                 (original, candidate,
                  Directory_Blocks.Block_Length (fs.blkSize), result);
               case result is
                  when Committer.Committed => status := Remove_Complete;
                  when Committer.Original_Restored => status := Remove_IO_Error;
                  when Committer.Recovery_Required =>
                     fs.writeQuarantined := True;
                     status := Remove_Recovery_Required;
               end case;
               return;
            when Directory_Blocks.Source_Not_Found => null;
            when Directory_Blocks.Invalid_Name =>
               status := Remove_Invalid_Name;
               return;
            when others =>
               status := Remove_Malformed;
               return;
         end case;
      end loop;
      status := Remove_Not_Found;
   end removeEntry;

   --  Return an inode to its group's bitmap. Counts are checked before the
   --  first write; a failed write quarantines the volume.
   procedure freeInode
     (fs : in out Filesystem; inodeNum : Unsigned_32; directory : Boolean;
      status : out Write_Status)
   is
      bitmapBuf : array (0 .. 4095) of Unsigned_8 with Alignment => 8;
      readSize : constant Unsigned_32 := Unsigned_32'Min
        ((fs.sb.inodesPerBlockGroup + 7) / 8, Unsigned_32 (bitmapBuf'Length));
      group, index : Unsigned_32;
      bgd : BlockGroupDescriptor;
      readStatus : Read_Status;
      mask : Unsigned_8;
   begin
      status := Write_Device_Error;
      if fs.sb.inodesPerBlockGroup = 0 or else inodeNum = 0 or else
        inodeNum > fs.sb.inodeCount or else fs.sb.freeInodes >= fs.sb.inodeCount
      then
         return;
      end if;
      group := (inodeNum - 1) / fs.sb.inodesPerBlockGroup;
      index := (inodeNum - 1) mod fs.sb.inodesPerBlockGroup;
      if index / 8 >= readSize then
         status := Write_Out_Of_Range;
         return;
      end if;
      readBGD (fs, group, bgd, readStatus);
      if readStatus /= Read_Complete then
         return;
      end if;
      --  Only the inode's bitmap byte is read and written.
      readBytes
        (fs, Storage_Offset (bgd.inodeBitmapAddr) * Storage_Offset (fs.blkSize) +
           Storage_Offset (index / 8),
         bitmapBuf (Natural (index / 8))'Address, 1, readStatus);
      mask := Shift_Left (Unsigned_8'(1), Natural (index mod 8));
      if readStatus /= Read_Complete or else
        (bitmapBuf (Natural (index / 8)) and mask) = 0 or else
        Unsigned_32 (bgd.numFreeInodes) >= fs.sb.inodesPerBlockGroup or else
        (directory and then bgd.numDirectories = 0)
      then
         return;
      end if;
      bitmapBuf (Natural (index / 8)) := bitmapBuf (Natural (index / 8)) and not mask;
      writeBytes
        (fs, Storage_Offset (bgd.inodeBitmapAddr) * Storage_Offset (fs.blkSize) +
           Storage_Offset (index / 8),
         bitmapBuf (Natural (index / 8))'Address, 1, status, Allocation_Metadata);
      if status = Write_Complete then
         bgd.numFreeInodes := bgd.numFreeInodes + 1;
         if directory then
            bgd.numDirectories := bgd.numDirectories - 1;
         end if;
         writeBGD (fs, group, bgd, status);
      end if;
      if status = Write_Complete then
         fs.sb.freeInodes := fs.sb.freeInodes + 1;
         writeSuperblock (fs, status);
      end if;
      if status /= Write_Complete then
         fs.writeQuarantined := True;
         status := Write_Recovery_Required;
      end if;
   end freeInode;

   --  Common guards of the removal operations.
   function removeRefusal (fs : Filesystem) return Remove_Status is
     (if fs.writeQuarantined then Remove_Recovery_Required
      elsif Is_Read_Only (fs.device.description) then Remove_Read_Only
      elsif not Is_Volatile (fs.device.description) and then
        not Can_Persist (fs.device.description)
      then Remove_Durability_Unsupported
      else Remove_Complete);

   ---------------------------------------------------------------------------
   --  The ext3 orphan list (journaled volumes): unlinked inodes still to be
   --  freed, from the superblock's s_last_orphan through each inode's dtime,
   --  as Linux keeps it. An inode unlinked while open joins it with its
   --  unlink and leaves it with its release, so a crash in between leaves
   --  it for the next mount (this service, Linux or e2fsck) to free.
   ---------------------------------------------------------------------------
   Last_Orphan_Offset : constant := 16#E8#;

   procedure readLastOrphan
     (fs : Filesystem; head : out Unsigned_32; status : out Read_Status) is
   begin
      readBytes (fs, SUPERBLOCK_OFFSET + Last_Orphan_Offset, head'Address, 4, status);
   end readLastOrphan;

   procedure writeLastOrphan
     (fs : in out Filesystem; head : Unsigned_32; status : out Write_Status)
   is
      value : Unsigned_32 := head;
   begin
      writeBytes (fs, SUPERBLOCK_OFFSET + Last_Orphan_Offset, value'Address, 4,
                  status, Allocation_Metadata);
   end writeLastOrphan;

   function validOrphan (fs : Filesystem; number : Unsigned_32) return Boolean is
     (number >= (if fs.sb.firstNonReservedInode > 0 then fs.sb.firstNonReservedInode
                 else 11) and then number <= fs.sb.inodeCount);

   --  Put an unlinked inode at the head of the list.
   procedure orphanAdd
     (fs : in out Filesystem; inodeNum : Unsigned_32; status : out Write_Status)
   is
      head : Unsigned_32;
      ino : Inode;
      readStatus : Read_Status;
   begin
      readLastOrphan (fs, head, readStatus);
      if readStatus = Read_Complete then
         readInode (fs, inodeNum, ino, readStatus);
      end if;
      if readStatus /= Read_Complete then
         status := Write_Device_Error;
         return;
      end if;
      ino.deletedTime := head;
      writeInode (fs, inodeNum, ino, status, Exact_Inode);
      if status = Write_Complete then
         writeLastOrphan (fs, inodeNum, status);
      end if;
   end orphanAdd;

   --  Take an inode off the list; found is False if it is not on it. The
   --  walk is bounded by the inode count (a cyclic list is malformed).
   procedure orphanRemove
     (fs : in out Filesystem; inodeNum : Unsigned_32; found : out Boolean;
      status : out Write_Status)
   is
      head, previous, current : Unsigned_32;
      ino, previousIno : Inode := NULL_INODE;
      readStatus : Read_Status;
   begin
      found := False;
      status := Write_Complete;
      readLastOrphan (fs, head, readStatus);
      if readStatus /= Read_Complete then
         status := Write_Device_Error;
         return;
      end if;
      previous := 0;
      current := head;
      for step in 1 .. fs.sb.inodeCount loop
         exit when current = 0 or else not validOrphan (fs, current);
         readInode (fs, current, ino, readStatus);
         if readStatus /= Read_Complete then
            status := Write_Device_Error;
            return;
         end if;
         if current = inodeNum then
            found := True;
            if previous = 0 then
               writeLastOrphan (fs, ino.deletedTime, status);
            else
               previousIno.deletedTime := ino.deletedTime;
               writeInode (fs, previous, previousIno, status, Exact_Inode);
            end if;
            return;
         end if;
         previous := current;
         previousIno := ino;
         current := ino.deletedTime;
      end loop;
   end orphanRemove;

   procedure reclaimInode
     (fs : in out Filesystem; inodeNum : Unsigned_32;
      status : out Remove_Status)
   is
      ino, emptied : Inode;
      readStatus : Read_Status;
      writeStatus : Write_Status;
      truncated : Truncate_Status;
   begin
      status := removeRefusal (fs);
      if status /= Remove_Complete then
         return;
      end if;
      readInode (fs, inodeNum, ino, readStatus);
      if readStatus /= Read_Complete then
         status := removeReadFailure (readStatus);
         return;
      elsif ino.numHardLinks /= 0 or else
        Ext2_Support.Check_File (ino, Unlinked_Allowed => True) /=
          Ext2_Support.File_Allowed
      then
         status := Remove_Malformed;
         return;
      end if;
      --  Detach the blocks; they are freed by the next commit or flush.
      --  A file with no blocks has nothing to detach.
      if emptyFile (ino) then
         emptied := ino;
         truncated := Truncate_Complete;
      else
         resizeInode (fs, inodeNum, 0, emptied, truncated);
      end if;
      case truncated is
         when Truncate_Complete => null;
         when Truncate_Read_Only => status := Remove_Read_Only; return;
         when Truncate_IO_Error => status := Remove_IO_Error; return;
         when Truncate_Unsupported | Truncate_Invalid =>
            status := Remove_Unsupported; return;
         when Truncate_Durability_Unsupported =>
            status := Remove_Durability_Unsupported; return;
         when Truncate_Recovery_Required =>
            status := Remove_Recovery_Required; return;
      end case;
      --  Mark the inode deleted, then release it: a crash between the two
      --  leaves an allocated deleted inode, which e2fsck frees.
      startHandle (fs, Remove_Credits + Orphan_Credits);
      if fs.journal.Active then
         declare
            listed : Boolean;
         begin
            orphanRemove (fs, inodeNum, listed, writeStatus);
         end;
      else
         writeStatus := Write_Complete;
      end if;
      emptied.deletedTime := deletionTime (fs);
      if writeStatus = Write_Complete then
         writeInode (fs, inodeNum, emptied, writeStatus, Exact_Inode);
      end if;
      if writeStatus = Write_Complete then
         freeInode (fs, inodeNum, False, writeStatus);
      else
         fs.writeQuarantined := True;
      end if;
      stopHandle (fs);
      status := (if writeStatus = Write_Complete then Remove_Complete
                 else Remove_Recovery_Required);
   end reclaimInode;

   procedure unlinkPath
     (fs : in out Filesystem; path : String; keepOrphan : Boolean;
      inodeNum : out Unsigned_32; unlinked : out Inode;
      status : out Remove_Status)
   is
      parentNum, target : Unsigned_32;
      leafFirst : Positive;
      lookup : Directory_Lookup_Status;
      dir, ino : Inode;
      readStatus : Read_Status;
      writeStatus : Write_Status;
      treeStatus : Truncate_Status;
   begin
      inodeNum := 0;
      unlinked := NULL_INODE;
      status := removeRefusal (fs);
      if status /= Remove_Complete then
         return;
      end if;
      resolveParent (fs, path, parentNum, leafFirst, lookup);
      if lookup /= Lookup_Found then
         status := removeLookupFailure (lookup);
         return;
      end if;
      declare
         leaf : String renames path (leafFirst .. path'Last);
      begin
         if not CuBit.Directory_Paths.Valid_Child_Name (leaf) then
            status := Remove_Invalid_Name;
            return;
         end if;
         readInode (fs, parentNum, dir, readStatus);
         if readStatus /= Read_Complete then
            status := removeReadFailure (readStatus);
            return;
         elsif inodeType (dir) /= INODE_DIRECTORY then
            status := Remove_Not_Found;
            return;
         elsif not plainDirectory (fs, dir) then
            status := Remove_Unsupported;
            return;
         end if;
         lookupInDir (fs, dir, leaf, target, lookup);
         if lookup /= Lookup_Found then
            status := removeLookupFailure (lookup);
            return;
         end if;
         readInode (fs, target, ino, readStatus);
         if readStatus /= Read_Complete then
            status := removeReadFailure (readStatus);
            return;
         elsif inodeType (ino) = INODE_DIRECTORY then
            status := Remove_Wrong_Type;
            return;
         elsif Ext2_Support.Check_File (ino) /= Ext2_Support.File_Allowed or else
           ino.fileACL /= 0
         then
            status := Remove_Unsupported;
            return;
         end if;
         --  Check the block tree before the name goes, so that reclaim
         --  cannot then refuse it and strand an orphan.
         validateBlockTree (fs, ino, treeStatus);
         if treeStatus /= Truncate_Complete then
            status := (if treeStatus = Truncate_IO_Error then Remove_IO_Error
                       else Remove_Unsupported);
            return;
         end if;
         startHandle (fs, Remove_Credits + Orphan_Credits);
         forgetName (fs, parentNum, leaf);
         removeEntry (fs, dir, leaf, target, status);
         if status /= Remove_Complete then
            uncertainDirectory (fs, parentNum);
            stopHandle (fs);
            return;
         end if;
      end;
      --  The name is gone; a crash here leaves a linked, unnamed inode
      --  (e2fsck moves it to lost+found). Journaled, both are one operation.
      ino.numHardLinks := 0;
      --  An unheld inode with no blocks, on a journaled volume: it is freed
      --  under the same handle as its name, so the transaction holds both
      --  or neither and it never needs the orphan list (Linux's unlink and
      --  eviction in one transaction). Blocks would need a truncate, which
      --  takes its own handle: those go the general way below.
      if fs.journal.Active and then not keepOrphan and then emptyFile (ino) then
         ino.deletedTime := deletionTime (fs);
         writeInode (fs, target, ino, writeStatus, Exact_Inode);
         if writeStatus = Write_Complete then
            freeInode (fs, target, False, writeStatus);
         end if;
         stopHandle (fs);
         if writeStatus /= Write_Complete then
            fs.writeQuarantined := True;
            status := Remove_Recovery_Required;
            return;
         end if;
         inodeNum := target;
         unlinked := ino;
         status := Remove_Complete;
         return;
      end if;
      writeInode (fs, target, ino, writeStatus);
      if writeStatus = Write_Complete and then fs.journal.Active then
         orphanAdd (fs, target, writeStatus);
         if writeStatus = Write_Complete then
            readInode (fs, target, ino, readStatus);
            if readStatus /= Read_Complete then
               writeStatus := Write_Device_Error;
            end if;
         end if;
      end if;
      stopHandle (fs);
      if writeStatus /= Write_Complete then
         fs.writeQuarantined := True;
         status := Remove_Recovery_Required;
         return;
      end if;
      inodeNum := target;
      unlinked := ino;
      if not keepOrphan then
         reclaimInode (fs, target, status);
      end if;
   end unlinkPath;

   procedure makeDirectoryPath
     (fs : in out Filesystem; path : String; inodeNum : out Unsigned_32;
      lookup : out Directory_Lookup_Status; status : out Write_Status)
   is
      parentNum : Unsigned_32;
      leafFirst : Positive;
   begin
      inodeNum := 0;
      status := Write_Out_Of_Range;
      resolveParent (fs, path, parentNum, leafFirst, lookup);
      if lookup /= Lookup_Found then
         return;
      end if;
      makeDirectory
        (fs, parentNum, path (leafFirst .. path'Last), inodeNum, status);
   end makeDirectoryPath;

   procedure removeDirectoryInner
     (fs : in out Filesystem; path : String; inodeNum : out Unsigned_32;
      status : out Remove_Status)
   is
      parentNum, target : Unsigned_32;
      leafFirst : Positive;
      lookup : Directory_Lookup_Status;
      parent, dir : Inode;
      readStatus : Read_Status;
      writeStatus : Write_Status;
      contents : Directory_Blocks.Block_Data := [others => 0];
      children : Directory_Blocks.Byte_Count;
      counted : Directory_Blocks.Prepare_Result;
      blocks : Natural;
   begin
      inodeNum := 0;
      status := removeRefusal (fs);
      if status /= Remove_Complete then
         return;
      end if;
      resolveParent (fs, path, parentNum, leafFirst, lookup);
      if lookup /= Lookup_Found then
         status := removeLookupFailure (lookup);
         return;
      end if;
      declare
         leaf : String renames path (leafFirst .. path'Last);
      begin
         if not CuBit.Directory_Paths.Valid_Child_Name (leaf) then
            status := Remove_Invalid_Name;
            return;
         end if;
         readInode (fs, parentNum, parent, readStatus);
         if readStatus /= Read_Complete then
            status := removeReadFailure (readStatus);
            return;
         elsif inodeType (parent) /= INODE_DIRECTORY then
            status := Remove_Not_Found;
            return;
         elsif not plainDirectory (fs, parent) then
            status := Remove_Unsupported;
            return;
         end if;
         lookupInDir (fs, parent, leaf, target, lookup);
         if lookup /= Lookup_Found then
            status := removeLookupFailure (lookup);
            return;
         elsif target = ROOT_INODE or else target = parentNum then
            status := Remove_Malformed;
            return;
         end if;
         readInode (fs, target, dir, readStatus);
         if readStatus /= Read_Complete then
            status := removeReadFailure (readStatus);
            return;
         elsif inodeType (dir) /= INODE_DIRECTORY then
            status := Remove_Wrong_Type;
            return;
         elsif not plainDirectory (fs, dir) or else dir.fileACL /= 0 or else
           dir.deletedTime /= 0 or else dir.fragmentBlockAddr /= 0
         then
            status := Remove_Unsupported;
            return;
         end if;
         blocks := Natural (dir.sizeLo / fs.blkSize);
         if dir.numDiskSectors /= Unsigned_32 (blocks) * (fs.blkSize / 512) or else
           (for some index in 0 .. blocks - 1 =>
              dir.directBlocks (index) < fs.sb.firstDataBlock or else
              dir.directBlocks (index) >= fs.sb.blockCount) or else
           (for some index in blocks .. NUM_DIRECT_BLOCKS - 1 =>
              dir.directBlocks (index) /= 0) or else
           parent.numHardLinks <= 2
         then
            status := Remove_Malformed;
            return;
         end if;
         for index in 0 .. blocks - 1 loop
            readBlock (fs, dir.directBlocks (index), contents'Address, readStatus);
            if readStatus /= Read_Complete then
               status := removeReadFailure (readStatus);
               return;
            end if;
            Directory_Blocks.Count_Children
              (contents, Directory_Blocks.Block_Length (fs.blkSize),
               fs.sb.inodeCount, children, counted);
            if counted /= Directory_Blocks.Prepared then
               status := Remove_Malformed;
               return;
            elsif children /= 0 then
               status := Remove_Not_Empty;
               return;
            end if;
         end loop;
         if dir.numHardLinks /= 2 then
            status := Remove_Malformed; -- no children yet other links
            return;
         end if;
         forgetName (fs, parentNum, leaf);
         Dentry_Cache.Forget_Directory (Names, fs.device.endpointSlot, target);
         removeEntry (fs, parent, leaf, target, status);
         if status /= Remove_Complete then
            uncertainDirectory (fs, parentNum);
            return;
         end if;
      end;
      inodeNum := target;
      --  Detach its blocks with the inode marked deleted; they are freed by
      --  the next commit or flush; then free the inode; the parent's ".."
      --  link goes last.
      declare
         retired : Release_List (1 .. blocks);
      begin
         for index in retired'Range loop
            retired (index) := dir.directBlocks (index - 1);
         end loop;
         dir.numHardLinks := 0;
         dir.sizeLo := 0;
         dir.directBlocks := [others => 0];
         dir.numDiskSectors := 0;
         dir.deletedTime := deletionTime (fs);
         writeInode (fs, target, dir, writeStatus, Exact_Inode);
         if writeStatus = Write_Complete then
            deferRelease (fs, retired, writeStatus);
         end if;
      end;
      if writeStatus = Write_Complete then
         freeInode (fs, target, True, writeStatus);
      end if;
      if writeStatus = Write_Complete then
         parent.numHardLinks := parent.numHardLinks - 1;
         writeInode (fs, parentNum, parent, writeStatus);
      end if;
      if writeStatus /= Write_Complete then
         fs.writeQuarantined := True;
         status := Remove_Recovery_Required;
      end if;
   end removeDirectoryInner;

   procedure removeDirectoryPath
     (fs : in out Filesystem; path : String; inodeNum : out Unsigned_32;
      status : out Remove_Status)
   is
   begin
      startHandle (fs, Remove_Credits);
      removeDirectoryInner (fs, path, inodeNum, status);
      stopHandle (fs);
   end removeDirectoryPath;

   --  At admission, as Linux's ext4_orphan_cleanup: free every unlinked
   --  inode on the orphan list (a crash came between its unlink and its
   --  release). An inode still linked is only taken off the list (this
   --  service never lists truncations). A malformed list is not followed.
   procedure processOrphans (fs : in out Filesystem; result : out Admission_Result) is
      head : Unsigned_32;
      ino : Inode;
      readStatus : Read_Status;
      removed : Remove_Status;
      writeStatus : Write_Status;
      flushStatus : Flush_Status;
      listed : Boolean;
   begin
      result := Admitted;
      for step in 1 .. fs.sb.inodeCount loop
         readLastOrphan (fs, head, readStatus);
         if readStatus /= Read_Complete then
            result := Device_Error;
            return;
         end if;
         exit when head = 0;
         if not validOrphan (fs, head) then
            debugPrint ("Ext2: malformed orphan list left for e2fsck" & ASCII.LF);
            return;
         end if;
         readInode (fs, head, ino, readStatus);
         if readStatus /= Read_Complete then
            result := Device_Error;
            return;
         end if;
         if ino.numHardLinks = 0 and then
           Ext2_Support.Check_File (ino, Unlinked_Allowed => True) =
             Ext2_Support.File_Allowed
         then
            reclaimInode (fs, head, removed);
            if removed /= Remove_Complete then
               result := Device_Error;
               return;
            end if;
         else
            startHandle (fs, Orphan_Credits);
            orphanRemove (fs, head, listed, writeStatus);
            stopHandle (fs);
            if writeStatus /= Write_Complete or else not listed then
               result := Device_Error;
               return;
            end if;
         end if;
      end loop;
      Flush (fs, flushStatus);
      if flushStatus /= Flush_Complete and then
        not (flushStatus = Flush_Unsupported and then Is_Volatile (fs.device.description))
      then
         result := Device_Error;
      end if;
   end processOrphans;

   --  Superblock journal fields beyond the prefix record (little-endian).
   Journal_Inode_Offset  : constant := 16#E0#;
   Journal_Device_Offset : constant := 16#E4#;
   --  ext4 inode flag: the block map is an extent tree, not block pointers.
   Inode_Extents_Flag : constant Unsigned_32 := 16#0008_0000#;
   --  Open an internal JBD2 journal before the volume is used. Committed
   --  transactions a crash left in the log are replayed first (idempotent;
   --  straight to the device, then a barrier), whether or not needs_recovery
   --  is set, as e2fsck does. Then, on a writable device, the journal is
   --  opened for this session: the log start is set and the sequence moves
   --  past every one a stale log block could carry, durably, and
   --  needs_recovery is set, as a Linux mount does; Detach undoes both. On a
   --  read-only device a clean journal is left alone and a dirty one refused.
   --  The cache is discarded after replay: it may have rewritten any block.
   procedure openJournal
     (fs : in out Filesystem; result : out Admission_Result)
   is
      blockBytes : constant Storage_Offset := Storage_Offset (fs.blkSize);
      size : constant Jbd2_Format.Block_Bytes := Jbd2_Format.Block_Bytes (fs.blkSize);
      journalInode, journalDevice : Unsigned_32 := 0;
      journal : Inode;
      journalBlocks : Unsigned_64;
      readStatus : Read_Status;
      writeStatus : Write_Status;
      data : Jbd2_Format.Block := [others => 0];
      super : Jbd2_Format.Journal_Superblock;
      valid, ok : Boolean;
      superHome : Unsigned_32;
      nextSequence : Unsigned_32;

      function Journal_Home (logical : Unsigned_32; physical : out Unsigned_32)
         return Boolean
      is
         leaf, middle : Unsigned_32;
         status : Read_Status;
      begin
         if Unsigned_64 (logical) >= journalBlocks then
            physical := 0;
            return False;
         end if;
         resolveBlock (fs, journal, logical, physical, leaf, middle, status);
         return status = Read_Complete and then physical /= 0;
      end Journal_Home;

      procedure Read_Log
        (Log_Block : Unsigned_32; Data : out Jbd2_Format.Block; Ok : out Boolean)
      is
         physical : Unsigned_32;
         status : Read_Status;
      begin
         Data := [others => 0];
         Ok := Journal_Home (Log_Block, physical);
         if Ok then
            deviceRead (fs, Storage_Offset (physical) * blockBytes, Data'Address,
                        Storage_Count (fs.blkSize), status);
            Ok := status = Read_Complete;
         end if;
      end Read_Log;

      procedure Write_Home
        (Home : Unsigned_64; Data : Jbd2_Format.Block; Ok : out Boolean)
      is
         status : Write_Status;
      begin
         deviceWrite (fs, Storage_Offset (Home) * blockBytes, Data'Address,
                      Storage_Count (fs.blkSize), status);
         Ok := status = Write_Complete;
      end Write_Home;

      package Recovery is new Jbd2_Recovery (Read_Log, Write_Home);
      use type Recovery.Outcome;

      procedure Put_Be32 (offset : Natural; value : Unsigned_32) is
      begin
         data (offset) := Unsigned_8 (Shift_Right (value, 24));
         data (offset + 1) := Unsigned_8 (Shift_Right (value, 16) and 16#FF#);
         data (offset + 2) := Unsigned_8 (Shift_Right (value, 8) and 16#FF#);
         data (offset + 3) := Unsigned_8 (value and 16#FF#);
      end Put_Be32;
   begin
      result := Invalid_Filesystem;
      readBytes (fs, SUPERBLOCK_OFFSET + Journal_Inode_Offset, journalInode'Address, 4,
                 readStatus);
      if readStatus = Read_Complete then
         readBytes (fs, SUPERBLOCK_OFFSET + Journal_Device_Offset, journalDevice'Address,
                    4, readStatus);
      end if;
      if readStatus /= Read_Complete then
         result := Device_Error;
         return;
      elsif journalDevice /= 0 then
         result := Unsupported_Filesystem; -- external journals
         return;
      end if;
      readInode (fs, journalInode, journal, readStatus);
      if readStatus /= Read_Complete then
         result := (if readStatus = Read_Out_Of_Range then Invalid_Filesystem
                    else Device_Error);
         return;
      elsif inodeType (journal) /= INODE_REGULAR_FILE or else
        (journal.flags and Inode_Extents_Flag) /= 0
      then
         result := Unsupported_Filesystem;
         return;
      end if;
      journalBlocks := fileSize (journal) / Unsigned_64 (fs.blkSize);
      if journalBlocks = 0 or else journalBlocks > Unsigned_64 (Unsigned_32'Last) or else
        not Journal_Home (0, superHome)
      then
         return;
      end if;
      deviceRead (fs, Storage_Offset (superHome) * blockBytes, data'Address,
                  Storage_Count (fs.blkSize), readStatus);
      if readStatus /= Read_Complete then
         result := Device_Error;
         return;
      end if;
      Jbd2_Format.Decode_Superblock
        (data, size, Unsigned_32 (journalBlocks),
         Recovery.Superblock_Checksum_Matches (data), super, valid);
      if not valid then
         result := Unsupported_Filesystem;
         return;
      elsif Is_Read_Only (fs.device.description) then
         --  Without replay a dirty journal's volume is not what Linux sees.
         result := (if super.Start = 0 then Admitted else Unsupported_Filesystem);
         return;
      end if;

      nextSequence := super.Sequence + 1;
      if super.Start /= 0 then
         declare
            outcome : Recovery.Outcome;
            transactions, written : Natural := 0;
         begin
            Recovery.Recover
              (super, size, fs.sb.blockCount, outcome, nextSequence, transactions, written);
            if outcome /= Recovery.Recovered then
               result := (if outcome = Recovery.Too_Many_Revokes
                          then Unsupported_Filesystem else Device_Error);
               return;
            end if;
            barrier (fs, ok);
            if not ok then
               result := Device_Error;
               return;
            end if;
         end;
         --  Replay may have rewritten the superblock itself: re-read and
         --  re-validate it.
         Block_Cache_Index.Discard_Volume (Cache_Index, fs.device.endpointSlot);
         invalidateBlockCache;
         deviceRead (fs, SUPERBLOCK_OFFSET, fs.sb'Address, Superblock'Size / 8, readStatus);
         if readStatus /= Read_Complete then
            result := Device_Error;
            return;
         elsif not Ext2_Support.Supported_Volume
             (fs.sb.majorVersion, fs.sb.creatorOS, fs.sb.compatibleFeatures,
              fs.sb.incompatibleFeatures, fs.sb.readOnlyFeatures) or else
           not supportedSuperblock (fs.sb) or else blockSize (fs.sb) /= fs.blkSize
         then
            result := Invalid_Filesystem;
            return;
         end if;
      end if;

      --  Open the log for this session.
      Put_Be32 (Journal_Start_Offset, super.First);
      Put_Be32 (Journal_Sequence_Offset, nextSequence);
      if Jbd2_Format.Checksummed (super.Incompat) then
         Put_Be32 (Jbd2_Format.Superblock_Checksum_Offset, 0);
         Put_Be32 (Jbd2_Format.Superblock_Checksum_Offset,
                   Jbd2_Format.Crc32c (16#FFFF_FFFF#, data, 0, Jbd2_Format.Superblock_Bytes));
      end if;
      writeDurable (fs, superHome, data'Address, ok);
      if ok and then (fs.sb.incompatibleFeatures and Ext2_Support.Incompat_Recover) = 0 then
         fs.sb.incompatibleFeatures :=
           fs.sb.incompatibleFeatures or Ext2_Support.Incompat_Recover;
         writeSuperblock (fs, writeStatus); -- write-through: not yet journaled
         ok := writeStatus = Write_Complete;
         if ok then
            barrier (fs, ok);
         end if;
      end if;
      if not ok then
         result := Device_Error;
         return;
      end if;
      fs.journal :=
        (Active => True, Journal_Inode => journal, Blocks => Unsigned_32 (journalBlocks),
         First => super.First, Max_Length => super.Max_Length, Sequence => nextSequence,
         Compat => super.Compat, Incompat => super.Incompat, Identity => super.Identity,
         Super_Home => superHome, Dirty_Metadata => 0, Handle_Depth => 0,
         Head => super.First, Tail => super.First, Tail_Sequence => nextSequence,
         Live_Blocks => 0);
      result := Admitted;
   end openJournal;

   procedure Detach (fs : in out Filesystem; status : out Flush_Status) is
      data : Jbd2_Format.Block := [others => 0];
      readStatus : Read_Status;
      writeStatus : Write_Status;
      ok : Boolean;
   begin
      Flush (fs, status);
      if not fs.journal.Active or else status /= Flush_Complete then
         return;
      end if;
      --  Every checkpoint write durable before the log is marked empty.
      if Home_Writes_Pending then
         barrier (fs, ok);
         if not ok then
            status := Flush_IO_Error;
            return;
         end if;
      end if;
      deviceRead (fs, Storage_Offset (fs.journal.Super_Home) * Storage_Offset (fs.blkSize),
                  data'Address, Storage_Count (fs.blkSize), readStatus);
      if readStatus /= Read_Complete then
         status := Flush_IO_Error;
         return;
      end if;
      --  An empty log: no start, and the next sequence.
      for index in 0 .. 3 loop
         data (Journal_Start_Offset + index) := 0;
      end loop;
      data (Journal_Sequence_Offset) :=
        Unsigned_8 (Shift_Right (fs.journal.Sequence, 24));
      data (Journal_Sequence_Offset + 1) :=
        Unsigned_8 (Shift_Right (fs.journal.Sequence, 16) and 16#FF#);
      data (Journal_Sequence_Offset + 2) :=
        Unsigned_8 (Shift_Right (fs.journal.Sequence, 8) and 16#FF#);
      data (Journal_Sequence_Offset + 3) := Unsigned_8 (fs.journal.Sequence and 16#FF#);
      if Jbd2_Format.Checksummed (fs.journal.Incompat) then
         declare
            sum : Unsigned_32;
         begin
            data (Jbd2_Format.Superblock_Checksum_Offset ..
                  Jbd2_Format.Superblock_Checksum_Offset + 3) := [others => 0];
            sum := Jbd2_Format.Crc32c (16#FFFF_FFFF#, data, 0, Jbd2_Format.Superblock_Bytes);
            for index in 0 .. 3 loop
               data (Jbd2_Format.Superblock_Checksum_Offset + index) :=
                 Unsigned_8 (Shift_Right (sum, 24 - 8 * index) and 16#FF#);
            end loop;
         end;
      end if;
      writeDurable (fs, fs.journal.Super_Home, data'Address, ok);
      if not ok then
         status := Flush_IO_Error;
         return;
      end if;
      --  The session is over: clear needs_recovery write-through.
      fs.journal.Active := False;
      fs.sb.incompatibleFeatures :=
        fs.sb.incompatibleFeatures and not Ext2_Support.Incompat_Recover;
      writeSuperblock (fs, writeStatus);
      if writeStatus = Write_Complete then
         barrier (fs, ok);
      else
         ok := False;
      end if;
      status := (if ok then Flush_Complete else Flush_IO_Error);
   end Detach;

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
             writeQuarantined => False, journal => <>, others => <>);
      result := Invalid_Session;
      if capSlot not in 1 .. 62 or else
         grantBuf = System.Null_Address or else grantBytes = 0
      then
         return;
      end if;
      --  Any admission attempt ends the endpoint's previous volume lifetime.
      Block_Cache_Index.Discard_Volume (Cache_Index, capSlot);
      Dentry_Cache.Discard_Volume (Names, capSlot);

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
      if (sb.compatibleFeatures and Ext2_Support.Compat_Has_Journal) /= 0 then
         openJournal (tmpFs, result);
         if result /= Admitted then
            debugPrint ("Ext2: journal " & Volume_Admission.Description (result) &
                        ASCII.LF);
            return;
         end if;
         processOrphans (tmpFs, result);
         if result /= Admitted then
            return;
         end if;
      elsif (sb.incompatibleFeatures and Ext2_Support.Incompat_Recover) /= 0 then
         result := Invalid_Filesystem; -- needs_recovery without a journal
         return;
      end if;
      fs := tmpFs;
      result := Admitted;
   end initBlockDevice;


   procedure configureCache (megabytes : Cache_Megabytes) is
   begin
      if not Store_Ready then
         Cache_Ways := Block_Cache_Index.Way_Count (megabytes / Megabytes_Per_Way);
         Block_Cache_Index.Clear (Cache_Index, Cache_Ways);
      end if;
   end configureCache;

   function cacheMegabytes return Cache_Megabytes is
     (Cache_Megabytes (Cache_Ways * Megabytes_Per_Way));

begin
   Block_Cache_Index.Clear (Cache_Index, Cache_Ways);
   Dentry_Cache.Clear (Names);
end Ext2;
