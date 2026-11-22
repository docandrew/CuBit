------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The volume caches: the set-associative block cache (Block_Cache_Index)
--  with its owned-memory store and journal spill, the per-level pointer-
--  block caches, and the name cache (Dentry_Cache). Reads fill; writes
--  patch (write-through) or, journaled, mark dirty for the next commit.
------------------------------------------------------------------------------
pragma Ada_2022;
with System.Storage_Elements; use System.Storage_Elements;
with Block_Cache_Index;
with Dentry_Cache;
with Ext2_Format;

private package Ext2.Block_Cache is

   use type Block_Cache_Index.Block_Key;

   use all type Block_Cache_Index.Block_Class;

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

   procedure invalidateBlockCache;

   procedure selectCacheIdentity (fs : Filesystem);

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

   Cached_Block_Bytes : constant := Ext2_Format.Maximum_Block_Bytes;

   --  Block memory comes from page-aligned owned-memory chunks.
   subtype Cached_Block is Ext2_Format.Byte_Array (0 .. Cached_Block_Bytes - 1);

   Cache_Index : Block_Cache_Index.Index;

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
   --  Block buffers are owned memory reached through addresses (toBlock).
   pragma No_Strict_Aliasing (Block_Access);

   function storeBlocks return Natural;

   --  The block buffer of an index slot (active ways only) or spill slot.
   function Cache_Blocks (slot : Natural) return Block_Access;

   --  Commits forced inside an open operation (spill full: its credits
   --  were understated). Diagnostic; zero in every test.
   Split_Commits : Natural := 0;

   --  Blocks an open operation placed beside a full cache set (spilled
   --  metadata, data sent home at once). Diagnostic for tests.
   Pressure_Blocks : Natural := 0;

   function splitCommits return Natural;

   function pressureBlocks return Natural;

   --  The spill buffer holding key, or -1.
   function spillSlot (key : Block_Cache_Index.Block_Key) return Integer;

   --  Drop a volume's spill entries (committed, or the volume discarded).
   procedure dropSpill (volume : Unsigned_64);

   --  File transfers from this size bypass the block cache: large writes
   --  go home directly (journaled volumes too) and large reads fill
   --  nothing, so bulk I/O cannot evict metadata. Smaller ones are cached
   --  like metadata (small files are read again soon, as Linux's page
   --  cache assumes).
   Direct_Data_Bytes : constant := 64 * 1024;

   --  Bulk file payload bypasses the cache so it cannot evict metadata.
   type Cache_Policy is (Cache_Fill, Cache_Bypass);

   function cacheKey (fs : Filesystem; number : Unsigned_32)
      return Block_Cache_Index.Block_Key;

   function cacheable (fs : Filesystem) return Boolean;

   --  Nothing is ever dirty in a write-through cache, so dropping a volume's
   --  entries loses nothing the device lacks.
   --  The name cache (see Dentry_Cache).
   Names : Dentry_Cache.Table;

   --  A name about to change in a directory is no longer cached.
   procedure rememberName
     (fs : Filesystem; directory : Unsigned_32; name : String; inode : Unsigned_32);

   --  A change to directory that may not have completed: names not cached
   --  there can no longer be taken as absent.
   procedure uncertainDirectory (fs : Filesystem; directory : Unsigned_32);

   function completeDirectory (fs : Filesystem; directory : Unsigned_32) return Boolean;

   procedure forgetName (fs : Filesystem; directory : Unsigned_32; name : String);

   procedure forgetVolume (fs : Filesystem);

   --  The slot holding a block, read on a miss. A failed read leaves no
   --  entry. found is False only if every way of the block's set were dirty,
   --  which a write-through cache never is; callers then read directly.
   procedure cachedBlock
     (fs : Filesystem; number : Unsigned_32; slot : out Buffer_Slot;
      found : out Boolean; status : out Read_Status);

   --  Read bytes through the block cache (whole blocks are cached) unless the
   --  caller bypasses it or the volume is not yet admitted.
   procedure readBytes
     (fs     : Filesystem;
      offset : Storage_Offset;
      dest   : System.Address;
      len    : Storage_Count;
      status : out Read_Status;
      policy : Cache_Policy := Cache_Fill);

   --  Copy device sector lba into the grant buffer from the filesystem
   --  blocks it overlaps, filling the cache with any that are missing: small
   --  metadata writes then need no read once their block is cached. found is
   --  False when the sector lies outside the volume's blocks (the caller
   --  reads it directly); failed reports a read that must end the write.
   procedure cachedSector
     (fs : Filesystem; lba, sectorBytes : Unsigned_64;
      found, failed : out Boolean);

   --  After the device acknowledged a write, patch any cached copy. A whole
   --  metadata block not yet cached is inserted: it will be read again
   --  (inode tables, directories, bitmaps, pointer blocks). File payload is
   --  only patched, so bulk writes cannot evict metadata.
   procedure writeThrough
     (fs : Filesystem; offset : Storage_Offset; src : System.Address;
      len : Storage_Count; class : Block_Class);

   --  File payload: cached (possibly dirty) blocks are copied; runs of
   --  uncached blocks are read from the device directly, without filling the
   --  cache, so bulk reads cannot evict metadata.
   procedure readPayload
     (fs : Filesystem; offset : Storage_Offset; dest : System.Address;
      len : Storage_Count; status : out Read_Status);

   procedure readBlock
     (fs       : Filesystem;
      blockNum : Unsigned_32;
      dest     : System.Address;
      status   : out Read_Status);

   procedure configureCache (megabytes : Cache_Megabytes);

   function cacheMegabytes return Cache_Megabytes;

   --  Hot paths: inlined into the other units' loops, as they were
   --  when ext2 was one unit.
   pragma Inline_Always (storeBlocks, Cache_Blocks, cacheKey, cacheable, completeDirectory);

end Ext2.Block_Cache;
