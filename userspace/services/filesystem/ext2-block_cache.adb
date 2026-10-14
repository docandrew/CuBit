------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  See the spec.
------------------------------------------------------------------------------
pragma Ada_2022;
with Ext2_Support; use type Ext2_Support.File_Admission;
with Ada.Unchecked_Conversion;
with CuBit.Messages; use CuBit.Messages;
with Ext2.Device_IO; use Ext2.Device_IO;

package body Ext2.Block_Cache is

   use type System.Address;

   cachedCapSlot       : Unsigned_64 := 0;

   --  One owned-memory allocation is at most 16 MiB.
   Maximum_Chunk_Bytes : constant := 16 * 1024 * 1024;

   Cache_Ways : Block_Cache_Index.Way_Count :=
     Block_Cache_Index.Way_Count (Default_Cache_Megabytes / Megabytes_Per_Way);

   function toBlock is new Ada.Unchecked_Conversion (System.Address, Block_Access);

   Chunk_Blocks : constant := Maximum_Chunk_Bytes / Cached_Block_Bytes;

   Maximum_Chunks : constant :=
     (Block_Cache_Index.Capacity + Spill_Capacity + Chunk_Blocks - 1) / Chunk_Blocks;

   subtype Chunk_Index is Natural range 0 .. Maximum_Chunks - 1;

   Chunk_Bases : array (Chunk_Index) of System.Address :=
     [others => System.Null_Address];

   Store_Ready : Boolean := False;

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
         cachedCapSlot /= fs.device.volumeKey
      then
         invalidateBlockCache;
         cachedCapSlot := fs.device.volumeKey;
         cacheIdentityValid := True;
      end if;
   end selectCacheIdentity;

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

   function splitCommits return Natural is (Split_Commits);

   function pressureBlocks return Natural is (Pressure_Blocks);

   function spillSlot (key : Block_Cache_Index.Block_Key) return Integer is
   begin
      for index in 0 .. Spill_Used - 1 loop
         if Spill_Keys (index) = key then
            return First_Spill_Slot + index;
         end if;
      end loop;
      return -1;
   end spillSlot;

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

   function cacheKey (fs : Filesystem; number : Unsigned_32)
      return Block_Cache_Index.Block_Key is
     ((Volume => fs.device.volumeKey, Block => number));

   function cacheable (fs : Filesystem) return Boolean is
     (fs.blkSize in 1024 | 2048 | 4096);

   procedure rememberName
     (fs : Filesystem; directory : Unsigned_32; name : String; inode : Unsigned_32)
   is
      displaced : Boolean;
      from : Dentry_Cache.Directory_Id;
   begin
      if Dentry_Cache.Cacheable (name) then
         Dentry_Cache.Insert
           (Names, Dentry_Cache.Make_Key (fs.device.volumeKey, directory, name),
            inode, displaced, from);
      end if;
   end rememberName;

   procedure uncertainDirectory (fs : Filesystem; directory : Unsigned_32) is
   begin
      Dentry_Cache.Mark_Incomplete (Names, fs.device.volumeKey, directory);
   end uncertainDirectory;

   function completeDirectory (fs : Filesystem; directory : Unsigned_32) return Boolean is
     (Dentry_Cache.Is_Complete (Names, fs.device.volumeKey, directory));

   procedure forgetName (fs : Filesystem; directory : Unsigned_32; name : String) is
   begin
      if Dentry_Cache.Cacheable (name) then
         Dentry_Cache.Forget
           (Names, Dentry_Cache.Make_Key (fs.device.volumeKey, directory, name));
      end if;
   end forgetName;

   procedure forgetVolume (fs : Filesystem) is
   begin
      Dentry_Cache.Discard_Volume (Names, fs.device.volumeKey);
      dropSpill (fs.device.volumeKey);
      Block_Cache_Index.Discard_Volume (Cache_Index, fs.device.volumeKey);
   end forgetVolume;

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
end Ext2.Block_Cache;
