------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  See the spec.
------------------------------------------------------------------------------
pragma Ada_2022;
with Ext2_Support; use type Ext2_Support.File_Admission;
with Sector_Accounting;
with CuBit.Block_Devices; use CuBit.Block_Devices;
with Ext2.Allocation; use Ext2.Allocation;
with Ext2.Device_IO; use Ext2.Device_IO;
with Ext2.Mapping; use Ext2.Mapping;

package body Ext2.Journal is

   use type System.Address;

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
   --  Set by commitTransaction: its last run committed a transaction, and
   --  so ended with every earlier write durable.
   Commit_Was_Barrier : Boolean := False;

   function dirtyBlocks (fs : Filesystem) return Boolean is
     (Block_Cache_Index.Has_Dirty (Cache_Index, fs.device.volumeKey) or else
      fs.pendingCount > 0);

   procedure commitTransaction (fs : in out Filesystem; status : out Write_Status) is
      volume : constant Unsigned_64 := fs.device.volumeKey;
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

end Ext2.Journal;
