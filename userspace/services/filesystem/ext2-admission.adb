------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  See the spec.
------------------------------------------------------------------------------
pragma Ada_2022;
with Ext2_Support; use type Ext2_Support.File_Admission;
with Block_Cache_Index;
with Jbd2_Recovery;
with Jbd2_Revokes;
with Dentry_Cache;
with CuBit.Block_Devices; use CuBit.Block_Devices;
with CuBit.Messages; use CuBit.Messages;
with System.Storage_Elements; use System.Storage_Elements;
with Ext2.Block_Cache; use Ext2.Block_Cache;
with Ext2.Device_IO; use Ext2.Device_IO;
with Ext2.Journal; use Ext2.Journal;
with Ext2.Mapping; use Ext2.Mapping;
with Ext2.Metadata; use Ext2.Metadata;
with Ext2.Removal; use Ext2.Removal;

package body Ext2.Admission is

   use type System.Address;

   --  ext4 inode flag: the block map is an extent tree, not block pointers.
   Inode_Extents_Flag : constant Unsigned_32 := Ext2_Format.Extents_Flag;

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
         Ext2.Metadata.readInode (fs, head, ino, readStatus);
         if readStatus /= Read_Complete then
            result := Device_Error;
            return;
         end if;
         if ino.numHardLinks = 0 and then
           Ext2_Support.Check_File (ino, Unlinked_Allowed => True) =
             Ext2_Support.File_Allowed
         then
            Ext2.Removal.reclaimInode (fs, head, removed);
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
      Ext2.Journal.Flush (fs, flushStatus);
      if flushStatus /= Flush_Complete and then
        not (flushStatus = Flush_Unsupported and then Is_Volatile (fs.device.description))
      then
         result := Device_Error;
      end if;
   end processOrphans;

   --  Open an internal JBD2 journal before the volume is used. Committed
   --  transactions a crash left in the log are replayed first (idempotent;
   --  straight to the device, then a barrier), whether or not needs_recovery
   --  is set, as e2fsck does. Then, on a writable device, the journal is
   --  opened for this session: the log start is set and the sequence moves
   --  past every one a stale log block could carry, durably, and
   --  needs_recovery is set, as a Linux mount does; Detach undoes both. On a
   --  read-only device a clean journal is left alone and a dirty one refused.
   --  The cache is discarded after replay: it may have rewritten any block.
   --  openJournal's revoke table for a replay (Jbd2_Recovery): service-
   --  owned, as Admission_Candidate below. Admissions never nest.
   Replay_Revokes : Jbd2_Revokes.Table;

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

      package Recovery is new Jbd2_Recovery (Read_Log, Write_Home, Replay_Revokes);
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
      Ext2.Metadata.readInode (fs, journalInode, journal, readStatus);
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
         Block_Cache_Index.Discard_Volume (Cache_Index, fs.device.volumeKey);
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
      Ext2.Journal.Flush (fs, status);
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

   --  initBlockDevice's volume under admission, published to its caller
   --  only when admitted. It lives here, not in initBlockDevice's frame:
   --  the image's pages are all backed when the service loads, while a
   --  stack frame this size is faulted in when a drive is attached, and on
   --  a full machine that fault kills the service. The service is
   --  single-threaded and admissions never nest.
   Admission_Candidate : Filesystem;

   procedure initBlockDevice
     (fs         : out Filesystem;
      endpoint   : CuBit.Handles.Endpoint;
      volume     : Unsigned_64;
      grant      : CuBit.Memory_Grants.Grant_Reference;
      grantBuf   : System.Address;
      grantBytes : Unsigned_32;
      window     : Volume_Window;
      result     : out Admission_Result)
   is
      sb : Superblock;
      description : Device_Description;
      describeMsg : Message;
      requiredBytes  : Unsigned_64;
      requiredBlocks : Unsigned_64;
      readStatus : Read_Status;
      tmpFs : Filesystem renames Admission_Candidate;
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
      if volume = 0 or else not CuBit.Handles.Valid (endpoint) or else
         grantBuf = System.Null_Address or else grantBytes = 0
      then
         return;
      end if;
      --  Any admission attempt ends the endpoint's previous volume lifetime.
      Block_Cache_Index.Discard_Volume (Cache_Index, volume);
      Dentry_Cache.Discard_Volume (Names, volume);

      describeMsg :=
        (tag      => (label => OP_DESCRIBE_DEVICE, length => 0,
                      protocol => 0, ipc => <>, reserved => 0),
         authorityTag => 0,
         words    => [others => 0]);
      describeMsg.tag := capCall (endpoint, describeMsg, CuBit.Messages.Wait_Forever);
      if describeMsg.tag.label = REPLY_NO_DEVICE then
         result :=
           (if describeMsg.tag.length = 1 and then
               describeMsg.tag.protocol = 0 and then describeMsg.tag.reserved = 0 and then
               describeMsg.words (0) = 0
            then No_Device else Invalid_Description);
         return;
      elsif describeMsg.tag.label /= REPLY_OK then
         result := Device_Error;
         return;
      end if;
      result := Invalid_Description;
      if describeMsg.tag.length /= 4 or else
         describeMsg.tag.protocol /= 0 or else describeMsg.tag.reserved /= 0 or else
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
      --  A partition: the volume sees only its window of the device.
      if window.Blocks > 0 then
         if window.First >= description.blockCount
           or else window.Blocks > description.blockCount - window.First
         then
            result := Invalid_Description;
            return;
         end if;
         description.blockCount := window.Blocks;
      end if;

      tmpFs := fs;   --  nothing kept from an earlier admission
      tmpFs.device :=
        (endpoint     => endpoint,
         volumeKey    => volume,
         grant        => grant,
         grantBuffer  => grantBuf,
         grantBytes   => grantBytes,
         firstBlock   => window.First,
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

end Ext2.Admission;
