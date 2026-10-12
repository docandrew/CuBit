------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  See the spec.
------------------------------------------------------------------------------
pragma Ada_2022;
with Ext2_Support; use type Ext2_Support.File_Admission;
with Dentry_Cache;
with Directory_Commit;
with CuBit.Directory_Paths;
with CuBit.Block_Devices; use CuBit.Block_Devices;
with System.Storage_Elements; use System.Storage_Elements;
with Ext2_Format; use Ext2_Format;
with Ext2_Geometry;
with Ext2_Bitmaps;
with Ext2.Allocation; use Ext2.Allocation;
with Ext2.Block_Cache; use Ext2.Block_Cache;
with Ext2.Directories; use Ext2.Directories;
with Ext2.Journal; use Ext2.Journal;
with Ext2.Lookup; use Ext2.Lookup;
with Ext2.Mapping; use Ext2.Mapping;
with Ext2.Metadata; use Ext2.Metadata;
with Ext2.Resize; use Ext2.Resize;

package body Ext2.Removal is

   use type System.Address;

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
         Ext2.Lookup.resolvePath (fs, path (path'First .. leafFirst - 2), parent, status);
      end if;
      if status /= Lookup_Found then
         parent := 0;
      end if;
   end resolveParent;

   function directOnlyDirectory (fs : Filesystem; dir : Inode) return Boolean is
     (writableDirectory (fs, dir) and then
      Unsigned_64 (dir.sizeLo) <=
        Unsigned_64 (NUM_DIRECT_BLOCKS) * Unsigned_64 (fs.blkSize) and then
      dir.singleIndirectBlock = 0 and then dir.doubleIndirectBlock = 0);

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

   function deletionTime (fs : Filesystem) return Unsigned_32 is
     (Unsigned_32'Max (1, (if Now_Seconds /= 0 then Now_Seconds else fs.sb.lastWriteTime)));

   --  A file with no data, no blocks and no block pointers.
   function emptyFile (ino : Inode) return Boolean is
     (ino.sizeLo = 0 and then ino.sizeHi_DirACL = 0 and then
      ino.numDiskSectors = 0 and then
      (for all b of ino.directBlocks => b = 0) and then
      ino.singleIndirectBlock = 0 and then ino.doubleIndirectBlock = 0 and then
      ino.tripleIndirectBlock = 0);

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
      blockValid : Boolean;
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
         directoryBlock (fs, dir, index, blockNumber, blockValid, readStatus);
         if not blockValid then
            status := (if readStatus = Read_Complete then Remove_Malformed
                       else removeReadFailure (readStatus));
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

   procedure freeInode
     (fs : in out Filesystem; inodeNum : Unsigned_32; directory : Boolean;
      status : out Write_Status)
   is
      layout : constant Ext2_Geometry.Geometry := Shape (fs);
      bitmapBuf : Ext2_Bitmaps.Bitmap (0 .. Maximum_Bitmap_Bytes - 1) with Alignment => 8;
      readSize : constant Unsigned_32 := Unsigned_32'Min
        ((fs.sb.inodesPerBlockGroup + 7) / 8, Unsigned_32 (bitmapBuf'Length));
      group : Group_Number;
      index : Ext2_Bitmaps.Bit_Index;
      byteAt : Natural;
      bgd : BlockGroupDescriptor;
      readStatus : Read_Status;
   begin
      status := Write_Device_Error;
      if fs.sb.inodesPerBlockGroup = 0 or else inodeNum = 0 or else
        inodeNum > fs.sb.inodeCount or else fs.sb.freeInodes >= fs.sb.inodeCount
      then
         return;
      end if;
      group := Ext2_Geometry.Group_Of_Inode (layout, Inode_Number (inodeNum));
      if Ext2_Geometry.Index_In_Group (layout, Inode_Number (inodeNum)) / Bits_Per_Byte >=
        Count_32 (readSize)
      then
         status := Write_Out_Of_Range;
         return;
      end if;
      index := Natural (Ext2_Geometry.Index_In_Group (layout, Inode_Number (inodeNum)));
      byteAt := index / Bits_Per_Byte;
      readBGD (fs, group, bgd, readStatus);
      if readStatus /= Read_Complete then
         return;
      end if;
      --  Only the inode's bitmap byte is read and written.
      readBytes
        (fs, Storage_Offset (Ext2_Geometry.Block_Offset
                               (layout, Block_Number (bgd.inodeBitmapAddr))) +
           Storage_Offset (byteAt),
         bitmapBuf (byteAt)'Address, 1, readStatus);
      if readStatus /= Read_Complete or else
        not Ext2_Bitmaps.Is_Set (bitmapBuf, index) or else
        Unsigned_32 (bgd.numFreeInodes) >= fs.sb.inodesPerBlockGroup or else
        (directory and then bgd.numDirectories = 0)
      then
         return;
      end if;
      Ext2_Bitmaps.Clear (bitmapBuf, index);
      writeBytes
        (fs, Storage_Offset (Ext2_Geometry.Block_Offset
                               (layout, Block_Number (bgd.inodeBitmapAddr))) +
           Storage_Offset (byteAt),
         bitmapBuf (byteAt)'Address, 1, status, Allocation_Metadata);
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
         Ext2.Metadata.readInode (fs, inodeNum, ino, readStatus);
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
         Ext2.Metadata.readInode (fs, current, ino, readStatus);
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
      Ext2.Metadata.readInode (fs, inodeNum, ino, readStatus);
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

   procedure releaseUnlinked
     (fs : in out Filesystem; target : Unsigned_32; ino : in out Inode;
      keepOrphan : Boolean; inodeNum : out Unsigned_32; unlinked : out Inode;
      status : out Remove_Status)
   is
      writeStatus : Write_Status;
      readStatus : Read_Status;
   begin
      inodeNum := 0;
      unlinked := NULL_INODE;
      status := Remove_Complete;
      --  The name is gone; a crash here leaves a linked, unnamed inode
      --  (e2fsck moves it to lost+found). Journaled, both are one operation.
      ino.numHardLinks := 0;
      Stamp (ino, Inode_Changed);
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
            Ext2.Metadata.readInode (fs, target, ino, readStatus);
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
   end releaseUnlinked;

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
         Ext2.Metadata.readInode (fs, parentNum, dir, readStatus);
         if readStatus /= Read_Complete then
            status := removeReadFailure (readStatus);
            return;
         elsif inodeType (dir) /= INODE_DIRECTORY then
            status := Remove_Not_Found;
            return;
         elsif not writableDirectory (fs, dir) then
            status := Remove_Unsupported;
            return;
         end if;
         Ext2.Lookup.lookupInDir (fs, dir, leaf, target, lookup);
         if lookup /= Lookup_Found then
            status := removeLookupFailure (lookup);
            return;
         end if;
         Ext2.Metadata.readInode (fs, target, ino, readStatus);
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
         unindexDirectory (fs, parentNum, dir, writeStatus);
         if writeStatus /= Write_Complete then
            fs.writeQuarantined := True;
            status := Remove_Recovery_Required;
            stopHandle (fs);
            return;
         end if;
         removeEntry (fs, dir, leaf, target, status);
         if status /= Remove_Complete then
            uncertainDirectory (fs, parentNum);
            stopHandle (fs);
            return;
         end if;
         --  The parent lost an entry: its content changed.
         declare
            timeStatus : Write_Status := Write_Complete;
            changed : Boolean;
         begin
            Stamp (dir, Content_Changed, changed);
            if changed then
               writeInode (fs, parentNum, dir, timeStatus);
            end if;
            if timeStatus /= Write_Complete then
               fs.writeQuarantined := True;
               status := Remove_Recovery_Required;
               stopHandle (fs);
               return;
            end if;
         end;
      end;
      releaseUnlinked (fs, target, ino, keepOrphan, inodeNum, unlinked, status);
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
      Ext2.Directories.makeDirectory
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
         Ext2.Metadata.readInode (fs, parentNum, parent, readStatus);
         if readStatus /= Read_Complete then
            status := removeReadFailure (readStatus);
            return;
         elsif inodeType (parent) /= INODE_DIRECTORY then
            status := Remove_Not_Found;
            return;
         elsif not writableDirectory (fs, parent) then
            status := Remove_Unsupported;
            return;
         end if;
         Ext2.Lookup.lookupInDir (fs, parent, leaf, target, lookup);
         if lookup /= Lookup_Found then
            status := removeLookupFailure (lookup);
            return;
         elsif target = ROOT_INODE or else target = parentNum then
            status := Remove_Malformed;
            return;
         end if;
         Ext2.Metadata.readInode (fs, target, dir, readStatus);
         if readStatus /= Read_Complete then
            status := removeReadFailure (readStatus);
            return;
         elsif inodeType (dir) /= INODE_DIRECTORY then
            status := Remove_Wrong_Type;
            return;
         elsif not directOnlyDirectory (fs, dir) or else dir.fileACL /= 0 or else
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
         unindexDirectory (fs, parentNum, parent, writeStatus);
         if writeStatus /= Write_Complete then
            fs.writeQuarantined := True;
            status := Remove_Recovery_Required;
            return;
         end if;
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
            retired (index) := Block_Number (dir.directBlocks (index - 1));
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
         Stamp (parent, Content_Changed);
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

end Ext2.Removal;
