------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  See the spec.
------------------------------------------------------------------------------
pragma Ada_2022;
with Ext2_Support; use type Ext2_Support.File_Admission;
with Ext2.Lookup;
with Dentry_Cache;
with Directory_Commit;
with CuBit.Directory_Paths;
with CuBit.Block_Devices; use CuBit.Block_Devices;
with Ext2_Format; use Ext2_Format;
with Ext2.Allocation; use Ext2.Allocation;
with Ext2.Block_Cache; use Ext2.Block_Cache;
with Ext2.Files; use Ext2.Files;
with Ext2.Journal; use Ext2.Journal;
with Ext2.Mapping; use Ext2.Mapping;
with Ext2.Metadata; use Ext2.Metadata;

package body Ext2.Directories is

   use type System.Address;

   --  The most blocks a directory may have: what readDirectoryPage follows
   --  (direct, single and double indirect).
   function directoryBlockLimit (fs : Filesystem) return Unsigned_64 is
     (Unsigned_64 (NUM_DIRECT_BLOCKS) + Unsigned_64 (fs.blkSize / 4) +
      Unsigned_64 (fs.blkSize / 4) * Unsigned_64 (fs.blkSize / 4));

   function writableDirectory (fs : Filesystem; dir : Inode) return Boolean is
     (inodeType (dir) = INODE_DIRECTORY and then
      (dir.flags and not Index_Flag) = 0 and then
      dir.sizeLo /= 0 and then dir.sizeLo mod fs.blkSize = 0 and then
      dir.sizeHi_DirACL = 0 and then dir.tripleIndirectBlock = 0 and then
      Unsigned_64 (dir.sizeLo / fs.blkSize) <= directoryBlockLimit (fs));

   procedure directoryBlock
     (fs : Filesystem; dir : Inode; index : Natural; number : out Unsigned_32;
      valid : out Boolean; readStatus : out Read_Status)
   is
      leaf, middle : Unsigned_32;
   begin
      resolveBlock (fs, dir, Unsigned_32 (index), number, leaf, middle, readStatus);
      valid := readStatus = Read_Complete and then
        number >= fs.sb.firstDataBlock and then number < fs.sb.blockCount;
   end directoryBlock;

   procedure unindexDirectory
     (fs : in out Filesystem; number : Unsigned_32; dir : in out Inode;
      status : out Write_Status)
   is
   begin
      status := Write_Complete;
      if (dir.flags and Index_Flag) /= 0 then
         dir.flags := dir.flags and not Index_Flag;
         writeInode (fs, number, dir, status);
      end if;
   end unindexDirectory;

   --  Add one empty block (Directory_Blocks.Empty_Block) to a writable
   --  directory, allocated and attached by appendRun as journaled directory
   --  data, then publish the larger size. Interrupted after the attach, the
   --  directory merely has an empty block; ENOSPC writes nothing.
   procedure growDirectory
     (fs : in out Filesystem; number : Unsigned_32; dir : in out Inode;
      status : out Write_Status)
   is
      contents : Directory_Blocks.Block_Data;
      grown : Inode := dir;
      consumed : Unsigned_64;
   begin
      if Unsigned_64 (dir.sizeLo / fs.blkSize) >= directoryBlockLimit (fs) or else
        Unsigned_64 (dir.sizeLo) + Unsigned_64 (fs.blkSize) >
          Unsigned_64 (Unsigned_32'Last)
      then
         status := Write_File_Range_Unsupported;
         return;
      end if;
      Directory_Blocks.Empty_Block
        (contents, Directory_Blocks.Block_Length (fs.blkSize));
      appendRun (fs, grown, Unsigned_64 (dir.sizeLo), contents'Address,
                 Unsigned_64 (fs.blkSize), consumed, status, Directory_Data);
      if status /= Write_Complete then
         return;
      elsif consumed /= Unsigned_64 (fs.blkSize) then
         fs.writeQuarantined := True;
         status := Write_Recovery_Required;
         return;
      end if;
      grown.sizeLo := dir.sizeLo + fs.blkSize;
      writeInode (fs, number, grown, status);
      if status /= Write_Complete then
         fs.writeQuarantined := True;
         status := Write_Recovery_Required;
         return;
      end if;
      dir := grown;
   end growDirectory;

   procedure findRoom
     (fs : in out Filesystem; number : Unsigned_32; dir : in out Inode;
      needed : Unsigned_32; targetBlock : out Unsigned_32; buffer : out String;
      entryOffset : out Storage_Offset; entrySpan : out Unsigned_16;
      status : out Write_Status)
   is
      blocks : constant Natural := Natural (dir.sizeLo / fs.blkSize);
      blockValid, found : Boolean := False;
      readStatus : Read_Status;

      procedure Uncertain is
      begin
         fs.writeQuarantined := True;
         status := Write_Recovery_Required;
      end Uncertain;
   begin
      targetBlock := 0;
      buffer := [others => Character'Val (0)];
      entryOffset := 0;
      entrySpan := 0;
      unindexDirectory (fs, number, dir, status);
      if status /= Write_Complete then
         Uncertain;
         return;
      end if;
      for index in reverse 0 .. blocks - 1 loop
         directoryBlock (fs, dir, index, targetBlock, blockValid, readStatus);
         if blockValid then
            readBlock (fs, targetBlock, buffer'Address, readStatus);
         end if;
         if not blockValid or else readStatus /= Read_Complete then
            status := Write_Device_Error;
            return;
         end if;
         declare
            position : Storage_Offset := 0;
         begin
            while position < Storage_Offset (fs.blkSize) loop
               --  Validate serialized record bounds before overlay/access.
               if Storage_Offset (fs.blkSize) - position < Record_Header_Bytes then
                  status := Write_Device_Error;
                  return;
               end if;
               declare
                  dent : DirectoryEntry
                    with Import, Address => buffer'Address + position;
                  used : Unsigned_16;
               begin
                  if not Fits ((Inode => Inode_Number (dent.inode),
                                Span => Natural (dent.length),
                                Length => Natural (dent.nameLength),
                                File_Type => dent.fileType),
                               Natural (Storage_Offset (fs.blkSize) - position))
                  then
                     status := Write_Device_Error;
                     return;
                  end if;
                  used := (if dent.inode = 0 then 0 else
                    Unsigned_16 (Record_Bytes (Natural (dent.nameLength))));
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

      if not found then
         --  No room: grow by an empty block (published with the new size;
         --  harmless if what follows fails), and insert there.
         growDirectory (fs, number, dir, status);
         if status /= Write_Complete then
            return;
         end if;
         directoryBlock (fs, dir, blocks, targetBlock, blockValid, readStatus);
         if blockValid then
            readBlock (fs, targetBlock, buffer'Address, readStatus);
         end if;
         if not blockValid or else readStatus /= Read_Complete then
            Uncertain;
            return;
         end if;
         entryOffset := 0;
         entrySpan := Unsigned_16 (fs.blkSize);
      end if;
      status := Write_Complete;
   end findRoom;

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
      Ext2.Metadata.readInode (fs, dirInodeNum, dirIno, readStatus);
      if readStatus /= Read_Complete then
         status := (if readStatus = Read_Out_Of_Range then
                       Rename_Out_Of_Range else Rename_IO_Error);
         return;
      elsif inodeType (dirIno) /= INODE_DIRECTORY then
         status := Rename_Malformed;
         return;
      elsif not writableDirectory (fs, dirIno) then
         --  Other inode flags may require semantics we do not implement. An
         --  htree index is cleared before the records change (below).
         status := Rename_Range_Unsupported;
         return;
      end if;

      --  Preflight the whole directory, not just the source block: another
      --  block may already contain the requested destination.
      Ext2.Lookup.lookupInDir (fs, dirIno, oldName, identity, lookupStatus);
      if lookupStatus /= Lookup_Found then
         status := Lookup_Failure (lookupStatus);
         return;
      end if;
      if oldName = newName then
         status := Rename_Complete;
         return;
      end if;
      Ext2.Lookup.lookupInDir (fs, dirIno, newName, identity, lookupStatus);
      if lookupStatus = Lookup_Found then
         status := Rename_Destination_Exists;
         return;
      elsif lookupStatus /= Lookup_Not_Found then
         status := Lookup_Failure (lookupStatus);
         return;
      end if;
      declare
         unindexed : Write_Status;
      begin
         unindexDirectory (fs, dirInodeNum, dirIno, unindexed);
         if unindexed /= Write_Complete then
            fs.writeQuarantined := True;
            status := Rename_Recovery_Required;
            return;
         end if;
      end;

      for index in 0 .. Natural (dirIno.sizeLo / fs.blkSize) - 1 loop
         declare
            valid : Boolean;
         begin
            directoryBlock (fs, dirIno, index, blockNumber, valid, readStatus);
            if not valid then
               status := (if readStatus = Read_Complete then Rename_Malformed
                          elsif readStatus = Read_Out_Of_Range then Rename_Out_Of_Range
                          else Rename_IO_Error);
               return;
            end if;
         end;
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
                  when Committer.Committed =>
                     status := Rename_Complete;
                     --  The directory's entries changed.
                     declare
                        timeStatus : Write_Status := Write_Complete;
                        changed : Boolean;
                     begin
                        Stamp (dirIno, Content_Changed, changed);
                        if changed then
                           writeInode (fs, dirInodeNum, dirIno, timeStatus);
                        end if;
                        if timeStatus /= Write_Complete then
                           fs.writeQuarantined := True;
                           status := Rename_Recovery_Required;
                        end if;
                     end;
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
               status := Rename_No_Room;
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

   --  Create an empty regular file or directory. Prepare the directory
   --  insertion before reserving anything; publish the initialized inode
   --  (and a directory's "."/".." block before it) before its name. A new
   --  directory's parent gains its ".." link before anything refers to it
   --  (an over-count is harmless). A parent with no room first grows by an
   --  empty block (growDirectory), so any directory size works.
   procedure createNodeInner
     (fs : in out Filesystem; dirInodeNum : Unsigned_32; name : String;
      directory : Boolean; inodeNum : out Unsigned_32; status : out Write_Status)
   is
      parent : Inode;
      fresh : Inode := NULL_INODE;
      readStatus : Read_Status;
      lookupStatus : Directory_Lookup_Status;
      existing, reservedInode, targetBlock : Unsigned_32 := 0;
      buffer : String (1 .. Natural (fs.blkSize)) := [others => Character'Val (0)]
        with Alignment => 8;
      entryOffset : Storage_Offset := 0;
      entrySpan : Unsigned_16 := 0;
      needed : Unsigned_32;
      cleanupStatus : Write_Status;
      knownAbsent : Boolean := False;
      parentTimes : Boolean;
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
      needed := Unsigned_32 (Record_Bytes (name'Length));
      --  A cached negative entry already proves the name absent.
      if Dentry_Cache.Cacheable (name) then
         declare
            cached : Boolean;
            found : Unsigned_32;
         begin
            Dentry_Cache.Find
              (Names, Dentry_Cache.Make_Key (fs.device.volumeKey, dirInodeNum, name),
               cached, found);
            knownAbsent := (cached and then found = Dentry_Cache.No_Inode) or else
              (not cached and then completeDirectory (fs, dirInodeNum));
         end;
      end if;
      forgetName (fs, dirInodeNum, name);
      Ext2.Metadata.readInode (fs, dirInodeNum, parent, readStatus);
      if readStatus /= Read_Complete then
         status := Write_Device_Error;
         return;
      elsif inodeType (parent) /= INODE_DIRECTORY then
         return;
      elsif not writableDirectory (fs, parent) then
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
         Ext2.Lookup.lookupInDir (fs, parent, name, existing, lookupStatus);
      end if;
      if lookupStatus = Lookup_Found then
         status := Write_Already_Exists;
         return;
      elsif lookupStatus /= Lookup_Not_Found then
         status := Write_Device_Error;
         return;
      end if;

      findRoom (fs, dirInodeNum, parent, needed, targetBlock, buffer,
                entryOffset, entrySpan, status);
      if status /= Write_Complete then
         return;
      end if;

      if directory then
         Ext2.Allocation.allocateBlock (fs, ownBlock, status);
         if status /= Write_Complete then
            return;
         end if;
      end if;

      Ext2.Allocation.allocateInode (fs, reservedInode, status, directory);
      if status /= Write_Complete then
         if status = Write_No_Space and then directory then
            --  The block was never published. Only a definite no-space
            --  rejection permits cleanup; transport uncertainty must stop.
            releaseBlocks (fs, [1 => Block_Number (ownBlock)], cleanupStatus);
            if cleanupStatus /= Write_Complete then
               Uncertain;
            end if;
         elsif directory then
            Uncertain;
         end if;
         return;
      end if;

      --  The parent gains an entry: its content changes.
      Stamp (parent, Content_Changed, parentTimes);
      if directory then
         --  The parent's ".." link first: until the name exists it is an
         --  over-count, which is harmless.
         parent.numHardLinks := parent.numHardLinks + 1;
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
      Stamp (fresh, Inode_Created);
      writeInode (fs, reservedInode, fresh, status);
      if status /= Write_Complete then
         Uncertain;
         return;
      end if;
      declare
         dent : DirectoryEntry
           with Import, Address => buffer'Address + entryOffset;
         entryName : String (1 .. name'Length)
           with Import, Address => buffer'Address + entryOffset + Record_Header_Bytes;
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
      if not directory and then parentTimes then
         --  A new file in an existing block: only the parent's times change
         --  (a new directory already wrote them with its ".." link).
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

end Ext2.Directories;
