------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  See the spec.
------------------------------------------------------------------------------
pragma Ada_2022;
with Ext2_Support; use type Ext2_Support.File_Admission;
with Ext2.Metadata;
with Ext2_Format; use Ext2_Format;
with Dentry_Cache;
with CuBit.File_Access;
with CuBit.Directory_Paths;
with System.Storage_Elements; use System.Storage_Elements;
with Ext2.Block_Cache; use Ext2.Block_Cache;

package body Ext2.Lookup is

   use type System.Address;

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
                  item : Record_Header;
                  span, length : Natural;
                  number : Unsigned_32;
               begin
                  while position < blockBytes loop
                     if blockBytes - position < Record_Header_Bytes then
                        clean := False;
                        exit;
                     end if;
                     item := Header (data, position);
                     if not Fits (item, blockBytes - position) or else
                       Unsigned_32 (item.Inode) > fs.sb.inodeCount or else
                       (item.Inode /= No_Inode and then item.Length = 0)
                     then
                        clean := False;
                        exit;
                     end if;
                     span := item.Span;
                     length := item.Length;
                     number := Unsigned_32 (item.Inode);
                     if number /= 0 and then length = name'Length then
                        declare
                           matches : Boolean := True;
                        begin
                           for k in 0 .. length - 1 loop
                              if Character'Val (data (position + Record_Header_Bytes + k)) /=
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
      entries   : Listed_Records;
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
                    entries (index).length = name'Length;
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
                     inodeNum := entries (index).inode;
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
            item : Record_Header;
            span, length : Natural;
            entryInode : Unsigned_32;
         begin
            while position < blockBytes loop
               if blockBytes - position < Record_Header_Bytes then
                  return;
               end if;
               item := Header (data, position);
               span := item.Span;
               length := item.Length;
               entryInode := Unsigned_32 (item.Inode);
               if not Fits (item, blockBytes - position) or else
                 entryInode > fs.sb.inodeCount or else
                 (item.Inode /= No_Inode and then length = 0)
               then
                  return;
               end if;
               if entryInode /= 0 then
                  declare
                     name : String (1 .. length);
                  begin
                     for k in 1 .. length loop
                        name (k) := Character'Val (data (position + Record_Header_Bytes - 1 + k));
                     end loop;
                     if name /= "." and then name /= ".." then
                        if not Dentry_Cache.Cacheable (name) then
                           clean := False;
                        else
                           Dentry_Cache.Insert
                             (Names,
                              Dentry_Cache.Make_Key (fs.device.volumeKey, number, name),
                              entryInode, displaced, from);
                           if displaced and then from.Parent = number and then
                             from.Volume = fs.device.volumeKey
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
         Dentry_Cache.Mark_Complete (Names, fs.device.volumeKey, number);
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
                    (Names, Dentry_Cache.Make_Key (fs.device.volumeKey, parent, name),
                     cached, found);
               end if;
               if cached and then found = Dentry_Cache.No_Inode then
                  status := Lookup_Not_Found;
                  return;
               elsif cached then
                  current := found;
               else
                  Ext2.Metadata.readInode (fs, current, ino, readStatus);
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
                       (Names, Dentry_Cache.Make_Key (fs.device.volumeKey, parent, name),
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

   procedure readDirectoryPage
     (fs          : Filesystem;
      dirIno      : Inode;
      cursor      : Unsigned_64;
      entries     : out Listed_Records;
      entryCount  : out Natural;
      nextCursor  : out Unsigned_64;
      status      : out Directory_Read_Status)
   is
      package DP renames CuBit.Directory_Pages;
      size : constant Unsigned_64 := fileSize (dirIno);
      blockBuf : String (1 .. Natural (fs.blkSize))
        with Alignment => 8;
      --  From the first record of the cursor's block (see the spec).
      scanCursor : Unsigned_64 := cursor - cursor mod Unsigned_64 (fs.blkSize);
      --  The page's bytes, as its records are counted (CuBit.Directory_Pages).
      pageUsed : Natural := DP.Header_Bytes;
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
      entries := [others => (others => <>)];
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
            if blockRemaining < Record_Header_Bytes or else
               fileRemaining < Record_Header_Bytes
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
               item : constant Record_Header :=
                 (Inode => Inode_Number (dent.inode), Span => Natural (dent.length),
                  Length => Natural (dent.nameLength), File_Type => dent.fileType);
               recordLength : constant Unsigned_64 :=
                 Unsigned_64 (dent.length);
               nameLength : constant Natural := Natural (dent.nameLength);
            begin
               --  Within its block and the directory's size.
               if not Fits (item, Natural (Unsigned_64'Min (blockRemaining, fileRemaining)))
                 or else dent.inode > fs.sb.inodeCount
               then
                  status := Directory_Malformed;
                  return;
               end if;

               if dent.inode /= 0 and then nameLength > 0 then
                  declare
                     entryName : String (1 .. nameLength)
                       with Import,
                            Address => blockBuf'Address +
                              Storage_Offset (blockOffset) + Record_Header_Bytes;
                     isDot : constant Boolean :=
                       (nameLength = 1 and then entryName (1) = '.') or else
                       (nameLength = 2 and then entryName (1) = '.' and then
                        entryName (2) = '.');
                  begin
                     if not isDot and then scanCursor >= cursor then
                        if entryCount = DP.Maximum_Entries or else
                          pageUsed > DP.Page_Bytes - DP.Record_Bytes (nameLength)
                        then
                           nextCursor := scanCursor;
                           status := Directory_Page_Complete;
                           return;
                        end if;
                        pageUsed := pageUsed + DP.Record_Bytes (nameLength);

                        entries (entryCount).inode := dent.inode;
                        entries (entryCount).length := nameLength;
                        entries (entryCount).kind :=
                          (case dent.fileType is
                              when FILETYPE_REGULAR => DP.Kind_File,
                              when FILETYPE_DIRECTORY => DP.Kind_Directory,
                              when FILETYPE_SYMLINK => DP.Kind_Symlink,
                              when others => DP.Kind_Unknown);
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

end Ext2.Lookup;
