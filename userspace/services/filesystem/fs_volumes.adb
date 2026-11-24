------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  A writable volume's format (see the spec).
------------------------------------------------------------------------------
package body Fs_Volumes is

   use Volume_Admission;
   use all type Fat32.Status;
   use all type Ext2.Read_Status, Ext2.Directory_Lookup_Status, Ext2.Directory_Read_Status,
     Ext2.Write_Status, Ext2.Truncate_Status, Ext2.Rename_Status, Ext2.Remove_Status,
     Ext2.Flush_Status;

   --  Fat32's results as the service's statuses of the same meaning.
   function Read_Of (S : Fat32.Status) return Ext2.Read_Status is
     (case S is
        when Done | Listing_End => Read_Complete,
        when Out_Of_Range | Malformed => Read_Out_Of_Range,
        when Range_Unsupported | Capacity => Read_File_Range_Unsupported,
        when Wrong_Object | Is_Directory | Not_Directory | Not_Found => Read_Object_Unsupported,
        when others => Read_Device_Error);

   function Lookup_Of (S : Fat32.Status) return Ext2.Directory_Lookup_Status is
     (case S is
        when Done => Lookup_Found,
        when Not_Found | Not_Directory => Lookup_Not_Found,
        when Device_Error => Lookup_Device_Error,
        when Out_Of_Range => Lookup_Out_Of_Range,
        when Range_Unsupported | Capacity => Lookup_Range_Unsupported,
        when others => Lookup_Malformed);

   function Directory_Of (S : Fat32.Status) return Ext2.Directory_Read_Status is
     (case S is
        when Done => Directory_Page_Complete,
        when Listing_End => Directory_End,
        when Device_Error => Directory_Device_Error,
        when Out_Of_Range => Directory_Out_Of_Range,
        when Range_Unsupported | Capacity => Directory_Range_Unsupported,
        when others => Directory_Malformed);

   function Write_Of (S : Fat32.Status) return Ext2.Write_Status is
     (case S is
        when Done => Write_Complete,
        when Read_Only => Write_Read_Only,
        when Out_Of_Range | Malformed => Write_Out_Of_Range,
        when Device_Error => Write_Device_Error,
        when Recovery_Required => Write_Recovery_Required,
        when Exists => Write_Already_Exists,
        when No_Space => Write_No_Space,
        when Range_Unsupported | Capacity => Write_File_Range_Unsupported,
        when others => Write_Object_Unsupported);

   function Truncate_Of (S : Fat32.Status) return Ext2.Truncate_Status is
     (case S is
        when Done => Truncate_Complete,
        when Read_Only => Truncate_Read_Only,
        when Device_Error => Truncate_IO_Error,
        when Recovery_Required => Truncate_Recovery_Required,
        when Range_Unsupported | Capacity | No_Space => Truncate_Unsupported,
        when others => Truncate_Invalid);

   function Rename_Of (S : Fat32.Status) return Ext2.Rename_Status is
     (case S is
        when Done => Rename_Complete,
        when Not_Found | Wrong_Object => Rename_Source_Not_Found,
        when Exists => Rename_Destination_Exists,
        when Invalid_Name => Rename_Invalid_Name,
        when Range_Unsupported | Capacity | No_Space | Listing_End => Rename_Range_Unsupported,
        when Read_Only => Rename_Read_Only,
        when Out_Of_Range => Rename_Out_Of_Range,
        when Device_Error => Rename_IO_Error,
        when Recovery_Required => Rename_Recovery_Required,
        when Not_Directory => Rename_Not_Directory,
        when Is_Directory => Rename_Is_Directory,
        when Not_Empty => Rename_Not_Empty,
        when Invalid_Move => Rename_Invalid_Move,
        when Malformed => Rename_Malformed);

   function Remove_Of (S : Fat32.Status) return Ext2.Remove_Status is
     (case S is
        when Done => Remove_Complete,
        when Not_Found => Remove_Not_Found,
        when Invalid_Name => Remove_Invalid_Name,
        when Is_Directory | Not_Directory | Wrong_Object => Remove_Wrong_Type,
        when Not_Empty => Remove_Not_Empty,
        when Range_Unsupported | Capacity | No_Space | Exists | Invalid_Move | Listing_End =>
          Remove_Unsupported,
        when Read_Only => Remove_Read_Only,
        when Out_Of_Range => Remove_Out_Of_Range,
        when Device_Error => Remove_IO_Error,
        when Recovery_Required => Remove_Recovery_Required,
        when Malformed => Remove_Malformed);

   function Flush_Of (S : Fat32.Status) return Ext2.Flush_Status is
     (case S is
        when Done => Flush_Complete,
        when Recovery_Required => Flush_Recovery_Required,
        when others => Flush_IO_Error);

   procedure initBlockDevice
     (fs : out Store; endpoint : CuBit.Handles.Endpoint; volume : Unsigned_64;
      grant : CuBit.Memory_Grants.Grant_Reference; grantBuf : System.Address; grantBytes : Unsigned_32;
      window : Ext2.Volume_Window; result : out Volume_Admission.Admission_Result) is
   begin
      fs.Format := Ext_Format;
      Ext2.initBlockDevice (fs.Ext, endpoint, volume, grant, grantBuf, grantBytes, window, result);
      if result = Unsupported_Filesystem then
         --  Not ext2's: a FAT32 volume, or nothing this service reads.
         Fat32.Mount (fs.Fat, endpoint, volume, grant, grantBuf, grantBytes,
                      (First => window.First, Blocks => window.Blocks), result);
         if result = Admitted then
            fs.Format := Fat32_Format;
         end if;
      end if;
   end initBlockDevice;

   function Session (fs : Store) return CuBit.Block_Devices.Device_Session is
     (case fs.Format is
        when Ext_Format => fs.Ext.device,
        when Fat32_Format => Fat32.Session (fs.Fat));

   function Quarantined (fs : Store) return Boolean is
     (case fs.Format is
        when Ext_Format => fs.Ext.writeQuarantined,
        when Fat32_Format => Fat32.Quarantined (fs.Fat));

   procedure Retire (fs : in out Store) is
   begin
      case fs.Format is
         when Ext_Format => fs.Ext.writeQuarantined := True;
         when Fat32_Format => Fat32.Retire (fs.Fat);
      end case;
   end Retire;

   procedure readInode
     (fs : Store; inodeNum : Unsigned_32; ino : out Ext2.Inode; status : out Ext2.Read_Status)
   is
      Fat : Fat32.Volume := fs.Fat;   --  its stores are shared; reads change nothing else
      S : Fat32.Status;
   begin
      case fs.Format is
         when Ext_Format =>
            Ext2.readInode (fs.Ext, inodeNum, ino, status);
         when Fat32_Format =>
            Fat32.Read_Object (Fat, inodeNum, ino, S);
            status := Read_Of (S);
            if S = Not_Found or else S = Wrong_Object then
               status := Read_Out_Of_Range;
            end if;
      end case;
   end readInode;

   procedure lookupInDir
     (fs : Store; dirIno : Ext2.Inode; name : String; inodeNum : out Unsigned_32;
      status : out Ext2.Directory_Lookup_Status)
   is
      Fat : Fat32.Volume := fs.Fat;
      S : Fat32.Status;
   begin
      case fs.Format is
         when Ext_Format =>
            Ext2.lookupInDir (fs.Ext, dirIno, name, inodeNum, status);
         when Fat32_Format =>
            Fat32.Lookup (Fat, dirIno, name, inodeNum, S);
            status := Lookup_Of (S);
      end case;
   end lookupInDir;

   procedure resolvePath
     (fs : Store; path : String; inodeNum : out Unsigned_32; status : out Ext2.Directory_Lookup_Status)
   is
      Fat : Fat32.Volume := fs.Fat;
      S : Fat32.Status;
   begin
      case fs.Format is
         when Ext_Format =>
            Ext2.resolvePath (fs.Ext, path, inodeNum, status);
         when Fat32_Format =>
            Fat32.Resolve (Fat, path, inodeNum, S);
            status := Lookup_Of (S);
            if S /= Done then
               inodeNum := 0;
            end if;
      end case;
   end resolvePath;

   procedure readDirectoryPage
     (fs : Store; dirIno : Ext2.Inode; cursor : Unsigned_64; entries : out Ext2.Listed_Records;
      entryCount : out Natural; nextCursor : out Unsigned_64; status : out Ext2.Directory_Read_Status)
   is
      Fat : Fat32.Volume := fs.Fat;
      S : Fat32.Status;
      Listed : Fat32.Listed_Records;
   begin
      case fs.Format is
         when Ext_Format =>
            Ext2.readDirectoryPage (fs.Ext, dirIno, cursor, entries, entryCount, nextCursor, status);
         when Fat32_Format =>
            entries := [others => (others => <>)];
            Fat32.Read_Directory_Page (Fat, dirIno, cursor, Listed, entryCount, nextCursor, S);
            for K in 0 .. entryCount - 1 loop
               entries (K) := (inode => Listed (K).Number, kind => Listed (K).Kind,
                               length => Listed (K).Length, name => Listed (K).Name);
            end loop;
            status := Directory_Of (S);
      end case;
   end readDirectoryPage;

   procedure readData
     (fs : Store; ino : Ext2.Inode; offset : Unsigned_64; buf : System.Address; count : Unsigned_64;
      bytesRead : out Unsigned_64; status : out Ext2.Read_Status)
   is
      Fat : Fat32.Volume := fs.Fat;
      S : Fat32.Status;
   begin
      case fs.Format is
         when Ext_Format =>
            Ext2.readData (fs.Ext, ino, offset, buf, count, bytesRead, status);
         when Fat32_Format =>
            Fat32.Read_Data (Fat, ino, offset, buf, count, bytesRead, S);
            status := Read_Of (S);
      end case;
   end readData;

   procedure writeData
     (fs : in out Store; inodeNum : Unsigned_32; ino : in out Ext2.Inode; offset : Unsigned_64;
      buf : System.Address; count : Unsigned_64; bytesWritten : out Unsigned_64;
      status : out Ext2.Write_Status)
   is
      S : Fat32.Status;
   begin
      case fs.Format is
         when Ext_Format =>
            Ext2.writeData (fs.Ext, inodeNum, ino, offset, buf, count, bytesWritten, status);
         when Fat32_Format =>
            Fat32.Write_Data (fs.Fat, inodeNum, ino, offset, buf, count, bytesWritten, S);
            status := Write_Of (S);
      end case;
   end writeData;

   procedure createFile
     (fs : in out Store; dirInodeNum : Unsigned_32; name : String; inodeNum : out Unsigned_32;
      status : out Ext2.Write_Status)
   is
      S : Fat32.Status;
   begin
      case fs.Format is
         when Ext_Format =>
            Ext2.createFile (fs.Ext, dirInodeNum, name, inodeNum, status);
         when Fat32_Format =>
            Fat32.Create_File (fs.Fat, dirInodeNum, name, inodeNum, S);
            status := Write_Of (S);
            if S /= Done then
               inodeNum := 0;
            end if;
      end case;
   end createFile;

   procedure truncateToEmpty
     (fs : in out Store; inodeNum : Unsigned_32; emptyInode : out Ext2.Inode;
      status : out Ext2.Truncate_Status)
   is
      S : Fat32.Status;
   begin
      case fs.Format is
         when Ext_Format =>
            Ext2.truncateToEmpty (fs.Ext, inodeNum, emptyInode, status);
         when Fat32_Format =>
            Fat32.Resize (fs.Fat, inodeNum, 0, emptyInode, S);
            status := Truncate_Of (S);
      end case;
   end truncateToEmpty;

   procedure resizeFile
     (fs : in out Store; inodeNum : Unsigned_32; newSize : Unsigned_64; resizedInode : out Ext2.Inode;
      status : out Ext2.Truncate_Status)
   is
      S : Fat32.Status;
   begin
      case fs.Format is
         when Ext_Format =>
            Ext2.resizeFile (fs.Ext, inodeNum, newSize, resizedInode, status);
         when Fat32_Format =>
            Fat32.Resize (fs.Fat, inodeNum, newSize, resizedInode, S);
            status := Truncate_Of (S);
      end case;
   end resizeFile;

   procedure renamePath
     (fs : in out Store; oldPath, newPath : String; keepReplaced : Boolean;
      replacedNumber : out Unsigned_32; replaced : out Ext2.Inode; status : out Ext2.Rename_Status)
   is
      S : Fat32.Status;
   begin
      case fs.Format is
         when Ext_Format =>
            Ext2.renamePath (fs.Ext, oldPath, newPath, keepReplaced, replacedNumber, replaced, status);
         when Fat32_Format =>
            Fat32.Rename (fs.Fat, oldPath, newPath, keepReplaced, replacedNumber, replaced, S);
            status := Rename_Of (S);
      end case;
   end renamePath;

   procedure unlinkPath
     (fs : in out Store; path : String; keepOrphan : Boolean; inodeNum : out Unsigned_32;
      unlinked : out Ext2.Inode; status : out Ext2.Remove_Status)
   is
      S : Fat32.Status;
   begin
      case fs.Format is
         when Ext_Format =>
            Ext2.unlinkPath (fs.Ext, path, keepOrphan, inodeNum, unlinked, status);
         when Fat32_Format =>
            Fat32.Unlink (fs.Fat, path, keepOrphan, inodeNum, unlinked, S);
            status := Remove_Of (S);
      end case;
   end unlinkPath;

   procedure reclaimInode (fs : in out Store; inodeNum : Unsigned_32; status : out Ext2.Remove_Status) is
      S : Fat32.Status;
   begin
      case fs.Format is
         when Ext_Format =>
            Ext2.reclaimInode (fs.Ext, inodeNum, status);
         when Fat32_Format =>
            Fat32.Reclaim (fs.Fat, inodeNum, S);
            status := Remove_Of (S);
      end case;
   end reclaimInode;

   procedure makeDirectoryPath
     (fs : in out Store; path : String; inodeNum : out Unsigned_32;
      lookup : out Ext2.Directory_Lookup_Status; status : out Ext2.Write_Status)
   is
      S, Parent : Fat32.Status;
   begin
      case fs.Format is
         when Ext_Format =>
            Ext2.makeDirectoryPath (fs.Ext, path, inodeNum, lookup, status);
         when Fat32_Format =>
            Fat32.Make_Directory (fs.Fat, path, inodeNum, Parent, S);
            lookup := Lookup_Of (Parent);
            status := Write_Of (S);
      end case;
   end makeDirectoryPath;

   procedure removeDirectoryPath
     (fs : in out Store; path : String; inodeNum : out Unsigned_32; status : out Ext2.Remove_Status)
   is
      S : Fat32.Status;
   begin
      case fs.Format is
         when Ext_Format =>
            Ext2.removeDirectoryPath (fs.Ext, path, inodeNum, status);
         when Fat32_Format =>
            Fat32.Remove_Directory (fs.Fat, path, inodeNum, S);
            status := Remove_Of (S);
      end case;
   end removeDirectoryPath;

   procedure Flush (fs : in out Store; status : out Ext2.Flush_Status) is
      S : Fat32.Status;
   begin
      case fs.Format is
         when Ext_Format =>
            Ext2.Flush (fs.Ext, status);
         when Fat32_Format =>
            Fat32.Flush (fs.Fat, S);
            status := Flush_Of (S);
      end case;
   end Flush;

   --  FAT32 keeps nothing past a flush: detaching is flushing.
   procedure Detach (fs : in out Store; status : out Ext2.Flush_Status) is
   begin
      case fs.Format is
         when Ext_Format => Ext2.Detach (fs.Ext, status);
         when Fat32_Format => Flush (fs, status);
      end case;
   end Detach;

   function dirtyBlocks (fs : Store) return Boolean is
     (case fs.Format is
        when Ext_Format => Ext2.dirtyBlocks (fs.Ext),
        when Fat32_Format => Fat32.Unflushed (fs.Fat));

end Fs_Volumes;
