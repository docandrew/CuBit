------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  A writable volume's format, ext2/ext3 or FAT32 (FS-030), behind the one
--  set of operations the service's requests call.
--
--  @description
--  The operations keep Ext2's names, parameters and statuses (the service's
--  vocabulary: an object number, an ext2-shaped inode as its metadata, the
--  Ext2 status types), so a request handler is the same for both formats.
--  Admission tries ext2 first and FAT32 when the volume is not ext2's. FAT32
--  results are mapped onto the statuses with the same meaning (Fat32's).
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System;
with CuBit.Handles;
with CuBit.Memory_Grants;
with CuBit.Block_Devices;
with Volume_Admission;
with Ext2;
with Fat32;

package Fs_Volumes is

   type Volume_Format is (Ext_Format, Fat32_Format);

   type Store is record
      Format : Volume_Format := Ext_Format;
      Ext    : Ext2.Filesystem;
      Fat    : Fat32.Volume;
   end record;

   procedure initBlockDevice
     (fs : out Store; endpoint : CuBit.Handles.Endpoint; volume : Unsigned_64;
      grant : CuBit.Memory_Grants.Grant_Reference; grantBuf : System.Address; grantBytes : Unsigned_32;
      window : Ext2.Volume_Window; result : out Volume_Admission.Admission_Result);

   --  For the volume description and the stop path.
   function Session (fs : Store) return CuBit.Block_Devices.Device_Session;
   function Quarantined (fs : Store) return Boolean;
   --  No more writes (a detached volume at stop).
   procedure Retire (fs : in out Store);

   procedure readInode
     (fs : Store; inodeNum : Unsigned_32; ino : out Ext2.Inode; status : out Ext2.Read_Status);
   procedure lookupInDir
     (fs : Store; dirIno : Ext2.Inode; name : String; inodeNum : out Unsigned_32;
      status : out Ext2.Directory_Lookup_Status);
   procedure resolvePath
     (fs : Store; path : String; inodeNum : out Unsigned_32; status : out Ext2.Directory_Lookup_Status);
   procedure readDirectoryPage
     (fs : Store; dirIno : Ext2.Inode; cursor : Unsigned_64; entries : out Ext2.Listed_Records;
      entryCount : out Natural; nextCursor : out Unsigned_64; status : out Ext2.Directory_Read_Status);
   procedure readData
     (fs : Store; ino : Ext2.Inode; offset : Unsigned_64; buf : System.Address; count : Unsigned_64;
      bytesRead : out Unsigned_64; status : out Ext2.Read_Status);
   procedure writeData
     (fs : in out Store; inodeNum : Unsigned_32; ino : in out Ext2.Inode; offset : Unsigned_64;
      buf : System.Address; count : Unsigned_64; bytesWritten : out Unsigned_64;
      status : out Ext2.Write_Status);
   procedure createFile
     (fs : in out Store; dirInodeNum : Unsigned_32; name : String; inodeNum : out Unsigned_32;
      status : out Ext2.Write_Status);
   procedure truncateToEmpty
     (fs : in out Store; inodeNum : Unsigned_32; emptyInode : out Ext2.Inode;
      status : out Ext2.Truncate_Status);
   procedure resizeFile
     (fs : in out Store; inodeNum : Unsigned_32; newSize : Unsigned_64; resizedInode : out Ext2.Inode;
      status : out Ext2.Truncate_Status);
   procedure renamePath
     (fs : in out Store; oldPath, newPath : String; keepReplaced : Boolean;
      replacedNumber : out Unsigned_32; replaced : out Ext2.Inode; status : out Ext2.Rename_Status);
   procedure unlinkPath
     (fs : in out Store; path : String; keepOrphan : Boolean; inodeNum : out Unsigned_32;
      unlinked : out Ext2.Inode; status : out Ext2.Remove_Status);
   procedure reclaimInode (fs : in out Store; inodeNum : Unsigned_32; status : out Ext2.Remove_Status);
   procedure makeDirectoryPath
     (fs : in out Store; path : String; inodeNum : out Unsigned_32;
      lookup : out Ext2.Directory_Lookup_Status; status : out Ext2.Write_Status);
   procedure removeDirectoryPath
     (fs : in out Store; path : String; inodeNum : out Unsigned_32; status : out Ext2.Remove_Status);
   procedure Flush (fs : in out Store; status : out Ext2.Flush_Status);
   procedure Detach (fs : in out Store; status : out Ext2.Flush_Status);
   function dirtyBlocks (fs : Store) return Boolean;

end Fs_Volumes;
