------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Ext2 operations over capability-bound block devices: the public API,
--  each operation implemented by the private child unit of its concern
--  (Ext2.Admission, Allocation, Block_Cache, Device_IO, Directories, Files,
--  Journal, Lookup, Mapping, Metadata, Removal, Rename, Resize) over the
--  proved core (Ext2_Format, Ext2_Geometry, Ext2_Bitmaps).
------------------------------------------------------------------------------
pragma Ada_2022;
with Ext2.Admission;
with Ext2.Allocation;
with Ext2.Block_Cache;
with Ext2.Device_IO;
with Ext2.Directories;
with Ext2.Files;
with Ext2.Journal;
with Ext2.Lookup;
with Ext2.Metadata;
with Ext2.Removal;
with Ext2.Rename;
with Ext2.Resize;

package body Ext2 is

   use Volume_Admission;

   procedure deviceRequests (requests, flushes : out Unsigned_64)
     renames Ext2.Device_IO.deviceRequests;

   function splitCommits return Natural
     renames Ext2.Block_Cache.splitCommits;

   function pressureBlocks return Natural
     renames Ext2.Block_Cache.pressureBlocks;

   procedure configureCache (megabytes : Cache_Megabytes)
     renames Ext2.Block_Cache.configureCache;

   function cacheMegabytes return Cache_Megabytes
     renames Ext2.Block_Cache.cacheMegabytes;

   procedure readInode
     (fs : Filesystem; inodeNum : Unsigned_32;
      ino : out Inode; status : out Read_Status)
     renames Ext2.Metadata.readInode;

   procedure lookupInDir
     (fs      : Filesystem;
      dirIno  : Inode;
      name : String;
      inodeNum : out Unsigned_32;
      status : out Directory_Lookup_Status)
     renames Ext2.Lookup.lookupInDir;

   procedure resolvePath
     (fs : Filesystem; path : String; inodeNum : out Unsigned_32;
      status : out Directory_Lookup_Status)
     renames Ext2.Lookup.resolvePath;

   procedure readDirectoryPage
     (fs          : Filesystem;
      dirIno      : Inode;
      cursor      : Unsigned_64;
      entries     : out Listed_Records;
      entryCount  : out Natural;
      nextCursor  : out Unsigned_64;
      status      : out Directory_Read_Status)
     renames Ext2.Lookup.readDirectoryPage;

   function dirtyBlocks (fs : Filesystem) return Boolean
     renames Ext2.Journal.dirtyBlocks;

   procedure Flush (fs : in out Filesystem; status : out Flush_Status)
     renames Ext2.Journal.Flush;

   procedure allocateBlock
     (fs       : in out Filesystem;
      blockNum : out Unsigned_32;
      status   : out Write_Status)
     renames Ext2.Allocation.allocateBlock;

   procedure allocateInode
     (fs       : in out Filesystem;
      inodeNum : out Unsigned_32;
      status   : out Write_Status;
      directory : Boolean := False)
     renames Ext2.Allocation.allocateInode;

   procedure readData
     (fs     : Filesystem;
      ino    : Inode;
      offset : Unsigned_64;
      buf    : System.Address;
      count  : Unsigned_64;
      bytesRead : out Unsigned_64;
      status    : out Read_Status)
     renames Ext2.Files.readData;

   procedure writeData
     (fs       : in out Filesystem;
      inodeNum : Unsigned_32;
      ino      : in out Inode;
      offset   : Unsigned_64;
      buf      : System.Address;
      count    : Unsigned_64;
      bytesWritten : out Unsigned_64;
      status       : out Write_Status)
     renames Ext2.Files.writeData;

   procedure renameEntry
     (fs : in out Filesystem; dirInodeNum : Unsigned_32;
      oldName, newName : String; status : out Rename_Status)
     renames Ext2.Directories.renameEntry;

   procedure createFile
     (fs : in out Filesystem; dirInodeNum : Unsigned_32; name : String;
      inodeNum : out Unsigned_32; status : out Write_Status)
     renames Ext2.Directories.createFile;

   procedure makeDirectory
     (fs : in out Filesystem; dirInodeNum : Unsigned_32; name : String;
      inodeNum : out Unsigned_32; status : out Write_Status)
     renames Ext2.Directories.makeDirectory;

   procedure resizeFile
     (fs : in out Filesystem; inodeNum : Unsigned_32;
      newSize : Unsigned_64; resizedInode : out Inode;
      status : out Truncate_Status)
     renames Ext2.Resize.resizeFile;

   procedure truncateToEmpty
     (fs : in out Filesystem; inodeNum : Unsigned_32;
      emptyInode : out Inode; status : out Truncate_Status)
     renames Ext2.Resize.truncateToEmpty;

   procedure reclaimInode
     (fs : in out Filesystem; inodeNum : Unsigned_32;
      status : out Remove_Status)
     renames Ext2.Removal.reclaimInode;

   procedure unlinkPath
     (fs : in out Filesystem; path : String; keepOrphan : Boolean;
      inodeNum : out Unsigned_32; unlinked : out Inode;
      status : out Remove_Status)
     renames Ext2.Removal.unlinkPath;

   procedure makeDirectoryPath
     (fs : in out Filesystem; path : String; inodeNum : out Unsigned_32;
      lookup : out Directory_Lookup_Status; status : out Write_Status)
     renames Ext2.Removal.makeDirectoryPath;

   procedure removeDirectoryPath
     (fs : in out Filesystem; path : String; inodeNum : out Unsigned_32;
      status : out Remove_Status)
     renames Ext2.Removal.removeDirectoryPath;

   procedure renamePath
     (fs : in out Filesystem; oldPath, newPath : String;
      keepReplaced : Boolean; replacedNumber : out Unsigned_32;
      replaced : out Inode; status : out Rename_Status)
     renames Ext2.Rename.renamePath;

   procedure Detach (fs : in out Filesystem; status : out Flush_Status)
     renames Ext2.Admission.Detach;

   procedure initBlockDevice
     (fs         : out Filesystem;
      endpoint   : CuBit.Handles.Endpoint;
      volume     : Unsigned_64;
      grant      : CuBit.Memory_Grants.Grant_Reference;
      grantBuf   : System.Address;
      grantBytes : Unsigned_32;
      result     : out Admission_Result)
     renames Ext2.Admission.initBlockDevice;

end Ext2;
