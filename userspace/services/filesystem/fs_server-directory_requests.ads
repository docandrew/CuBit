------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Directory requests of the filesystem service: open, child, rewind, seek, pages, close.
------------------------------------------------------------------------------
with CuBit.Filesystems; use CuBit.Filesystems;
private package Fs_Server.Directory_Requests is

   --  Open a directory by bootstrap path. The returned object is a distinct
   --  PID-bound directory handle; subsequent enumeration carries no path.
   procedure handleOpenDirectory (sender : Process_ID; msg : Message);

   procedure handleOpenChildDirectory (sender : Process_ID; msg : Message);

   procedure handleRewindDirectory (sender : Process_ID; msg : Message);

   --  What an ext2 inode records, as Directory.Inspection.V1. Ext2 keeps
   --  seconds since the epoch; i_ctime is the change time.
   function inspectionOf
     (ino : Ext2.Inode; volume : Volume_Index; inodeNum : Unsigned_32)
      return Entry_Inspection;

   --  Queue_Seek_Directory: continue the handle's listing at a resume token
   --  (0: from the start).
   procedure handleSeekDirectory (owner : Process_ID; handleWord, token : Unsigned_64);

   --  Write one Directory.Page.V2 (CuBit.Directory_Pages) of the handle's
   --  directory at target, continuing from its cursor. Metadata: fill each
   --  entry's facts from its inode (else only kind and object). The page is
   --  built in service memory and copied out whole.
   procedure readDirectoryInto
     (handle : Natural; target : System.Address; metadata : Boolean;
      label : out Unsigned_32; listed : out Natural; ended : out Boolean);

   --  OP_READ_DIRECTORY_PAGE: one Directory.Page.V2, with metadata, into a
   --  one-page writable grant.
   procedure handleReadDirectoryPage (sender : Process_ID; msg : Message);

   procedure handleCloseDirectory (sender : Process_ID; msg : Message);

end Fs_Server.Directory_Requests;
