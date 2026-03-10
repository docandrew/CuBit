------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  CPIO newc archive parser for ramdisk images.
--  Parses the "070701" (newc) format used by Linux initramfs.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System;

package Cpio is

   MAX_FILES : constant := 32;

   type FileInfo is record
      nameOff  : Unsigned_64;   --  byte offset to filename in archive
      nameLen  : Natural;       --  filename length (excluding NUL)
      dataOff  : Unsigned_64;   --  byte offset to file data in archive
      dataSize : Unsigned_64;   --  file size in bytes
   end record;

   type FileIndex is array (0 .. MAX_FILES - 1) of FileInfo;

   type Archive is record
      base  : System.Address;
      size  : Unsigned_64;
      files : FileIndex;
      count : Natural := 0;
   end record;

   ---------------------------------------------------------------------------
   --  init
   --  Scan all cpio newc headers, populate ar.files index.
   ---------------------------------------------------------------------------
   procedure init
     (ar   : out Archive;
      base : System.Address;
      size : Unsigned_64;
      ok   : out Boolean);

   ---------------------------------------------------------------------------
   --  findFile
   --  Linear scan of index, compare names from archive memory.
   --  Returns file index, or ar.count if not found.
   ---------------------------------------------------------------------------
   function findFile (ar : Archive; name : String) return Natural;

   ---------------------------------------------------------------------------
   --  readData
   --  Direct memcpy from archive at dataOff + offset.
   --  Returns number of bytes actually copied.
   ---------------------------------------------------------------------------
   function readData
     (ar      : Archive;
      fileIdx : Natural;
      offset  : Unsigned_64;
      buf     : System.Address;
      count   : Unsigned_64) return Unsigned_64;

   ---------------------------------------------------------------------------
   --  listFiles
   --  Write newline-separated filenames into dest buffer.
   --  Returns the number of bytes written.
   ---------------------------------------------------------------------------
   function listFiles
     (ar       : Archive;
      dest     : System.Address;
      destSize : Unsigned_64) return Unsigned_64;

end Cpio;
