-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2026 Jon Andrew
--
-- Kernel-side CPIO newc archive parser.
-- Parses the "070701" (newc) format to extract service binaries from the
-- initrd image loaded by GRUB.
-------------------------------------------------------------------------------
with System;
with System.Storage_Elements; use System.Storage_Elements;

package Cpio with
    SPARK_Mode => On
is
    MAX_FILES : constant := 64;

    type FileEntry is record
        dataAddr : System.Address;      -- kernel virtual address of file data
        dataSize : Storage_Count;       -- file size in bytes
        nameAddr : System.Address;      -- kernel virtual address of filename
        nameLen  : Natural;             -- filename length (excluding NUL)
    end record;

    type FileIndex is array (Natural range 0 .. MAX_FILES - 1) of FileEntry;

    type Archive is record
        base  : System.Address;
        size  : Storage_Count;
        files : FileIndex;
        count : Natural := 0;
    end record;

    ---------------------------------------------------------------------------
    -- init
    -- Scan all CPIO newc headers, populate ar.files index.
    ---------------------------------------------------------------------------
    procedure init
      (ar   : out Archive;
       base : System.Address;
       size : Storage_Count;
       ok   : out Boolean);

    ---------------------------------------------------------------------------
    -- findFile
    -- Linear scan of index, compare names from archive memory.
    -- Returns file index, or ar.count if not found.
    ---------------------------------------------------------------------------
    function findFile (ar : Archive; name : String) return Natural;

end Cpio;
