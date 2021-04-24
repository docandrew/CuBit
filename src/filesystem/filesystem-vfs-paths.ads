-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2020 Jon Andrew
--
-- @summary
-- File Table
--
-------------------------------------------------------------------------------
with Config;

package Filesystem.VFS.Paths with SPARK_Mode => On is

    subtype Filename is String(1..Config.FILENAME_MAX_LENGTH);
    subtype Path     is String(1..Config.FILEPATH_MAX_LENGTH);

    type FileTableEntry is record
        --@TODO use a device ID and inode # here to save room.
        file     : Path;
        refCount : Natural := 0;
    end record;

    type FileTable is array (Natural range 1..Config.MAX_OPEN_FILES) of FileTableEntry;

    ---------------------------------------------------------------------------
    -- lookupInode
    -- Given a file path to an extant file or directory, return the
    -- corresponding VFS Inode. If file does not exist, returns null.
    ---------------------------------------------------------------------------
    function lookupInode (path : in String) return access VFS.Inode;

end Filesystem.VFS.Paths;
