-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2020 Jon Andrew
--
-------------------------------------------------------------------------------
with Filesystem.Ext2;
with Filesystem.VFS;
with Process;

package body Filesystem.VFS.Paths is

    ---------------------------------------------------------------------------
    -- PathType
    -- ABSOLUTE_ON_DRIVE means it's an absolute path, but no drive specified,
    -- so use the working drive of the process.
    ---------------------------------------------------------------------------
    type PathType is (INVALID, ABSOLUTE, RELATIVE, DRIVE_ROOT);

    ---------------------------------------------------------------------------
    -- getPathType
    -- Absolute paths will have a drive letter or device name at the front
    ---------------------------------------------------------------------------
    function getPathType (path : in String) return PathType is
    begin
        -- Empty path
        if path'Length = 0 then
            return INVALID;
        end if;

        -- Single character paths.
        if path'Length = 1 and path(path'First) = '/' then
            return DRIVE_ROOT;
        elsif path'Length = 1 then
            return RELATIVE;
        end if;

        -- Drive root folder.
        if path(path'First) in 'A'..'Z' and path(path'First) = ':' then
            return ABSOLUTE;
        end if;

        if path(path'First) = '/' then
            return DRIVE_ROOT;
        end if;

        return RELATIVE;
    end getPathType;

    ---------------------------------------------------------------------------
    -- lookupInode
    -- given a file path, determine the VFS Inode it corresponds to.
    ---------------------------------------------------------------------------
    function lookupInode (path : in String) return access VFS.Inode is
    begin
        return null;
    end lookupInode;
   
end Filesystem.VFS.Paths;
