-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2020 Jon Andrew
--
-- @summary
-- File Table
--
-- @description
-- Table of open files on the system and routines for dealing with files.
-------------------------------------------------------------------------------
with Config;

package Files with SPARK_Mode => On is

    subtype Filename is String(1..Config.FILENAME_MAX_LENGTH);
    subtype Path     is String(1..Config.FILEPATH_MAX_LENGTH);

    type FileTableEntry is record
        --@TODO use a device ID and inode # here to save room.
        file     : Path;
        refCount : Natural := 0;
    end record;

    type FileTable is array (Natural range 1..Config.MAX_OPEN_FILES) of FileTableEntry;

end Files;
