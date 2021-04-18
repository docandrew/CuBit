-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2020 Jon Andrew
--
-- Virtual Filesystem Layer
--
-- This is a work-in-progress. We'll need to add dispatching routines here for
-- each of the necessary disk routines. (get superblock, get inode, get block,
-- etc.)
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;

with Devices;

package Filesystem.VFS with
    SPARK_Mode => On
is
    ---------------------------------------------------------------------------
    -- Drive LBA Addressing
    ---------------------------------------------------------------------------
    subtype LBA48 is Unsigned_64 range 0 .. 16#FFFF_FFFF_FFFF#;

end Filesystem.VFS;
