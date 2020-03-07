-------------------------------------------------------------------------------
-- CubitOS
-- Copyright (C) 2020 Jon Andrew
--
-- Virtual Filesystem Layer
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package Filesystem.vfs with
    SPARK_Mode => On
is
    ---------------------------------------------------------------------------
    -- Drive LBA Addressing
    ---------------------------------------------------------------------------
    subtype LBA48 is Unsigned_64 range 0..16#FFFF_FFFF_FFFF#;
end Filesystem.vfs;