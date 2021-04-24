-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2020 Jon Andrew
--
-- Virtual Filesystem Layer
--
-- This is a work-in-progress. We'll need to add dispatching routines here for
-- each of the necessary disk routines. (get superblock, get inode, get block,
-- etc.)
--
-- CuBit uses drive letters to uniquely identify a Device + Filesystem combo.
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

    -- Drive letters A to Z for now, will consider expanding this later to
    -- use volume names or drive serial numbers.
    type DriveLetter is (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z, NODRIVE);
   
    ---------------------------------------------------------------------------
    -- FSType
    -- The underlying filesystem driver used 
    ---------------------------------------------------------------------------
    type FSType is (NONE, EXT2);

    ---------------------------------------------------------------------------
    -- Filesystem
    -- Pointer to the driver used for basic file operations as well as the
    -- device this file system lives on.
    ---------------------------------------------------------------------------
    type Drive is
    record
        present : Boolean := False;
        kind    : FSType  := NONE;
        --fs      : Filesystem;
        --device  : Devices.DeviceID;
    end record;

    ---------------------------------------------------------------------------
    -- VFS Inode (abstraction of on-disk inodes)
    ---------------------------------------------------------------------------
    type Inode is
    record
        drive  : DriveLetter;
        num    : Unsigned_64;
    end record;

    ---------------------------------------------------------------------------
    -- Filesystem drivers need to support these operations
    ---------------------------------------------------------------------------
    -- type FSDriver is
    -- record;
    -- 
    -- end record;

    -- Maps drive letters on our machine to a particular filesystem.
    type FSArray is array (DriveLetter) of Drive;
    fstab : FSArray := (others => (present => False, kind => NONE));

end Filesystem.VFS;
