-------------------------------------------------------------------------------
-- CuBit
-- Copyright (C) 2021 Jon Andrew
--
-- @summary
-- Ramdisk driver
--
-- @description
-- The ramdisk driver is loaded as a module at boot time.
-------------------------------------------------------------------------------
with CuBit.Messages; use CuBit.Messages;
with Interfaces; use Interfaces;
with System; use System;
with System.Address_Image;
with System.Storage_Elements; use System.Storage_Elements;

package body CuBit.Drivers.Ramdisk is

    ---------------------------------------------------------------------------
    -- cpio record header
    -- initrd image uses cpio format
    ---------------------------------------------------------------------------
    type CPIOHeader is
    record
        c_magic     : Unsigned_16;
        c_dev       : Unsigned_16;
        c_ino       : Unsigned_16;
        c_mode      : Unsigned_16;
        c_uid       : Unsigned_16;
        c_gid       : Unsigned_16;
        c_nlink     : Unsigned_16;
        c_rdev      : Unsigned_16;
        c_mtime     : Unsigned_32;
        c_namesize  : Unsigned_16;
        c_filesize  : Unsigned_32;
    end record;

    ---------------------------------------------------------------------------
    -- setup
    ---------------------------------------------------------------------------
    procedure setup (rdAddr : System.Address) with SPARK_Mode => On
    is
        use ASCII;
        header : CPIOHeader with Import, Address => rdAddr;
    begin
        -- Check the initrd image passed to us by the kernel
        if rdAddr /= System.Null_Address then
            debugPrint ("Ramdisk: Got initrd image at " & System.Address_Image(rdAddr) & LF);
        else
            debugPrint ("Ramdisk: No initrd image available.");
        end if;
    end setup;

end CuBit.Drivers.Ramdisk;