-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2020 Jon Andrew
--
-- @summary
-- Device/Driver types and identification
--
-- @description
-- CuBit uses the Major, Minor numbering scheme for device class and instance
-- of that device type. Major specifies the driver to use, and Minor specifies
-- a device instance within that driver type.
-- 
-- We have drive letters as well, which we map to a particular Major, Minor
-- number.
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package Devices with
    SPARK_Mode => On
is
    -- pragma Pure;

    -- Expand this as more drivers are added.
    type MajorNumber is (
        NO_MAJOR,
        RAMDISK,
        ATA
    );

    type MinorNumber is new Unsigned_8;

    NO_MINOR    : constant MinorNumber := 0;

    type DeviceID is record
        major       : MajorNumber;
        minor       : MinorNumber;
        reserved    : Unsigned_16 := 0;
    end record with Size => 32;

    for DeviceID use
    record
        major       at 0 range 0..7;
        minor       at 0 range 8..15;
        reserved    at 0 range 16..31;
    end record;

end Devices;
