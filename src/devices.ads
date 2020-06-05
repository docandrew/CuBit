-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2020 Jon Andrew
--
-- @summary
-- Device/Driver types and identification
--
-- @description
-- CuBit uses the Major, Minor numbering scheme for device class and instance
-- of that device type.
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package Devices with
    SPARK_Mode => On
is
    pragma Pure;

    type MajorNumber is (
        NO_MAJOR,
        IDE
    );

    type MinorNumber is new Unsigned_8;

    NO_MINOR    : constant MinorNumber := 0;

    type DeviceID is record
        major       : MajorNumber;
        minor       : MinorNumber;
        reserved    : Unsigned_16;
    end record with Size => 32;

    for DeviceID use
    record
        major       at 0 range 0..7;
        minor       at 0 range 8..15;
        reserved    at 0 range 16..31;
    end record;

end Devices;
