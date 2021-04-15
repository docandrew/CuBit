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
        ATA
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

    -- Drive letters A to Z for now, will consider expanding this later to
    -- use volume names or drive serial numbers.
    type DriveLetter is (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z, NODRIVE);
    
    -- Map a drive letter to a particular device. May change from DeviceID to
    -- a wrapper record to support things like network drives in the future.
    type DriveArray is array (DriveLetter) of DeviceID;

    -- Mounted drives in the system
    drives : DriveArray := (others => (major => Devices.NO_MAJOR, 
                                       minor => Devices.NO_MINOR, 
                                       reserved => 0));
end Devices;
