-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2021 Jon Andrew
--
-- @summary
-- Query mechanism. This is a way for user-space drivers to get information
-- from the kernel.
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;

with Process;

package Sysinfo is

    -- Query Categories, Details
    subtype QueryID is Unsigned_64;

    MAGIC_RAMDISK_ADDRESS : constant QueryID := 1000;
    SECONDARY_STACK_START : constant QueryID := 1001;
    REGISTERED_DRIVER     : constant QueryID := 2000;

    subtype DriverID is QueryID range 0..127;

    DRIVER_NULL     : constant DriverID := 0;
    DRIVER_KEYBOARD : constant DriverID := 1;

    -- List of processes registered as a particular driver.
    type DriverList is array (DriverID) of Process.ProcessID;

    ---------------------------------------------------------------------------
    -- getInfo
    -- Request information from the kernel from userspace.
    ---------------------------------------------------------------------------
    function getInfo (query  : Unsigned_64;
                      detail : Unsigned_64) return Unsigned_64
        with SPARK_Mode => On;

    ---------------------------------------------------------------------------
    -- registerDriver
    -- Register a user-space process as a driver.
    -- @param pid - Process ID of the user-space process
    -- @param driver - unique identifier for the device class.
    ---------------------------------------------------------------------------
    function registerDriver (pid    : Process.ProcessID;
                             driver : DriverID) return Unsigned_64
        with SPARK_Mode => On;

end Sysinfo;
