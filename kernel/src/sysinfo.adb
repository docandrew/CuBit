-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2021 Jon Andrew
--
-- @summary
-- Query mechanism. This is a way for user-space drivers to get information
-- from the kernel.
-------------------------------------------------------------------------------
with Modules;
with TextIO; use TextIO;
with Util;

package body Sysinfo is

    registeredDrivers : DriverList := (others => Process.NO_PROCESS);
    
    ---------------------------------------------------------------------------
    -- getInfo
    ---------------------------------------------------------------------------
    function getInfo (query  : Unsigned_64;
                      detail : Unsigned_64) return Unsigned_64
        with SPARK_Mode => On
    is
    begin
        case query is
            when MAGIC_RAMDISK_ADDRESS =>
                -- println ("Sysinfo: received query for magic ramdisk address");
                return Util.addrToNum (Modules.MAGIC_RAMDISK_ADDRESS);
            when SECONDARY_STACK_START =>
                -- println ("Sysinfo: received query for secondary stack start");
                return Util.addrToNum (Process.SECONDARY_STACK_START);
            when REGISTERED_DRIVER =>
                return Unsigned_64(registeredDrivers(DriverID(detail)));
            when others =>
                return Unsigned_64'Last;
        end case;
    end getInfo;

    ---------------------------------------------------------------------------
    -- registerDriver
    ---------------------------------------------------------------------------
    function registerDriver (pid    : Process.ProcessID;
                             driver : DriverID) return Unsigned_64
        with SPARK_Mode => On
    is
    begin
        registeredDrivers (driver) := pid;
        return Unsigned_64(pid);
    end registerDriver;

end Sysinfo;
