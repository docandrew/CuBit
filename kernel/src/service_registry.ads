-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2021 Jon Andrew
--
-- @summary
-- The service registry: which process is registered for each service role
-- (driver class), by SYSCALL_REGISTER_DRIVER under a CAP_NOTIFICATION with
-- RIGHT_WRITE for that role.
--
-- @description
-- The kernel uses it to route notification sends, keyboard input and boot
-- milestones, and to authorize publishers (SYSCALL_PUBLISH). User space
-- reads it from the registry publication (System_Publications), which this
-- package keeps current; there is no query system call.
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;

with Process;
with System_Publications;

package Service_Registry is

    subtype DriverID is Unsigned_64 range 0 .. System_Publications.Maximum_Role;

    DRIVER_NULL     : constant DriverID := 0;
    DRIVER_KEYBOARD : constant DriverID := 1;
    DRIVER_ATA      : constant DriverID := 2;
    DRIVER_NETSTACK : constant DriverID := 3;
    DRIVER_PROCMGR  : constant DriverID := 4;
    DRIVER_NVME     : constant DriverID := 5;
    DRIVER_FS       : constant DriverID := 6;
    DRIVER_DEVMGR   : constant DriverID := 7;
    DRIVER_HDA      : constant DriverID := 8;
    DRIVER_MIXER    : constant DriverID := 9;
    DRIVER_MOUSE    : constant DriverID := 10;
    DRIVER_CONFIG   : constant DriverID := 11;
    DRIVER_NETMGR   : constant DriverID := 12;
    DRIVER_LOGSTORE : constant DriverID := 13;
    DRIVER_IPCTEST  : constant DriverID := 14;
    DRIVER_DESKTOP  : constant DriverID := 15;
    DRIVER_DISPLAY  : constant DriverID := 16;
    DRIVER_GPU      : constant DriverID := 17;
    DRIVER_CCL_TEST : constant DriverID := 18;
    DRIVER_CLOCK    : constant DriverID := 19;

    -- The process registered for Driver now, or Process.NO_PROCESS.
    function Registered (Driver : DriverID) return Process.ProcessID
        with SPARK_Mode => Off, Volatile_Function; -- live registry

    -- Register pid for driver, replacing any earlier registrant, and
    -- publish its identity.
    procedure Register (pid : Process.ProcessID; driver : DriverID)
        with SPARK_Mode => Off; -- effectful registration and logging

    -- Remove every registration of pid (process teardown).
    procedure Unregister_Process (pid : Process.ProcessID)
        with SPARK_Mode => Off; -- live volatile registry

end Service_Registry;
