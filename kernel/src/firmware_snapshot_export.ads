-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary
-- FIRMWARE_SNAPSHOT (Firmware_Snapshot_ABI): the kernel's half. Checks the
-- authority against the calling process, then serves the request over the
-- sealed boot snapshot (acpi.Describe_Snapshot_Table, acpi.Copy_Table_Part)
-- with Firmware_Snapshot_Service, writing to the caller only through
-- Process.User_Memory.Copy_To_User.
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;

with Process;
with Firmware_Snapshot_ABI;

package Firmware_Snapshot_Export with
    SPARK_Mode => Off -- capability table, user memory and acpi's snapshot
is
    -- Handle_Word names a CAP_FIRMWARE_SNAPSHOT with RIGHT_READ in Caller's
    -- own table, or the result is Not_Authorized (and Caller's supervisor
    -- hears of the capability fault) before any request word is looked at.
    -- Result is a Firmware_Snapshot_ABI.Outcome word.
    procedure Handle
      (Caller      : Process.ProcessID;
       Handle_Word : Unsigned_64;
       Words       : Firmware_Snapshot_ABI.Request_Words;
       Reply       : Unsigned_64;
       Result      : out Unsigned_64);
end Firmware_Snapshot_Export;
