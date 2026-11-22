-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary
-- The CAP_FIRMWARE_SNAPSHOT kernel object (Firmware_Snapshot_ABI), reached
-- by Call on its handle (Kernel_Object_Calls): serves the request over the
-- sealed boot snapshot (acpi.Describe_Snapshot_Table, acpi.Copy_Table_Part)
-- with Firmware_Snapshot_Service, writing to the caller only through
-- Process.User_Memory.Copy_To_User.
-------------------------------------------------------------------------------
with Process;
with Firmware_Snapshot_ABI;

package Firmware_Snapshot_Export with
    SPARK_Mode => Off -- capability table, user memory and acpi's snapshot
is
    -- Request on Caller's own CAP_FIRMWARE_SNAPSHOT handle, whose rights
    -- include Read when Readable: without it the answer is Not_Authorized,
    -- before any request word is looked at. Reply carries the Outcome.
    procedure Call
      (Caller   : Process.ProcessID;
       Readable : Boolean;
       Request  : Process.Message;
       Reply    : out Process.Message);
end Firmware_Snapshot_Export;
