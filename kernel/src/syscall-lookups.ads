-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2020 Jon Andrew
--
-- The caller's own capability lookups shared by the system-call handler
-- units (moved from Syscall.IPC, IPC revision 2 S0).
-------------------------------------------------------------------------------
with Capabilities;
with Process;
with Process_Capabilities;

private package Syscall.Lookups with
    SPARK_Mode => Off
is
    ---------------------------------------------------------------------------
    -- Authority by handle (CAP-002): every call names the capability it
    -- relies on; nothing searches the caller's table for one of a type.
    -- The *Held forms expect the caller's mailbox lock to be held.
    ---------------------------------------------------------------------------
    function onlyRight (right : Capabilities.CapabilityRight)
                        return Capabilities.CapabilityRights;

    function findOwn (callerPID : Process.ProcessID;
                      handle    : Unsigned_64;
                      kind      : Capabilities.CapabilityType;
                      rights    : Capabilities.CapabilityRights)
                      return Process_Capabilities.Found;

end Syscall.Lookups;
