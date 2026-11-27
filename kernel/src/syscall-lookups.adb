-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2020 Jon Andrew
--
-- Syscall.Lookups - implementation.
-------------------------------------------------------------------------------
with Spinlocks;

package body Syscall.Lookups with
    SPARK_Mode => Off
is
    function onlyRight (right : Capabilities.CapabilityRight)
                        return Capabilities.CapabilityRights
    is
        rights : Capabilities.CapabilityRights := Capabilities.NO_RIGHTS;
    begin
        rights (right) := True;
        return rights;
    end onlyRight;

    function findOwn (callerPID : Process.ProcessID;
                      handle    : Unsigned_64;
                      kind      : Capabilities.CapabilityType;
                      rights    : Capabilities.CapabilityRights)
                      return Process_Capabilities.Found
    is
        found : Process_Capabilities.Found;
    begin
        Spinlocks.enterCriticalSection (Process.mailtab(callerPID).lock);
        found := Process_Capabilities.Find
          (Process.proctab(callerPID).caps, handle, kind, rights);
        Spinlocks.exitCriticalSection (Process.mailtab(callerPID).lock);
        return found;
    end findOwn;

end Syscall.Lookups;
