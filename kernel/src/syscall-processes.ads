-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2020 Jon Andrew
--
-- Syscall process creation handlers (SPAWN).
-------------------------------------------------------------------------------
with Process;

package Syscall.Processes with
    SPARK_Mode => Off
is

    -- SPAWN (CAP-002): arg0 = spawn authority (a CAP_PROCESS handle with
    -- RIGHT_EXECUTE naming every process), arg1 = address of a
    -- Capability_ABI.Spawn_Request, arg2 = address of a
    -- Capability_ABI.Spawn_Result. Creates the process suspended. The
    -- parent is the caller or a process the authority holds Grant over; the
    -- returned handles carry at most the authority's rights, the endpoint and
    -- CSPACE only with Grant (Spawn_Admission). Returns 0, or a refusal code.
    procedure handleSpawn (callerPID : Process.ProcessID;
                           arg0, arg1, arg2 : Unsigned_64;
                           retval     : out Unsigned_64);

end Syscall.Processes;
