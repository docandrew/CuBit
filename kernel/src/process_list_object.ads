-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary
-- The process-list kernel object: Process_List_ABI's List on a
-- CAP_PROCESS_LIST handle, reached through Call (ABI-002). Replaces
-- PROCLIST, which any process could call with its own process handle.
-------------------------------------------------------------------------------
pragma Ada_2022;
with Process;

package Process_List_Object is

    -- A Call on a process-list handle. Readable: the handle carries Read.
    -- The entries go to Caller's own memory; Reply is the whole answer.
    procedure Call (Caller   : Process.ProcessID;
                    Readable : Boolean;
                    Request  : Process.Message;
                    Reply    : out Process.Message);
end Process_List_Object;
