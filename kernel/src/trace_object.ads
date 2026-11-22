-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary
-- The kernel trace object: Trace_ABI's operations on a CAP_TRACE handle,
-- reached through Call (ABI-002). Replaces TRACE_RESET and TRACE_SUMMARY.
--
-- @description
-- Trace_ABI.Decide (proved) decides; this adapter runs the Trace operation
-- and answers. Summary also prints the caller's CPU accounting, as
-- TRACE_SUMMARY did.
-------------------------------------------------------------------------------
pragma Ada_2022;
with Process;

package Trace_Object is

    -- A Call on a trace handle. Writable: the handle carries Write. Reply
    -- is the whole reply message (label echoed, one outcome word).
    procedure Call (Writable : Boolean;
                    Request  : Process.Message;
                    Reply    : out Process.Message);
end Trace_Object;
