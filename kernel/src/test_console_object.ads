-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary
-- The test console object: Test_Console_ABI's Write on a CAP_TEST_CONSOLE
-- handle, reached through Call (ABI-002). Test builds' logstore mirrors
-- programs' debug text through it; SYSCALL_WRITE is retired.
--
-- @description
-- Test_Console_ABI.Decide (proved) decides; this adapter copies the bytes
-- out of the caller's own memory (Process.User_Memory) and prints them as
-- a process's text (TextIO.printUser): never a kernel line, so never in
-- the kernel log ring.
-------------------------------------------------------------------------------
pragma Ada_2022;
with Process;

package Test_Console_Object is

    -- A Call on a test console handle. Writable: the handle carries
    -- Write. Reply is the whole reply message (label echoed, one outcome
    -- word).
    procedure Call (Caller   : Process.ProcessID;
                    Writable : Boolean;
                    Request  : Process.Message;
                    Reply    : out Process.Message);
end Test_Console_Object;
