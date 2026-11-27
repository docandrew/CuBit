-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2021 Jon Andrew
--
-- @summary
-- Deferred process teardown (moved from Process.IPC, IPC revision 2 S0):
-- the reaper keeps a process's PID while grants it owns are live or DMA
-- records remain, and the retirement worker releases its DMA in bounded
-- steps once address-space teardown is complete. Under grantLock.
-------------------------------------------------------------------------------
package Process.Teardown is

    -- Called by process teardown.  When acquired grants remain, retain the
    -- PID until teardown is complete and the final acquisition is returned.
    procedure prepareGrantProtectedTeardown
      (pid         : ProcessID;
       pidReusable : Boolean;
       deferred    : out Boolean);

    procedure finishGrantProtectedTeardown (pid : ProcessID);

    -- Retirement-worker-only bounded step, after DMA.Take_Ready; at most
    -- 64 ownership-tag releases. PID stays reserved until all records retire.
    procedure releaseDMAAllocations (pid : ProcessID);

    -- owner's deferred teardown is ready and nothing it granted is live:
    -- queue its DMA release (enqueue only). Caller holds grantLock.
    procedure completeOwnerPIDIfReady (owner : ProcessID);

end Process.Teardown;
