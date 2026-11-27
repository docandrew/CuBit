-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2021 Jon Andrew
--
-- @summary
-- Process.Teardown - implementation.
-------------------------------------------------------------------------------
with Process.DMA;
with Process.IPC;
with Process.Reports;
with Spinlocks;
with TLB_Shootdown;

package body Process.Teardown is

    procedure releaseDMAAllocations (pid : ProcessID) is
        complete : Boolean;
        reusable : Boolean;
    begin
        -- Only the retirement worker calls this, after Take_Ready. No grant
        -- lock is held across the bounded ownership-tag cleanup.
        Spinlocks.enterCriticalSection (grantLock);
        if not proctab(pid).grantTeardownPending or else
           not proctab(pid).grantTeardownReady or else IPC.hasActiveGrants (pid)
        then
            DMA.Finish_Step (pid, True);
            Spinlocks.exitCriticalSection (grantLock);
            return;
        end if;
        Spinlocks.exitCriticalSection (grantLock);
        DMA.Retire_Step (pid, True, complete);
        Spinlocks.enterCriticalSection (grantLock);
        DMA.Finish_Step (pid, complete);
        if complete then
            reusable := proctab(pid).pidReusableAfterGrants;
            proctab(pid).grantTeardownPending := False;
            proctab(pid).grantTeardownReady := False;
            proctab(pid).pidReusableAfterGrants := False;
            if reusable then
                Reports.releaseRetiredPID (pid);
            end if;
        end if;
        Spinlocks.exitCriticalSection (grantLock);
    end releaseDMAAllocations;

    procedure completeOwnerPIDIfReady (owner : ProcessID) is
    begin
        if proctab(owner).grantTeardownPending and then
           proctab(owner).grantTeardownReady and then
           not IPC.hasActiveGrants (owner)
        then
            -- Enqueue only: never walk allocation pages while holding
            -- grantLock. Repeated notifications cannot duplicate a worker job.
            DMA.Enqueue (owner);
        end if;
    end completeOwnerPIDIfReady;

    procedure prepareGrantProtectedTeardown
      (pid         : ProcessID;
       pidReusable : Boolean;
       deferred    : out Boolean)

    is
    begin
        deferred := False;
        Spinlocks.enterCriticalSection (grantLock);
        deferred := IPC.hasActiveGrants (pid) or else DMA.Has_Records (pid);

        if deferred then
            proctab(pid).grantTeardownPending := True;
            proctab(pid).grantTeardownReady := False;
            proctab(pid).pidReusableAfterGrants := pidReusable;
        end if;
        Spinlocks.exitCriticalSection (grantLock);
    end prepareGrantProtectedTeardown;

    procedure finishGrantProtectedTeardown (pid : ProcessID)

    is
    begin
        -- CPU execution and address-space destruction are complete before
        -- this call. Require synchronous all-CPU translation retirement too.
        TLB_Shootdown.Invalidate_All;
        Spinlocks.enterCriticalSection (grantLock);
        if proctab(pid).grantTeardownPending then
            proctab(pid).grantTeardownReady := True;
            completeOwnerPIDIfReady (pid);
        end if;
        Spinlocks.exitCriticalSection (grantLock);
        IPC.ringGrantNotices;
        notifyDMAWork;
    end finishGrantProtectedTeardown;

end Process.Teardown;
