with PerCPUData;
with Process.Queues;
package body Process is
    procedure ready (PID : ProcessID) is
        Ignored : ProcessID;
    begin
        -- Check the production queue code's synchronization at the transition,
        -- not just its eventual contents. The old timer path violates this.
        pragma Assert (Spinlocks.ownedBy (lock, PerCPUData.getCPUNumber));
        pragma Assert (not Spinlocks.ownedBy
                       (sleepList.lock, PerCPUData.getCPUNumber));
        pragma Assert (proctab(PID).state = SLEEPING);
        proctab(PID).state := READY;
        Queues.enqueue (readyList, PID, Ignored);
        Ready_Count := Ready_Count + 1;
    end ready;
end Process;
