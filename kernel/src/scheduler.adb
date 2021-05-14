-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2019 Jon Andrew
--
-- System Scheduler
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System;
with System.Storage_Elements; use System.Storage_Elements;

with Mem_mgr;
with Process.Queues;
with TextIO; use TextIO;
with x86;

package body Scheduler with
    -- Refined_State => (
    --     SchedulerState => ()),
    SPARK_Mode => On
is
    ---------------------------------------------------------------------------
    -- Enter the scheduler from a process.
    -- TODO: Add checking for stack canary when we enter the scheduler.
    ---------------------------------------------------------------------------
    procedure enter with
        SPARK_Mode => On
    is
        perCPUAddr : constant System.Address := PerCPUData.getPerCPUDataAddr;
    begin
        getCPUContext: declare
            cpuData : PerCPUData.PerCPUData with
                Import, Address => perCPUAddr;
        begin
            Process.switch (cpuData.oldContext'Address, cpuData.schedulerContext);
        end getCPUContext;
    end enter;

    ---------------------------------------------------------------------------
    -- schedule
    -- @NOTE This process is only _called_ once per CPU, at bootup.
    -- Future entries back into this function are through the enter
    --  procedure which means we'll pick up where we left off,
    --  trying to run the next process in the proctab.
    ---------------------------------------------------------------------------
    procedure schedule (cpuData : in out PerCPUData.PerCPUData) with
        SPARK_Mode => On
    is
        use Spinlocks;
        use Process;

        pid : ProcessID;
        ign : ProcessID;
        fxarea : System.Address;
    begin

        startSearch : loop
            x86.sti;

            if x86.panicked then
                x86.cli;
                x86.halt;
            end if;

            -- @NOTE This lock is released either by process.start (if this is the process'
            --  first time executing); process.yield (if the process is continuing from the 
            --  call to scheduler.enter from the last time it yielded); or at the symmetric
            --  exitCriticalSection call in this function.
            --
            -- The process must release the lock when it executes, and reacquire it before
            -- coming back here to the scheduler.
            --
            -- println ("Scheduler.schedule: acquiring proctab lock");
            enterCriticalSection (Process.lock);

            -- Remove this process from the ready list. This makes it easier to put on a
            -- different list if it blocks during its run.
            -- println ("Scheduler - Ready List: ");
            -- Process.Queues.print (Process.readyList);

            pid := Process.Queues.dequeue (Process.readyList);

            -- print ("Scheduler: running "); print (Process.proctab(pid).name); print(" pid "); println (Integer(pid));

            if pid = Process.NO_PROCESS then
                raise SchedulerException with "Scheduler.schedule: No idle process in ready list";
            end if;

            Process.proctab(pid).state  := RUNNING;

            cpuData.currentPID          := pid;
            cpuData.currentContext      := Process.proctab(pid).context; -- save this address so we can switch back

            -- switch address spaces if appropriate
            cpuData.savedKernelRSP      := Process.proctab(pid).kernelStackTop;
            cpuData.tss.rsp0            := Process.proctab(pid).kernelStackTop;
            
            -- Only change address spaces if we're switching to a user-mode process.
            if Process.proctab(pid).mode = Process.USER then
                Process.switchAddressSpace (pid);
            end if;

            -- print ("Scheduler: Switching to context "); println (cpuData.currentContext);

            Process.restoreFPUState (pid);

            -- Start executing new process
            Process.switch (cpuData.schedulerContext'Address, cpuData.currentContext);

            -- when process pauses its run, we return here
            cpuData.currentPID := Process.NO_PROCESS;

            -- switch back to kernel page tables if we weren't just running a kernel thread
            if Process.proctab(pid).mode = Process.USER then
                Mem_mgr.switchAddressSpace;
            end if;

            -- Update the process' context pointer.
            case Process.proctab(pid).state is
                
                when INVALID =>
                    -- Don't save the context here
                    null;
                    -- print ("Scheduler: process "); print (i);
                    -- println (" is terminated");
                
                when RUNNING =>
                    -- print ("Scheduler: process "); print (i);
                    -- print (" is interrupted, making READY and saving context: ");
                    -- println (cpuData.oldContext);
                    Process.proctab(pid).context := cpuData.oldContext;
                    Process.proctab(pid).state   := READY;
                    Process.saveFPUState (pid);

                    -- @TODO adjust priority here if we eat up full time-slice
                    -- put us back on the ready list.
                    ign := Process.Queues.insert (q   => Process.readyList,
                                                  pid => pid,
                                                  key => Process.proctab(pid).priority);
                
                when WAITING | RECEIVING | SENDING | WAITINGFOREVENT | WAITINGFORREPLY |
                     SUSPENDED | SLEEPING =>
                    -- print ("Scheduler: process "); print (i); 
                    -- print (" is blocked (waiting), saving context: ");
                    -- println (cpuData.oldContext);
                    Process.proctab(pid).context := cpuData.oldContext;
                    Process.saveFPUState (pid);

                when others =>
                    raise SchedulerException with "Scheduler: Process in unknown state.";
            end case;

        -- println ("Scheduler.schedule: releasing proctab lock");
        exitCriticalSection (Process.lock);

        end loop startSearch;
    end schedule;

end Scheduler;
