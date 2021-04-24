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
with PerCPUData;
with TextIO; use TextIO;
with Util;
with x86;
--with Spinlocks;

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
            --textmode.println("Entering scheduler");
            Process.switch (cpuData.oldContext'Address, cpuData.schedulerContext);
        end getCPUContext;
    end enter;

    ---------------------------------------------------------------------------
    -- Note: this process is only _called_ once, at bootup (per-CPU).
    -- Future entries back into this function are through the enter
    --  procedure which means we'll pick up where we left off,
    --  trying to run the next process in the proctab.
    ---------------------------------------------------------------------------
    procedure schedule (cpuData : in out PerCPUData.PerCPUData) with
        SPARK_Mode => On
    is
        use Spinlocks;
        use Process;

        pidIndex : ProcessID := 1;
    begin
        -- a bit inefficient, but for now we just linearly search
        -- through the proctab, starting where we last left off.
        startSearch : loop
            x86.sti;

            -- print ("CPU "); print (cpuData.cpuNum);
            -- println (": Checking for processes to run.");

            -- We'll always have an idle process to run
            -- reached end of proctab without finding any READY
            -- processes, so just idle until the next tick
            -- if pidIndex = Process.proctab'Last then
            --     idle;             -- when we return from idle...
            --     pidIndex := 1;    -- ... we'll go back to the beginning and check.
            -- end if;

            if x86.panicked then
                x86.cli;
                x86.halt;
            end if;

            -- NOTE: this lock is released either by process.start (if this is the process'
            --  first time executing); process.yield (if the process is continuing from the 
            --  call to scheduler.enter from the last time it yielded); or at the symmetric
            --  exitCriticalSection call in this function (if no processes were READY and
            --  we left the for loop).
            -- println (" Scheduler: grabbing Process lock");
            enterCriticalSection (Process.lock);

            for i in pidIndex .. Process.proctab'Last loop

                if Process.proctab(i).state = READY then
                    -- print ("Scheduler: found READY process ");
                    -- println (i);
                    
                    -- print ("Scheduler: switching to pid ");
                    -- print (i);
                    -- print (" context: ");
                    -- println (Process.proctab(i).context);
                    
                    Process.proctab(i).state    := RUNNING;

                    cpuData.currentPID          := i;
                    cpuData.currentContext      := Process.proctab(i).context; -- save this address so we can switch back

                    -- switch address spaces if appropriate
                    cpuData.tss.rsp0            := Unsigned_64(Process.proctab(i).kernelStackTop);
                    cpuData.savedKernelRSP      := To_Address(Process.proctab(i).kernelStackTop);
                    
                    cpuData.currentPID          := i;
                    
                    -- Only change address spaces if we're switching to a user-mode process.
                    if Process.proctab(i).mode = Process.USER then
                        Process.switchAddressSpace (Process.proctab(i).pgTable'Address);
                    end if;

                    -- Start executing new process
                    Process.switch (cpuData.schedulerContext'Address, cpuData.currentContext);

                    -- when process pauses its run, we return here

                    cpuData.currentPID := Process.NO_PROCESS;

                    -- switch back to kernel page tables if we weren't just running a kernel thread
                    if Process.proctab(i).mode = Process.USER then
                        Mem_mgr.switchAddressSpace;
                    end if;

                    -- Update the process' context pointer.
                    -- (currentContext was set in Scheduler.enter)
                    -- If the process was RUNNING before and got switched against its will, 
                    -- set it back to READY.
                    case Process.proctab(i).state is
                        
                        when INVALID =>
                            -- Don't save the context here
                            null;
                            -- print ("Scheduler: process "); print (i);
                            -- println (" is terminated");
                        
                        when RUNNING =>
                            -- print ("Scheduler: process "); print (i);
                            -- print (" is interrupted, making READY and saving context: ");
                            -- println (cpuData.oldContext);
                            Process.proctab(i).context := cpuData.oldContext;
                            Process.proctab(i).state   := READY;
                        
                        when WAITING =>
                            -- print ("Scheduler: process "); print (i); 
                            -- print (" is blocked (waiting), saving context: ");
                            -- println (cpuData.oldContext);
                            Process.proctab(i).context := cpuData.oldContext;

                        when RECEIVING =>
                            -- print ("Scheduler: process "); print (i);
                            -- print (" is blocked (receiving), saving context: ");
                            -- println (cpuData.oldContext);
                            Process.proctab(i).context := cpuData.oldContext;

                        when SUSPENDED =>
                            -- print ("Scheduler: process "); print (i); 
                            -- print (" was suspended, saving context: ");
                            -- println (cpuData.oldContext);
                            Process.proctab(i).context := cpuData.oldContext;
                        
                        when SLEEPING =>
                            -- print ("Scheduler: process "); print (i);
                            -- print (" is sleeping, saving context: ");
                            -- println (cpuData.oldContext);
                            Process.proctab(i).context := cpuData.oldContext;

                        when others =>
                            raise SchedulerException with "Scheduler: Process in unknown state.";
                    end case;
                end if;
            end loop;

            exitCriticalSection (Process.lock);
            -- println (" Scheduler: released Process lock (2)");

        end loop startSearch;
    end schedule;

    -- procedure idle with
    --     SPARK_Mode => On
    -- is
    -- begin
    --     -- TODO: add some counting of ticks here so we can
    --     -- keep track of CPU usage.
    --     x86.halt;
    -- end idle;

    -- function getCurrentPID return process.ProcessID with
    --     SPARK_Mode => On
    -- is
    -- begin
    --     return currentPID;
    -- end getCurrentPID;

end Scheduler;
