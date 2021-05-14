-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2019 Jon Andrew
--
-- CuBitOS Processes
--
--@TODO There are a lot of interwoven locks here that would be nice to model in
-- SPARK Mode to get some guarantees of correctness.
-------------------------------------------------------------------------------
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System.Machine_Code; use System.Machine_Code;

with BuddyAllocator;
with Config;
with Mem_mgr;
with PerCPUData;
with Process.Queues;
with Scheduler;
with Segment;
with TextIO; use TextIO;
with x86;

package body Process
    with SPARK_Mode => On
is

    ---------------------------------------------------------------------------
    -- setup
    ---------------------------------------------------------------------------
    procedure setup is
    begin
        -- ProcList.setup (allProcs, Config.MAX_PROCESSES);
        FrameLists.setup (Config.MAX_PROCESSES * Config.PAGES_PER_PROCESS);
        -- MsgQueue.setup (Config.MAX_PROCESSES);
    end setup;

    ---------------------------------------------------------------------------
    -- addToProctab
    ---------------------------------------------------------------------------
    procedure addToProctab (proc : in Process) with
        SPARK_Mode => On
    is
    begin
        -- println ("Process.addToProctab: acquiring proctab lock");
        Spinlocks.enterCriticalSection (lock);

        proctab(proc.pid) := proc;

        -- println ("Process.addToProctab: releasing proctab lock");
        Spinlocks.exitCriticalSection (lock);
    end addToProctab;

    ---------------------------------------------------------------------------
    -- createKernelThread
    ---------------------------------------------------------------------------
    function createKernelThread (procStart  : in System.Address;
                                 name       : in ProcessName;
                                 pid        : in ProcessID;
                                 priority   : in ProcessPriority) return Process
    is
        proc : Process;
    begin
        PIDTracker.allocSpecificPID (pid);

        proc.pid      := pid;
        proc.ppid     := pid;       -- For IPC, we want this to have its own mailbox.
        proc.name     := name;
        proc.mode     := KERNEL;
        proc.state    := SUSPENDED;
        proc.priority := priority;

        -- Depending on how crazy these threads get, we may consider making this
        -- a different type with more room.
        proc.kernelStack    := new ProcessKernelStack;
        proc.kernelStackTop := proc.kernelStack.all'Address + ProcessKernelStack'Size / 8;

        proc.kernelStack.filler := (others => 0);

        proc.kernelStack.interruptFrame := (
                interruptNumber => 0,    
                rip             => procStart,
                rsp             => proc.kernelStackTop,
                rflags          => x86.FLAGS_INTERRUPT,
                cs              => Segment.GDTOffset'Enum_Rep(Segment.GDT_OFFSET_KERNEL_CODE) or 0,
                ss              => Segment.GDTOffset'Enum_Rep(Segment.GDT_OFFSET_KERNEL_DATA) or 0,
                others          => 0
            );

        proc.kernelStack.returnAddress := interruptReturn'Address;
        proc.kernelStack.context       := (rip => start'Address, others => 0);

        proc.context := proc.kernelStack.context'Address;

        return proc;
    end createKernelThread;

    ---------------------------------------------------------------------------
    -- startKernelThread
    ---------------------------------------------------------------------------
    procedure startKernelThread (procStart  : in System.Address;
                                 name       : in ProcessName;
                                 pid        : in ProcessID;
                                 priority   : in ProcessPriority)
    is
        proc : constant Process := createKernelThread (procStart, name, pid, priority);
    begin
        if proc.pid /= 0 then
            addToProctab (proc);
            resume (proc.pid);
        else
            raise ProcessException with "Process.startKernelThread: failed createKernelThread";
        end if;
    end startKernelThread;

    ---------------------------------------------------------------------------
    -- addStackPage
    -- Allocate an additional page of memory for this process or thread's stack,
    -- map it into the process or thread's parent process page tables, and add
    -- it to the list of pages used by this process or thread's parent process.
    ---------------------------------------------------------------------------
    procedure addStackPage (proc : in out Process) is
        newFrame : Virtmem.PhysAddress;
        ok : Boolean;
        MapException : exception;

        procedure mapPage is new Virtmem.mapPage (BuddyAllocator.allocFrame);
    begin
        BuddyAllocator.allocFrame (newFrame);

        -- @TODO this shouldn't be a fatal error for kernel but works for now to detect errors.
        if newFrame = 0 then
            raise ProcessException with "Unable to allocate memory for Process' stack expansion.";
        end if;
        
        if proc.isThread then
            FrameLists.insertFront (proctab(proc.ppid).frames, newFrame);
        else
            FrameLists.insertFront (proc.frames, newFrame);
        end if;

        -- Map the frame just below the thread's current stack.
        mapPage (phys    => newFrame,
                 virt    => To_Integer(proc.stackTop - Storage_Count((proc.numStackFrames + 1) * Virtmem.PAGE_SIZE)),
                 flags   => Virtmem.PG_USERDATA,
                 myP4    => addrtab(proc.pgTable),
                 success => ok);

        -- print ("Process.addStackPage: Mapping new frame at "); println(To_Integer(proc.stackTop - Storage_Count((proc.numStackFrames + 1) * Virtmem.PAGE_SIZE)));

        if not ok then
            raise MapException with "Process.addStackPage - can't map process' stack";
        end if;

        proc.numStackFrames := proc.numStackFrames + 1;
    end addStackPage;

    ---------------------------------------------------------------------------
    -- addPage
    -- Allocate a page of memory for a process or thread's parent process, map
    -- at the specified address and adds the memory to the process or thread's
    -- parent process frame list so it will be freed on exit.
    ---------------------------------------------------------------------------
    procedure addPage (proc    : in out Process;
                       mapTo   : in System.Address;
                       storage : out System.Address;
                       flags   : in Unsigned_64 := Virtmem.PG_USERDATA) is
        newFrame : Virtmem.PhysAddress;
        ok : Boolean;
        MapException : exception;
    
        procedure mapPage is new Virtmem.mapPage (BuddyAllocator.allocFrame);
    begin

        BuddyAllocator.allocFrame (newFrame);

        if newFrame = 0 then
            raise ProcessException with "Process.addPage: Unable to allocate memory for Process";
        end if;

        storage := Virtmem.P2Va (newFrame);

        if proc.isThread then
            FrameLists.insertFront (proctab(proc.ppid).frames, newFrame);
        else
            FrameLists.insertFront (proc.frames, newFrame);
        end if;

        mapPage (phys    => newFrame,
                 virt    => To_Integer(mapTo),
                 flags   => flags,
                 myP4    => addrtab(proc.pgTable),
                 success => ok);

        if not ok then
            raise MapException with "Process.addPage - can't map process page";
        end if;
    end addPage;

    ---------------------------------------------------------------------------
    -- create
    ---------------------------------------------------------------------------
    function create (procStart   : in System.Address;
                     ppid        : in ProcessID;
                     name        : in ProcessName;
                     priority    : in ProcessPriority;
                     procStack   : in System.Address;
                     thread      : in Boolean := False) return Process
        with SPARK_Mode => Off
    is
        proc            : Process;

        procedure zeroize is new Virtmem.zeroize (Virtmem.P4);
    begin
        PIDTracker.allocPID (proc.pid);

        -- sanity checks
        if proc.pid = 0 then
            raise ProcessException with "Unable to create new process. No free PIDs";
        end if;

        proc.ppid        := ppid;
        proc.name        := name;
        proc.mode        := USER;
        proc.state       := SUSPENDED;
        proc.priority    := priority;
        proc.stackTop    := procStack;
        proc.stackBottom := procStack - STACK_SIZE;

        -- heap can't be calculated until the image segments are added to this
        -- process, so set to non-canonical address to start
        proc.heapStart   := BAD_HEAP_ADDRESS;
        proc.kernelStack := new ProcessKernelStack;

        FrameLists.create (proc.frames, MAX_STACK_FRAMES + MAX_HEAP_FRAMES);

        proc.kernelStackTop := proc.kernelStack.all'Address + ProcessKernelStack'Size / 8;

        -- Build the initial kernel stack.
        proc.kernelStack.filler := (others => 0);

        -- Since we use iretq to enter usermode initially, we need an "interrupt
        -- frame" to set up the proper rip, rsp, flags and segments.
        -- Set the interrupt stack frame RIP to the process main, so when
        -- the interrupt returns, we start executing code there...
        proc.kernelStack.interruptFrame := (
                interruptNumber => 0, 
                rip             => procStart,
                rsp             => proc.stackTop,
                rflags          => x86.FLAGS_INTERRUPT,
                cs              => Segment.GDTOffset'Enum_Rep(Segment.GDT_OFFSET_USER_CODE) or 3,
                ss              => Segment.GDTOffset'Enum_Rep(Segment.GDT_OFFSET_USER_DATA) or 3,
                others          => 0);

        -- ... but first we need "start" to return to the usermode entry point...
        proc.kernelStack.returnAddress := interruptReturn'Address;

        -- ... but before that, we need "switch" to return to the start; procedure
        proc.kernelStack.context := (rip => start'Address, others => 0);

        proc.context := proc.kernelStack.context'Address;

        if not thread then
            -- For heavyweight processes, set up the send/recv queues and the
            -- address space it (and any child threads) will be using.
            proc.pgTable := proc.pid;

            mailtab(proc.pid).recvQueue := (
                lock => (name => null, others => <>),
                head => NO_PROCESS,
                tail => NO_PROCESS
            );

            mailtab(proc.pid).sendQueue := (
                lock => (name => null, others => <>),
                head => NO_PROCESS,
                tail => NO_PROCESS
            );

            proc.mail := proc.pid;

            zeroize (addrtab(proc.pgTable));
            Mem_mgr.mapKernelMemIntoProcess (addrtab(proc.pgTable));
        else
            -- for threads, point to parent's page table and mailbox
            proc.pgTable := proc.ppid;
            proc.mail    := proc.ppid;
        end if;

        -- Add a page for the process' stack
        addStackPage (proc);

        return proc;
    end create;

    ---------------------------------------------------------------------------
    -- This is where READY processes continue executing after the scheduler 
    --  puts them in the RUNNING state. Note that the scheduler acquires the
    --  proctab lock in schedule;, but we must release that lock here.
    -- @TODO annotate that a process used its full time-slice here so we can
    --  de-prioritize it.
    ---------------------------------------------------------------------------
    procedure yield with
        SPARK_Mode => On
    is
    begin
        -- println ("Process.yield: acquiring proctab lock");
        Spinlocks.enterCriticalSection (lock);
        
        Scheduler.enter;

        -- continue execution here after context switch back to this process.
        -- println ("Process.yield: releasing proctab lock");
        Spinlocks.exitCriticalSection (lock);
    end yield;

    ---------------------------------------------------------------------------
    -- ready
    -- Move a process into the ready list and change its state to READY
    ---------------------------------------------------------------------------
    procedure ready (pid : ProcessID) with
        SPARK_Mode => On
    is
        ret : ProcessID;
    begin
        proctab(pid).state := READY;
        ret := Queues.insert (readyList, pid, proctab(pid).priority);

        if ret /= pid then
            raise ProcessException with "Process.ready: Error adding pid to ready list.";
        end if;
    end ready;

    ---------------------------------------------------------------------------
    -- Release our hold on a resource and go into WAITING state.
    ---------------------------------------------------------------------------
    procedure wait (channel      : in WaitChannel;
                    resourceLock : in out Spinlocks.spinlock) with
        SPARK_Mode => On
    is
        pid : constant ProcessID := PerCPUData.getCurrentPID;
    begin
        -- Need to get process lock, otherwise we may be woken up by another
        -- thread during their call to schedule once we release our resource
        -- lock.
        -- println ("Process.wait: acquiring proctab lock");
        Spinlocks.enterCriticalSection (lock);
        Spinlocks.exitCriticalSection (resourceLock);

        -- Begin waiting and reschedule.
        proctab(pid).state := WAITING;
        proctab(pid).channel := channel;
        Scheduler.enter;

        -- Resume here when woken.
        proctab(pid).channel := NO_CHANNEL;

        -- Should only be woken when we can acquire the resource lock.
        -- println ("Process.wait: releasing proctab lock");
        Spinlocks.exitCriticalSection (lock);
        Spinlocks.enterCriticalSection (resourceLock);
    end wait;

    ---------------------------------------------------------------------------
    -- goAheadBody
    -- @TODO this is probably a poor implementation, may cause thrashing when
    -- all the woken threads attempt to get the same resource.
    ---------------------------------------------------------------------------
    procedure goAheadBody (channel : in WaitChannel) with
        SPARK_Mode => On
    is
    begin
        for p of proctab loop
            if p.state = WAITING and p.channel = channel then
                p.state := READY;
            end if;
        end loop;
    end goAheadBody;

    ---------------------------------------------------------------------------
    -- goAhead - public interface for internal goAheadBody, to ensure locks are
    -- held.
    ---------------------------------------------------------------------------
    procedure goAhead (channel : in WaitChannel) with
        SPARK_Mode => On
    is
    begin
        -- println ("Process.goAhead: acquiring proctab lock");
        Spinlocks.enterCriticalSection (lock);
        
        goAheadBody (channel);
        
        -- println ("Process.goAhead: releasing proctab lock");
        Spinlocks.exitCriticalSection (lock);
    end goAhead;

    ---------------------------------------------------------------------------
    -- suspend
    ---------------------------------------------------------------------------
    procedure suspend with
        SPARK_Mode => On
    is
        pid : constant ProcessID := PerCPUData.getCurrentPID;
    begin
        -- println ("Process.suspend: acquiring proctab lock");
        Spinlocks.enterCriticalSection (lock);

        -- Begin suspension and reschedule.
        proctab(pid).state := SUSPENDED;
        Scheduler.enter;

        -- Resume here when woken.
        -- Should only be woken when we can acquire the resource lock.
        -- println ("Process.suspend: releasing proctab lock");
        Spinlocks.exitCriticalSection (lock);
    end suspend;

    ---------------------------------------------------------------------------
    -- resume
    ---------------------------------------------------------------------------
    procedure resume (pid : ProcessID) with
        SPARK_Mode => On
    is
        ignore : ProcessID;
    begin
        -- println ("Process.resume: acquiring proctab lock");
        Spinlocks.enterCriticalSection (lock);

        if proctab(pid).state /= SUSPENDED then
            raise ProcessException with "Process.resume: Attempting to resume non-suspended process.";
        end if;

        ready (pid);

        -- println ("Process.resume: releasing proctab lock");
        Spinlocks.exitCriticalSection (lock);
    end resume;

    ---------------------------------------------------------------------------
    -- inform
    ---------------------------------------------------------------------------
    procedure inform (pid : ProcessID) with
        SPARK_Mode => On
    is
        ignore : ProcessID;
    begin
        Spinlocks.enterCriticalSection (lock);

        if proctab(pid).state /= WAITINGFOREVENT and
           proctab(pid).state /= WAITINGFORREPLY then
            raise ProcessException with "Process.inform: Attempting to inform process not waiting for event.";
        end if;

        ready (pid);

        Spinlocks.exitCriticalSection (lock);
    end inform;

    ---------------------------------------------------------------------------
    -- sleep
    ---------------------------------------------------------------------------
    procedure sleep (us : Time.Duration) with
        SPARK_Mode => On
    is
        pid    : ProcessID := PerCPUData.getCurrentPID;
        ignore : ProcessID;
    begin
        -- Put on sleep list.
        -- println ("Going to sleep.");
        proctab(pid).state := SLEEPING;

        ignore := Queues.insertDelta (q            => sleepList,
                                      pid          => pid,
                                      delayFromNow => Integer(us / 1000));
        yield;

    end sleep;

    ---------------------------------------------------------------------------
    -- This is where the scheduler will initially switch() to.
    ---------------------------------------------------------------------------
    procedure start with
        SPARK_Mode => On
    is
    begin
        -- println ("Process.start: releasing proctab lock");
        Spinlocks.exitCriticalSection (lock);
        -- Return to interruptReturn.
    end start;

    ---------------------------------------------------------------------------
    -- symbols needed for createFirstProcess to load the init binary image.
    ---------------------------------------------------------------------------
    initBinaryStart     : Util.Symbol with
        Import => True, External_Name => "_binary_build_init_bin_start";

    initBinarySize      : Util.Symbol with
        Import => True, External_Name => "_binary_build_init_bin_size";
    
    ---------------------------------------------------------------------------
    -- createFirstProcess
    -- The first process (called init in other systems) just makes syscalls to
    --  start running the first executable from disk.
    ---------------------------------------------------------------------------
    procedure createFirstProcess with
        SPARK_Mode => Off   -- use of 'Address
    is
        -- This is kind of funky. The initBinarySize gets stored as though it were
        -- an address.
        initSize : constant Storage_Count := Storage_Count(Util.addrToNum (initBinarySize'Address));

        initProcess : Process;

        InitImageTooBigException : exception;

        alignedStart    : System.Address;  -- will be start from ELF process
        ignore          : System.Address;
    begin
        if initSize > Virtmem.PAGE_SIZE then
            raise InitImageTooBigException with "Init image is too big to fit in one page.";
        end if;

        -- @TODO put the stack way up on top of lower-half like a real process.
        initProcess := create (procStart   => To_Address(0),
                               ppid        => 1,
                               name        => "init            ",
                               priority    => 3,
                               procStack   => PROCESS_STACK_TOP_VIRT);

        -- add page to process, copy the init image to it
        addPage (proc    => initProcess,
                 mapTo   => To_Address(0),
                 storage => alignedStart,
                 flags   => Virtmem.PG_USERCODE);

        ignore := Util.memcpy (alignedStart,
                               initBinaryStart'Address,
                               initSize);

        addToProctab (initProcess);
        resume (initProcess.pid);

    end createFirstProcess;

    ---------------------------------------------------------------------------
    -- getParent
    ---------------------------------------------------------------------------
    function getParent (pid : in ProcessID) return ProcessID with
        SPARK_Mode => On
    is
    begin
        return proctab(pid).ppid;
    end getParent;

    ---------------------------------------------------------------------------
    -- switchAddressSpace
    ---------------------------------------------------------------------------
    procedure switchAddressSpace (pid : in ProcessID) with
        SPARK_Mode => On
    is
        p4addr : System.Address;
    begin
        if proctab(pid).isThread then
            p4addr := addrtab(getParent (pid))'Address;
        else
            p4addr := addrtab(pid)'Address;
        end if;

        Virtmem.setActiveP4 (Virtmem.K2P (p4addr));
    end switchAddressSpace;

    ---------------------------------------------------------------------------
    -- removeFromMailQueue
    -- If a process is blocked in the SENDING or RECEIVING states, this 
    -- procedure will identify the appropriate mailbox it is waiting on, and
    -- remove it from the list.
    ---------------------------------------------------------------------------
    procedure removeFromMailQueue (pid : in ProcessID) with
        SPARK_Mode => On
    is
        mailbox : constant ProcessID    := proctab(pid).queueKey;
        state   : constant ProcessState := proctab(pid).state;
        ignore  : ProcessID;
    begin
        if state = SENDING then
            ignore := Queues.popItem (mailtab(mailbox).sendQueue, pid);
        elsif state = RECEIVING then
            ignore := Queues.popItem (mailtab(mailbox).recvQueue, pid);
        else
            raise ProcessException with "Process.removeFromMailQueue called on non-SENDING/RECEIVING process.";
        end if;
    end removeFromMailQueue;

    ---------------------------------------------------------------------------
    -- kill
    ---------------------------------------------------------------------------
    procedure kill (pid : in ProcessID) with
        SPARK_Mode => On
    is
        -- recursively unmaps/deallocates process' full paging hierarchy
        procedure deleteP4 is new Virtmem.deleteP4 (BuddyAllocator.freeFrame);
        
        procedure free is new Ada.Unchecked_Deallocation (Object => ProcessKernelStack,
                                                          Name   => ProcessKernelStackPtr);
        ignore : ProcessID;
    begin
        -- Back to kernel addressing.
        Mem_mgr.switchAddressSpace;

        -- println ("Process.kill: acquiring proctab lock");
        Spinlocks.enterCriticalSection (lock);

        -- Remove this process from whatever list it was on (if any)
          case proctab(pid).state is
            when READY =>
                ignore := Queues.popItem (readyList, pid);
            
            when SLEEPING =>
                ignore := Queues.popItem (sleepList, pid);
            
            when SENDING | RECEIVING =>
                removeFromMailQueue (pid);

            when others =>
                null;
        end case;

        proctab(pid).state := INVALID;

        if proctab(pid).mode = USER and not proctab(pid).isThread then

            loop
                BuddyAllocator.freeFrame (FrameLists.front(proctab(pid).frames));
                FrameLists.popFront (proctab(pid).frames);

                exit when proctab(pid).frames.length = 0;
            end loop;

            FrameLists.delete (proctab(pid).frames);

            -- Need to unmap Kernel mem here so when we delete page tables we
            -- only delete the process' page tables.
            Mem_mgr.unmapKernelMemFromProcess (addrtab(pid));
            deleteP4 (addrtab(pid));
            proctab(pid).pgTable := NO_PROCESS;
        end if;
        
        free (proctab(pid).kernelStack);
        
        -- Nothing to return to, go back to scheduler.
        Scheduler.enter;

        -- Should never actually get here.
        raise ProcessException with "Somehow returned from scheduler in Process.kill";
    end kill;

    ---------------------------------------------------------------------------
    -- getRunningProcess
    ---------------------------------------------------------------------------
    -- function getRunningProcess return ProcPtr
    -- is
    -- begin
    --     return Proctab(PerCPUData.getCurrentPID)'Access;
    -- end getRunningProcess;

    ---------------------------------------------------------------------------
    -- pageFault
    ---------------------------------------------------------------------------
    procedure pageFault (pid : ProcessID; addr : System.Address) with
        SPARK_Mode => On
    is
        ignore : System.Address;
    begin
        -- Valid stack or heap address? heapEnd and heapStart should always be
        -- page aligned, so we can round down to lower page here when mapping.
        if (addr <= Proctab(pid).stackTop and addr >= Proctab(pid).stackBottom) or
           (addr <= Proctab(pid).heapEnd  and addr >= Proctab(pid).heapStart) then

            -- print ("Process: Adding page to process "); println (pid);
            addPage (proc    => Proctab(pid),
                     mapTo   => To_Address (To_Integer (addr) and Virtmem.PAGE_MASK),   -- round down
                     storage => ignore);
            -- print ("Process: Added page at "); println (To_Address (To_Integer (addr) and Virtmem.PAGE_MASK));
        else
            -- @TODO use a heuristic here to figure out if this was a stack 
            -- overflow, or heap over/underflow and signal the process either way.
            -- (something like distance to stackBottom < distance to heapEnd = stack overflow)
            print ("Process: Illegal memory access, killing pid "); println (pid);
            kill (pid);
        end if;
    end pageFault;

    ---------------------------------------------------------------------------
    -- enableFPU
    -- Turn on FPU state saving/restoring for this process.
    ---------------------------------------------------------------------------
    procedure enableFPU with SPARK_Mode => On
    is
        pid : constant ProcessID := PerCPUData.getCurrentPID;
        newCR0 : Unsigned_64 := x86.getCR0;
        newCR4 : Unsigned_64 := x86.getCR4;
    begin
        Util.clearBit (newCR0, 2);  -- clear CR0.EM
        Util.setBit (newCR0, 1);    -- set CR0.MP
        Util.setBit (newCR4, 9);    -- set CR4.OSFXSR
        Util.setBit (newCR4, 10);   -- set CR4.OSXMMEXCPT

        proctab(pid).fpu := proctab(pid).kernelStack.all.fpuarea'Address;
        
        x86.setCR0 (newCR0);
        x86.setCR4 (newCR4);
    end enableFPU;

    ---------------------------------------------------------------------------
    -- disableFPU
    -- Disable FPU across all processes. This will cause an exception when FPU
    -- is used again.
    ---------------------------------------------------------------------------
    procedure disableFPU with SPARK_Mode => On
    is
        newCR0 : Unsigned_64 := x86.getCR0;
        newCR4 : Unsigned_64 := x86.getCR4;
    begin
        Util.setBit (newCR0, 2);        -- set CR0.EM
        Util.clearBit (newCR0, 1);      -- clear CR0.MP
        Util.clearBit (newCR4, 9);      -- clear CR4.OSFXSR
        Util.clearBit (newCR4, 10);     -- clear CR4.OSXMMEXCPT

        x86.setCR0 (newCR0);
        x86.setCR4 (newCR4);
    end disableFPU;

    ---------------------------------------------------------------------------
    -- saveFPUState
    ---------------------------------------------------------------------------
    procedure saveFPUState (pid : ProcessID) with SPARK_Mode => On
    is
    begin
        if proctab(pid).fpu /= System.Null_Address then
            x86.fxsave (proctab(pid).fpu);

            -- When saving the FPU state, disable the FPU so we can detect use when
            -- the next process runs, and if used, set up the save area.
            disableFPU;
        end if;

    end saveFPUState;

    ---------------------------------------------------------------------------
    -- restoreFPUState
    ---------------------------------------------------------------------------
    procedure restoreFPUState (pid : ProcessID) with SPARK_Mode => On
    is
    begin
        -- If this process uses the FPU, go ahead and enable it.
        if proctab(pid).fpu /= System.Null_Address then
            x86.fxrstor (proctab(pid).fpu);
            enableFPU;
        end if;
    end restoreFPUState;

    ---------------------------------------------------------------------------
    -- print
    ---------------------------------------------------------------------------
    procedure print (p : ProcPtr) is
    begin
        -- @TODO
        null;
    end print;

    ---------------------------------------------------------------------------
    -- Track which PIDs are in use, allocate new ones.
    ---------------------------------------------------------------------------
    package body PIDTracker with 
        Refined_State => (PIDTrackerState => (pidMap, pidLock, trackerLockName))
    is

        -- TODO: this is basically cut-n-paste from the bootmem allocator.
        -- might be kind of nice to genericize these into a "bitmap" package

        -- Find a free PID and mark it as in use. Uses spinlock
        -- to ensure that two processes don't share the same PID
        -- if this were called by two threads at once.
        procedure allocPID(pid : out ProcessID) with
            SPARK_Mode => On
        is
            use Spinlocks;
        begin
            enterCriticalSection (pidLock);
            pid := findFreePID;
            -- if no free PIDs, we'll mark PID 0 as used again, which is true.
            markUsed (pid);
            exitCriticalSection (pidLock);
        end allocPID;

        procedure allocSpecificPID (pid : in ProcessID) with
            SPARK_Mode => On
        is
            use Spinlocks;
        begin
            enterCriticalSection (pidLock);
            
            if pidMap (pid) = False then
                raise ProcessException with "Attempted to use specific PID already in use";
            end if;
            
            markUsed (pid);
            exitCriticalSection (pidLock);
        end allocSpecificPID;


        -- Mark a PID as free.
        procedure freePID(pid : in ProcessID) with
            SPARK_Mode => On
        is
        begin
            markFree(pid);
        end freePID;


        -- Find a free PID
        function findFreePID return ProcessID with
            SPARK_Mode => On
        is
            --block : Unsigned_64;
            --retPID : ProcessID := 0;
        begin

            -- linearly iterate through the list looking for a 0. We reserve
            -- the first few PIDs for the kernel to use for tasks with specific
            -- IDs.
            for i in 16..ProcessID'Last loop
                if (pidMap(i)) then
                    return i;
                end if;
            end loop;

            return 0;
        end findFreePID;


        -- Mark a particular PID as used.
        procedure markUsed(pid : in ProcessID) with
            SPARK_Mode => On
        is
            --block : constant PIDBlock := getBlock(pid);
            --offset : constant PIDOffset := getOffset(pid);
        begin
            --util.setBit(pidMap(block), offset);
            pidMap(pid) := False;
        end markUsed;
        

        -- Mark a PID as free.
        procedure markFree(pid : in ProcessID) with
            SPARK_Mode => On
        is
            -- block : constant PIDBlock := getBlock(pid);
            -- offset : constant PIDOffset := getOffset(pid);
        begin
            --util.clearBit(pidmap(block), offset);
            pidMap(pid) := True;
        end markFree;


        -- -- Return the index into bitmap array in which this PID resides.
        -- function getBlock(pid : in ProcessID) return PIDBlock with
        --     SPARK_Mode => On is
        -- begin
        --     return Natural(pid / 64);
        -- end getBlock;


        -- -- Return the bit within a Unsigned_64 representing this single PID.
        -- function getOffset(pid : in ProcessID) return PIDOffset with
        --     SPARK_Mode => On is
        -- begin
        --     return Natural(pid mod 64);
        -- end getOffset;

    end PIDTracker;

end Process;