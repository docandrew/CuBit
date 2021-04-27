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
with System.Address_to_Access_Conversions;
with System.Machine_Code; use System.Machine_Code;
with System.Storage_Elements; use System.Storage_Elements;

with BuddyAllocator;
with Config;
with Mem_mgr;
with PerCPUData;
with Scheduler;
with Segment;
with Serial;
with TextIO; use TextIO;
with x86;

package body Process
    with SPARK_Mode => On
is
    ---------------------------------------------------------------------------
    -- setup
    ---------------------------------------------------------------------------
    -- procedure setup is
    -- begin
    --     ProcList.setup (allProcs, Config.MAX_PROCESSES);
    -- end setup;

    ---------------------------------------------------------------------------
    -- addToProctab
    ---------------------------------------------------------------------------
    procedure addToProctab (proc : in Process) with
        SPARK_Mode => On
    is
    begin
        Spinlocks.enterCriticalSection (lock);

        proctab(proc.pid) := proc;

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
        proc.ppid     := 0;
        proc.name     := name;
        proc.mode     := KERNEL;
        proc.state    := READY;
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
        proc : Process := createKernelThread (procStart, name, pid, priority);
    begin
        if proc.pid /= 0 then
            addToProctab (proc);
        end if;
    end startKernelThread;

    ---------------------------------------------------------------------------
    -- addStackPage
    -- Allocate an additional page of memory for this process' stack, map it
    -- into the process' page tables, and add it to the list of pages used by
    -- this process.
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
        
        FrameList.insertFront (proc.frames, newFrame);

        -- Map the frame just below the current stack.
        mapPage (newFrame,
                 To_Integer(proc.stackTop - Storage_Count((proc.numStackFrames + 1) * Virtmem.PAGE_SIZE)),
                 Virtmem.PG_USERDATA,
                 proc.pgTable,
                 ok);

        if not ok then
            raise MapException with
            "Process.create - can't map process' stack";
        end if;

        proc.numStackFrames := proc.numStackFrames + 1;
    end addStackPage;

    ---------------------------------------------------------------------------
    -- create
    ---------------------------------------------------------------------------
    function create (imageStart  : in System.Address;
                     imageSize   : in Storage_Count;
                     procStart   : in System.Address;
                     ppid        : in ProcessID;
                     name        : in ProcessName;
                     priority    : in ProcessPriority;
                     procStack   : in System.Address) return Process
        with SPARK_Mode => Off
    is
        proc            : Process;
        pgTablePhys     : Virtmem.PhysAddress;

        -- Address of our allocated stack (initial)
        userStack       : System.Address;
        userStackPhys   : Virtmem.PhysAddress;
    begin
        PIDTracker.allocPID (proc.pid);

        -- sanity checks
        if proc.pid = 0 then
            raise ProcessException with "Unable to create new process. No free PIDs";
        end if;

        proc.ppid        := ppid;
        proc.name        := name;
        proc.mode        := USER;
        proc.state       := READY;
        proc.priority    := priority;
        proc.stackTop    := procStack;
        proc.kernelStack := new ProcessKernelStack;

        FrameList.setup (proc.frames, MAX_STACK_FRAMES + MAX_HEAP_FRAMES);

        proc.kernelStackTop := proc.kernelStack.all'Address + ProcessKernelStack'Size / 8;

        -- Build the initial kernel stack.
        setupKernelStack : declare
        begin
            proc.kernelStack.filler := (others => 0);

            -- Since we use iretq to enter usermode, we need an "interrupt frame"
            -- to set up the proper rip, rsp, flags and segments.
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
        end setupKernelStack;

        -- Map the necessary text, stack into the user process' page tables.
        setupPageTables : declare

            procedure mapPage is new Virtmem.mapPage (BuddyAllocator.allocFrame);
            procedure zeroize is new Virtmem.zeroize (Virtmem.P4);

            ok : Boolean;
            MapException : exception;
        begin
            zeroize (proc.pgTable);

            Mem_mgr.mapKernelMemIntoProcess (proc.pgTable);

            -- map the process' image
            -- TODO: load it from an ELF image.
            -- for pg in processStackPages loop, etc...
            -- print ("Mapping process image from "); print (imageStart);
            -- println(" to virtual address 0");
            mapPage (Virtmem.V2P (imageStart),
                     0,
                     Virtmem.PG_USERCODE,
                     proc.pgTable,
                     ok);

            if not ok then
                raise MapException with
                "Process.create - can't map process' image";
            end if;

            addStackPage (proc);
        end setupPageTables;

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
        Spinlocks.enterCriticalSection (lock);
        Scheduler.enter;

        -- continue execution here after context switch back to this process.
        Spinlocks.exitCriticalSection (lock);
    end yield;

    ---------------------------------------------------------------------------
    -- receive
    -- Return the message if it exists, yield otherwise.
    ---------------------------------------------------------------------------
    function receive return Unsigned_64 with SPARK_Mode => On is
        pid     : constant ProcessID := PerCPUData.getCurrentPID;
        message : Unsigned_64;
    begin
        Spinlocks.enterCriticalSection (lock);

        message := proctab(pid).mail.message;

        if not proctab(pid).mail.hasMsg then
            proctab(pid).state := RECEIVING;
            Scheduler.enter;
        end if;

        proctab(pid).mail.hasMsg := False;
        
        Spinlocks.exitCriticalSection (lock);

        return message;
    end receive;

    ---------------------------------------------------------------------------
    -- receiveNB
    ---------------------------------------------------------------------------
    function receiveNB return Unsigned_64 with SPARK_Mode => On is
        pid     : constant ProcessID := PerCPUData.getCurrentPID;
        message : Unsigned_64;
    begin
        Spinlocks.enterCriticalSection (lock);

        if proctab(pid).mail.hasMsg then
            message := proctab(pid).mail.message;
            proctab(pid).mail.hasMsg := False;
        else
            message := Unsigned_64'Last;
        end if;

        Spinlocks.exitCriticalSection (lock);

        return message;
    end receiveNB;

    ---------------------------------------------------------------------------
    -- send
    -- This may be called by an interrupt handler, so not appropriate to 
    -- re-enter the scheduler. Deadlock will result.
    ---------------------------------------------------------------------------
    procedure send (dest : ProcessID; msg : Unsigned_64) with SPARK_Mode => On is
    begin
        Spinlocks.enterCriticalSection (lock);

        proctab(dest).mail.hasMsg  := True;
        proctab(dest).mail.message := msg;

        if proctab(dest).state = RECEIVING then
            proctab(dest).state := READY;
        end if;

        Spinlocks.exitCriticalSection (lock);
    end send;

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
        Spinlocks.enterCriticalSection(lock);
        Spinlocks.exitCriticalSection(resourceLock);

        -- Begin waiting and reschedule.
        proctab(pid).state := WAITING;
        proctab(pid).channel := channel;
        Scheduler.enter;

        -- Resume here when woken.
        proctab(pid).channel := NO_CHANNEL;

        -- Should only be woken when we can acquire the resource lock.
        Spinlocks.exitCriticalSection(lock);
        Spinlocks.enterCriticalSection(resourceLock);
    end wait;

    ---------------------------------------------------------------------------
    -- goAheadBody
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
        Spinlocks.enterCriticalSection(lock);
        goAheadBody(channel);
        Spinlocks.exitCriticalSection(lock);
    end goAhead;

    ---------------------------------------------------------------------------
    -- This is where the scheduler will initially switch() to.
    ---------------------------------------------------------------------------
    procedure start with
        SPARK_Mode => On
    is
    begin
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
        initSize : Storage_Count := Storage_Count(Util.addrToNum (initBinarySize'Address));

        initProcess : Process;

        InitImageTooBigException : exception;

        alignedStart    : System.Address;  -- will be start from ELF process
        ignore          : System.Address;
    begin
        if initSize > Virtmem.PAGE_SIZE then
            raise InitImageTooBigException with "Init image is too big to fit in one page.";
        end if;

        -- get a new page for us to copy the image to
        BuddyAllocator.alloc (BuddyAllocator.getOrder (initSize), alignedStart);

        -- copy the init image to a new page, aligned at 0
        ignore := Util.memcpy (alignedStart,
                               initBinaryStart'Address,
                               Integer(initSize));

        -- @TODO put the stack way up on top of lower-half like a real process.
        initProcess := create (imageStart  => alignedStart,
                               imageSize   => initSize,
                               procStart   => To_Address(0),
                               ppid        => 1,
                               name        => "init            ",
                               priority    => 3,
                               procStack   => To_Address(2 * Virtmem.PAGE_SIZE));

        addToProctab (initProcess);

    end createFirstProcess;

    ---------------------------------------------------------------------------
    -- switchAddressSpace
    ---------------------------------------------------------------------------
    procedure switchAddressSpace (processP4 : in System.Address) with
        SPARK_Mode => On
    is
    begin
        Virtmem.setActiveP4 (Virtmem.K2P (To_Integer(processP4)));
    end switchAddressSpace;

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
    begin
        -- Back to kernel addressing.
        Mem_mgr.switchAddressSpace;

        Spinlocks.enterCriticalSection (lock);

        proctab(pid).state := INVALID;
        if proctab(pid).mode = USER then

            --@TODO memory leak here with first process' .text. Not a huge
            -- deal since eventually once the first process exits we'll shut down.
            -- Free all memory used by the process.
            loop
                BuddyAllocator.freeFrame (FrameList.front(proctab(pid).frames));
                FrameList.popFront (proctab(pid).frames);

                exit when proctab(pid).frames.length = 0;
            end loop;

            FrameList.teardown (proctab(pid).frames);

            -- Need to unmap Kernel mem here so when we delete page tables we
            -- only delete the process' page tables.
            Mem_mgr.unmapKernelMemFromProcess (proctab(pid).pgTable);
            deleteP4 (proctab(pid).pgTable);
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
    function getRunningProcess return ProcPtr
    is
    begin
        return Proctab(PerCPUData.getCurrentPID)'Access;
    end getRunningProcess;

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
            enterCriticalSection(pidLock);
            pid := findFreePID;
            -- if no free PIDs, we'll mark PID 0 as used again, which is true.
            markUsed(pid);
            exitCriticalSection(pidLock);
        end allocPID;

        procedure allocSpecificPID (pid : in ProcessID) with
            SPARK_Mode => On
        is
            use Spinlocks;
        begin
            enterCriticalSection (pidLock);
            
            if pidMap(pid) = False then
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

            -- linearly iterate through the list looking for a 0
            for i in ProcessID loop
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