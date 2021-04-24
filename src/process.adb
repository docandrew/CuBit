-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2019 Jon Andrew
--
-- CuBitOS Processes
--
--@TODO There are a lot of interwoven locks here that would be nice to model in
-- SPARK Mode to get some guarantees of correctness.
-------------------------------------------------------------------------------
with System.Address_to_Access_Conversions;
with System.Machine_Code; use System.Machine_Code;
with Ada.Unchecked_Conversion;

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
        -- add our new entry to the proctab.
        -- println ("Process.addToProcTab grabbing Process lock");
        Spinlocks.enterCriticalSection (lock);
        proctab(proc.pid) := proc;
        Spinlocks.exitCriticalSection (lock);
        -- println ("Process.addToProcTab released Process lock");
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
        -- kStackPhys : Virtmem.PhysAddress;
        -- kStackAddr : Virtmem.VirtAddress;
    begin
        -- println ("Process: createKernelThread");
        PIDTracker.allocSpecificPID (pid);
        -- PIDTracker.allocPID (proc.pid);

        -- if proc.pid = 0 then
        --     println ("Process: Unable to create new kernel thread. No free PIDs");
        --     return proc;
        -- end if;

        proc.pid         := pid;
        proc.ppid        := 0;
        proc.name        := name;
        proc.mode        := KERNEL;
        proc.state       := READY;
        proc.priority    := priority;

        -- Depending on how crazy these threads get, we may consider making this
        -- a different type with more room.
        proc.kernelStack := new ProcessKernelStack;
        
        -- print ("Process: New kernel thread PID: "); println (proc.pid);
        -- Allocate thread's stack
        -- BuddyAllocator.alloc (2, kStackPhys);
        -- if kStackPhys = 0 then
        --     proc.pid := 0;
        --     return proc;
        -- end if;
        proc.kernelStackTop := proc.kernelStack.all'Address + ProcessKernelStack'Size / 8;
        -- print ("Process: New kernel thread stack: "); println (kStackAddr);

        -- setupKernelStack : declare
        --     kernStack : ProcessKernelStack with Import, Address => To_Address(kStackAddr);
        -- begin
        proc.kernelStack.filler := (others => 0);

        proc.kernelStack.interruptFrame := (
                interruptNumber => 0,    
                rip             => procStart,
                rsp             => kStackAddr,
                rflags          => x86.FLAGS_INTERRUPT,
                cs              => Segment.GDTOffset'Enum_Rep(Segment.GDT_OFFSET_KERNEL_CODE) or 0,
                ss              => Segment.GDTOffset'Enum_Rep(Segment.GDT_OFFSET_KERNEL_DATA) or 0,
                others          => 0
            );

        proc.kernelStack.returnAddress := interruptReturn'Address;
        proc.kernelStack.context       := (rip => start'Address, others => 0);

        --proc.kernelStackBottom  := Virtmem.P2V (kStackPhys);
        --    proc.kernelStackTop     := kStackAddr;
        proc.context := proc.kernelStack.context'Address;
        -- end setupKernelStack;

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
        procedure mapPage is new Virtmem.mapPage (BuddyAllocator.allocFrame);
    begin
        -- Allocate the new frame
        BuddyAllocator.allocFrame (newFrame);

        -- @TODO this shouldn't be a fatal error but works for now to detect
        -- errors.
        if newFrame = 0 then
            raise ProcessException with "Unable to allocate memory for Process' stack expansion.";
        end if;
        
        ProcessPageList.insertFront (newFrame);

        -- Map the frame just below the current stack.
        mapPage (newFrame,
                 proc.stackTop - ((proc.numStackFrames + 1) * Virtmem.PAGE_SIZE),
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
    function create (imageStart  : in Virtmem.PhysAddress;
                     imageSize   : in Unsigned_64;
                     procStart   : in System.Address;
                     ppid        : in ProcessID;
                     name        : in ProcessName;
                     priority    : in ProcessPriority;
                     procStack   : in Virtmem.VirtAddress) return Process
        with SPARK_Mode => Off
    is
        proc            : Process;

        pgTablePhys     : Virtmem.PhysAddress;
        -- textPhys        : Virtmem.PhysAddress;

        -- Address of our allocated stack (initial)
        userStack       : System.Address;

        -- 
        userStackPhys   : Virtmem.PhysAddress;
        userRSP         : Virtmem.VirtAddress := procStack - Virtmem.PAGE_SIZE;

        -- kStackPhys      : Virtmem.PhysAddress;
        -- kStackAddr      : Virtmem.VirtAddress;
    begin
        PIDTracker.allocPID (proc.pid);

        -- sanity checks
        if proc.pid = 0 then
            raise ProcessException with "Unable to create new process. No free PIDs";
            -- println ("ERR: Unable to create new process. No free PIDs");
            -- return proc;
        end if;

        -- print ("New process PID: "); println (newProc.pid);

        proc.ppid        := ppid;
        proc.name        := name;
        proc.mode        := USER;
        proc.state       := READY;
        proc.priority    := priority;
        proc.stackTop    := procStack;
        proc.kernelStack := new ProcessKernelStack;

        ProcessPageList.setup(proc.pages);

        proc.kernelStackTop := proc.kernelStack.all'Address + ProcessKernelStack'Size / 8;

        -- Allocate memory for the process' top-level page table
        -- BuddyAllocator.allocFrame(To_Integer(newProc.pgTable));
        -- if newProc.pgTable = 0 then
        --     newProc.pid := 0;
        --     return newProc;
        -- end if;

        -- Allocate the process' kernel-space stack.
        -- BuddyAllocator.alloc (2, kStackPhys);
        -- if kStackPhys = 0 then
        --     newProc.pid := 0;
        --     return newProc;
        -- end if;

        -- Allocate one page for the process' user-space stack to start.
        

        -- newProc.stackBottomPhys := uStackPhys;      -- physical frames(s) for stack
        -- newProc.stackBottom     := uStackAddr;      -- virtual page addr of stack

        -- Allocate memory for the process' image
        -- BuddyAllocator.allocFrame(procPhys);
        -- if procPhys = 0 then
        --     newProc.pid := 0;
        --     return newProc;
        -- end if;

        -- put the process' kernel stack in the 4th page we allocated.
        -- kStackAddr := Virtmem.P2V(kStackPhys + 3 * Virtmem.PAGE_SIZE);

        -- Build the initial kernel stack.
        setupKernelStack : declare

            -- newKernelStack : ProcessKernelStack with 
            --     Import, Address => To_Address(kStackAddr);
        begin

            -- println (" Kernel Stack Layout: ");
            -- print ("  kStackAddr:     "); println(kStackAddr);
            --print("  filler:         "); println(newKernelStack.filler'Address);
            --print("  filler size:    "); println(Unsigned_64(PKStackFiller'Object_Size / 8));
            --print("  context:        "); println(newKernelStack.context'Address);
            --print("  contextSize:    "); println(Unsigned_64(SavedState'Object_Size / 8));
            --print("  return Address: "); println(newKernelStack.returnAddress'Address);
            --print("  returnSize:     "); println(Unsigned_64(System.Address'Object_Size / 8));
            --print("  interruptFrame: "); println(newKernelStack.interruptFrame'Address);
            --print("  intFrameSize:   "); println(Unsigned_64(interrupt.ExceptionStackFrame'Object_Size / 8));

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

            --newKernelStack.context.rbp := Util.addrToNum(newKernelStack.context'Address);
            -- print (" nks.context.rip: "); println (newKernelStack.context.rip);
            -- print (" nks.context.rbp: "); println (newKernelStack.context.rbp);

            -- Set the process' kernel stack
            -- newProc.kernelStackBottom := Virtmem.P2V (kStackPhys);
            -- newProc.kernelStackTop    := kStackAddr + (Virtmem.FRAME_SIZE);

            -- print (" newProc.kernelStackBottom: "); println (newProc.kernelStackBottom);
            -- print (" newProc.kernelStackTop: ");    println (newProc.kernelStackTop);

            newProc.context := proc.kernelStack.context'Address;
            -- print (" newProc.context: "); println (newProc.context);
        end setupKernelStack;

        -- Map the kernel into the process' address space. We don't give the
        -- user the full set of kernel page tables, only the .text, .bss and
        -- .data for use during interrupts.
        --
        -- This gives us a limited form of KPTI, but we can further restrict
        -- what gets mapped into user space (TODO).
        -- BuddyAllocator.allocFrame(pageTableAddr);
        -- if pageTableAddr = 0 then
        --     newProc.pid := 0;
        --     return newProc;
        -- end if;
        -- println("Setting up process page tables");
        setupPageTables : declare
            -- processP4 : Virtmem.P4 with
            --     Import, Address => To_Address(Virtmem.P2V(pageTableAddr));

            procedure mapPage is new Virtmem.mapPage (BuddyAllocator.allocFrame);
            procedure zeroize is new Virtmem.zeroize (Virtmem.P4);

            ok : Boolean;
            MapException : exception;
        begin
            -- println (" Zeroizing process P4 page table");
            zeroize (newProc.pgTable);

            -- The process needs at least some part of the kernel mapped
            -- during operations, so that when interrupts and syscalls happen,
            -- the kernel code can execute. We could copy entries from the
            -- kernel's page tables, but we re-allocate new ones here so later
            -- it's easier for us to free the whole table and its associated
            -- memory in one fell-swoop using Virtmem.deleteP4
            -- println ("Mapping higher-half into process' space");
            Mem_mgr.mapKernelMemIntoProcess (newProc.pgTable);
            -- println("Linear-mapping first MiB in process' address space");
            -- for pg in Virtmem.addrToPFN(0)..Virtmem.addrToPFN(16#100000#) loop
            --     mapPage (Virtmem.pfnToAddr(pg),
            --              Virtmem.P2V(Virtmem.pfnToAddr(pg)),
            --              Virtmem.PG_KERNELDATA,
            --              newProc.pgTable,
            --              ok);

            --     if not ok then
            --         raise MapException with 
            --             "Process.create - can't map first 1MiB";
            --     end if;
            -- end loop;

            -- println ("Mapping kernel .text in process' address space");
            -- -- map kernel pages in the process' address space.
            -- for pg in Mem_mgr.kernelTextPages'Range loop
            --     mapPage(Virtmem.pfnToAddr(pg),
            --             Virtmem.pfnToAddr(pg) + Virtmem.KERNEL_BASE,
            --             Virtmem.PG_KERNELCODE,
            --             newProc.pgTable,
            --             ok);

            --     if not ok then
            --         raise MapException with 
            --             "Process.create - can't map kernel's .text";
            --     end if;
            -- end loop;

            -- println ("Mapping kernel .rodata in process' address space");
            -- for pg in Mem_mgr.kernelROPages loop
            --     mapPage(Virtmem.pfnToAddr(pg),
            --             Virtmem.pfnToAddr(pg) + Virtmem.KERNEL_BASE,
            --             Virtmem.PG_KERNELDATARO,
            --             newProc.pgTable,
            --             ok);
            --     if not ok then
            --         raise MapException with
            --             "Process.create - can't map kernel's RO data";
            --     end if;
            -- end loop;

            -- println ("Mapping kernel .data in process' address space");
            -- for pg in Mem_mgr.kernelRWPages loop
            --     mapPage(Virtmem.pfnToAddr(pg),
            --             Virtmem.pfnToAddr(pg) + Virtmem.KERNEL_BASE,
            --             Virtmem.PG_KERNELDATA,
            --             newProc.pgTable,
            --             ok);
            --     if not ok then
            --         raise MapException with
            --             "Process.create - can't map kernel's RW data";
            --     end if;
            -- end loop;
            
            -- println ("Mapping kernel stack in process' address space");
            -- for pg in Mem_mgr.kernelStackPages loop
            --     mapPage(Virtmem.pfnToAddr(pg),
            --             Virtmem.P2V(Virtmem.pfnToAddr(pg)),
            --             Virtmem.PG_KERNELDATA,
            --             newProc.pgTable,
            --             ok);
            --     if not ok then
            --         raise MapException with
            --             "Process.create - can't map kernel's RW data";
            --     end if;
            -- end loop;

            -- map the process' image
            -- TODO: load it from an ELF image.
            -- for pg in processStackPages loop, etc...
            -- print("Mapping process image from "); print(imageStart);
            -- println(" to virtual address 0");
            mapPage (imageStart,
                     0,
                     Virtmem.PG_USERCODE,
                     newProc.pgTable,
                     ok);

            if not ok then
                raise MapException with
                "Process.create - can't map process' image";
            end if;

            -- map the process' user stack
            -- print ("Mapping process stack from "); print (uStackPhys);
            -- print (" to virtual address "); println (uStackAddr);
            -- mapPage (uStackPhys,
            --          uStackAddr,
            --          Virtmem.PG_USERDATA,
            --          newProc.pgTable,
            --          ok);
            addStackPage (newProc);

            -- if not ok then
            --     raise MapException with
            --     "Process.create - can't map process' stack";
            -- end if;

            -- -- map all of the process' kernel stack pages
            -- print ("Mapping process kernel stack from "); print (kStackPhys);
            -- print (" to virtual address "); println (Virtmem.P2V(kStackPhys));
            
            -- mapPage (kStackPhys,
            --          Virtmem.P2V(kStackPhys),
            --          Virtmem.PG_KERNELDATA,
            --          newProc.pgTable,
            --          ok);
            -- mapPage (kStackPhys + Virtmem.PAGE_SIZE,
            --          Virtmem.P2V(kStackPhys + Virtmem.PAGE_SIZE),
            --          Virtmem.PG_KERNELDATA,
            --          newProc.pgTable,
            --          ok);
            -- mapPage (kStackPhys + 2 * Virtmem.PAGE_SIZE,
            --          Virtmem.P2V(kStackPhys + 2 * Virtmem.PAGE_SIZE),
            --          Virtmem.PG_KERNELDATA,
            --          newProc.pgTable,
            --          ok);
            -- mapPage (kStackPhys + 3 * Virtmem.PAGE_SIZE,
            --          Virtmem.P2V(kStackPhys + 3 * Virtmem.PAGE_SIZE),
            --          Virtmem.PG_KERNELDATA,
            --          newProc.pgTable,
            --          ok);

            -- if not ok then
            --     raise MapException with
            --     "Process.create - can't map process' kernel stack";
            -- end if;
            
            -- TODO: get stack size from the ELF image
            -- TODO: keep a list of process pages so we can delete them later.
        end setupPageTables;

        return newProc;
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
        -- println ("Process.yield grabbing Process lock");
        Spinlocks.enterCriticalSection (lock);
        --textmode.println("Yielding.");
        Scheduler.enter;
        -- continue execution here after context switch back to this process.
        Spinlocks.exitCriticalSection (lock);
        -- println ("Process.yield released Process lock");
    end yield;

    ---------------------------------------------------------------------------
    -- receive
    -- Return the message if it exists, yield otherwise.
    ---------------------------------------------------------------------------
    function receive return Unsigned_64 with SPARK_Mode => On is
        pid     : constant ProcessID := PerCPUData.getCurrentPID;
        message : Unsigned_64;
    begin
        -- println ("Process.receive grabbing Process lock");
        Spinlocks.enterCriticalSection (lock);

        message := proctab(pid).mail.message;

        if not proctab(pid).mail.hasMsg then
            proctab(pid).state := RECEIVING;
            Scheduler.enter;
        end if;

        proctab(pid).mail.hasMsg := False;
        
        Spinlocks.exitCriticalSection (lock);
        -- println ("Process.receive released Process lock");
        return message;
    end receive;

    ---------------------------------------------------------------------------
    -- receiveNB
    ---------------------------------------------------------------------------
    function receiveNB return Unsigned_64 with SPARK_Mode => On is
        pid     : constant ProcessID := PerCPUData.getCurrentPID;
        message : Unsigned_64;
    begin
        -- println ("Process.receiveNB grabbing Process lock");
        Spinlocks.enterCriticalSection (lock);

        if proctab(pid).mail.hasMsg then
            message := proctab(pid).mail.message;
            proctab(pid).mail.hasMsg := False;
        else
            message := Unsigned_64'Last;
        end if;

        Spinlocks.exitCriticalSection (lock);
        -- println ("Process.receiveNB releasing Process lock");

        return message;
    end receiveNB;

    ---------------------------------------------------------------------------
    -- send
    -- This may be called by an interrupt handler, so not appropriate to 
    -- re-enter the scheduler. Deadlock will result.
    ---------------------------------------------------------------------------
    procedure send (dest : ProcessID; msg : Unsigned_64) with SPARK_Mode => On is
    begin
        -- println ("Process.send grabbing Process lock");
        Spinlocks.enterCriticalSection (lock);

        proctab(dest).mail.hasMsg  := True;
        proctab(dest).mail.message := msg;

        if proctab(dest).state = RECEIVING then
            proctab(dest).state := READY;
        end if;

        Spinlocks.exitCriticalSection (lock);
        -- println ("Process.send released Process lock");
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
        --serial.send(config.serialMirrorPort, 'A');
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
        -- initStart       : Integer_Address := 
        --     To_Integer(initBinaryStart'Address);

        initSize        : Unsigned_64 := Util.addrToNum(initBinarySize'Address);

        initProcess     : Process;

        InitImageTooBigException : exception;

        alignedStart    : Integer_Address;
        ignore          : System.Address;
    begin
        if initSize > Virtmem.PAGE_SIZE then
            raise InitImageTooBigException with "Init image is too big to fit in one page.";
        end if;

        -- get a new page for us to copy the image to
        BuddyAllocator.alloc (0, alignedStart);

        -- copy the init image to a new page, aligned at 0
        ignore := Util.memcpy (To_Address(Virtmem.P2V(alignedStart)),
                               initBinaryStart'Address,
                               Integer(initSize));

        -- print ("Creating init process, image at: "); print (alignedStart);
        -- print (", size: "); printdln (initSize);

        initProcess := create (imageStart  => alignedStart,
                               imageSize   => initSize,
                               procStart   => To_Address(0),
                               ppid        => 1,
                               name        => "init            ",
                               priority    => 3,
                               procStack   => Integer_Address(2 * Virtmem.PAGE_SIZE));

        -- println ("Adding to proctab");
        addToProctab (initProcess);
        -- println ("Done creating process.");
    end createFirstProcess;

    ---------------------------------------------------------------------------
    -- switchAddressSpace
    ---------------------------------------------------------------------------
    procedure switchAddressSpace (processP4 : in System.Address) with
        SPARK_Mode => On
    is
    begin
        -- print("Setting p4 to "); println(Virtmem.K2P(To_Integer(processP4)));
        Virtmem.setActiveP4(Virtmem.K2P(To_Integer(processP4)));
    end switchAddressSpace;


    ---------------------------------------------------------------------------
    -- kill
    ---------------------------------------------------------------------------
    procedure kill (pid : in ProcessID) with
        SPARK_Mode => On
    is
        -- recursively unmaps/deallocates process' full paging hierarchy
        procedure deleteP4 is new Virtmem.deleteP4 (BuddyAllocator.freeFrame);
        
        kStackPhys : Virtmem.PhysAddress := Virtmem.V2P(proctab(pid).kernelStackBottom);
        uStackPhys : Virtmem.PhysAddress;

    begin
        -- Back to kernel addressing.
        Mem_mgr.switchAddressSpace;

        -- println ("Process.kill grabbing Process lock");
        Spinlocks.enterCriticalSection (lock);

        proctab(pid).state := INVALID;

        if proctab(pid).mode = USER then
            uStackPhys := proctab(pid).stackBottomPhys;

            -- Need to unmap Kernel mem here so when we delete page tables we
            -- only delete the process' page tables.
            Mem_mgr.unmapKernelMemFromProcess (proctab(pid).pgTable);
            deleteP4 (proctab(pid).pgTable);

            print (" Freeing user frames");
            loop
                BuddyAllocator.freeFrame (ProcessPageList.front(proctab(pid).frames));
                ProcessPageList.popFront (proctab(pid).frames);

                exit when ProcessPageList.length(proctab(pid).frames) = 0;
            end loop;
            ProcessPageList.teardown (proctab(pid).frames);
        end if;
        
        -- print (" Freeing kernel stack at "); println (kStackPhys);
        --BuddyAllocator.free (2, kStackPhys);
        Ada.Unchecked_Deallocation (proctab(pid).kernelStack);
        
        -- Nothing to return to, go back to scheduler.
        Scheduler.enter;

        -- Should never actually get here.
        raise ProcessException with "Somehow returned from scheduler in Process.kill";
        Spinlocks.exitCriticalSection(lock);
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
    procedure print (p : ProcessPtr) is
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