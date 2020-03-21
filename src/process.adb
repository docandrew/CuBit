-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2019 Jon Andrew
--
-- CuBitOS Processes
-------------------------------------------------------------------------------
with System.Address_to_Access_Conversions;
with System.Machine_Code; use System.Machine_Code;
with Ada.Unchecked_Conversion;

with BuddyAllocator;
with Config;
with Mem_mgr;
with Scheduler;
with Segment;
with Serial;
with Textmode; use Textmode;
with x86;

package body Process
    with SPARK_Mode => On
is

    function create(imageStart  : in Virtmem.PhysAddress;
                    imageSize   : in Unsigned_64;
                    procStart   : in Virtmem.VirtAddress;
                    ppid        : in ProcessID;
                    name        : in ProcessName;
                    priority    : in ProcessPriority;
                    procStack   : in Virtmem.VirtAddress) return Process
        with SPARK_Mode => Off
    is
        
        newProc         : Process;

        pgTablePhys     : Virtmem.PhysAddress;
        textPhys        : Virtmem.PhysAddress;

        uStackPhys      : Virtmem.PhysAddress;
        uStackAddr      : Virtmem.VirtAddress := procStack - Virtmem.PAGE_SIZE;

        kStackPhys      : Virtmem.PhysAddress;
        kStackAddr      : Virtmem.VirtAddress;
    begin
        PIDTracker.allocPID(newProc.pid);

        -- sanity checks
        if newProc.pid = 0 then
            println("ERR: Unable to create new process. No free PIDs");
            return newProc;
        end if;

        print("New process PID: "); println(newProc.pid);

        newProc.ppid        := ppid;
        newProc.name        := name;
        newProc.state       := READY;
        --newProc.securityTag := secTag;
        newProc.priority    := priority;
        --newProc.privilege   := privilege;
        newProc.stackTop    := procStack;

        -- Allocate memory for the process' top-level page table
        -- BuddyAllocator.allocFrame(To_Integer(newProc.pgTable));
        -- if newProc.pgTable = 0 then
        --     newProc.pid := 0;
        --     return newProc;
        -- end if;

        -- Allocate the process' kernel-space stack.
        BuddyAllocator.alloc(2, kStackPhys);
        if kStackPhys = 0 then
            newProc.pid := 0;
            return newProc;
        end if;

        -- Allocate a page for the process' user-space stack.
        BuddyAllocator.alloc(0, uStackPhys);
        if uStackPhys = 0 then
            newProc.pid := 0;
            return newProc;
        end if;

        -- Allocate memory for the process' image
        -- BuddyAllocator.allocFrame(procPhys);
        -- if procPhys = 0 then
        --     newProc.pid := 0;
        --     return newProc;
        -- end if;

        -- put the process' kernel stack in the 4th page we allocated.
        kStackAddr := Virtmem.P2V(kStackPhys + 3 * Virtmem.PAGE_SIZE);

        -- Build the initial kernel stack.
        setupKernelStack : declare

            newKernelStack : ProcessKernelStack with 
                Import, Address => To_Address(kStackAddr);
        begin

            println(" Kernel Stack Layout: ");
            print("  kStackAddr:     "); println(kStackAddr);
            --print("  filler:         "); println(newKernelStack.filler'Address);
            --print("  filler size:    "); println(Unsigned_64(PKStackFiller'Object_Size / 8));
            --print("  context:        "); println(newKernelStack.context'Address);
            --print("  contextSize:    "); println(Unsigned_64(SavedState'Object_Size / 8));
            --print("  return Address: "); println(newKernelStack.returnAddress'Address);
            --print("  returnSize:     "); println(Unsigned_64(System.Address'Object_Size / 8));
            --print("  interruptFrame: "); println(newKernelStack.interruptFrame'Address);
            --print("  intFrameSize:   "); println(Unsigned_64(interrupt.ExceptionStackFrame'Object_Size / 8));

            newKernelStack.filler := (others => 0);

            -- Since we use iretq to enter usermode, we need an "interrupt frame"
            -- to set up the proper rip, rsp, flags and segments.
            -- Set the interrupt stack frame RIP to the process main, so when
            -- the interrupt returns, we start executing code there...
            newKernelStack.interruptFrame := (
                    interruptNumber => 0, 
                    rip             => Unsigned_64(procStart),
                    rsp             => Unsigned_64(procStack),
                    rflags          => x86.FLAGS_INTERRUPT,
                    cs              => 
                        Segment.GDTOffset'Enum_Rep(Segment.GDT_OFFSET_USER_CODE) or 3,
                    ss              =>
                        Segment.GDTOffset'Enum_Rep(Segment.GDT_OFFSET_USER_DATA) or 3,
                    others          => 0);

            -- ... but first we need "start" to return to the usermode entry point...
            newKernelStack.returnAddress := interruptReturn'Address;

            -- ... but before that, we need "switch" to return to the start; procedure
            newKernelStack.context := (others => 0);    -- clear it out
            newKernelStack.context.rip := Util.addrToNum(start'Address);

            --newKernelStack.context.rbp := Util.addrToNum(newKernelStack.context'Address);
            print(" nks.context.rip: "); println(newKernelStack.context.rip);
            print(" nks.context.rbp: "); println(newKernelStack.context.rbp);

            -- Set the process' kernel stack
            newProc.kernelStackBottom := Virtmem.P2V(kStackPhys);
            newProc.kernelStackTop := kStackAddr + (Virtmem.FRAME_SIZE);

            print(" newProc.kernelStackBottom: "); println(newProc.kernelStackBottom);
            print(" newProc.kernelStackTop: "); println(newProc.kernelStackTop);

            newProc.context := newKernelStack.context'Address;
            print(" newProc.context: "); println(newProc.context);
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
        println("Setting up process page tables");
        setupPageTables : declare
            -- processP4 : Virtmem.P4 with
            --     Import, Address => To_Address(Virtmem.P2V(pageTableAddr));

            procedure mapPage is new Virtmem.mapPage(BuddyAllocator.allocFrame);
            procedure zeroize is new Virtmem.zeroize(Virtmem.P4);

            ok : Boolean;
            MapException : exception;
        begin
            println(" Zeroizing process P4 page table");
            zeroize(newProc.pgTable);

            -- The process needs at least some part of the kernel mapped
            -- during operations, so that when interrupts and syscalls happen,
            -- the kernel code can execute. We could copy entries from the
            -- kernel's page tables, but we re-allocate new ones here so later
            -- it's easier for us to free the whole table and its associated
            -- memory in one fell-swoop using Virtmem.deleteP4
            -- println(" Linear-mapping first MiB in process' address space");
            -- for pg in Virtmem.addrToPFN(0)..Virtmem.addrToPFN(16#100000#) loop
            --     mapPage(Virtmem.pfnToAddr(pg),
            --             Virtmem.P2V(Virtmem.pfnToAddr(pg)),
            --             Virtmem.PG_KERNELDATA,
            --             newProc.pgTable,
            --             ok);

            --     if not ok then
            --         raise MapException with 
            --             "Process.create - can't map kernel's .text";
            --     end if;
            -- end loop;

            println(" Mapping kernel in process' address space");
            -- map kernel pages in the process' address space.
            for pg in Mem_mgr.kernelTextPages'Range loop
                mapPage(Virtmem.pfnToAddr(pg),
                        Virtmem.pfnToAddr(pg) + Virtmem.KERNEL_BASE,
                        Virtmem.PG_KERNELCODE,
                        newProc.pgTable,
                        ok);

                if not ok then
                    raise MapException with 
                        "Process.create - can't map kernel's .text";
                end if;
            end loop;

            for pg in Mem_mgr.kernelROPages loop
                mapPage(Virtmem.pfnToAddr(pg),
                        Virtmem.pfnToAddr(pg) + Virtmem.KERNEL_BASE,
                        Virtmem.PG_KERNELDATARO,
                        newProc.pgTable,
                        ok);
                if not ok then
                    raise MapException with
                        "Process.create - can't map kernel's RO data";
                end if;
            end loop;

            for pg in Mem_mgr.kernelRWPages loop
                mapPage(Virtmem.pfnToAddr(pg),
                        Virtmem.pfnToAddr(pg) + Virtmem.KERNEL_BASE,
                        Virtmem.PG_KERNELDATA,
                        newProc.pgTable,
                        ok);
                if not ok then
                    raise MapException with
                        "Process.create - can't map kernel's RW data";
                end if;
            end loop;
            
            for pg in Mem_mgr.kernelStackPages loop
                mapPage(Virtmem.pfnToAddr(pg),
                        Virtmem.P2V(Virtmem.pfnToAddr(pg)),
                        Virtmem.PG_KERNELDATA,
                        newProc.pgTable,
                        ok);
                if not ok then
                    raise MapException with
                        "Process.create - can't map kernel's RW data";
                end if;
            end loop;

            -- map the process' image
            -- TODO: load it from an ELF image.
            -- for pg in processStackPages loop, etc...
            print("Mapping process image from "); print(imageStart);
            println(" to virtual address 0");
            mapPage(imageStart,
                    0,
                    Virtmem.PG_USERCODE,
                    newProc.pgTable,
                    ok);

            if not ok then
                raise MapException with
                "Process.create - can't map process' image";
            end if;

            -- map the process' user stack
            println("Mapping process stack from "); print(uStackPhys);
            print(" to virtual address "); println(uStackAddr);
            mapPage(uStackPhys,
                    uStackAddr,
                    Virtmem.PG_USERDATA,
                    newProc.pgTable,
                    ok);

            if not ok then
                raise MapException with
                "Process.create - can't map process' stack";
            end if;

            -- map all of the process' kernel stack pages
            print("Mapping process kernel stack from "); print(kStackPhys);
            print(" to virtual address "); println(Virtmem.P2V(kStackPhys));
            mapPage(kStackPhys,
                    Virtmem.P2V(kStackPhys),
                    Virtmem.PG_KERNELDATA,
                    newProc.pgTable,
                    ok);
            mapPage(kStackPhys + Virtmem.PAGE_SIZE,
                    Virtmem.P2V(kStackPhys + Virtmem.PAGE_SIZE),
                    Virtmem.PG_KERNELDATA,
                    newProc.pgTable,
                    ok);
            mapPage(kStackPhys + 2 * Virtmem.PAGE_SIZE,
                    Virtmem.P2V(kStackPhys + 2 * Virtmem.PAGE_SIZE),
                    Virtmem.PG_KERNELDATA,
                    newProc.pgTable,
                    ok);
            mapPage(kStackPhys + 3 * Virtmem.PAGE_SIZE,
                    Virtmem.P2V(kStackPhys + 3 * Virtmem.PAGE_SIZE),
                    Virtmem.PG_KERNELDATA,
                    newProc.pgTable,
                    ok);

            if not ok then
                raise MapException with
                "Process.create - can't map process' kernel stack";
            end if;
            
            -- TODO: get stack size from the ELF image
            -- TODO: keep a list of process pages so we can delete them later.
        end setupPageTables;

        return newProc;
    end create;


    procedure addToProctab(proc : in Process) with
        SPARK_Mode => On
    is
    begin
        -- add our new entry to the proctab.
        Spinlock.enterCriticalSection(lock);
        proctab(proc.pid) := proc;
        Spinlock.exitCriticalSection(lock);
    end addToProctab;


    ---------------------------------------------------------------------------
    -- This is where READY processes continue executing after the scheduler 
    --  puts them in the RUNNING state. Note that the scheduler acquires the
    --  proctab lock in schedule;, but we must release that lock here.
    ---------------------------------------------------------------------------
    procedure yield with
        SPARK_Mode => On
    is
    begin
        Spinlock.enterCriticalSection(lock);
        --textmode.println("Yielding.");
        Scheduler.enter;
        -- continue execution here after context switch back to this process.
        Spinlock.exitCriticalSection(lock);
    end yield;


    ---------------------------------------------------------------------------
    -- This is where the scheduler will initially switch() 
    ---------------------------------------------------------------------------
    procedure start with
        SPARK_Mode => On
    is
    begin
        --textmode.println("starting");

        Spinlock.exitCriticalSection(lock);
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
        BuddyAllocator.alloc(0, alignedStart);

        -- copy the init image to a new page, aligned at 0
        ignore := Util.memcpy(To_Address(Virtmem.P2V(alignedStart)),
                              initBinaryStart'Address,
                              Integer(initSize));

        print(" Creating init process, image at: "); print(alignedStart);
        print(", size: "); printdln(initSize);

        initProcess := create(
                imageStart  => alignedStart,
                imageSize   => initSize,
                procStart   => 0,
                ppid        => 0,
                name        => "init            ",
                priority    => 3,
                procStack   => Integer_Address(2 * Virtmem.PAGE_SIZE));

        println("Adding to proctab");
        addToProctab(initProcess);
        println("Done creating process.");
    end createFirstProcess;


    ---------------------------------------------------------------------------
    -- switchAddressSpace
    ---------------------------------------------------------------------------
    procedure switchAddressSpace(processP4 : in System.Address) with
        SPARK_Mode => On
    is
    begin
        print("Setting p4 to "); println(Virtmem.K2P(To_Integer(processP4)));
        Virtmem.setActiveP4(Virtmem.K2P(To_Integer(processP4)));
    end switchAddressSpace;


    ---------------------------------------------------------------------------
    -- kill
    -- End this process
    ---------------------------------------------------------------------------
    procedure kill(pid : in ProcessID) with
        SPARK_Mode => On
    is
        -- recursively unmaps/deallocates process' full paging hierarchy
        procedure deleteP4 is new Virtmem.deleteP4(BuddyAllocator.freeFrame);
    begin
        Spinlock.enterCriticalSection(lock);

        proctab(pid).state := INVALID;
        deleteP4(proctab(pid).pgTable);

        -- free memory allocated for the stacks.
        -- TODO: we'll need to keep a list of physical pages used by this
        -- process once we start allocating more memory for it.
        BuddyAllocator.free(2, Virtmem.V2P(proctab(pid).kernelStackBottom));
        BuddyAllocator.free(0, Virtmem.V2P(To_Integer(proctab(pid).stackBottom)));
        
        Spinlock.exitCriticalSection(lock);
    end kill;

    ---------------------------------------------------------------------------
    -- Track which PIDs are in use, allocate new ones.
    ---------------------------------------------------------------------------
    package body PIDTracker with 
        Refined_State => (PIDTrackerState => (pidMap, pidLock))
    is

        -- TODO: this is basically cut-n-paste from the bootmem allocator.
        -- might be kind of nice to genericize these into a "bitmap" package

        -- Find a free PID and mark it as in use. Uses spinlock
        -- to ensure that two processes don't share the same PID
        -- if this were called by two threads at once.
        procedure allocPID(pid : out ProcessID) with
            SPARK_Mode => On
        is
            use spinlock;
        begin
            enterCriticalSection(pidLock);
            pid := findFreePID;
            -- if no free PIDs, we'll mark PID 0 as used again, which is true.
            markUsed(pid);
            exitCriticalSection(pidLock);
        end allocPID;


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