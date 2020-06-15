-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2019 Jon Andrew
--
-- CuBitOS Entry Point / Main routine
-------------------------------------------------------------------------------
with Ada.Unchecked_Deallocation;
with System.Address_To_Access_Conversions;

pragma Warnings (Off);
with System.Secondary_Stack;
pragma Warnings (On);

with System.Storage_Elements; use System.Storage_Elements;

with ACPI;
with ATA;
with BootAllocator;
with BuddyAllocator;
with Config;
with Cpuid;
with Devices;
--with Debug;

with Filesystem.Ext2;
with Filesystem.VFS;

with Ioapic;
with Interrupt;
with Lapic;
with LinkedList;
with Mem_mgr;
with MemoryAreas;
--with Pagetable;
with Pci;
with PerCpuData;
with Pic;
with Process;
with Scheduler;
with Serial;
with SlabAllocator;
with Textmode; use Textmode;
with Time;
with Timer_pit;
with Virtmem; use Virtmem;
with x86;

-- Not explicitly called, need to be with'd here for compilation
with Last_Chance_Handler;
pragma Unreferenced(Last_Chance_Handler);

with Syscall;
pragma Unreferenced(Syscall);

with FileCache;
pragma Unreferenced(FileCache);

package body kmain is

-- For testing only. Can remove later.
-- package U64List is new LinkedList(Unsigned_64, Textmode.printd);

-- CPU-local data for our BSP
cpu0Data        : PerCPUData.PerCPUData;

apicBase        : virtmem.PhysAddress := 0;
ioapicBase      : virtmem.PhysAddress := 0;
ioapicVirtBase  : virtmem.VirtAddress := 0;

procedure kmain(magic : Unsigned_32; 
                mbInfo_orig : in MultibootInfo)
    with SPARK_Mode => Off
is
    mbOK        : constant Boolean := (magic = 16#2BADB002#);
    mbInfo      : constant MultibootInfo := mbInfo_orig;
    ssPtr       : System.Secondary_Stack.SS_Stack_Ptr;

    NoMultibootException    : exception;
    NoMemoryMapException    : exception;
    NoACPIException         : exception;
    NoAPICException         : exception;
begin
    clear (BLACK);

    initSerial: declare
    begin
        if (config.serialMirror) then
            serial.init(serial.COM1);
            --println("Setting up serial port", textmode.LT_BLUE,
            --    textmode.BLACK);
        end if;
    end initSerial;


    initHello: declare
    begin
        println("CuBitOS v0.0.1 ", LT_BLUE, BLACK);
        println("");

        print("Virtual Kernel Area:    "); 
        print(KERNEL_START_VIRT'Address);
        print(" - "); println(KERNEL_END_VIRT'Address);

        print("Bootstrap Stack Area:   ");
        print(STACK_BOTTOM);
        print(" - "); println(STACK_TOP);

        print("DMA Area:               ");
        print(DMA_REGION_START);
        print(" - "); println(DMA_REGION_END);
    end initHello;

    -- Do this early, so any attempt to use the secondary stack will raise an
    -- exception (see PerCPUData)
    -- (not sure if it's necessary - unless SS_Init is called, it shouldn't 
    --  work.)
    -- x86.wrmsr(x86.MSRs.KERNEL_GS_BASE, 0);

    -- make sure we were loaded by a multiboot-compliant loader
    if not mbOK then
        raise NoMultibootException with "Not loaded by multiboot-compliant loader.";
    end if;


    initCPUID: declare
    begin
        println("Checking CPU Capabilities...", textmode.LT_BLUE, textmode.BLACK);
        print(" Standard CPUID level: "); println(cpuid.getMaxStandardFunction);
        print(" Extended CPUID level: "); println(cpuid.getMaxExtendedFunction);
        cpuid.setupCPUID;

        print("CPU Vendor: "); println(cpuid.cpuVendor, GREEN, BLACK);
        print("CPU Model:  "); println(cpuid.cpuBrand);
        cpuid.printCacheInfo;
    end initCPUID;


    initSections: declare
    begin
        println("Kernel sections:", textmode.LT_BLUE, textmode.BLACK);
        print("text:     "); print(stext'Address);
            print("-"); println(etext'Address);
        print("rodata:   "); print(srodata'Address); 
            print("-"); println(erodata'Address);
        print("data:     "); print(sdata'Address); 
            print("-"); println(edata'Address);
        print("bss:      "); print(sbss'Address); 
            print("-"); println(ebss'Address);
    end initSections;


    initOtherMBInfo: declare
    begin
        if mbInfo.flags.hasBootDevice then
            print("Boot device: "); println(mbInfo.boot_device);
        end if;

        if mbInfo.flags.hasELFSectionHeader then
        println("ELF Sections available");
        end if;

        if mbInfo.flags.hasVideoInfo then
            println("VESA available");
        end if;
    end initOtherMBInfo;


    initPerCPU: declare
    begin
        println("Initializing per-CPU data for CPU #0", Textmode.LT_BLUE,
            Textmode.BLACK);

        PerCPUData.setupPerCPUData( 0, 
                                    cpu0Data, 
                                    cpu0Data'Address,
                                    cpu0Data.gdt'Address, 
                                    cpu0Data.gdtPointer'Address, 
                                    cpu0Data.tss'Address);
    end initPerCPU;


    initSS: declare
    begin
        println("Initializing Secondary Stack for CPU #0", Textmode.LT_BLUE,
            Textmode.BLACK);

        ssPtr := PerCPUData.getSecondaryStack;
        System.Secondary_Stack.SS_Init(ssPtr);
    end initSS;


    initInterruptVectors: declare
    begin
        -- This doesn't have to be done so early, but it's nice to avoid
        -- double-faults early in the boot process for debugging GPFs, etc.
        println("Installing Interrupt Vector Table", textmode.LT_BLUE, 
            textmode.BLACK);
        Interrupt.setupIDT;
        Interrupt.loadIDT;
    end initInterruptVectors;


    initEarlyMemory : declare
        -- add extra area for the framebuffer
        memAreas : MemoryAreas.MemoryAreaArray(1..Multiboot.numAreas(mbInfo) + 1);
    begin
        println("Detecting memory", Textmode.LT_BLUE, Textmode.BLACK);
        if mbInfo.flags.hasMemoryMap then
            memAreas := Multiboot.getMemoryAreas(mbInfo);
        else
            raise NoMemoryMapException with "No memory map available.";
        end if;
        -- add framebuffer info if appropriate.
        if mbInfo.flags.hasFramebuffer then
            memAreas(memAreas'Last).kind        := MemoryAreas.VIDEO;
            memAreas(memAreas'Last).startAddr   := mbInfo.framebuffer_addr;
            memAreas(memAreas'Last).startAddr   := 16#B8000#;
            memAreas(memAreas'Last).endAddr     := 16#BFFFF#;
            -- memAreas(memAreas'Last).endAddr     := mbInfo.framebuffer_addr + 
            --                     Integer_Address(mbInfo.framebuffer_width * 
            --                     mbInfo.framebuffer_height * 
            --                     Unsigned_32(mbInfo.framebuffer_bpp / 8) - 1);

            print("Framebuffer at "); print(mbInfo.framebuffer_addr);
            print(", size: ");  printd(mbInfo.framebuffer_width);
            print("x");         printd(mbInfo.framebuffer_height);
            print(", ");        print(Integer(mbInfo.framebuffer_bpp));
            print(" bpp");
        end if;

        println;
        println("-----------------------------------------------------");
        println("                     Memory Areas                    ");
        println("-----------------------------------------------------");
        println("      Start                 End            Type");
        for area of memAreas loop
            print(area.startAddr); print(" - "); print(area.endAddr); 
            print("   ");
            case area.kind is
                when MemoryAreas.USABLE     => 
                    println("Usable", LT_GREEN, BLACK);

                when MemoryAreas.RESERVED   => 
                    println("Reserved", YELLOW, BLACK);

                when MemoryAreas.ACPI       => 
                    println("ACPI", MAGENTA, BLACK);

                when MemoryAreas.HIBERNATE  => 
                    println("Hibernate", BROWN, BLACK);

                when MemoryAreas.BAD        => 
                    println("Bad", RED, BLACK);

                when MemoryAreas.VIDEO      => 
                    println("Framebuffer", CYAN, BLACK);
            end case;
        end loop;
        println;

        initBootAllocator: declare
            freeMem : Unsigned_64;
        begin
            println("Setting up boot frame allocator...", Textmode.LT_BLUE,
                Textmode.BLACK);

            BootAllocator.setup(memAreas, freeMem);

            printd(freeMem / 16#100000#); println(" MiB free in boot allocator");
            print(" Max addressable : "); println(Virtmem.MAX_PHYS_ADDRESSABLE);
            print(" Max usable      : "); println(Virtmem.MAX_PHYS_USABLE);
        end initBootAllocator;


        initLinearMapping: declare
        begin
            println("Creating kernel page tables",
                    Textmode.LT_BLUE,
                    Textmode.BLACK);

            Mem_mgr.setupLinearMapping(memAreas);
        end initLinearMapping;


        initBuddyAllocator: declare
        begin
            println("Setting up Buddy Allocator...",
                    Textmode.LT_BLUE,
                    Textmode.BLACK);

            BuddyAllocator.setup(memAreas);
            BuddyAllocator.print;
        end initBuddyAllocator;

        --printd(BuddyAllocator.getFreeBytes / 16#100000#); println(" MiB free");
    end initEarlyMemory;
    
    
    initACPI: declare
    begin
        println("Setting up ACPI", textmode.LT_BLUE, textmode.BLACK);
        if not acpi.setup then
            raise NoACPIException with "ACPI Setup Failed, MP tables for SMP data not implemented.";
        end if;
    end initACPI;


    initPIC: declare
    begin
        -- PIC needs to be set up for proper interrupt re-mapping/masking
        println("Setting up 8259 PIC", textmode.LT_BLUE, textmode.BLACK);
        pic.setupPIC;
        interrupt.setInterruptController(interrupt.LEGACY_PIC);
    end initPIC;


    initPIT: declare
    begin
        -- Enable PIT for now just so we have a clock to calibrate the APIC with.
        println("Setting up PIT and enabling timer interrupts", textmode.LT_BLUE, 
            textmode.BLACK);
        timer_pit.setupPIT;
        timer_pit.enable;
        x86.sti;
    end initPIT;


    initAPIC: declare
    begin
        if cpuid.leaf1ecx.hasX2APIC then
            -- TODO: see if x2APIC is available, we'll use it if it is... eventually.
            println("X2APIC Available");
        end if;
        -- TODO: make this the same if statement when x2APIC is done.

        if cpuid.leaf1edx.hasAPIC then
            println("Setting up Local APIC", textmode.LT_BLUE, textmode.BLACK);
            apicBase := PhysAddress(x86.rdmsr(x86.MSRs.APIC_BASE) and x86.MSRs.APIC_BASE_MASK);
            --print("APIC enabled? "); println(x86.rdmsr(x86.MSRs.APIC_BASE));
            -- If ACPI didn't have a good address, we'll try the MSR APIC_BASE.
            if acpi.lapicAddr /= 0 then
                println(" Using APIC base address from ACPI tables. ");

                if acpi.lapicAddr /= apicBase then
                    println("WARNING: ACPI table APIC address doesn't match APIC_BASE MSR.");
                end if;

                apicBase := acpi.lapicAddr;
            else
                print(" APIC address not found in ACPI, using APIC_BASE MSR: ");
            end if;
            
            setupLAPIC : declare
                package myLapic is new lapic(To_Address(virtmem.P2V(apicBase)));
            begin
                myLapic.setupLAPIC_BSP;
            end setupLAPIC;

            interrupt.setInterruptController(interrupt.APIC);
            interrupt.setLAPICBaseAddress(apicBase);
            pic.disable;
            x86.sti;
        else
            -- TODO: not a big deal to fall-back to the PIC, but we need to
            -- handle it.
            raise NoAPICException with "APIC not available.";
        end if;
    end initAPIC;

    initTimerCalibration: declare
    begin
        -- At this point, interrupts are enabled (using either PIC or APIC),
        -- and we can calibrate the TSC ticks.
        time.calibrateTSC;
        print(" TSC timer calibration: ");
        printd(time.tscPerDuration);
        println(" ticks/ms");

        if not cpuid.hasInvariantTSC then
            println(" CAUTION: Time-Stamp Counter is not invariant and may vary with CPU speed.",
                Textmode.YELLOW, Textmode.BLACK);
        else
            println(" TSC is invariant.");
        end if;

    end initTimerCalibration;

    initIOAPIC: declare
    begin
        ioapicBase := acpi.ioapicAddr;

        -- TODO: break this out into a subprogram and put it in ioapic.
        if ioapicBase = 0 then
            println(" WARNING: No I/O APIC found in the ACPI tables. ");
            println(" Defaulting to legacy PIC controller.");
        else
            -- disable caching on the mmap-ed I/O APIC registers,
            -- re-map it into the higher-half.
            ioapicVirtBase := Virtmem.P2V(ioapicBase);
            
            print("Setting up I/O APIC at address: ", textmode.LT_BLUE, textmode.BLACK);
            println(ioapicVirtBase);

            setupIOAPIC: declare
                package io_apic is new ioapic(To_Address(ioapicVirtBase));
                function mapIOFrame is new Mem_mgr.mapIOFrame(BuddyAllocator.allocFrame);
                IORemapException : exception;
            begin    
                if not mapIOFrame(ioapicBase) then
                    raise IORemapException with "Unable to remap I/O APIC registers.";
                else
                    io_apic.setupIOAPIC(acpi.ioapicID);
                    --io_apic.enableIRQ(33, 0);         -- to test keyboard interrupts
                end if;
            end setupIOAPIC;
        end if;
    end initIOAPIC;


    initPCI: declare
    begin
        println("Searching for PCI devices", textmode.LT_BLUE, textmode.BLACK);
        PCI.enumerateDevices;
    end initPCI;


    initFileCache: declare
    begin
        println("Initializing File Cache", textmode.LT_BLUE, textmode.BLACK);
        FileCache.setup;
    end initFileCache;


    initATA: declare
    begin
        println("Checking ATA disk controller", textmode.LT_BLUE, textmode.BLACK);
        ATA.setupATA;

        println("Checking main filesystem", textmode.LT_BLUE, textmode.BLACK);
        testATA: declare
            package Ext2 renames Filesystem.Ext2;
            use ATA;
            
            sblock      : Ext2.SuperBlock;
            ataResult   : ATA.ATAResult;
            driveID     : Devices.DeviceID;
            bgdtAddr    : System.Address;
            bgdtOrder   : BuddyAllocator.Order;
            bgdtLength  : Natural;
            rootInode   : Ext2.Inode;
        begin
            println("Attempting to locate main disk");

            driveID.major := Devices.ATA;
            driveID.reserved := 0;

            -- Try and find a ext2 superblock on each ATA drive present
            for minor in ATA.drives'Range loop

                if ATA.drives(minor).present and ATA.drives(minor).kind = ATA.PATA then
                    print("Checking PATA disk "); printdln(Unsigned_32(minor));

                    driveID.minor := minor;
                    Ext2.readSuperBlock(device  => driveID,
                                        sb      => sblock);

                    if sblock.signature = Ext2.EXT2_SUPER_MAGIC then
                        print(" signature: ");
                        println(Unsigned_32(sblock.signature));
                        println(" Found Ext2 filesystem", 
                                textmode.LT_GREEN,
                                textmode.BLACK);

                        --@TODO Sanity-checking to make sure CuBit supports this
                        -- filesystem's settings. We're picky for now about the
                        -- Ext2 parameters used.
                        Ext2.print(sblock);

                        -- Read the block group descriptors.
                        Ext2.readBlockGroupDescriptors(device       => driveID,
                                                       sb           => sblock,
                                                       bgdt         => bgdtAddr,
                                                       bgdtOrder    => bgdtOrder,
                                                       bgdtLength   => bgdtLength);

                        checkBGDT: declare
                            bgdt : Ext2.BlockGroupDescriptorTable(0..bgdtLength) with
                                Import, Address => bgdtAddr;
                        begin
                            print("Free blocks: "); println(bgdt(0).numFreeBlocks);
                            print("Free inodes: "); println(bgdt(0).numFreeInodes);
                            print("Num folders: "); println(bgdt(0).numDirectories);
                        end checkBGDT;

                        -- Read /
                        -- Ext2.readInode(device   => driveID,
                        --                sb       => sblock,
                        --                inodeNum => 2,
                        --                outInode => rootInode);
                    end if;
                end if;
            end loop;
        end testATA;
    end initATA;

    initSMP: declare
        package myLapic is new lapic(To_Address(virtmem.P2V(apicBase)));
        cpu : Natural := 1;
    begin
        println("Starting SMP CPUs", textmode.LT_BLUE, textmode.BLACK);
        for cpu in 1..(acpi.numCPUs - 1) loop
            startingCPU := Unsigned_32(cpu);
            myLapic.bootAP(Unsigned_8(cpu), 16#7000#);
            while startingCPU /= 0 loop
                time.sleep(1 * time.Milliseconds);
            end loop;
        end loop;

    end initSMP;

    Process.createFirstProcess;

    initScheduler: declare
    begin
        println("Starting scheduler on CPU 0");
        Scheduler.schedule(cpu0Data);
    end initScheduler;
end kmain;


-------------------------------------------------------------------------------
-- apEnter
-- This function is called once per AP CPU startup. It creates the per-CPU data
-- and starts the scheduler.
--
-- Note that we place the per-CPU data on the CPU's stack and then give its
-- address to the KERNELGS_BASE MSR for later Per-CPU access (in 
-- setupPerCPUData). Since this procedure never exits, the stack is a safe
-- place to keep it.
-------------------------------------------------------------------------------
procedure apEnter(cpuNum : in Unsigned_32) is
    cpuData         : aliased PerCpuData.PerCPUData;
    ssPtr           : System.Secondary_Stack.SS_Stack_Ptr;
    DummyException  : exception;
begin
    print("CPU started: ", textmode.GREEN, textmode.BLACK); printdln(cpuNum);
    
    PerCPUData.setupPerCPUData( Integer(cpuNum),
                                cpuData,
                                cpuData'Address,
                                cpuData.gdt'Address,
                                cpuData.gdtPointer'Address,
                                cpuData.tss'Address);

    -- set up secondary stack for this CPU
    ssPtr := PerCPUData.getSecondaryStack;
    System.Secondary_Stack.SS_Init(ssPtr);

    -- print("# zeroes = "); printd(cpuNum); print(" "); println(allZeroes(Integer(cpuNum)));

    -- print("CPU local data:       "); println(cpuData'Address);
    -- print(" as by getPerCPUData: "); println(PerCPUData.getPerCPUDataAddr);
    
    Interrupt.loadIDT;

    -- switch to the kernel's primary page tables.
    Mem_mgr.switchAddressSpace;

    -- now that we're up, we can signal the startup loop to continue
    startingCPU := 0;

    Scheduler.schedule(cpuData);
    x86.halt;

    -- will never get here
    raise DummyException;
end apEnter;

    --package Tests is
    --
    --
    -- procedure task1
    --     with SPARK_Mode => Off
    -- is
    --     ignore : Unsigned_64;
    -- begin
    --     while True loop
    --         time.sleep(1 * time.Seconds);
    --         println("Task 1 in USERMODE");
    --         println("This next instruction should cause an exception:");
    --         time.sleep(5 * time.Seconds);
    --         ignore := x86.rdmsr(x86.MSRs.MISC_ENABLE);
    --     end loop;
    -- end task1;

    -- procedure task2
    --     with SPARK_Mode => Off
    -- is
    -- begin
    --     while True loop
    --         time.sleep(1 * time.Seconds);
    --         println("Task 2");
    --     end loop;
    -- end task2;

    -- procedure task3
    --     with SPARK_Mode => Off
    -- is
    -- begin
    --     while True loop
    --         time.sleep(1 * time.Seconds);
    --         println("Task 3");
    --     end loop;
    -- end task3;

    -- println("Test sleep for 5 seconds using APIC timer");
    -- for i in 1..5 loop
    --     time.bootCalibrationSleep(1000);
    --     print(".");
    -- end loop;
    -- println;
    -- println("Test sleep for 60 seconds using TSC timer");
    -- for i in 1..60 loop
    --     for j in 1..1_000 loop
    --         time.sleep(1 * time.Milliseconds);
    --     end loop;
    --     print(i);
    -- end loop;
    -- println;

    --println("Test Allocations");
    --testMemory;

    -- makeTestProcesses : declare
    --     taskone : Process.ProcessID;
    --     tasktwo : Process.ProcessID;
    --     taskthree : Process.ProcessID;
    -- begin
    --     println("Creating test tasks:");
    --     Process.create(procMain => task1'Address, 
    --                 ppid => 0,
    --                 lightweight => True,
    --                 name => "Task 1          ",
    --                 secTag => 0,
    --                 privilege => Process.KERNEL,
    --                 priority => 3,
    --                 pid => taskone);
    --     print("Task 1 PID: "); println(taskone);
        
    --     Process.create(procMain => task2'Address,
    --                 ppid => 0,
    --                 lightweight => True,
    --                 name => "Task 2          ",
    --                 secTag => 0,
    --                 privilege => Process.KERNEL,
    --                 priority => 4,
    --                 pid => tasktwo);
    --     print("Task 2 PID: "); println(tasktwo);

    --     Process.create(procMain => task3'Address,
    --                 ppid => 0,
    --                 lightweight => True,
    --                 name => "Task 3          ",
    --                 secTag => 0,
    --                 privilege => Process.KERNEL,
    --                 priority => 4,
    --                 pid => taskthree);
    --     print("Task 3 PID: "); println(taskthree);
    -- end makeTestProcesses;

    -- testBootAllocator : declare
    --     allocAddr : Virtmem.PhysAddress;
    -- begin
    --     println("Test boot allocator allocation: ");
    --     print("Free frames: "); printdln(BootAllocator.getFreeFrameCount);
    --     print("Allocate 40 frames: "); 
    --     BootAllocator.allocFrames(40, allocAddr);
    --     println(allocAddr);
    --     print("Free frames: "); printdln(BootAllocator.getFreeFrameCount);
    --     print("Allocate 128k frames: ");
    --     BootAllocator.allocFrames(128_000, allocAddr);
    --     println(allocAddr);
    --     print("Free frames: "); printdln(BootAllocator.getFreeFrameCount);
    -- end testBootAllocator;

    -- testSlab: declare
    --     type myObject is record
    --         a : Unsigned_64;
    --         b : Unsigned_64;
    --         c : Unsigned_64;
    --         d : Unsigned_64;
    --     end record;

    --     objSlab : SlabAllocator.Slab;

    --     type myObjPtr is access myObject;
    --     for myObjPtr'Simple_Storage_Pool use objSlab;

    --     objs : array (Natural range 1..64) of myObjPtr;
    --     package getAddr is new System.Address_To_Access_Conversions(myObject);

    --     procedure free is new Ada.Unchecked_Deallocation(myObject, myObjPtr);
    --     obj : myObjPtr;
    -- begin
    --     println("Setting up Slab Allocator");
    --     SlabAllocator.setup(objSlab, myObject'Size, alignment => 64);
    --     print("Block address: "); println(objSlab.blockAddr);
    --     print("Capacity: "); println(objSlab.capacity);
    --     print("Free objects: "); println(objSlab.numFree);

    --     println("Allocating an object.");
    --     obj := new myObject;
    --     print("Got object with address: "); println(getAddr.To_Address(getAddr.Object_Pointer(obj)));
    --     print("Free objects: "); println(objSlab.numFree);
    --     println("Freeing object.");
    --     free(obj);
    --     print("Free objects: "); println(objSlab.numFree);

    --     println("Allocating a bunch of objects");
    --     for o of objs loop
    --         o := new myObject;
    --     end loop;
    --     print("Free objects: "); println(objSlab.numFree);

    --     for o of objs loop
    --         print("Object with address: "); println(getAddr.To_Address(getAddr.Object_Pointer(o)));
    --     end loop;

    --     println("Freeing a bunch of objects");
    --     for o of objs loop
    --         free(o);
    --     end loop;
    --     print("Free objects: "); println(objSlab.numFree);

    --     println("Allocating a bunch of objects");
    --     for o of objs loop
    --         o := new myObject;
    --     end loop;
    --     print("Free objects: "); println(objSlab.numFree);

    --     for o of objs loop
    --         print("Object with address: "); println(getAddr.To_Address(getAddr.Object_Pointer(o)));
    --     end loop;

    --     println("Freeing a bunch of objects");
    --     for o of objs loop
    --         free(o);
    --     end loop;
    --     print("Free objects: "); println(objSlab.numFree);

    --     println("Tearing down Slab Allocator");
    --     SlabAllocator.teardown(objSlab);
    --     println("Done.");
    -- end testSlab;

    -- testLinkedLists: declare

    --     dummy: Unsigned_64;
    --     u64s : U64List.List;
    -- begin
    --     U64List.setup(U64s, 100);
        
    --     print("List: "); U64List.print(u64s);
    --     print("Length: "); println(u64s.length);
    --     print("Capacity: "); println(u64s.capacity);

    --     println("Inserting 0 in new linked list");
    --     U64List.insertFront(u64s, 0);
    --     print("List: "); U64List.print(u64s);

    --     println("Removing 0");
    --     U64List.popFront(u64s);
    --     print("List: "); U64List.print(u64s);

    --     println("Inserting 1 in linked list");
    --     U64List.insertBack(u64s, 1);
    --     print("List: "); U64List.print(u64s);

    --     println("Removing 1");
    --     U64List.popFront(u64s);
    --     print("List: "); U64List.print(u64s);

    --     println("Inserting 15 U64s in new linked list");

    --     for i in 1..15 loop
    --         U64List.insertBack(u64s, Unsigned_64(i));   
    --     end loop;

    --     print("List:"); U64List.print(u64s);
    --     print("Front: "); printdln(U64List.front(u64s));
    --     print("Back: "); printdln(U64List.back(u64s));

    --     print("Adding 36 to the front of the list");
    --     U64List.insertFront(u64s, Unsigned_64(36));
    --     print("List: "); U64List.print(u64s);
    --     print("Front: "); printdln(U64List.front(u64s));
    --     print("Back: "); printdln(U64List.back(u64s));
    --     print("Length: "); println(u64s.length);

    --     print("Removing from front of list");
    --     U64List.popFront(u64s);
    --     print("List: "); U64List.print(u64s);
    --     print("Front: "); printdln(U64List.front(u64s));
    --     print("Back: "); printdln(U64List.back(u64s));
    --     print("Length: "); println(u64s.length);

    --     print("Removing from back of list twice");
    --     U64List.popBack(u64s);
    --     U64List.popBack(u64s);
    --     print("List: "); U64List.print(u64s);
    --     print("Front: "); printdln(U64List.front(u64s));
    --     print("Back: "); printdln(U64List.back(u64s));
    --     print("Length: "); println(u64s.length);
        
    --     print("Clearing list");
    --     U64List.clear(u64s);
    --     print("List: "); U64List.print(u64s);
    --     print("Length: "); println(u64s.length);
    -- end testLinkedLists;

    --end Tests;

end kmain; -- end package