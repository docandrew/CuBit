-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2019 Jon Andrew
--
-- CuBitOS Entry Point / Main routine
-------------------------------------------------------------------------------
pragma Warnings (Off);
with System.Secondary_Stack;
pragma Warnings (On);

with System.Storage_Elements; use System.Storage_Elements;

with ACPI;
with ATA;
with BootAllocator;
with BuddyAllocator;
with Build;
with Config;
with Cpuid;
with Devices;

with Filesystem.Ext2;
with Filesystem.VFS;

with Ioapic;
with Interrupts;
with Lapic;
with Mem_mgr;
with MemoryAreas;
with Modules;
with Pci;
with PerCpuData;
with Pic;
with Process;
with Scheduler;
with Serial;
with Services.Idle;
with Services.Keyboard;
with StoragePools;
with TextIO; use TextIO;
with Time;
with Timer_pit;
with Video;
with Video.EGA;
with Video.VGA;
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

NoMultibootException : exception;
NoMemoryMapException : exception;
NoACPIException      : exception;
NoAPICException      : exception;

-------------------------------------------------------------------------------
-- kmain
--
-- Entry point for the kernel.
-------------------------------------------------------------------------------
procedure kmain (magic       : Unsigned_32; 
                 mbInfo_orig : in MultibootInfo) with SPARK_Mode => Off
is
    mbOK     : constant Boolean := (magic = 16#2BADB002#);
    mbInfo   : constant MultibootInfo := mbInfo_orig;
    ssPtr    : System.Secondary_Stack.SS_Stack_Ptr;
    memAreas : MemoryAreas.MemoryAreaArray(1..Multiboot.numAreas(mbInfo) + 1);
begin

    if (config.serialMirror) then
        Serial.init (serial.COM1);
        TextIO.enableSerial;
        print ("CuBit v0.0.1");
    end if;

    if not mbOK then
        raise NoMultibootException with "Not loaded by multiboot-compliant loader.";
    end if;

    if not mbInfo.flags.hasMemoryMap then
        raise NoMemoryMapException with "No memory map available from bootloader";
    end if;

    cpuid.setupCPUID;

    PerCPUData.setup (0,
                      cpu0Data, 
                      cpu0Data'Address,
                      cpu0Data.gdt'Address, 
                      cpu0Data.gdtPointer'Address, 
                      cpu0Data.tss'Address);

    ssPtr := PerCPUData.getSecondaryStack;

    System.Secondary_Stack.SS_Init (ssPtr);

    Interrupts.setup;

    memAreas := Multiboot.getMemoryAreas (mbInfo);

    BootAllocator.setup (memAreas);
    Mem_mgr.setup (memAreas);
    BuddyAllocator.setup (memAreas);
    StoragePools.setup;

    -- Use video driver chosen by GRUB
    if mbInfo.framebuffer_width = 80 then
        println ("Using EGA driver");
        TextIO.setVideo (Video.EGA.getTextInterface);
        TextIO.clear (BLUE);
    else
        println ("Using VGA driver");
        Video.VGA.setup (mbInfo);
        TextIO.setVideo (Video.VGA.getTextInterface);
    end if;

    TextIO.clear (BLACK);

    println ("-----------------------------------------------------");
    println ("                  CuBitOS v0.0.1                     ", LT_BLUE, BLACK);
    println ("-----------------------------------------------------");
    print ("Build Date:    "); println (Build.DATE);
    print ("Git Commit:    "); println (Build.COMMIT);
    print ("Source Hash:   "); println (Build.HASH);
    print ("Compiled With: "); println (Standard'Compiler_Version);
    println;

    println ("-----------------------------------------------------");
    println ("                  Virtual Memory                     ", LT_BLUE, BLACK);
    println ("-----------------------------------------------------");
    print ("Virtual Kernel Area:    ");
    print (KERNEL_START_VIRT'Address);
    print (" - "); println(KERNEL_END_VIRT'Address);

    print ("Bootstrap Stack Area:   ");
    print (STACK_BOTTOM);
    print (" - "); println (STACK_TOP);

    print ("DMA Area:               ");
    print (DMA_REGION_START);
    print (" - "); println (DMA_REGION_END);
    println;

    println ("-----------------------------------------------------");
    println ("                    Processor                        ", LT_BLUE, BLACK);
    println ("-----------------------------------------------------");
    print ("Standard CPUID level: "); println (cpuid.getMaxStandardFunction);
    print ("Extended CPUID level: "); println (cpuid.getMaxExtendedFunction);
    print ("CPU Vendor: "); println (cpuid.cpuVendor, GREEN, BLACK);
    print ("CPU Model:  "); println (cpuid.cpuBrand);
    println;

    println ("-----------------------------------------------------");
    println ("                   Kernel Sections                   ", LT_BLUE, BLACK);
    println ("-----------------------------------------------------");
    print ("text:     "); print (stext'Address);
        print("-"); println (etext'Address);
    print("rodata:   "); print (srodata'Address); 
        print("-"); println (erodata'Address);
    print("data:     "); print (sdata'Address); 
        print("-"); println (edata'Address);
    print("bss:      "); print (sbss'Address); 
        print("-"); println (ebss'Address);

    println;
    println ("-----------------------------------------------------");
    println ("                     Memory Areas                    ", LT_BLUE, BLACK);
    println ("-----------------------------------------------------");
    println ("      Start                 End            Type");
    for area of memAreas loop
        print (area.startAddr); print (" - "); print (area.endAddr); 
        print ("   ");
        case area.kind is
            when MemoryAreas.USABLE => 
                println ("Usable", LT_GREEN, BLACK);

            when MemoryAreas.RESERVED => 
                println ("Reserved", YELLOW, BLACK);

            when MemoryAreas.ACPI => 
                println ("ACPI", MAGENTA, BLACK);

            when MemoryAreas.HIBERNATE => 
                println ("Hibernate", BROWN, BLACK);

            when MemoryAreas.BAD => 
                println ("Bad", RED, BLACK);

            when MemoryAreas.VIDEO => 
                println ("Framebuffer", CYAN, BLACK);

            when MemoryAreas.IO =>
                null;       -- Not something Multiboot gives us
        end case;
    end loop;
    println;

    println ("-----------------------------------------------------");
    println ("                     Framebuffer                     ", LT_BLUE, BLACK);
    println ("-----------------------------------------------------");
    print ("Location: "); print (mbInfo.framebuffer_addr);
    print (", size: ");   printd (mbInfo.framebuffer_width);
    print ("x");          printd (mbInfo.framebuffer_height);
    print (", ");         print (Integer(mbInfo.framebuffer_bpp));
    println (" bpp");
    print ("Type:        "); println (Integer(mbInfo.framebuffer_type));
    print ("Pitch:       "); println (Integer(mbInfo.framebuffer_pitch));
    print ("Red field:   "); println (Integer(mbInfo.framebuffer_red_field_position));
    print ("Red size:    "); println (Integer(mbInfo.framebuffer_red_mask_size));
    print ("Blue field:  "); println (Integer(mbInfo.framebuffer_blue_field_position));
    print ("Blue size:   "); println (Integer(mbInfo.framebuffer_blue_mask_size));
    print ("Green field: "); println (Integer(mbInfo.framebuffer_green_field_position));
    print ("Green size:  "); println (Integer(mbInfo.framebuffer_green_mask_size));
    println;

    BuddyAllocator.print;
    
    initACPI: declare
    begin
        println("Setting up ACPI", LT_BLUE, BLACK);
        if not acpi.setup then
            raise NoACPIException with "ACPI Setup Failed, MP tables for SMP data not implemented.";
        end if;
    end initACPI;


    initPIC: declare
    begin
        -- PIC needs to be set up for proper interrupt re-mapping/masking
        println("Setting up 8259 PIC", LT_BLUE, BLACK);
        pic.setupPIC;
        Interrupts.setInterruptController (Interrupts.LEGACY_PIC);
    end initPIC;


    initPIT: declare
    begin
        -- Enable PIT for now just so we have a clock to calibrate the APIC with.
        println("Setting up PIT and enabling timer interrupts", LT_BLUE, BLACK);
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
            println("Setting up Local APIC", LT_BLUE, BLACK);
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

            Interrupts.setInterruptController (Interrupts.APIC);
            Interrupts.setLAPICBaseAddress (apicBase);
            pic.disable;
        else
            -- @TODO not a big deal to fall-back to the PIC, but we need to
            -- handle it.
            raise NoAPICException with "APIC not available.";
        end if;
    end initAPIC;

    initTimerCalibration: declare
    begin
        x86.sti;
        -- At this point, interrupts are enabled (using either PIC or APIC),
        -- and we can calibrate the TSC ticks.
        Time.calibrateTSC;
        print (" TSC timer calibration: ");
        printd (time.tscPerDuration);
        println (" ticks/ms");

        if not cpuid.hasInvariantTSC then
            println (" CAUTION: Time-Stamp Counter is not invariant and may vary with CPU speed.",
                YELLOW, BLACK);
        else
            println (" TSC is invariant.");
        end if;

        -- experiment
        x86.cli;
    end initTimerCalibration;

    initIOAPIC: declare
    begin
        ioapicBase := acpi.ioapicAddr;

        -- TODO: break this out into a subprogram and put it in ioapic.
        if ioapicBase = 0 then
            println (" WARNING: No I/O APIC found in the ACPI tables. ");
            println (" Defaulting to legacy PIC controller.");
        else
            -- disable caching on the mmap-ed I/O APIC registers,
            -- re-map it into the higher-half.
            ioapicVirtBase := Virtmem.P2V(ioapicBase);
            
            print ("Setting up I/O APIC at address: ", LT_BLUE, BLACK);
            println (ioapicVirtBase);

            setupIOAPIC: declare
                package io_apic is new ioapic(To_Address(ioapicVirtBase));
                function mapIOFrame is new Mem_mgr.mapIOFrame(BuddyAllocator.allocFrame);
                IORemapException : exception;
            begin    
                if not mapIOFrame (ioapicBase) then
                    raise IORemapException with "Unable to remap I/O APIC registers.";
                else
                    io_apic.setupIOAPIC (acpi.ioapicID);
                    
                    -- Enable Keyboard Interrupts
                    println ("Enabling keyboard");
                    io_apic.enableIRQ (33, 0);
                end if;
            end setupIOAPIC;
        end if;
    end initIOAPIC;


    initPCI: declare
        procedure mapBigIOArea is new Mem_mgr.mapBigIOArea(BuddyAllocator.allocFrame);

        -- PCI extended config area should be exactly 256M.
        startAddr : constant Virtmem.PhysAddress := Integer_Address(acpi.pcieConfig.baseAddr);
        endAddr   : constant Virtmem.PhysAddress := startAddr + 268_435_456 - 1;

        pciConfigArea : MemoryAreas.MemoryArea := (kind      => MemoryAreas.IO,
                                                   startAddr => startAddr,
                                                   endAddr   => endAddr);
    begin
        println;
        println ("-----------------------------------------------------");
        println ("                       PCI Bus                       ", LT_BLUE, BLACK);
        println ("-----------------------------------------------------");

        mapBigIOArea (pciConfigArea);
        PCI.enumerateDevicesPCIe (Virtmem.P2Va (startAddr));
    end initPCI;


    -- @TODO not sure if it makes sense to map the entire configuration space here,
    -- given that it's going to be around 256M. Maybe map as large pages?
    -- initNVMe: declare
    --     procedure mapIOArea is new Mem_mgr.mapIOArea(BuddyAllocator.allocFrame);

    --     pciConfigArea : MemoryAreas.MemoryArea := (kind      => MemoryAreas.IO,
    --                                                startAddr => acpi.pcieConfig.baseAddr,
    --                                                endAddr   => acpi.pcieConfig.baseAddr + );
    -- begin

    --     -- Map PCI Configuration Space into our kernel. It should be exactly
    --         --
    --     mapBigIOArea (pciConfigArea);
    -- end initNVMe;


    initFileCache: declare
    begin
        println("Initializing File Cache", LT_BLUE, BLACK);
        FileCache.setup;
    end initFileCache;


    initATA: declare
    begin
        println ("Checking ATA disk controller", LT_BLUE, BLACK);
        ATA.setupATA;
    end initATA;

    initFS : declare
        package Ext2 renames Filesystem.Ext2;
        package VFS renames Filesystem.VFS;
        use ATA;
        
        fs           : Ext2.Ext2Filesystem;
        currentDrive : VFS.DriveLetter := VFS.A;
        inode        : Ext2.Inode;
    begin
        println ("Detecting Filesystems on ATA drives", LT_BLUE, BLACK);
        for minor in ATA.drives'Range loop
            if ATA.drives(minor).present and ATA.drives(minor).kind = ATA.PATA then

                fs := Ext2.setup (device => (major => Devices.ATA, minor => minor, others => <>));

                if fs.initialized then
                    print (" Found compatible Ext2 filesystem, mounting at ", LT_GREEN, BLACK);
                    print (Character'Val(VFS.DriveLetter'Pos(currentDrive) + 65), LT_BLUE, BLACK);
                    println (":");
                    print ("Number of block groups:"); println(Integer(fs.bgdt'Length));
                    VFS.fstab(currentDrive) := (present => True, kind => VFS.EXT2);
                    inode := Ext2.readInode (fs, 2);
                    Ext2.dumpDirs (fs, inode.directBlock0, inode.sizeLo);
                        
                    currentDrive := VFS.DriveLetter'succ(currentDrive);
                end if;
            end if;
        end loop;
    end initFS;

    initModules: declare
    begin
        println;
        println ("-----------------------------------------------------");
        println ("                   CuBit Modules                     ", LT_BLUE, BLACK);
        println ("-----------------------------------------------------");

        Modules.setup (mbInfo);
    end initModules;

    -- if acpi.numCPUs > 1 then
    --     initSMP: declare
    --         package myLapic is new lapic(To_Address(virtmem.P2V(apicBase)));
    --         cpu : Natural := 1;
    --     begin
    --         println("Starting SMP CPUs", textmode.LT_BLUE, textmode.BLACK);

    --         for cpu in 1..(acpi.numCPUs - 1) loop
                
    --             startingCPU := Unsigned_32(cpu);
                
    --             myLapic.bootAP(Unsigned_8(cpu), 16#7000#);

    --             while startingCPU /= 0 loop
    --                 Time.sleep(1 * time.Milliseconds);
    --             end loop;
    --         end loop;
    --     end initSMP;
    -- end if;

    println ("Starting idle service");
    -- @TODO may need to start one of these per-CPU
    Process.startKernelThread (procStart => Services.Idle.start'Address,
                               name      => "Idle            ",
                               pid       => Config.SERVICE_IDLE_PID,
                               priority  => -1);

    println ("Starting keyboard service");
    Process.startKernelThread (procStart => Services.Keyboard.start'Address,
                               name      => "Keyboard        ",
                               pid       => Config.SERVICE_KEYBOARD_PID,
                               priority  => 1);

    -- println (testKThread1'Address);
    -- Process.startKernelThread (testKThread1'Address, "kthread1        ", 1);
    -- print ("Creating kernel thread 2, procedure address: ");
    -- println (testKThread2'Address);
    -- Process.startKernelThread (testKThread2'Address, "kthread2        ", 2);
    
    println ("Creating User Process");

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
procedure apEnter (cpuNum : in Unsigned_32) is
    cpuData         : aliased PerCpuData.PerCPUData;
    ssPtr           : System.Secondary_Stack.SS_Stack_Ptr;
    DummyException  : exception;
begin
    print ("CPU started: ", GREEN, BLACK); printdln (cpuNum);
    
    PerCPUData.setup (Integer(cpuNum),
                      cpuData,
                      cpuData'Address,
                      cpuData.gdt'Address,
                      cpuData.gdtPointer'Address,
                      cpuData.tss'Address);

    -- set up secondary stack for this CPU
    ssPtr := PerCPUData.getSecondaryStack;
    System.Secondary_Stack.SS_Init (ssPtr);

    -- print("# zeroes = "); printd(cpuNum); print(" "); println(allZeroes(Integer(cpuNum)));

    -- print("CPU local data:       "); println(cpuData'Address);
    -- print(" as by getPerCPUData: "); println(PerCPUData.getPerCPUDataAddr);
    
    Interrupts.loadIDT;

    -- switch to the kernel's primary page tables.
    Mem_mgr.switchAddressSpace;

    -- now that we're up, we can signal the startup loop to continue
    startingCPU := 0;

    Scheduler.schedule (cpuData);
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