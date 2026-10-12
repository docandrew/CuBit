-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2021 Jon Andrew
--
-- Multiboot Module Loading
--
-- Loads the CPIO initrd from GRUB, finds devmgr.svc, maps the initrd into
-- the device manager's address space, grants it CAP_PROCESS + CAP_IOPORT,
-- and resumes it. All remaining boot policy (PCI scanning, DMA allocation,
-- per-driver capability granting, cross-service endpoints, CPU affinity,
-- startup ordering) is handled by the userspace device manager.
-------------------------------------------------------------------------------
with BuddyAllocator;
with Capabilities;
with Capabilities.Operations;
with Device_Vector_ABI;
with Cpio;
with ELF;
with Process;
with Process.Loader;
with Multiboot;
with Boot_Modules;
with Boot_Timeline;
with Boot_Timeline_Layout;
with Process_Identities;
with Owned_Memory_Layout;
with Launch_Configuration;
with Process_Launch;
with Publication_Pages;
with Util;
with Physical_Range_Map;
with Device_Memory_Diagnostics;
with Physical_Ranges;
with Service_Registry;
with TextIO; use TextIO;
with Virtmem;

package body Modules with SPARK_Mode => Off is

    -- devmgr's device-memory roots (CAP-001): one slot per device run,
    -- Physical_Ranges.Max_Roots of them, clear of devmgr's other slots.
    DEVICE_ROOT_FIRST_SLOT : constant Capabilities.CapabilitySlot := 48;
    pragma Compile_Time_Error
      (DEVICE_ROOT_FIRST_SLOT + Physical_Ranges.Max_Roots - 1 >=
         Capabilities.REPLY_CAP_SLOT,
       "device-memory root slots overlap the reply slot");

    initrdAddr  : Virtmem.PhysAddress := 0;
    initrdSize  : Storage_Count := 0;
    devmgrPID   : Process.ProcessID;
    setupStarted : Boolean := False;

    -- Package-level to avoid 2KB+ stack usage from FileIndex array
    cpioArchive : Cpio.Archive;

    function residentInitrdRange
      (Physical : Integer_Address; Length : Storage_Count) return Boolean
    is
    begin
        return initrdAddr /= 0 and then Physical >= Integer_Address (initrdAddr)
          and then Physical - Integer_Address (initrdAddr) < Integer_Address (initrdSize)
          and then Integer_Address (Length) <= Integer_Address (initrdSize) -
            (Physical - Integer_Address (initrdAddr));
    end residentInitrdRange;

    ---------------------------------------------------------------------------
    -- loadFromCpio
    -- Find a named file in the CPIO archive, validate it as ELF, and load
    -- it as a new process (suspended). Returns the new PID.
    ---------------------------------------------------------------------------
    function loadFromCpio (ar       : in out Cpio.Archive;
                           name     : String;
                           priority : Process.ProcessPriority := 1)
        return Process.ProcessID

    is
        idx     : Natural;
        elfAddr : System.Address;
        elfSize : Storage_Count;
        pid     : Process.ProcessID;

        -- Allocator for page tables created during process loading
        procedure mapPage is new Virtmem.mapPage (BuddyAllocator.allocFrame);
        pragma Unreferenced (mapPage);
        started : constant Unsigned_64 := Boot_Timeline.Now;
    begin
        idx := Cpio.findFile (ar, name);
        if idx >= ar.count then
            print ("Modules: not found in CPIO: ");
            println (name);
            return Process.NO_PROCESS;
        end if;

        elfAddr := ar.files (idx).dataAddr;
        elfSize := ar.files (idx).dataSize;

        if elfSize < ELF.ELFFileHeader'Size / System.Storage_Unit then
            print ("Modules: truncated ELF header: "); println (name);
            return Process.NO_PROCESS;
        end if;

        declare
            hdr : ELF.ELFFileHeader with Import, Address => elfAddr;
        begin
            if not Process.Loader.isValidELF (hdr) then
                print ("Modules: invalid ELF: "); println (name);
                return Process.NO_PROCESS;
            end if;

            pid := Process.Loader.load (
                elfHeader => hdr,
                objStart  => elfAddr,
                size      => elfSize,
                strAddr   => name'Address,
                priority  => priority);
        end;

        if pid = Process.NO_PROCESS then
            print ("Modules: load failed: "); println (name);
        else
            Boot_Timeline.Note_Spawn
              (Slot     => Natural (pid),
               Identity => Process_Identities.To_Word (Process.identityOf (pid)),
               Spawner  => Boot_Timeline_Layout.No_Identity,
               Spawner_Slot => 0,
               Name     => name,
               Started  => started);
            print ("Modules: loaded "); print (name);
            print (" as PID "); println (Integer (pid));
        end if;

        return pid;
    end loadFromCpio;

    ---------------------------------------------------------------------------
    -- mapInitrd
    -- Map the initrd image into a process's address space at
    -- 0x5000_0000_0000 (read-only).
    ---------------------------------------------------------------------------
    procedure mapInitrd (pid  : Process.ProcessID;
                         addr : Virtmem.PhysAddress;
                         size : Storage_Count)
    is
        ok : Boolean;

        base : constant System.Address := To_Address (16#0000_5000_0000_0000#);

        numPages : constant Storage_Count :=
            (size + Virtmem.PAGE_SIZE - 1) / Virtmem.PAGE_SIZE;

        procedure mapPage is new Virtmem.mapPage (BuddyAllocator.allocFrame);
    begin
        print ("Modules: Mapping "); print (Integer(numPages));
        if Owned_Memory_Layout.Conflicts
          (Unsigned_64 (To_Integer (base)), Unsigned_64 (numPages) * Virtmem.PAGE_SIZE)
        then
            raise ModuleException with "Initrd overlaps owned-memory aperture";
        end if;
        print (" initrd pages into pid "); println (Integer(pid));

        for i in 0..numPages-1 loop
            mapPage (phys    => addr +
                         Virtmem.PhysAddress(i * Virtmem.PAGE_SIZE),
                     virt    => To_Integer(base + (i * Virtmem.PAGE_SIZE)),
                     flags   => Virtmem.PG_USERDATARO,
                     myP4    => Process.addrtab(pid),
                     success => ok);

            if not ok then
                print ("Modules: Error mapping initrd page ");
                print (Integer(i));
                raise ModuleException with "Initrd mapping failed; device manager remains suspended";
            end if;
        end loop;

        println ("Modules: Initrd mapping complete.");
    end mapInitrd;

    ---------------------------------------------------------------------------
    -- Initrd_Pages
    ---------------------------------------------------------------------------
    procedure Initrd_Pages (Offset, Bytes : Unsigned_64;
                            Physical      : out Unsigned_64;
                            Valid         : out Boolean)
    is
        Page : constant Unsigned_64 := Unsigned_64 (Virtmem.PAGE_SIZE);
        Image_Pages : constant Unsigned_64 :=
          (Unsigned_64 (initrdSize) + Page - 1) / Page;
    begin
        Physical := 0;
        Valid := initrdAddr /= 0 and then Offset mod Page = 0 and then
          Bytes /= 0 and then Offset <= Image_Pages * Page and then
          Bytes <= Image_Pages * Page - Offset;
        if Valid then
            Physical := Unsigned_64 (initrdAddr) + Offset;
        end if;
    end Initrd_Pages;

    ---------------------------------------------------------------------------
    -- installDevmgrConfiguration
    -- devmgr's typed launch configuration (Launch_Configuration): the boot
    -- image's size and the boot framebuffer's pages. Installed as a launch
    -- block, exactly as SYSCALL_INSTALL_LAUNCH_ARGUMENTS would, before its
    -- first instruction.
    ---------------------------------------------------------------------------
    procedure installDevmgrConfiguration (pid : Process.ProcessID) is
        use type Process.Page_Allocation_Result;
        package LC renames Launch_Configuration;
        Framebuffer : LC.Physical_Span;
        Block : LC.Encoded;
        storage : System.Address;
        result : Process.Page_Allocation_Result;
        Ignore : System.Address;
    begin
        if Multiboot.Has_Graphics then
            Framebuffer :=
              (First => Unsigned_64 (Multiboot.Framebuffer.Map_First),
               Bytes => Unsigned_64 (Multiboot.Framebuffer.Map_Limit) -
                        Unsigned_64 (Multiboot.Framebuffer.Map_First));
        end if;
        Block := LC.Encode (LC.Device_Manager_Configuration'
          (Boot_Image  => Unsigned_64 (initrdSize),
           Framebuffer => Framebuffer));
        Process.lockAddressSpace (pid);
        Process.proctab (pid).frames.capacity :=
          Process.proctab (pid).frames.capacity + 1;
        Process.tryAddPage
          (proc    => Process.proctab (pid),
           mapTo   => To_Address (Process_Launch.Arguments_Base),
           storage => storage,
           result  => result,
           flags   => Virtmem.PG_USERDATARO);
        Process.unlockAddressSpace (pid);
        if result /= Process.Page_Added then
            raise ModuleException with "No page for the device manager's configuration";
        end if;
        -- Fresh frames are zeroed.
        Ignore := Util.memcpy (storage, Block.Words'Address,
                               Storage_Count (Block.Length * LC.Word_Bytes));
        Process.threadOf (pid).kernelStack.interruptFrame.rdi :=
          Unsigned_64 (Block.Length * LC.Word_Bytes);
        Process.proctab (pid).launch := Process_Launch.Arguments_Installed;
    end installDevmgrConfiguration;

    ---------------------------------------------------------------------------
    -- setup
    --
    -- Find the CPIO initrd among multiboot modules, parse it, load only
    -- devmgr.svc, map the initrd into it, grant it CAP_PROCESS + CAP_IOPORT,
    -- and resume it. The device manager handles all remaining boot policy.
    ---------------------------------------------------------------------------
    procedure setup
    is
        cpioOk : Boolean;
    begin
        if setupStarted then
            raise ModuleException with "Boot module startup is one-shot";
        end if;
        setupStarted := True;
        -- The physical classification is final before any user process
        -- exists; device-memory authority consults nothing else.
        Physical_Range_Map.Seal_From_Boot;
        Device_Memory_Diagnostics.Publish_Classification;
        -- System facts are final now; every process maps them from its start.
        Publication_Pages.Publish_Facts;
        initrdAddr  := 0;
        initrdSize  := 0;
        devmgrPID   := Process.NO_PROCESS;

        if Multiboot.Boot_Module_Count = 0 then
            println ("Modules: No boot modules found.");
            return;
        end if;

        -- ---------------------------------------------------------------
        -- Phase 1: Find the init.img CPIO archive among multiboot modules
        -- ---------------------------------------------------------------
        for I in 1 .. Multiboot.Boot_Module_Count loop
            declare
                Item : constant Boot_Modules.Image := Multiboot.Boot_Module (I);
                Size : constant Storage_Count := Storage_Count (Item.Limit - Item.First);
                Name : constant String := Item.Name.Text (1 .. Item.Name.Length);
            begin
                print ("Module: "); print (Name);
                print (" ("); print (Integer (Size)); println (" bytes)");
                if Name = "init.img" then
                    initrdAddr := Virtmem.PhysAddress (Item.First);
                    initrdSize := Size;
                end if;
            end;
        end loop;

        if initrdAddr = 0 or initrdSize = 0 then
            println ("Modules: No init.img found among boot modules.");
            return;
        end if;

        -- ---------------------------------------------------------------
        -- Phase 2: Parse the CPIO archive
        -- ---------------------------------------------------------------
        declare
            initrdVirt : constant System.Address :=
                Virtmem.P2Va (Integer_Address(initrdAddr));
        begin
            Cpio.init (cpioArchive, initrdVirt, initrdSize, cpioOk);
        end;

        if not cpioOk then
            println ("Modules: Failed to parse CPIO initrd.");
            return;
        end if;

        Boot_Timeline.Mark_Stage (Boot_Timeline_Layout.Initrd_Parsed);
        print ("Modules: CPIO initrd contains ");
        print (Integer(cpioArchive.count)); println (" files.");

        -- ---------------------------------------------------------------
        -- Phase 3: Load devmgr.svc from CPIO
        -- ---------------------------------------------------------------
        devmgrPID := loadFromCpio (cpioArchive, "devmgr.svc",
                                   priority => 5);
        if devmgrPID = Process.NO_PROCESS then
            println ("Modules: devmgr.svc not found, falling back.");
            return;
        end if;

        -- ---------------------------------------------------------------
        -- Phase 4: Map initrd into devmgr, grant capabilities, resume
        -- ---------------------------------------------------------------

        -- Map initrd into devmgr's address space (read-only)
        mapInitrd (devmgrPID, initrdAddr, initrdSize);
        installDevmgrConfiguration (devmgrPID);

        -- Slot 4: process-management policy (not capability construction).
        Capabilities.Operations.insertCapAt (
            table => Process.proctab(devmgrPID).caps,
            slot  => 4,
            cap   => (capType  => Capabilities.CAP_PROCESS,
                      rights   => (Capabilities.RIGHT_READ    => True,
                                   Capabilities.RIGHT_EXECUTE => True,
                                   Capabilities.RIGHT_GRANT   => True,
                                   Capabilities.RIGHT_WRITE   => True,
                                   others => False),
                      authorityTag => Capabilities.NO_AUTHORITY_TAG,
                      object   => (ref => 0, param => 0),
                      gen      => Capabilities.INITIAL_GENERATION));

        -- Slot 8: explicit capability-space policy root. This is the only
        -- authority that permits SYSCALL_POLICY_MINT_CAPABILITY; CAP_PROCESS/GRANT does not.
        Capabilities.Operations.insertCapAt (
            table => Process.proctab(devmgrPID).caps,
            slot  => 8,
            cap   => (capType  => Capabilities.CAP_CSPACE,
                      rights   => (Capabilities.RIGHT_GRANT => True,
                                   others => False),
                      authorityTag => Capabilities.NO_AUTHORITY_TAG,
                      object   => (ref => 0, param => 0),
                      gen      => Capabilities.INITIAL_GENERATION));

        -- Slot 5: CAP_IOPORT for PCI config space (0xCF8, 8 ports)
        Capabilities.Operations.insertCapAt (
            table => Process.proctab(devmgrPID).caps,
            slot  => 5,
            cap   => (capType  => Capabilities.CAP_IOPORT,
                      rights   => Capabilities.READ_WRITE,
                      authorityTag => Capabilities.NO_AUTHORITY_TAG,
                      object   => (ref => 16#CF8#, param => 8),
                      gen      => Capabilities.INITIAL_GENERATION));

        -- Slot 6: CAP_NOTIFICATION for DRIVER_DEVMGR registration
        Capabilities.Operations.insertCapAt (
            table => Process.proctab(devmgrPID).caps,
            slot  => 6,
            cap   => (capType  => Capabilities.CAP_NOTIFICATION,
                      rights   => (Capabilities.RIGHT_WRITE => True,
                                   others => False),
                      authorityTag => Capabilities.NO_AUTHORITY_TAG,
                      object   => (ref   => Unsigned_64 (Service_Registry.DRIVER_DEVMGR),
                                   param => 0),
                      gen      => Capabilities.INITIAL_GENERATION));

        -- Slots 48..55: device-memory roots (CAP-001). One CAP_DEVICE_MEM
        -- per maximal run of device-mappable pages in the boot
        -- classification (PCI holes, firmware-reserved MMIO such as ECAM,
        -- and everything above the top of described memory, so BARs above
        -- 4 GiB are covered), largest first. Never RAM, kernel memory, boot
        -- modules, ACPI memory or kernel MMIO. devmgr maps BARs (its own
        -- MSI-X tables, drivers' registers via MAP_INTO) under these; the
        -- initrd it shares is a boot module, not device memory.
        declare
            Runs  : Physical_Ranges.Run_Array;
            Count : Physical_Ranges.Root_Count;
        begin
            Physical_Ranges.Largest_Device_Runs
              (Physical_Range_Map.Kernel_Map, Runs, Count);
            for Index in 1 .. Count loop
                Capabilities.Operations.insertCapAt (
                    table => Process.proctab(devmgrPID).caps,
                    slot  => DEVICE_ROOT_FIRST_SLOT + (Index - 1),
                    cap   => (capType  => Capabilities.CAP_DEVICE_MEM,
                              rights   => Capabilities.READ_WRITE,
                              authorityTag => Capabilities.NO_AUTHORITY_TAG,
                              object   => (ref   => Runs (Index).First,
                                           param => Runs (Index).Last -
                                                    Runs (Index).First + 1),
                              gen      => Capabilities.INITIAL_GENERATION));
                print ("Modules: devmgr device root ");
                print (Runs (Index).First);
                print (" .. ");
                println (Runs (Index).Last);
            end loop;
        end;

        -- Device vector authority (docs/msi-interrupts.md): a CAP_IRQ naming
        -- the whole device range. GRANT allocates and releases blocks for
        -- drivers; READ reads per-vector counts and owners.
        Capabilities.Operations.insertCapAt (
            table => Process.proctab(devmgrPID).caps,
            slot  => Device_Vector_ABI.Vector_Authority_Slot,
            cap   => (capType  => Capabilities.CAP_IRQ,
                      rights   => (Capabilities.RIGHT_READ  => True,
                                   Capabilities.RIGHT_GRANT => True,
                                   others => False),
                      authorityTag => Capabilities.NO_AUTHORITY_TAG,
                      object   => (ref   => Device_Vector_ABI.First_Vector,
                                   param => Device_Vector_ABI.Vector_Count),
                      gen      => Capabilities.INITIAL_GENERATION));

        println ("Modules: Starting device manager.");
        Process.resume (devmgrPID);
        Boot_Timeline.Mark_Stage (Boot_Timeline_Layout.Device_Manager_Loaded);

    end setup;

end Modules;
