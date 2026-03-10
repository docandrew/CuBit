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
with Interfaces; use Interfaces;

with BuddyAllocator;
with Capabilities;
with Capabilities.Operations;
with Cpio;
with ELF;
with Process;
with Process.Loader;
with Strings;
with Sysinfo;
with TextIO; use TextIO;
with Virtmem;

package body Modules is

    initrdAddr  : Virtmem.PhysAddress;
    initrdSize  : Storage_Count;
    devmgrPID   : Process.ProcessID;

    -- Package-level to avoid 2KB+ stack usage from FileIndex array
    cpioArchive : Cpio.Archive;

    ---------------------------------------------------------------------------
    -- loadFromCpio
    -- Find a named file in the CPIO archive, validate it as ELF, and load
    -- it as a new process (suspended). Returns the new PID.
    ---------------------------------------------------------------------------
    function loadFromCpio (ar       : in out Cpio.Archive;
                           name     : String;
                           priority : Process.ProcessPriority := 1)
        return Process.ProcessID
        with SPARK_Mode => On
    is
        idx     : Natural;
        elfAddr : System.Address;
        elfSize : Storage_Count;
        pid     : Process.ProcessID;

        -- Allocator for page tables created during process loading
        procedure mapPage is new Virtmem.mapPage (BuddyAllocator.allocFrame);
        pragma Unreferenced (mapPage);
    begin
        idx := Cpio.findFile (ar, name);
        if idx >= ar.count then
            print ("Modules: not found in CPIO: ");
            println (name);
            return Process.NO_PROCESS;
        end if;

        elfAddr := ar.files (idx).dataAddr;
        elfSize := ar.files (idx).dataSize;

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
                         size : Storage_Count) with
        SPARK_Mode => On
    is
        ok : Boolean;

        base : constant System.Address := To_Address (16#0000_5000_0000_0000#);

        numPages : constant Storage_Count :=
            (size + Virtmem.PAGE_SIZE - 1) / Virtmem.PAGE_SIZE;

        procedure mapPage is new Virtmem.mapPage (BuddyAllocator.allocFrame);
    begin
        print ("Modules: Mapping "); print (Integer(numPages));
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
                println (" a successful boot is unlikely.");
                return;
            end if;
        end loop;

        MAGIC_RAMDISK_ADDRESS := base;
        MAGIC_RAMDISK_SIZE    := size;
        println ("Modules: Initrd mapping complete.");
    end mapInitrd;

    ---------------------------------------------------------------------------
    -- setup
    --
    -- Find the CPIO initrd among multiboot modules, parse it, load only
    -- devmgr.svc, map the initrd into it, grant it CAP_PROCESS + CAP_IOPORT,
    -- and resume it. The device manager handles all remaining boot policy.
    ---------------------------------------------------------------------------
    procedure setup (mbinfo : in Multiboot.MultibootInfo) with
        SPARK_Mode => On
    is
        cpioOk : Boolean;
    begin
        MAGIC_RAMDISK_ADDRESS := System.Null_Address;
        initrdAddr  := 0;
        initrdSize  := 0;
        devmgrPID   := Process.NO_PROCESS;

        if not mbinfo.flags.hasModules then
            println ("Modules: No boot modules found.");
            return;
        end if;

        -- ---------------------------------------------------------------
        -- Phase 1: Find the init.img CPIO archive among multiboot modules
        -- ---------------------------------------------------------------
        declare
            type ModuleList is
                array (Unsigned_32 range 1..mbinfo.mods_count)
                    of Multiboot.MBModule
                with Convention => C;

            mods : ModuleList
                with Import,
                     Address => Virtmem.P2Va (
                         Integer_Address(mbinfo.mods_addr));

            modName : String(1..16);
        begin
            for m of mods loop
                declare
                    strAddr  : constant System.Address :=
                        Virtmem.P2Va (Integer_Address(m.mod_string));
                    modStart : constant System.Address :=
                        Virtmem.P2Va (Integer_Address(m.mod_start));
                    modEnd   : constant System.Address :=
                        Virtmem.P2Va (Integer_Address(m.mod_end));
                    size     : constant Storage_Count :=
                        modEnd - modStart;
                begin
                    Strings.toAda(strAddr, modName);

                    print ("Module: "); print (modName);
                    print (" ("); print (Integer(size));
                    println (" bytes)");

                    if modName(1..8) = "init.img" then
                        initrdAddr := Virtmem.PhysAddress(m.mod_start);
                        initrdSize := size;
                    end if;
                end;
            end loop;
        end;

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

        -- Slot 4: CAP_PROCESS (READ + EXECUTE + GRANT + WRITE)
        Capabilities.Operations.insertCapAt (
            table => Process.proctab(devmgrPID).caps,
            slot  => 4,
            cap   => (capType  => Capabilities.CAP_PROCESS,
                      rights   => (Capabilities.RIGHT_READ    => True,
                                   Capabilities.RIGHT_EXECUTE => True,
                                   Capabilities.RIGHT_GRANT   => True,
                                   Capabilities.RIGHT_WRITE   => True,
                                   others => False),
                      capBadge => Capabilities.NO_BADGE,
                      object   => (ref => 0, param => 0),
                      gen      => Capabilities.INITIAL_GENERATION));

        -- Slot 5: CAP_IOPORT for PCI config space (0xCF8, 8 ports)
        Capabilities.Operations.insertCapAt (
            table => Process.proctab(devmgrPID).caps,
            slot  => 5,
            cap   => (capType  => Capabilities.CAP_IOPORT,
                      rights   => Capabilities.READ_WRITE,
                      capBadge => Capabilities.NO_BADGE,
                      object   => (ref => 16#CF8#, param => 8),
                      gen      => Capabilities.INITIAL_GENERATION));

        -- Slot 6: CAP_NOTIFICATION for DRIVER_DEVMGR registration
        Capabilities.Operations.insertCapAt (
            table => Process.proctab(devmgrPID).caps,
            slot  => 6,
            cap   => (capType  => Capabilities.CAP_NOTIFICATION,
                      rights   => (Capabilities.RIGHT_WRITE => True,
                                   others => False),
                      capBadge => Capabilities.NO_BADGE,
                      object   => (ref   => Unsigned_64 (Sysinfo.DRIVER_DEVMGR),
                                   param => 0),
                      gen      => Capabilities.INITIAL_GENERATION));

        -- Slot 7: CAP_DEVICE_MEM for virtToPhys (devmgr needs phys addrs)
        Capabilities.Operations.insertCapAt (
            table => Process.proctab(devmgrPID).caps,
            slot  => 7,
            cap   => (capType  => Capabilities.CAP_DEVICE_MEM,
                      rights   => Capabilities.READ_WRITE,
                      capBadge => Capabilities.NO_BADGE,
                      object   => (ref => 0, param => 16#FFFF_FFFF#),
                      gen      => Capabilities.INITIAL_GENERATION));

        println ("Modules: Starting device manager.");
        Process.resume (devmgrPID);

    end setup;

end Modules;
