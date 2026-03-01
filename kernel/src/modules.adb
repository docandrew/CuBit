-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2021 Jon Andrew
--
-- Multiboot Module Loading
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;

with BuddyAllocator;
with Config;
with ELF;
with Process;
with Process.Loader;
with Strings;
with TextIO; use TextIO;
with Virtmem;

package body Modules is

    -- We need to special-case the Ramdisk driver, filesystem server, and
    -- initrd image, so as we find those during module loading keep track here.
    ramdiskPID    : Process.ProcessID;
    filesystemPID : Process.ProcessID;
    initrdAddr    : Virtmem.PhysAddress;
    initrdSize    : Storage_Count;

    ---------------------------------------------------------------------------
    -- printModuleInfo
    ---------------------------------------------------------------------------
    procedure printModule (m : in Multiboot.MBModule) with
        SPARK_Mode => On
    is
        strAddr  : constant System.Address := Virtmem.P2Va (Integer_Address(m.mod_string));
        modStart : constant System.Address := Virtmem.P2Va (Integer_Address(m.mod_start));
        modEnd   : constant System.Address := Virtmem.P2Va (Integer_Address(m.mod_end));
        size     : constant Storage_Count  := modEnd - modStart;
        contents : String(1..Natural(size)) with Import, Address => modStart;

        modName  : String(1..16);
    begin
        Strings.toAda(strAddr, modName);
        println;
        print ("Module name:  "); print (modName); println;
        print ("Module start: "); println (modStart);
        print ("Module end:   "); println (modEnd);
        -- print (" contents :   "); println (contents);
    end printModule;

    ---------------------------------------------------------------------------
    -- loadModule
    ---------------------------------------------------------------------------
    procedure loadModule (m : in Multiboot.MBModule) with
        SPARK_Mode => On
    is
        use type ELF.SegmentType;

        strAddr   : System.Address := Virtmem.P2Va (Integer_Address(m.mod_string));
        modStart  : System.Address := Virtmem.P2Va (Integer_Address(m.mod_start));
        modEnd    : System.Address := Virtmem.P2Va (Integer_Address(m.mod_end));
        size      : Storage_Count  := modEnd - modStart;
        elfHeader : ELF.ELFFileHeader with Import, Address => modStart;
        pid       : Process.ProcessID;

        modName   : String(1..16);
    begin
        Strings.toAda(strAddr, modName);

        -- If this file is an ELF object, load it and start it up.
        -- For the filesystem server, assign the well-known PID.
        if Process.Loader.isValidELF (elfHeader) then
            if modName(1..14) = "filesystem.svc" then
                pid := Process.Loader.load (elfHeader, modStart, size, strAddr,
                                            requestedPID => Config.SERVICE_FILESYSTEM_PID);
            else
                pid := Process.Loader.load (elfHeader, modStart, size, strAddr);
            end if;
        end if;

        -- If this file is the initrd image, save the info so we can
        -- map it into the ramdisk driver. If it's the ramdisk driver, save its
        -- pid so we know _who_ to map the initrd image into.
        if modName(1..8) = "init.img" then

            -- check for duplicate modules with same name.
            if initrdAddr /= 0 then
                println ("Modules: Multiple init.img files found, using first one found.");
                return;
            end if;

            initrdAddr := Virtmem.PhysAddress(m.mod_start);
            initrdSize := size;

        elsif modName(1..11) = "ramdisk.drv" then

            if ramdiskPID /= Process.NO_PROCESS then
                println ("Modules: Multiple ramdisk.drv files found, using first one found.");
                return;
            end if;

            ramdiskPID := pid;

        elsif modName(1..14) = "filesystem.svc" then

            if filesystemPID /= Process.NO_PROCESS then
                println ("Modules: Multiple filesystem.svc files found, using first one found.");
                return;
            end if;

            filesystemPID := pid;

        else
            -- Generic ELF module: resume immediately
            if pid /= Process.NO_PROCESS then
                print ("Modules: Starting module "); print (modName); println;
                Process.resume (pid);
            end if;
        end if;

    end loadModule;

    ---------------------------------------------------------------------------
    -- mapInitrd
    -- map the initial ramdisk image into the ramdisk driver.
    ---------------------------------------------------------------------------
    procedure mapInitrd (pid  : Process.ProcessID;
                         addr : Virtmem.PhysAddress;
                         size : Storage_Count) with
        SPARK_Mode => On
    is
        ok : Boolean;
        MapException : exception;

        base : constant System.Address := To_Address (16#0000_5000_0000_0000#);

        numPages : constant Storage_Count := (size + Virtmem.PAGE_SIZE - 1) / Virtmem.PAGE_SIZE;

        procedure mapPage is new Virtmem.mapPage (BuddyAllocator.allocFrame);
    begin

        print ("Modules: Mapping "); print (Integer(numPages));
        print (" initrd pages into pid "); println (Integer(pid));

        for i in 0..numPages-1 loop
            mapPage (phys    => addr + Virtmem.PhysAddress(i * Virtmem.PAGE_SIZE),
                     virt    => To_Integer(base + (i * Virtmem.PAGE_SIZE)),
                     flags   => Virtmem.PG_USERDATARO,
                     myP4    => Process.addrtab(pid),
                     success => ok);

            if not ok then
                print ("Modules: Error mapping initrd page "); print (Integer(i));
                println (" a successful boot is unlikely.");
                return;
            end if;
        end loop;

        MAGIC_RAMDISK_ADDRESS := base;
        println ("Modules: Initrd mapping complete.");
    end mapInitrd;

    ---------------------------------------------------------------------------
    -- setup
    ---------------------------------------------------------------------------
    procedure setup (mbinfo : in Multiboot.MultibootInfo) with
        SPARK_Mode => On
    is
    begin
        -- Only set if we actually loaded the initrd image
        MAGIC_RAMDISK_ADDRESS := System.Null_Address;

        initrdAddr    := 0;
        initrdSize    := 0;
        ramdiskPID    := Process.NO_PROCESS;
        filesystemPID := Process.NO_PROCESS;

        if mbinfo.flags.hasModules then
            declare
                type ModuleList is array (Unsigned_32 range 1..mbinfo.mods_count) of Multiboot.MBModule
                    with Convention => C;
                
                mods : ModuleList
                    with Import, Address => Virtmem.P2Va (Integer_Address(mbinfo.mods_addr));
            begin
                for m of mods loop
                    printModule (m);
                    loadModule (m);
                end loop;
            end;

            -- If ramdisk driver loaded and initrd image present, map it into the
            -- ramdisk driver's address space.
            if ramdiskPID /= Process.NO_PROCESS and
               initrdAddr /= 0 and
               initrdSize /= 0 then

               mapInitrd (ramdiskPID, initrdAddr, initrdSize);

               println ("Modules: Starting Ramdisk driver.");
               Process.resume (ramdiskPID);
            end if;

            -- If filesystem server loaded and initrd image present, map
            -- the initrd into the filesystem server's address space too.
            if filesystemPID /= Process.NO_PROCESS and
               initrdAddr /= 0 and
               initrdSize /= 0 then

               mapInitrd (filesystemPID, initrdAddr, initrdSize);

               println ("Modules: Starting Filesystem server.");
               Process.resume (filesystemPID);

            elsif filesystemPID /= Process.NO_PROCESS then
               -- No initrd, start FS server anyway (it will detect no ramdisk)
               println ("Modules: Starting Filesystem server (no initrd).");
               Process.resume (filesystemPID);
            end if;
        else
            println ("Modules: No boot drivers or services found.");
        end if;
    end setup;

end Modules;
