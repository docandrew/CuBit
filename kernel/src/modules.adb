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
with Capability_ABI;
with Firmware_Data_ABI;
with InterruptNumbers;
with Process_Capabilities;
with Device_Vector_ABI;
with Cpio;
with ELF;
with Process.Loader;
with Multiboot;
with Boot_Modules;
with Boot_Timeline;
with Boot_Diagnostics;
with Boot_Panel;
with Boot_Timeline_Layout;
with Process_Identities;
with Owned_Memory_Layout;
with Launch_Configuration;
with Root_Export_Admission;
with Process_Launch;
with Publication_Pages;
with Cpu_Times_Page;
with acpi;
with Config;
with Time;
with Util;
with Physical_Range_Map;
with Device_Memory_Diagnostics;
with DMA_Remapping;
with Device_Memory_Authority;
with Device_Memory_Admission;
with Physical_Ranges;
with Service_Registry;
with Spinlocks;
with TLB_Shootdown;
with Boot_Module_Lifetime;
with TextIO; use TextIO;
with Virtmem;

package body Modules with SPARK_Mode => Off is

    pragma Compile_Time_Error
      (Launch_Configuration.Maximum_Block_Bytes > Process_Launch.Maximum_Bytes,
       "devmgr's configuration must fit a launch block");
    pragma Compile_Time_Error
      (Capabilities.FIRMWARE_NVS_TAG /= Capability_ABI.Firmware_NVS_Tag
       or else Capabilities.FIRMWARE_CONTROL_TAG /= Capability_ABI.Firmware_Control_Tag
       or else Capabilities.FIRMWARE_PCI_CONFIG_TAG /= Capability_ABI.Firmware_PCI_Config_Tag
       or else Capabilities.FIRMWARE_DATA_TAG /= Firmware_Data_ABI.Firmware_Data_Tag,
       "the kernel and the runtime must agree on the firmware-memory tags");

    initrdAddr  : Virtmem.PhysAddress := 0;
    initrdSize  : Storage_Count := 0;
    devmgrPID   : Process.ProcessID;
    setupStarted : Boolean := False;

    -- Package-level to avoid 2KB+ stack usage from FileIndex array
    cpioArchive : Cpio.Archive;

    ---------------------------------------------------------------------------
    -- Boot module lifetime state (see the spec). The image lock orders every
    -- change of a page's mapping count against copies out of the image and
    -- against handing released pages to the buddy allocator. Lock order:
    -- an address-space lock, then the image lock, then the buddy lock.
    ---------------------------------------------------------------------------
    package BML renames Boot_Module_Lifetime;
    use type BML.Phase;
    use type BML.Reference_Count;

    INITRD_NAME : constant String := "init.img";
    PAGE_BYTES : constant Unsigned_64 := Unsigned_64 (Virtmem.PAGE_SIZE);
    -- User space ends here (the canonical lower half).
    USER_ADDRESS_LIMIT : constant Unsigned_64 := 16#0000_8000_0000_0000#;
    -- Page-table geometry for the leaf walk.
    TABLE_LEVELS  : constant := 4;
    TABLE_ENTRIES : constant := Virtmem.NUM_PAGE_TABLE_ENTRIES;
    ENTRY_BYTES   : constant := 8;
    TOP_SHIFT     : constant := 39;
    LEVEL_SHIFT   : constant := 9;
    USER_TOP_ENTRIES : constant := TABLE_ENTRIES / 2;

    phases : array (Boot_Modules.Module_Index) of BML.Phase :=
      (others => BML.Retained);
    initrdIndex : Boot_Modules.Module_Count := 0;
    -- Pages of the shared image and its reference table (buddy memory, one
    -- Reference_Count per page), present while the image is Shared.
    initrdPages : BML.Page_Count := 0;
    livePages   : BML.Page_Count := 0;
    tableAddr   : System.Address := System.Null_Address;
    tableOrder  : BuddyAllocator.Order := 0;
    -- Boot-image mappings each process holds: a holder with none is never
    -- walked when it exits.
    held : array (Process.ProcessID) of Unsigned_64 := (others => 0);
    releasedBytes : Unsigned_64 := 0;
    imageLockName : aliased constant String := "boot image";
    imageLock : Spinlocks.Spinlock;

    function Released_Bytes return Unsigned_64 is (releasedBytes);

    function imageShared return Boolean is
      (initrdIndex /= 0 and then phases (initrdIndex) = BML.Shared);

    -- The page offset of a physical address inside the shared image.
    -- Callers hold the image lock.
    procedure imagePage (Physical : Unsigned_64; Page : out BML.Page_Offset;
                         Inside : out Boolean) is
    begin
        Page := 0;
        Inside := imageShared and then
          Physical >= Unsigned_64 (initrdAddr) and then
          Physical - Unsigned_64 (initrdAddr) < Unsigned_64 (initrdPages) * PAGE_BYTES;
        if Inside then
            Page := BML.Page_Offset ((Physical - Unsigned_64 (initrdAddr)) / PAGE_BYTES);
        end if;
    end imagePage;

    -- A run of released pages (image lock held). Contiguous pages are
    -- handed over together, so the buddy allocator admits aligned blocks.
    type Release_Run is record
        First : Unsigned_64 := 0;
        Pages : Natural := 0;
        Total : Natural := 0;
    end record;

    procedure flushRun (Run : in out Release_Run) is
    begin
        if Run.Pages > 0 then
            BuddyAllocator.adoptFrames (Virtmem.PhysAddress (Run.First), Run.Pages);
            releasedBytes := releasedBytes + Unsigned_64 (Run.Pages) * PAGE_BYTES;
            Run.Total := Run.Total + Run.Pages;
            Run.Pages := 0;
        end if;
    end flushRun;

    -- Processes read the managed total (system facts, CPU times): it grew.
    procedure publishTotal is
        Total : constant Unsigned_64 := Unsigned_64 (BuddyAllocator.getTotalBytes);
    begin
        Publication_Pages.Set_Memory_Bytes (Total);
        Cpu_Times_Page.Set_Memory (Total);
    end publishTotal;

    -- One mapping of the image page at Physical is gone (image lock held;
    -- the page must be the image's with a nonzero count).
    procedure dropPage (Physical : Unsigned_64; Run : in out Release_Run) is
        Page : BML.Page_Offset;
        Inside, Freed : Boolean;
    begin
        imagePage (Physical, Page, Inside);
        if not Inside then
            raise ModuleException with "Boot image page drop outside the image";
        end if;
        declare
            refs : BML.Reference_Table (0 .. initrdPages - 1)
              with Import, Address => tableAddr;
        begin
            if refs (Page) = 0 then
                raise ModuleException with "Boot image page dropped after release";
            end if;
            BML.Drop (refs, Page, Freed);
        end;
        if Freed then
            if Run.Pages = 0 or else
              Run.First + Unsigned_64 (Run.Pages) * PAGE_BYTES /= Physical
            then
                flushRun (Run);
                Run.First := Physical;
            end if;
            Run.Pages := Run.Pages + 1;
        end if;
    end dropPage;

    -- Finish a batch of drops (image lock held): adopt the last run, and
    -- when the image's last page went, the module is Released.
    procedure finishDrops (Run : in out Release_Run) is
    begin
        flushRun (Run);
        if Run.Total > 0 then
            publishTotal;
            livePages := livePages - Run.Total;
            if livePages = 0 then
                BML.Advance (phases (initrdIndex), BML.Released);
                BuddyAllocator.free (tableOrder, tableAddr);
                tableAddr := System.Null_Address;
                println ("Modules: boot image fully released");
            end if;
        end if;
    end finishDrops;

    procedure Acquire_Initrd_Pages (Offset, Bytes : Unsigned_64;
                                    Physical      : out Unsigned_64;
                                    Granted       : out Boolean)
    is
    begin
        Physical := 0;
        Granted := False;
        Spinlocks.enterCriticalSection (imageLock);
        if imageShared and then
          Offset mod PAGE_BYTES = 0 and then Bytes /= 0 and then Bytes mod PAGE_BYTES = 0
          and then Offset < Unsigned_64 (initrdPages) * PAGE_BYTES
          and then Bytes <= Unsigned_64 (initrdPages) * PAGE_BYTES - Offset
        then
            declare
                refs : BML.Reference_Table (0 .. initrdPages - 1)
                  with Import, Address => tableAddr;
            begin
                BML.Acquire (refs, BML.Page_Offset (Offset / PAGE_BYTES),
                             BML.Page_Count (Bytes / PAGE_BYTES), Granted);
            end;
            if Granted then
                Physical := Unsigned_64 (initrdAddr) + Offset;
            end if;
        end if;
        Spinlocks.exitCriticalSection (imageLock);
    end Acquire_Initrd_Pages;

    procedure Initrd_Pages (Offset, Bytes : Unsigned_64;
                            Physical      : out Unsigned_64;
                            Valid         : out Boolean)
    is
    begin
        Physical := 0;
        Valid := initrdAddr /= 0 and then Offset mod PAGE_BYTES = 0 and then
          Bytes /= 0 and then Offset <= Unsigned_64 (initrdPages) * PAGE_BYTES and then
          Bytes <= Unsigned_64 (initrdPages) * PAGE_BYTES - Offset;
        if Valid then
            Physical := Unsigned_64 (initrdAddr) + Offset;
        end if;
    end Initrd_Pages;

    procedure Return_Initrd_Pages (Physical, Bytes : Unsigned_64) is
        Run : Release_Run;
        Cursor : Unsigned_64 := Physical;
    begin
        Spinlocks.enterCriticalSection (imageLock);
        while Cursor < Physical + Bytes loop
            dropPage (Cursor, Run);
            Cursor := Cursor + PAGE_BYTES;
        end loop;
        finishDrops (Run);
        Spinlocks.exitCriticalSection (imageLock);
    end Return_Initrd_Pages;

    procedure Note_Mapped (Holder : Process.ProcessID; Pages : Unsigned_64) is
    begin
        Spinlocks.enterCriticalSection (imageLock);
        held (Holder) := held (Holder) + Pages;
        Spinlocks.exitCriticalSection (imageLock);
    end Note_Mapped;

    procedure Copy_Resident (Physical    : Integer_Address;
                             Length      : Storage_Count;
                             Destination : System.Address;
                             Copied      : out Boolean)
    is
        First, Last : BML.Page_Offset;
        Inside_First, Inside_Last : Boolean;
    begin
        Copied := False;
        if Length = 0 then
            return;
        end if;
        Spinlocks.enterCriticalSection (imageLock);
        imagePage (Unsigned_64 (Physical), First, Inside_First);
        imagePage (Unsigned_64 (Physical) + Unsigned_64 (Length) - 1, Last, Inside_Last);
        if Inside_First and then Inside_Last then
            declare
                refs : BML.Reference_Table (0 .. initrdPages - 1)
                  with Import, Address => tableAddr;
            begin
                if BML.Live (refs, First, Last - First + 1) then
                    Util.memCopy (Destination, Virtmem.P2Va (Physical), Length);
                    Copied := True;
                end if;
            end;
        end if;
        Spinlocks.exitCriticalSection (imageLock);
    end Copy_Resident;

    -- The leaf entry mapping Virtual in Holder's tables (address-space lock
    -- held): its address, or Null_Address when a table is missing or a
    -- large page maps it.
    function leafEntry (Holder : Process.ProcessID; Virtual : Unsigned_64)
      return System.Address
    is
        use Virtmem;
        Table : PhysAddress := Process.rootPhysical (Holder);
        Shift : Natural := TOP_SHIFT;
        Item : System.Address := System.Null_Address;
    begin
        for Level in 1 .. TABLE_LEVELS loop
            Item := P2Va (Table) + Storage_Offset
              (((Virtual / 2 ** Shift) mod TABLE_ENTRIES) * ENTRY_BYTES);
            exit when Level = TABLE_LEVELS;
            declare
                Upper : PageTableEntry with Import, Address => Item;
            begin
                if not Upper.present or else Upper.size then
                    return System.Null_Address;
                end if;
                Table := PFNToAddr (Upper.pgNum);
            end;
            Shift := Shift - LEVEL_SHIFT;
        end loop;
        return Item;
    end leafEntry;

    procedure Release_View (Holder   : Process.ProcessID;
                            Virtual  : Unsigned_64;
                            Bytes    : Unsigned_64;
                            Released : out Boolean)
    is
        Pages : constant Unsigned_64 := Bytes / PAGE_BYTES;
        Valid : Boolean := True;
        Run : Release_Run;
    begin
        Released := False;
        if Holder = Process.NO_PROCESS or else Bytes = 0 or else
          Virtual mod PAGE_BYTES /= 0 or else Bytes mod PAGE_BYTES /= 0 or else
          Pages > Maximum_Release_Pages or else
          Virtual >= USER_ADDRESS_LIMIT or else Bytes > USER_ADDRESS_LIMIT - Virtual
        then
            return;
        end if;
        -- The address-space lock is held throughout: no other change to
        -- these entries while they pass through their three steps.
        Process.lockAddressSpace (Holder);
        -- 1. Every page a present, user, read-only mapping of a live image
        --    page, before any is touched.
        Spinlocks.enterCriticalSection (imageLock);
        Valid := held (Holder) >= Pages;
        for I in 0 .. Pages - 1 loop
            exit when not Valid;
            declare
                Item : constant System.Address := leafEntry (Holder, Virtual + I * PAGE_BYTES);
                Page : BML.Page_Offset;
                Inside : Boolean;
            begin
                Valid := Item /= System.Null_Address;
                if Valid then
                    declare
                        Leaf : Virtmem.PageTableEntry with Import, Address => Item;
                    begin
                        imagePage (Unsigned_64 (Virtmem.PFNToAddr (Leaf.pgNum)), Page, Inside);
                        Valid := Leaf.present and then Leaf.user and then
                          not Leaf.writable and then Inside;
                        if Valid then
                            declare
                                refs : BML.Reference_Table (0 .. initrdPages - 1)
                                  with Import, Address => tableAddr;
                            begin
                                Valid := refs (Page) > 0;
                            end;
                        end if;
                    end;
                end if;
            end;
        end loop;
        Spinlocks.exitCriticalSection (imageLock);
        if not Valid then
            Process.unlockAddressSpace (Holder);
            return;
        end if;
        -- 2. Not present (the frame number stays in the entry for step 3),
        --    and every CPU forgets the translations. Each page still holds
        --    its count, so none can go to the buddy allocator yet.
        for I in 0 .. Pages - 1 loop
            declare
                Leaf : Virtmem.PageTableEntry with Import,
                  Address => leafEntry (Holder, Virtual + I * PAGE_BYTES);
            begin
                Leaf.present := False;
            end;
        end loop;
        TLB_Shootdown.Invalidate_All;
        -- 3. Drop each mapping's count, adopting pages that had no other,
        --    and clear the entries.
        Spinlocks.enterCriticalSection (imageLock);
        for I in 0 .. Pages - 1 loop
            declare
                Leaf : Virtmem.PageTableEntry with Import,
                  Address => leafEntry (Holder, Virtual + I * PAGE_BYTES);
            begin
                dropPage (Unsigned_64 (Virtmem.PFNToAddr (Leaf.pgNum)), Run);
                Leaf := Virtmem.EMPTY_PTE;
            end;
        end loop;
        finishDrops (Run);
        held (Holder) := held (Holder) - Pages;
        Spinlocks.exitCriticalSection (imageLock);
        Process.unlockAddressSpace (Holder);
        Released := True;
    end Release_View;

    procedure Forget_Process (Holder : Process.ProcessID) is
        use Virtmem;
        Run : Release_Run;
        Remaining : Unsigned_64;

        -- Every present 4 KiB user leaf below Table (level 1 = leaves).
        procedure walk (Table : PhysAddress; Level : Positive; Entries : Natural) is
        begin
            for Index in 0 .. Entries - 1 loop
                exit when Remaining = 0;
                declare
                    Item : PageTableEntry with Import, Address =>
                      P2Va (Table) + Storage_Offset (Index * ENTRY_BYTES);
                    Page : BML.Page_Offset;
                    Inside : Boolean;
                begin
                    if Item.present and then not Item.size then
                        if Level > 1 then
                            walk (PFNToAddr (Item.pgNum), Level - 1, TABLE_ENTRIES);
                        else
                            imagePage (Unsigned_64 (PFNToAddr (Item.pgNum)), Page, Inside);
                            if Inside then
                                declare
                                    refs : BML.Reference_Table (0 .. initrdPages - 1)
                                      with Import, Address => tableAddr;
                                begin
                                    -- A released page in a dead process's
                                    -- tables is an ordinary frame it owned.
                                    if refs (Page) > 0 then
                                        dropPage (Unsigned_64 (PFNToAddr (Item.pgNum)), Run);
                                        Remaining := Remaining - 1;
                                    end if;
                                end;
                            end if;
                        end if;
                    end if;
                end;
            end loop;
        end walk;
    begin
        if Holder = Process.NO_PROCESS then
            return;
        end if;
        Spinlocks.enterCriticalSection (imageLock);
        Remaining := held (Holder);
        if Remaining > 0 and then imageShared then
            -- The holder never runs again and its tables are not yet freed;
            -- no CPU translates through them (reclaimProcess's contract).
            walk (Process.rootPhysical (Holder), TABLE_LEVELS, USER_TOP_ENTRIES);
            finishDrops (Run);
            if Remaining > 0 then
                -- Mappings counted but no longer in its tables (replaced or
                -- removed elsewhere): those pages stay reserved. Never a
                -- premature release; say so.
                print ("Modules: boot image pages left held by exited PID ");
                println (Integer (Holder));
            end if;
        end if;
        held (Holder) := 0;
        Spinlocks.exitCriticalSection (imageLock);
    end Forget_Process;

    -- Share the image with its first consumer: a reference table with one
    -- mapping per page (the root mapping installed next).
    procedure shareImage (Holder : Process.ProcessID) is
        Bytes : constant Storage_Count :=
          Storage_Count (initrdPages) * (BML.Reference_Count'Size / System.Storage_Unit);
        Block : System.Address;
    begin
        tableOrder := BuddyAllocator.getOrder (Bytes);
        BuddyAllocator.alloc (tableOrder, Block);
        if Block = BuddyAllocator.NO_BLOCK_AVAILABLE then
            raise ModuleException with "No memory for the boot image reference table";
        end if;
        tableAddr := Block;
        declare
            refs : BML.Reference_Table (0 .. initrdPages - 1)
              with Import, Address => tableAddr;
        begin
            BML.Share (refs);
        end;
        livePages := initrdPages;
        BML.Advance (phases (initrdIndex), BML.Shared);
        held (Holder) := Unsigned_64 (initrdPages);
    end shareImage;

    -- Every module but the boot image has no consumer: release it now,
    -- before anything is published or mapped.
    procedure releaseUnconsumed is
    begin
        for I in 1 .. Multiboot.Boot_Module_Count loop
            declare
                Item : constant Boot_Modules.Image := Multiboot.Boot_Module (I);
                Name : constant String := Item.Name.Text (1 .. Item.Name.Length);
                Frames : constant Natural :=
                  Natural ((Item.Page_Limit - Item.First) / Boot_Modules.Page_Bytes);
            begin
                if Name = INITRD_NAME and then initrdIndex = 0 then
                    initrdIndex := I;
                elsif phases (I) = BML.Retained then
                    BuddyAllocator.adoptFrames (Virtmem.PhysAddress (Item.First), Frames);
                    BML.Advance (phases (I), BML.Released);
                    releasedBytes := releasedBytes + Unsigned_64 (Frames) * PAGE_BYTES;
                    print ("Modules: released unconsumed module "); print (Name);
                    print (": "); print (Frames * Integer (PAGE_BYTES) / 1024);
                    print (" KiB (buddy total ");
                    print (Integer (BuddyAllocator.getTotalBytes / 1024));
                    println (" KiB)");
                end if;
            end;
        end loop;
    end releaseUnconsumed;

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
        shareImage (pid);

        for i in 0..numPages-1 loop
            mapPage (phys    => addr +
                         Virtmem.PhysAddress(i * Virtmem.PAGE_SIZE),
                     virt    => To_Integer(base + (i * Virtmem.PAGE_SIZE)),
                     flags   => Virtmem.PG_USERDATARO,
                     myP4    => Process.addrtab(pid),
                     success => ok);

            if not ok then
                -- Root mappings installed so far stay counted (held); the
                -- device manager never runs, so they are never released.
                print ("Modules: Error mapping initrd page ");
                print (Integer(i));
                raise ModuleException with "Initrd mapping failed; device manager remains suspended";
            end if;
        end loop;

        println ("Modules: Initrd mapping complete.");
    end mapInitrd;

    ---------------------------------------------------------------------------
    -- The device manager's typed launch configuration (Launch_Configuration):
    -- the boot image's size, the boot framebuffer's pages, its named handles
    -- and one handle per root, as many as the classification has. Installed
    -- as a launch block, exactly as SYSCALL_INSTALL_LAUNCH_ARGUMENTS would,
    -- before its first instruction: Open_Configuration maps the pages the
    -- block needs and writes the prefix, Put_Root_Word each root's handle,
    -- Close_Configuration starts it with RDI = the block's length.
    ---------------------------------------------------------------------------
    Config_Page_Bytes : constant := Process_Launch.Page_Bytes;
    Config_Words_Per_Page : constant := Config_Page_Bytes / Launch_Configuration.Word_Bytes;
    Maximum_Config_Pages : constant :=
      (Launch_Configuration.Maximum_Block_Bytes + Config_Page_Bytes - 1) / Config_Page_Bytes;
    subtype Config_Page_Count is Natural range 0 .. Maximum_Config_Pages;
    subtype Config_Page_Index is Positive range 1 .. Maximum_Config_Pages;
    configPages : array (Config_Page_Index) of System.Address := (others => System.Null_Address);
    configLength : Launch_Configuration.Word_Count := 0;

    procedure Put_Word (index : Launch_Configuration.Word_Index; value : Unsigned_64) is
        page : constant Config_Page_Index := (index - 1) / Config_Words_Per_Page + 1;
        offset : constant Storage_Offset :=
          Storage_Offset (((index - 1) mod Config_Words_Per_Page) * Launch_Configuration.Word_Bytes);
        target : Unsigned_64 with Import, Address => configPages (page) + offset;
    begin
        target := value;
    end Put_Word;

    procedure Open_Configuration
      (pid : Process.ProcessID; value : Launch_Configuration.Device_Manager_Configuration)
    is
        use type Process.Page_Allocation_Result;
        package LC renames Launch_Configuration;
        Prefix : constant LC.Encoded := LC.Encode_Prefix (value);
        Length : constant LC.Word_Index :=
          LC.Device_Manager_Last (value.Device_Roots, value.Firmware_Roots);
        Pages : constant Config_Page_Count :=
          (Length * LC.Word_Bytes + Config_Page_Bytes - 1) / Config_Page_Bytes;
        result : Process.Page_Allocation_Result;
    begin
        Process.lockAddressSpace (pid);
        Process.proctab (pid).frames.capacity :=
          Process.proctab (pid).frames.capacity + Pages;
        for page in 1 .. Pages loop
            Process.tryAddPage
              (proc    => Process.proctab (pid),
               mapTo   => To_Address (Integer_Address (Process_Launch.Arguments_Base) +
                                      Integer_Address (page - 1) * Config_Page_Bytes),
               storage => configPages (page),
               result  => result,
               flags   => Virtmem.PG_USERDATARO);
            if result /= Process.Page_Added then
                Process.unlockAddressSpace (pid);
                raise ModuleException with "No page for the device manager's configuration";
            end if;
        end loop;
        Process.unlockAddressSpace (pid);
        -- Fresh frames are zeroed.
        for index in 1 .. Prefix.Length loop
            Put_Word (index, Prefix.Words (index));
        end loop;
        configLength := Length;
    end Open_Configuration;

    procedure Close_Configuration (pid : Process.ProcessID) is
    begin
        Process.threadOf (pid).kernelStack.interruptFrame.rdi :=
          Unsigned_64 (configLength * Launch_Configuration.Word_Bytes);
        Process.proctab (pid).launch := Process_Launch.Arguments_Installed;
    end Close_Configuration;

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
        use type Root_Export_Admission.Outcome;
        Root_Selection : Root_Export_Admission.Selection;

        function Select_Roots return Root_Export_Admission.Selection is
            package PR renames Physical_Ranges;
            M : PR.Map renames Physical_Range_Map.Kernel_Map;
            use type Root_Export_Admission.Root_Demand;
            Device_Total, Firmware_Total : Root_Export_Admission.Root_Demand := 0;
            Left_Out : Natural := 0;
            Found : Boolean;
            First, Last : PR.Address;
            Run : PR.Firmware_Run;
            Cursor : PR.Firmware_Cursor;
            From : PR.Page_Number := 0;
        begin
            loop
                PR.Next_Device_Run (M, From, Found, First, Last);
                exit when not Found;
                Device_Total := Device_Total + 1;
                exit when PR.Page_Of (Last) = PR.Last_Page;
                From := PR.Page_Of (Last) + 1;
            end loop;
            Cursor := PR.First_Firmware_Cursor (M);
            loop
                PR.Next_Firmware_Run (M, Cursor, Found, Run, Left_Out);
                exit when not Found;
                Firmware_Total := Firmware_Total + 1;
            end loop;
            return Root_Export_Admission.Choose (Device_Total, Firmware_Total);
        end Select_Roots;
    begin
        if setupStarted then
            raise ModuleException with "Boot module startup is one-shot";
        end if;
        setupStarted := True;
        Spinlocks.Initialize (imageLock, imageLockName'Access);
        -- Before any figure of memory is published: modules nobody reads
        -- go back now. This also finds the boot image.
        releaseUnconsumed;
        -- The physical classification is final before any user process
        -- exists; device-memory authority consults nothing else.
        Physical_Range_Map.Seal_From_Boot;
        Device_Memory_Diagnostics.Publish_Classification;
        -- DMA remapping on before any process can bind a device (IOMMU-001);
        -- without an IOMMU this records the degraded state.
        DMA_Remapping.Setup;
        -- System facts are final now; every process maps them from its start.
        Publication_Pages.Publish_Facts;
        -- Per-CPU times: the CPUs kmain starts (acpi.numCPUs, at most
        -- Config.MAX_SMP_CPUS), each record written from its first boundary.
        Cpu_Times_Page.Publish
          (Processors     => Unsigned_64 (Natural'Min (acpi.numCPUs, Config.MAX_SMP_CPUS)),
           Tick_Frequency => Unsigned_64 (Time.tscFrequencyHz),
           Memory_Bytes   => Unsigned_64 (BuddyAllocator.getTotalBytes));
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
                if I = initrdIndex then
                    initrdAddr := Virtmem.PhysAddress (Item.First);
                    initrdSize := Size;
                    initrdPages := BML.Page_Count
                      ((Item.Page_Limit - Item.First) / Boot_Modules.Page_Bytes);
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
        --  Refuse an indivisible inventory before creating a process,
        --  placing capabilities, or opening a launch configuration.
        Root_Selection := Select_Roots;
        if Root_Selection.Status = Root_Export_Admission.Inventory_Capacity then
            println ("Modules: stage root-inventory fail " &
                     Root_Export_Admission.Capacity_Reason);
            println ("Modules: devmgr not started: ROOT_INVENTORY_CAPACITY");
            println ("Modules: ACPI backend not started: ROOT_INVENTORY_CAPACITY");
            Boot_Timeline.Mark_Stage
              (Boot_Timeline_Layout.Device_Manager_Roots_Refused);
            Boot_Diagnostics.Set_Evidence
              (Boot_Panel.Firmware_Evidence,
               "devmgr/acpi-backend refused: ROOT_INVENTORY_CAPACITY");
            return;
        end if;
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

        -- devmgr's roots (CAP-002), placed where its table chooses and
        -- reported to it by handle in its typed configuration. Everything
        -- devmgr hands out later is a narrowed copy of one of these (or a
        -- kernel object SPAWN gives it); no system call makes authority from
        -- nothing.
        declare
            package LC renames Launch_Configuration;
            handles : LC.Device_Manager_Handles;
            Framebuffer_Span : LC.Physical_Span;
            GRANT_ONLY : constant Capabilities.CapabilityRights :=
              (Capabilities.RIGHT_GRANT => True, others => False);
            READ_GRANT : constant Capabilities.CapabilityRights :=
              (Capabilities.RIGHT_READ | Capabilities.RIGHT_GRANT => True, others => False);
            RW_GRANT : constant Capabilities.CapabilityRights :=
              (Capabilities.RIGHT_READ | Capabilities.RIGHT_WRITE |
               Capabilities.RIGHT_GRANT => True, others => False);
            -- The largest single real-time share a root may hand out: half
            -- a CPU (Process.reserveRealtime's own bound on a budget).
            SCHEDULING_ROOT_BUDGET_US : constant := 500_000;
            SCHEDULING_ROOT_PERIOD_US : constant := 1_000_000;
            FIRST_LEGACY_LINE : constant := InterruptNumbers.PS2KEYBOARD;
            LAST_LEGACY_LINE  : constant := InterruptNumbers.IDE2;
            PORT_SPACE        : constant := 16#1_0000#;
            PCI_CONFIG_PORT   : constant := 16#CF8#;
            PCI_CONFIG_PORTS  : constant := 8;
            FIRST_ROLE        : constant := 1;

            procedure place (cap : Capabilities.Capability; word : out Unsigned_64) is
            begin
                Process_Capabilities.Insert (Process.proctab(devmgrPID).caps, cap, word);
                if not Capability_ABI.Is_Handle_Word (word) then
                    raise ModuleException with "No room for the device manager's roots";
                end if;
            end place;

            function root (kind : Capabilities.CapabilityType;
                           rights : Capabilities.CapabilityRights;
                           ref, param : Unsigned_64) return Capabilities.Capability is
              ((capType      => kind,
                rights       => rights,
                authorityTag => Capabilities.NO_AUTHORITY_TAG,
                object       => (ref => ref, param => param),
                gen          => Capabilities.INITIAL_GENERATION));
        begin
            if Multiboot.Has_Graphics then
                Framebuffer_Span :=
                  (First => Unsigned_64 (Multiboot.Framebuffer.Map_First),
                   Bytes => Unsigned_64 (Multiboot.Framebuffer.Map_Limit) -
                            Unsigned_64 (Multiboot.Framebuffer.Map_First));
            end if;
            handles.Self := Process.proctab(devmgrPID).selfHandle;
            handles.Self_Process := Process.proctab(devmgrPID).selfProcessHandle;
            -- An untagged endpoint to itself: the children's "ready" and
            -- broker endpoints are badged copies of it.
            place ((capType      => Capabilities.CAP_ENDPOINT,
                    rights       => RW_GRANT,
                    authorityTag => Capabilities.NO_AUTHORITY_TAG,
                    object       => (ref => Unsigned_64 (devmgrPID), param => 0),
                    gen          => Process.generationOf (devmgrPID)),
                   handles.Self_Grantable);
            -- Spawn and process control over every process (not capability
            -- construction): the next CSPACE-style narrowing candidate.
            place (root (Capabilities.CAP_PROCESS,
                         (Capabilities.RIGHT_READ | Capabilities.RIGHT_EXECUTE |
                          Capabilities.RIGHT_GRANT | Capabilities.RIGHT_WRITE => True,
                          others => False), 0, 0),
                   handles.Process_Root);
            place (root (Capabilities.CAP_IOPORT, Capabilities.READ_WRITE,
                         PCI_CONFIG_PORT, PCI_CONFIG_PORTS), handles.PCI_Ports);
            place (root (Capabilities.CAP_NOTIFICATION,
                         (Capabilities.RIGHT_WRITE => True, others => False),
                         Unsigned_64 (Service_Registry.DRIVER_DEVMGR), 1),
                   handles.Registration);
            -- Device vector authority (docs/msi-interrupts.md): a CAP_IRQ
            -- naming the whole device range. GRANT allocates and releases
            -- blocks for drivers; READ reads per-vector counts and owners.
            -- Never copied (Capability_Install.Copyable).
            place (root (Capabilities.CAP_IRQ, READ_GRANT,
                         Device_Vector_ABI.First_Vector, Device_Vector_ABI.Vector_Count),
                   handles.Vector_Authority);
            place (root (Capabilities.CAP_IOPORT, RW_GRANT, 0, PORT_SPACE),
                   handles.IO_Ports);
            place (root (Capabilities.CAP_IRQ, READ_GRANT, FIRST_LEGACY_LINE,
                         LAST_LEGACY_LINE - FIRST_LEGACY_LINE + 1),
                   handles.Legacy_Interrupts);
            place (root (Capabilities.CAP_NOTIFICATION, RW_GRANT, FIRST_ROLE,
                         Unsigned_64 (Service_Registry.DriverID'Last) - FIRST_ROLE + 1),
                   handles.Notifications);
            place (root (Capabilities.CAP_SCHEDULING, GRANT_ONLY,
                         SCHEDULING_ROOT_BUDGET_US, SCHEDULING_ROOT_PERIOD_US),
                   handles.Scheduling);
            -- The only firmware snapshot reader (Firmware_Snapshot_ABI):
            -- read the sealed boot copy of the ACPI tables, nothing else.
            -- devmgr passes it to procmgr, which installs a read-only copy
            -- only where a manifest declares it and the startup plan
            -- approves it. Minted even if the snapshot failed: the call
            -- then answers Not_Ready.
            place (root (Capabilities.CAP_FIRMWARE_SNAPSHOT, READ_GRANT, 0, 0),
                   handles.Firmware_Snapshot);

            -- Device-memory roots (CAP-001): one CAP_DEVICE_MEM per maximal
            -- run of device-mappable pages in the boot classification (PCI
            -- holes, firmware-reserved MMIO such as ECAM, and everything
            -- above the top of described memory, so BARs above 4 GiB are
            -- covered), ascending, every one of them (LIM-001b). Never RAM,
            -- kernel memory, boot modules, ACPI memory or kernel MMIO.
            -- devmgr maps BARs and hands drivers subranges of these; the
            -- initrd it shares is a boot module, not device memory.
            --
            -- Firmware-memory roots (the ACPI hardware grant): one
            -- CAP_DEVICE_MEM per run of whole ACPI NVS pages (tagged
            -- FIRMWARE_NVS_TAG), the FACS page (FIRMWARE_CONTROL_TAG), PCI
            -- segment 0 bus 0's ECAM slice (FIRMWARE_PCI_CONFIG_TAG), then
            -- the table-named firmware-data runs (FIRMWARE_DATA_TAG)
            -- (Physical_Ranges.Next_Firmware_Run). Only MAP_DEVICE into the
            -- holder maps them, NVS and the FACS page write-back, PCI
            -- configuration uncached and read-only (its root has no write
            -- right, so no derivation has one either), firmware data
            -- uncached, and no device-memory capability maps their NVS
            -- pages (Device_Memory_Authority). devmgr claims the NVS, FACS
            -- and PCI roots for ACPI and hands them, exactly, to the program
            -- procmgr's startup plan approves (acpi-hardware
            -- approve-declared; the PCI slice only with acpi-pci-config
            -- approve-declared too); it keeps the data roots and narrows one
            -- envelope at a time for a program approved with
            -- acpi-firmware-data.
            --
            --  The preflight counted this sealed, deterministic inventory
            --  before loading devmgr. Place exactly the admitted inventory.
            declare
                package PR renames Physical_Ranges;
                M : PR.Map renames Physical_Range_Map.Kernel_Map;
                Left_Out : Natural := 0;
                Kept_Device : constant LC.Root_Count := Root_Selection.Device;
                Kept_Firmware : constant LC.Root_Count := Root_Selection.Firmware;
                Found : Boolean;
                First, Last : PR.Address;
                Run : PR.Firmware_Run;
                Cursor : PR.Firmware_Cursor;
                From : PR.Page_Number;

                function Firmware_Root (R : PR.Firmware_Run) return Capabilities.Capability is
                  ((capType      => Capabilities.CAP_DEVICE_MEM,
                    rights       =>
                      (if Device_Memory_Authority.Firmware_Mode
                            (R.Kind, Device_Memory_Admission.Read_Write)
                       then RW_GRANT else READ_GRANT),
                    authorityTag => Device_Memory_Authority.Tag_Of (R.Kind),
                    object       => (ref => R.First, param => R.Last - R.First + 1),
                    gen          => Capabilities.INITIAL_GENERATION));
            begin
                Open_Configuration
                  (devmgrPID,
                   (Device_Roots   => Kept_Device,
                    Firmware_Roots => Kept_Firmware,
                    Boot_Image     => Unsigned_64 (initrdSize),
                    Framebuffer    => Framebuffer_Span,
                    Handles        => handles));

                From := 0;
                for Index in 1 .. Natural (Kept_Device) loop
                    PR.Next_Device_Run (M, From, Found, First, Last);
                    exit when not Found;
                    declare
                        word : Unsigned_64;
                    begin
                        place (root (Capabilities.CAP_DEVICE_MEM, RW_GRANT,
                                     First, Last - First + 1), word);
                        Put_Word (LC.Device_Root_Word (Index), word);
                    end;
                    print ("Modules: devmgr device root ");
                    print (First);
                    print (" .. ");
                    println (Last);
                    exit when PR.Page_Of (Last) = PR.Last_Page;
                    From := PR.Page_Of (Last) + 1;
                end loop;
                Cursor := PR.First_Firmware_Cursor (M);
                Left_Out := 0;
                for Index in 1 .. Natural (Kept_Firmware) loop
                    PR.Next_Firmware_Run (M, Cursor, Found, Run, Left_Out);
                    exit when not Found;
                    declare
                        word : Unsigned_64;
                    begin
                        place (Firmware_Root (Run), word);
                        Put_Word (LC.Firmware_Root_Word (Kept_Device, Index), word);
                    end;
                    print ("Modules: devmgr firmware-memory root ");
                    print (Run.First);
                    print (" .. ");
                    print (Run.Last);
                    println ((case Run.Kind is
                                when PR.NVS_Memory        => " (NVS)",
                                when PR.Control_Memory    => " (FACS)",
                                when PR.PCI_Config_Memory => " (PCI configuration)",
                                when PR.Data_Memory       => " (firmware data)"));
                end loop;
                if Left_Out > 0 then
                    print ("Modules: firmware-memory runs left out: ");
                    println (Left_Out);
                end if;
                print ("Modules: devmgr roots: ");
                print (Integer (Kept_Device));
                print (" device, ");
                print (Integer (Kept_Firmware));
                println (" firmware");
            end;
            Close_Configuration (devmgrPID);
        end;

        println ("Modules: Starting device manager.");
        Process.resume (devmgrPID);
        Boot_Timeline.Mark_Stage (Boot_Timeline_Layout.Device_Manager_Loaded);

    end setup;

end Modules;
