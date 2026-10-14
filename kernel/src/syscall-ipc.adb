-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2020 Jon Andrew
--
-- Syscall IPC, memory, and process creation handler implementations.
-------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Unchecked_Conversion;
with System;
with System.Storage_Elements; use System.Storage_Elements;

with acpi;
with Boot_Timeline;
with BuddyAllocator;
with Memory_Accounting;
with Process_Memory_Budget;
with Boot_Framebuffer;
with Multiboot;
with Capabilities;
with Capabilities.IRQ;
with Capabilities.Operations;
with Capability_ABI;
with Process_Capabilities;
with Device_Memory_Admission;
with Device_Memory_Authority;
with Device_Memory_Diagnostics;
with Device_Memory_Publication;
with InterruptNumbers;
with IPC_Labels;
with ELF;
with Heap_Admission;
with Heap_Growth;
with Interrupts;
with Memory_Grants;
with Modules;
with Owned_Memory_Layout;
with PerCpuData;
with PerCPUData;
with Process;
with Process_Identities;
with Process.DMA;
with Process.IPC;
with Process.Owned_Memory;
with Process.Loader;
with Process.User_Memory;
with Spinlocks;
with Process_Lifetime;
with Physical_Range_Map;
with Service_Registry;
with System_Publications;
with TextIO; use TextIO;
with TLB_Shootdown;
with Util;
with User_Buffer_Copy;
with Boot_Output;
with Virtmem;
with x86;

use type Process.MessageTag;
use type Process.ProcessMode;
use type Capabilities.Operations.OperationStatus;

package body Syscall.IPC is
    use type Process_Capabilities.Tables.Slot_Base;
    -- Shared-memory grant flags (arg3 of the create-grant calls): bit 0
    -- read-write, then these.
    Grant_Flag_Forwardable : constant Unsigned_64 := 2;
    -- Post grant lifecycle events for it (docs/data-plane.md).
    Grant_Flag_Notify      : constant Unsigned_64 := 4;

    --  Bound one privileged mapping operation so a malformed request cannot
    --  monopolize the kernel.  Larger regions are mapped by a sequence of
    --  bounded calls.
    MAX_MAP_INTO_PAGES_PER_CALL : constant Unsigned_64 := 1024;
    -- MAP_INTO arg4: what kind of physical memory is shared.
    -- Boot image: arg1 is a byte offset into the initrd, never a physical
    -- address (docs/syscall-retirement.md); the pages map read-only.
    MAP_INTO_BOOT_MODULE : constant Unsigned_64 := 1;
    MAP_INTO_DEVICE      : constant Unsigned_64 := 2;  -- uncached device pages
    -- Exclusive end of the lower-half user address space.
    USER_ADDRESS_LIMIT   : constant Unsigned_64 := 16#0000_8000_0000_0000#;

    function toErr is
        new Ada.Unchecked_Conversion (Long_Integer, Unsigned_64);
    reterr : constant Unsigned_64 := toErr (-1);

    function tagToU64 is new Ada.Unchecked_Conversion
        (Process.MessageTag, Unsigned_64);
    function u64ToTag is new Ada.Unchecked_Conversion
        (Unsigned_64, Process.MessageTag);
    function u64ToRights is new Ada.Unchecked_Conversion
        (Unsigned_8, Capabilities.CapabilityRights);

    ---------------------------------------------------------------------------
    -- Authority by handle (CAP-002): every call names the capability it
    -- relies on; nothing searches the caller's table for one of a type.
    -- The *Held forms expect the caller's mailbox lock to be held.
    ---------------------------------------------------------------------------
    function onlyRight (right : Capabilities.CapabilityRight)
                        return Capabilities.CapabilityRights
    is
        rights : Capabilities.CapabilityRights := Capabilities.NO_RIGHTS;
    begin
        rights (right) := True;
        return rights;
    end onlyRight;

    function findOwn (callerPID : Process.ProcessID;
                      handle    : Unsigned_64;
                      kind      : Capabilities.CapabilityType;
                      rights    : Capabilities.CapabilityRights)
                      return Process_Capabilities.Found
    is
        found : Process_Capabilities.Found;
    begin
        Spinlocks.enterCriticalSection (Process.mailtab(callerPID).lock);
        found := Process_Capabilities.Find
          (Process.proctab(callerPID).caps, handle, kind, rights);
        Spinlocks.exitCriticalSection (Process.mailtab(callerPID).lock);
        return found;
    end findOwn;

    -- A CAP_PROCESS with RIGHT_GRANT for targetPID (or the wildcard).
    function processAuthorityHeld (callerPID : Process.ProcessID;
                                   handle    : Unsigned_64;
                                   targetPID : Process.ProcessID) return Boolean
    is
        found : constant Process_Capabilities.Found := Process_Capabilities.Find
          (Process.proctab(callerPID).caps, handle, Capabilities.CAP_PROCESS,
           onlyRight (Capabilities.RIGHT_GRANT));
    begin
        return found.OK and then
          (found.Cap.object.ref = 0 or else
           (found.Cap.object.ref = Unsigned_64 (targetPID) and then
            found.Cap.gen = Process.generationOf (targetPID)));
    end processAuthorityHeld;

    -- The capability a handle names in the caller's table (a snapshot), or
    -- the null capability.
    function ownCap (callerPID : Process.ProcessID; handle : Unsigned_64)
                     return Capabilities.Capability
    is
        found : Process_Capabilities.Found;
    begin
        Spinlocks.enterCriticalSection (Process.mailtab(callerPID).lock);
        found := Process_Capabilities.Find_Any (Process.proctab(callerPID).caps, handle);
        Spinlocks.exitCriticalSection (Process.mailtab(callerPID).lock);
        return (if found.OK then found.Cap else Capabilities.NULL_CAPABILITY);
    end ownCap;

    -- The CAP_DEVICE_MEM a handle names (no particular right: Authorize
    -- checks the mode), or a null capability.
    function deviceMemoryOf (callerPID : Process.ProcessID; handle : Unsigned_64)
                             return Capabilities.Capability
    is
        found : constant Process_Capabilities.Found :=
          findOwn (callerPID, handle, Capabilities.CAP_DEVICE_MEM, Capabilities.NO_RIGHTS);
    begin
        return (if found.OK then found.Cap else Capabilities.NULL_CAPABILITY);
    end deviceMemoryOf;

    procedure handleSpawn (callerPID : Process.ProcessID;
                           arg0, arg1, arg2 : Unsigned_64;
                           retval    : out Unsigned_64) with
        SPARK_Mode => Off   -- Raw syscall/process integration; pure helpers proved separately
    is
        package ABI renames Capability_ABI;
        request  : aliased ABI.Spawn_Request := [others => 0];
        result   : aliased ABI.Spawn_Result := [others => 0];
        elfAddr  : System.Address;
        elfSize  : Storage_Count;
        priority : Process.ProcessPriority;
        elfHeader : aliased ELF.ELFFileHeader;
        name : aliased Process.ProcessName := [others => ASCII.NUL];
        copied : Boolean;
        newPID   : Process.ProcessID;
        parent   : Process.ProcessID := callerPID;
        limit    : Process_Capabilities.Tables.Handle_Limit;
        limitOK  : Boolean;
        started  : constant Unsigned_64 := Boot_Timeline.Now;
        refuse   : ABI.Refusal := ABI.Invalid_Argument;

        procedure refused (why : ABI.Refusal; text : String) is
        begin
            println (text);
            retval := ABI.Refusal_Word (why);
        end refused;
    begin
        retval := ABI.Refusal_Word (ABI.Invalid_Argument);

        -- arg0 names the spawn authority: a CAP_PROCESS with RIGHT_EXECUTE.
        if not findOwn (callerPID, arg0, Capabilities.CAP_PROCESS,
                        onlyRight (Capabilities.RIGHT_EXECUTE)).OK
        then
            refused (ABI.Not_Permitted, "SPAWN: denied, no CAP_PROCESS/EXECUTE");
            return;
        end if;
        Process.User_Memory.Copy
          (callerPID, arg1, request'Address, ABI.Spawn_Request_Bytes, copied);
        if not copied or else
           not Process.User_Memory.Writable_Range (callerPID, arg2, ABI.Spawn_Result_Bytes)
        then
            refused (refuse, "SPAWN: request or result unreadable");
            return;
        end if;
        Process_Capabilities.Limit_Of
          (request (ABI.Limit), request (ABI.Limit_Count), limit, limitOK);
        if not limitOK then
            refused (refuse, "SPAWN: invalid handle limit");
            return;
        elsif request (ABI.Image_Bytes) < ELF.ELFFileHeader'Size / 8 or else
          not User_Buffer_Copy.Valid_Range (request (ABI.Image), request (ABI.Image_Bytes))
        then
            refused (refuse, "SPAWN: invalid ELF source range");
            return;
        end if;
        elfAddr := Util.numToAddr (request (ABI.Image));
        -- Validate before converting the unsigned ABI length or reading it.
        elfSize := Storage_Count (request (ABI.Image_Bytes));
        Process.User_Memory.Copy
          (callerPID, request (ABI.Image), elfHeader'Address, ELF.ELFFileHeader'Size / 8, copied);
        if not copied then
            refused (refuse, "SPAWN: ELF header source unreadable");
            return;
        end if;

        if not Process.Loader.isValidELF (elfHeader) then
            refused (refuse, "SPAWN: invalid ELF header");
            return;
        end if;

        if request (ABI.Priority) > 10 then
            priority := 5;
        elsif request (ABI.Priority) = 0 then
            priority := 1;
        else
            priority := Process.ProcessPriority (request (ABI.Priority));
        end if;

        -- The parent by identity; a stale one refuses the spawn rather than
        -- naming whoever holds the slot now.
        if request (ABI.Parent) /= 0 then
            parent := Process.processOfIdentity (Process_Identities.From_Word (request (ABI.Parent)));
            if parent = Process.NO_PROCESS then
                refused (ABI.Target_Gone, "SPAWN: parent identity is stale");
                return;
            end if;
        end if;

        -- The name (0 = default "spawned").
        if request (ABI.Name) /= 0 then
            Process.User_Memory.Copy_Name (callerPID, request (ABI.Name), name, copied);
            if not copied then
                refused (refuse, "SPAWN: process name unreadable");
                return;
            end if;
        else
            name (1 .. 7) := "spawned";
        end if;

        newPID := Process.Loader.load (
            elfHeader    => elfHeader,
            objStart     => elfAddr,
            size         => elfSize,
            strAddr      => name'Address,
            requestedPID => Process.NO_PROCESS,
            priority     => priority,
            ppid         => parent,
            sourcePID    => callerPID,
            handleLimit  => limit);

        if newPID = Process.NO_PROCESS then
            refused (ABI.Out_Of_Memory, "SPAWN: load failed");
            return;
        end if;

        -- The spawner's handles for the new process, kernel objects for
        -- their owner: control (all rights), an untagged endpoint it may
        -- badge and pass on, and the CSPACE for installing the child's
        -- startup capabilities until it first runs. Placed under the
        -- spawner's mailbox lock.
        declare
            gen : constant Capabilities.Generation := Process.generationOf (newPID);
            ref : constant Unsigned_64 := Unsigned_64 (newPID);
            words : array (1 .. 3) of Unsigned_64;
        begin
            Spinlocks.enterCriticalSection (Process.mailtab(callerPID).lock);
            Process_Capabilities.Insert
              (Process.proctab(callerPID).caps,
               (capType => Capabilities.CAP_PROCESS,
                rights  => (Capabilities.RIGHT_READ | Capabilities.RIGHT_WRITE |
                            Capabilities.RIGHT_EXECUTE | Capabilities.RIGHT_GRANT => True,
                            others => False),
                authorityTag => Capabilities.NO_AUTHORITY_TAG,
                object  => (ref => ref, param => 0),
                gen     => gen), words (1));
            Process_Capabilities.Insert
              (Process.proctab(callerPID).caps,
               (capType => Capabilities.CAP_ENDPOINT,
                rights  => (Capabilities.RIGHT_READ | Capabilities.RIGHT_WRITE |
                            Capabilities.RIGHT_GRANT => True, others => False),
                authorityTag => Capabilities.NO_AUTHORITY_TAG,
                object  => (ref => ref, param => 0),
                gen     => gen), words (2));
            Process_Capabilities.Insert
              (Process.proctab(callerPID).caps,
               (capType => Capabilities.CAP_CSPACE,
                rights  => (Capabilities.RIGHT_GRANT => True, others => False),
                authorityTag => Capabilities.NO_AUTHORITY_TAG,
                object  => (ref => ref, param => 0),
                gen     => gen), words (3));
            if not (for all W of words => ABI.Is_Handle_Word (W)) then
                -- The spawner's table is full: undo, and discard the child.
                for W of words loop
                    if ABI.Is_Handle_Word (W) then
                        declare
                            ignoreCap  : Capabilities.Capability;
                            ignoreWord : Unsigned_64;
                        begin
                            Process_Capabilities.Close
                              (Process.proctab(callerPID).caps, W, ignoreCap, ignoreWord);
                        end;
                    else
                        refuse := ABI.To_Refusal (W);
                    end if;
                end loop;
                Spinlocks.exitCriticalSection (Process.mailtab(callerPID).lock);
                -- Published and suspended, never run: stop it (the reaper
                -- releases it).
                if not Process.killProcess (newPID, Process.generationOf (newPID)) then
                    println ("SPAWN: rollback stop refused");
                end if;
                refused (refuse, "SPAWN: no room for the spawner's handles");
                return;
            end if;
            Spinlocks.exitCriticalSection (Process.mailtab(callerPID).lock);
            result :=
              [ABI.Child              => Process_Identities.To_Word (Process.identityOf (newPID)),
               ABI.Process            => words (1),
               ABI.Endpoint           => words (2),
               ABI.Cspace             => words (3),
               ABI.Child_Self         => Process.proctab(newPID).selfHandle,
               ABI.Child_Self_Process => Process.proctab(newPID).selfProcessHandle];
        end;

        Boot_Timeline.Note_Spawn
          (Slot     => Natural (newPID),
           Identity => result (ABI.Child),
           Spawner  => Process_Identities.To_Word (Process.identityOf (callerPID)),
           Spawner_Slot => Natural (callerPID),
           Name     => name,
           Started  => started);
        Process.User_Memory.Copy_To_User
          (callerPID, arg2, result'Address, ABI.Spawn_Result_Bytes, copied);
        -- Always spawn suspended; the spawner installs and resumes.
        print ("SPAWN: created suspended PID ");
        println (Integer (newPID));
        retval := (if copied then 0 else ABI.Refusal_Word (ABI.Invalid_Argument));
    end handleSpawn;

    ---------------------------------------------------------------------------
    -- handleMapDevice
    -- Extracted to its own procedure to keep mapPage instantiation off
    -- syscallHandler's stack frame.
    ---------------------------------------------------------------------------
    procedure handleMapDevice (callerPID : Process.ProcessID;
                               arg0      : Unsigned_64;
                               arg1      : Unsigned_64;
                               arg2      : Unsigned_64;
                               arg3      : Unsigned_64;
                               arg4      : Unsigned_64;
                               retval    : out Unsigned_64) with
        SPARK_Mode => Off   -- generic instantiation
    is

        procedure mapPageInst is new Virtmem.mapPage
            (BuddyAllocator.allocFrame);

        physAddr : constant Virtmem.PhysAddress :=
            Virtmem.PhysAddress (arg0);
        virtAddr : constant Integer_Address := Integer_Address (arg1);
        numPages : constant Unsigned_64 := arg2;
        capAllowed : Boolean;
        ok : Boolean := True;
        -- arg4 names the CAP_DEVICE_MEM this mapping relies on.
        authority : constant Capabilities.Capability := deviceMemoryOf (callerPID, arg4);
    begin
        retval := reterr;

        -- arg3: 0 = read/write, 1 = read-only. Never silently widen a
        -- requested inspection mapping, including when a broader cap exists.
        if numPages = 0 or else numPages > 1024 or else arg3 > 1 or else
          arg0 mod 4096 /= 0 or else arg1 = 0 or else arg1 mod 4096 /= 0 or else
          arg0 > Unsigned_64'Last - numPages * 4096 or else
          arg1 >= 16#0000_8000_0000_0000# or else
          numPages * 4096 > 16#0000_8000_0000_0000# - arg1
        then
            Device_Memory_Diagnostics.Record_Denial
              (callerPID, Device_Memory_Publication.Map_Device, arg0,
               (if numPages <= 1024 then numPages * Unsigned_64 (Virtmem.PAGE_SIZE) else 0),
               (if arg3 = 1 then Device_Memory_Admission.Read_Only
                else Device_Memory_Admission.Read_Write),
               (Why => Device_Memory_Publication.Invalid_Range, others => <>));
            return;
        end if;
        if Memory_Grants.Overlaps_Received_Region
          (arg1, Memory_Grants.Page_Count (numPages))
          or else Owned_Memory_Layout.Conflicts
            (arg1, numPages * Unsigned_64 (Virtmem.PAGE_SIZE))
        then
            return; -- only the grant subsystem may mutate its aperture
        end if;

        -- Device-mappable pages only (CAP-001): RAM, kernel memory, boot
        -- modules, ACPI memory and kernel MMIO are refused whatever
        -- CAP_DEVICE_MEM the caller holds.
        capAllowed := Device_Memory_Authority.Authorize
          (authority, Physical_Range_Map.Kernel_Map,
           arg0, numPages * Unsigned_64 (Virtmem.PAGE_SIZE),
           (if arg3 = 0 then Device_Memory_Admission.Read_Write
            else Device_Memory_Admission.Read_Only));

        if not capAllowed then
            Device_Memory_Diagnostics.Record_Denial
              (callerPID, Device_Memory_Publication.Map_Device, arg0,
               numPages * Unsigned_64 (Virtmem.PAGE_SIZE),
               (if arg3 = 0 then Device_Memory_Admission.Read_Write
                else Device_Memory_Admission.Read_Only),
               Device_Memory_Authority.Explain
                 (authority, Physical_Range_Map.Kernel_Map,
                  arg0, numPages * Unsigned_64 (Virtmem.PAGE_SIZE),
                  (if arg3 = 0 then Device_Memory_Admission.Read_Write
                   else Device_Memory_Admission.Read_Only)));
            return;
        end if;

        Process.Owned_Memory.Lock;
        if Process.Owned_Memory.Physical_Conflict
          (arg0, numPages * Unsigned_64 (Virtmem.PAGE_SIZE)) then
            Process.Owned_Memory.Unlock;
            return;
        end if;
        Process.lockAddressSpace (callerPID);
        for i in 0 .. numPages - 1 loop
            declare
                pageOk : Boolean;
            begin
                mapPageInst (
                    phys    => physAddr +
                        Virtmem.PhysAddress (
                            i * Unsigned_64 (Virtmem.PAGE_SIZE)),
                    virt    => virtAddr +
                        Integer_Address (
                            i * Unsigned_64 (Virtmem.PAGE_SIZE)),
                    flags   => (if arg3 = 1 then
                                  Virtmem.PG_USERIO and not Virtmem.PG_WRITABLE
                                else Virtmem.PG_USERIO),
                    myP4    => Process.addrtab(callerPID),
                    success => pageOk);
                if not pageOk then
                    ok := False;
                end if;
            end;
        end loop;
        Process.unlockAddressSpace (callerPID);
        Process.Owned_Memory.Unlock;

        if ok then
            retval := 0;
        end if;
    end handleMapDevice;

    ---------------------------------------------------------------------------
    -- handleAllocDma
    -- Extracted to its own procedure to avoid inflating syscallHandler's
    -- stack frame with mapPage generic instantiation + locals.
    ---------------------------------------------------------------------------
    procedure allocateDma (callerPID : Process.ProcessID;
                              authority : Unsigned_64;
                              arg0      : Unsigned_64;
                              arg1      : Unsigned_64;
                              arg2      : Unsigned_64;
                              retain    : Boolean;
                              largePage : Boolean;
                              ceiling   : Unsigned_64;
                              retval    : out Unsigned_64) with
        SPARK_Mode => Off   -- generic instantiation
    is
        use type Capabilities.CapabilityType;
        use type Process.ProcessState;


        targetPID : Process.ProcessID;
        hasCap    : Boolean := False;
        order     : BuddyAllocator.Order;
        dmaAddr   : System.Address;
        dmaPhys   : Virtmem.PhysAddress;
        virtBase  : Virtmem.VirtAddress;
        ok        : Boolean;
        claimedPages : Natural := 0;
        mappedPages  : Natural := 0;
        tracking     : Process.DMA.Ticket;
        metadataOK   : Boolean;
        charge : Memory_Accounting.Identity := 0;
        chargedPages : Unsigned_64 := 0;
        boundPages : Unsigned_64 := 0;
        chargeOK : Boolean;

        procedure Cancel_Unbound_Charge is
            cancelled : Boolean;
        begin
            if chargedPages > boundPages then
                Memory_Accounting.Cancel_Unbound
                  (charge, chargedPages - boundPages, cancelled);
                if not cancelled then
                    println ("FATAL: DMA unbound charge rollback failed");
                    x86.panic;
                end if;
            end if;
        end Cancel_Unbound_Charge;

        procedure mapPage is new Virtmem.mapPage
            (BuddyAllocator.allocFrame);
        procedure mapBigPage is new Virtmem.mapBigPage
            (BuddyAllocator.allocFrame);
    begin
        retval := reterr;

        if arg0 > Unsigned_64 (Process.ProcessID'Last) or arg0 = 0 then
            return;
        elsif arg1 > Unsigned_64 (BuddyAllocator.Order'Last) then
            println ("ALLOC_DMA: order too large");
            return;
        end if;

        targetPID := Process.ProcessID (arg0);
        order := BuddyAllocator.Order (arg1);
        virtBase := Virtmem.VirtAddress (arg2);
        -- Explicit driver-private 2 MiB mode. No implicit promotion of memory
        -- whose caller expects subpage grants. Keep the entire range canonical.
        if arg2 mod Unsigned_64 (Virtmem.PAGE_SIZE) /= 0 or else
          arg2 >= 16#0000_8000_0000_0000# or else
          (2 ** Natural (order)) * Unsigned_64 (Virtmem.PAGE_SIZE) >
            16#0000_8000_0000_0000# - arg2 or else
          (largePage and then
            (arg1 /= 9 or else arg2 mod 16#20_0000# /= 0))
        then
            return;
        end if;
        if Memory_Grants.Overlaps_Received_Bytes
          (arg2, (2 ** Natural (order)) * Unsigned_64 (Virtmem.PAGE_SIZE))
          or else Owned_Memory_Layout.Conflicts
            (arg2, (2 ** Natural (order)) * Unsigned_64 (Virtmem.PAGE_SIZE))
        then
            return;
        end if;

        -- The caller names a CAP_PROCESS with RIGHT_GRANT for the target
        -- (or the wildcard). Its mailbox lock is held (handleAllocDma).
        hasCap := processAuthorityHeld (callerPID, authority, targetPID);

        if not hasCap then
            println ("ALLOC_DMA: denied, no RIGHT_GRANT");
            return;
        elsif Process.threadOf (targetPID).state = Process.INVALID then
            println ("ALLOC_DMA: target not valid");
            return;
        end if;

        -- The ordered mailbox locks keep this account's incarnation alive.
        -- Both retained and ordinary DMA use the same quota as ordinary pages.
        chargedPages := 2 ** Natural (order);
        Memory_Accounting.Reserve (Process.proctab(targetPID).memoryAccount,
          (if retain then Process_Memory_Budget.Retained_DMA_Backing
           else Process_Memory_Budget.DMA_Backing),
          chargedPages, charge, chargeOK);
        if not chargeOK then
            println ("ALLOC_DMA: owner memory quota denied");
            return;
        end if;
        Process.DMA.Reserve (targetPID, Natural (order), retain, tracking, metadataOK);
        if not metadataOK then
            Cancel_Unbound_Charge;
            println ("ALLOC_DMA: tracking metadata unavailable");
            return;
        end if;
        if ceiling = 0 then
            BuddyAllocator.alloc (order, dmaAddr);
        else
            BuddyAllocator.allocBelow (order, ceiling, dmaAddr);
        end if;

        if System."=" (dmaAddr, BuddyAllocator.NO_BLOCK_AVAILABLE) then
            Cancel_Unbound_Charge;
            Process.DMA.Cancel (tracking);
            println ("ALLOC_DMA: alloc failed");
            return;
        end if;

        dmaPhys := Virtmem.V2P (dmaAddr);
        declare
            numPages : constant Natural := 2 ** Natural (order);
            mappingStep : constant Natural := (if largePage then 512 else 1);
            numMappings : constant Natural := numPages / mappingStep;
        begin
            -- DMA is still userspace-owned memory.  Tag every frame to the
            -- driver so it may grant a deliberately selected subrange while
            -- raw MMIO, which has no owner tag, remains ungrantable.
            for i in 0 .. numPages - 1 loop
                BuddyAllocator.claimUserFrame
                  (dmaPhys + Virtmem.PhysAddress (i * Virtmem.PAGE_SIZE),
                   BuddyAllocator.Frame_Owner (targetPID), ok);
                if not ok then
                    if claimedPages > 0 then
                        for page in 0 .. claimedPages - 1 loop
                            BuddyAllocator.releaseUserFrame
                              (dmaPhys + Virtmem.PhysAddress
                                 (page * Virtmem.PAGE_SIZE),
                               BuddyAllocator.Frame_Owner (targetPID));
                        end loop;
                    end if;
                    BuddyAllocator.free (order, dmaAddr);
                    Cancel_Unbound_Charge;
                    Process.DMA.Cancel (tracking);
                    println ("ALLOC_DMA: ownership tagging failed");
                    return;
                end if;
                claimedPages := claimedPages + 1;
            end loop;

            -- Bind before any user mapping is published. A partial bind is
            -- refunded by actual block free; cancel only the unbound suffix.
            for i in 0 .. numPages - 1 loop
                BuddyAllocator.bindFrameCharge
                  (dmaPhys + Virtmem.PhysAddress (i * Virtmem.PAGE_SIZE),
                   charge, chargeOK);
                if not chargeOK then
                    for page in 0 .. numPages - 1 loop
                        BuddyAllocator.releaseUserFrame
                          (dmaPhys + Virtmem.PhysAddress (page * Virtmem.PAGE_SIZE),
                           BuddyAllocator.Frame_Owner (targetPID));
                    end loop;
                    BuddyAllocator.free (order, dmaAddr);
                    Cancel_Unbound_Charge;
                    Process.DMA.Cancel (tracking);
                    println ("ALLOC_DMA: charge binding failed");
                    return;
                end if;
                boundPages := boundPages + 1;
            end loop;

            Process.lockAddressSpace (targetPID);
            for i in 0 .. numMappings - 1 loop
                -- mapPage permits replacement for privileged callers. DMA
                -- allocation must not replace an existing target mapping.
                ok := Virtmem.tableWalk
                  (virtBase + Virtmem.VirtAddress (i * Virtmem.PAGE_SIZE),
                   Process.addrtab (targetPID), Allow_Big => True) = 0;
                if ok then
                  if largePage then
                    mapBigPage
                      (dmaPhys, virtBase, Virtmem.PG_USERDATA,
                       Process.addrtab (targetPID), ok);
                  else
                    mapPage (
                    phys    => dmaPhys +
                        Virtmem.PhysAddress (i * Virtmem.PAGE_SIZE),
                    virt    => virtBase +
                        Virtmem.VirtAddress (i * Virtmem.PAGE_SIZE),
                    flags   => Virtmem.PG_USERDATA,
                    myP4    => Process.addrtab (targetPID),
                    success => ok);
                  end if;
                end if;

                if not ok then
                    print ("ALLOC_DMA: map fail pg ");
                    println (i);
                    if mappedPages > 0 then
                        for page in 0 .. mappedPages - 1 loop
                            Virtmem.unmapPage
                              (virt => virtBase + Virtmem.VirtAddress
                               (page * mappingStep * Virtmem.PAGE_SIZE),
                               myP4 => Process.addrtab (targetPID),
                               success => ok);
                            if not ok then
                                println ("FATAL: DMA rollback unmap failed; backing retained");
                                x86.panic;
                            end if;
                        end loop;
                        -- A running target may have cached a partial mapping.
                        -- Retire translations before returning backing to buddy.
                        TLB_Shootdown.Invalidate_All;
                    end if;
                    for page in 0 .. numPages - 1 loop
                        BuddyAllocator.releaseUserFrame
                          (dmaPhys + Virtmem.PhysAddress
                             (page * Virtmem.PAGE_SIZE),
                           BuddyAllocator.Frame_Owner (targetPID));
                    end loop;
                    BuddyAllocator.free (order, dmaAddr);
                    Process.unlockAddressSpace (targetPID);
                    Process.DMA.Cancel (tracking);
                    return;
                end if;
                mappedPages := mappedPages + 1;
            end loop;

            -- Publication cannot allocate metadata: reservation preceded DMA.
            Process.DMA.Commit (tracking, Unsigned_64 (dmaPhys));
            Process.unlockAddressSpace (targetPID);

            retval := Unsigned_64 (dmaPhys);
        end;
    end allocateDma;

    procedure allocateDmaRequest
      (callerPID : Process.ProcessID;
       authority : Unsigned_64;
       arg0, arg1, arg2, arg3, arg4 : Unsigned_64;
       retval : out Unsigned_64) with SPARK_Mode => Off
    is
    begin
        retval := reterr;
        if arg3 = 0 then
            allocateDma (callerPID, authority, arg0, arg1, arg2, False, False, arg4, retval);
            return;
        elsif (arg3 /= 1 and then arg3 /= 3) or else arg1 > 14 or else
          (arg3 = 3 and then arg1 /= 9)
        then
            return;
        end if;
        -- The common ledger reserves global and owner budgets together after
        -- authorization. Bound charges refund only at actual physical free.
        allocateDma (callerPID, authority, arg0, arg1, arg2, True, arg3 = 3, arg4, retval);
    end allocateDmaRequest;

    procedure handleAllocDma
      (callerPID : Process.ProcessID;
       arg0, arg1, arg2, arg3, arg4, arg5 : Unsigned_64;
       retval : out Unsigned_64) with SPARK_Mode => Off
    is
        use type Process.ProcessID;
        use type Capabilities.Generation;
        resolvedTarget : Process.ProcessID;
        resolvedGeneration : Capabilities.Generation;
    begin
        retval := reterr;
        if callerPID = Process.NO_PROCESS then return; end if;
        Process.resolveIdentity (Process_Identities.From_Word (arg0), resolvedTarget, resolvedGeneration);
        if resolvedTarget = Process.NO_PROCESS then return; end if;
        declare
            Target : constant Process.ProcessID := resolvedTarget;
            First : constant Process.ProcessID := Process.ProcessID'Min (callerPID, Target);
            Last : constant Process.ProcessID := Process.ProcessID'Max (callerPID, Target);
        begin
            -- Same ordered mailbox locks used by cross-process publication.
            -- Pin both the authority table and destination incarnation until
            -- physical allocation, mapping and tracking publication complete.
            Spinlocks.enterCriticalSection (Process.mailtab(First).lock);
            if Last /= First then
                Spinlocks.enterCriticalSection (Process.mailtab(Last).lock);
            end if;
            if Process.proctab(callerPID).admitted and then
              Process.proctab(Target).admitted and then
              Process.generationOf (Target) = resolvedGeneration and then
              not Process_Lifetime.Closing (Process.threadOf (callerPID).lifetime) and then
              not Process_Lifetime.Closing (Process.threadOf (Target).lifetime)
            then
                -- Below this point the target is the resolved slot.
                allocateDmaRequest
                  (callerPID, arg5, Unsigned_64 (Target), arg1, arg2, arg3, arg4, retval);
            else
                println ("ALLOC_DMA: process closing or unavailable");
            end if;
            if Last /= First then
                Spinlocks.exitCriticalSection (Process.mailtab(Last).lock);
            end if;
            Spinlocks.exitCriticalSection (Process.mailtab(First).lock);
        end;
    end handleAllocDma;

    ---------------------------------------------------------------------------
    -- handleMapInto
    -- Extracted to its own procedure to avoid inflating syscallHandler's
    -- stack frame with mapPage generic instantiation + locals.
    ---------------------------------------------------------------------------
    procedure handleMapInto (callerPID : Process.ProcessID;
                             arg0      : Unsigned_64;
                             arg1      : Unsigned_64;
                             arg2      : Unsigned_64;
                             arg3      : Unsigned_64;
                             arg4      : Unsigned_64;
                             arg5      : Unsigned_64;
                             arg6      : Unsigned_64;
                             retval    : out Unsigned_64) with
        SPARK_Mode => Off   -- generic instantiation
    is
        use type Capabilities.CapabilityType;
        use type Process.ProcessState;


        use type Capabilities.Generation;
        targetPID : Process.ProcessID;
        targetGeneration : Capabilities.Generation;
        hasCap    : Boolean := False;
        ok        : Boolean;
        pgFlags   : Unsigned_64;
        -- The first physical page: arg1 itself for device pages, the
        -- resolved boot-image offset for the boot image.
        physFirst : Unsigned_64 := arg1;

        procedure mapPage is new Virtmem.mapPage
            (BuddyAllocator.allocFrame);

        -- Every refusal of what may be shared is a device-memory denial
        -- record (Device_Memory_Diagnostics), with the request as given.
        procedure Refuse_Into (Why : Device_Memory_Authority.Explanation) is
        begin
            Device_Memory_Diagnostics.Record_Denial
              (callerPID, Device_Memory_Publication.Map_Into, arg1,
               (if arg3 <= MAX_MAP_INTO_PAGES_PER_CALL
                then arg3 * Unsigned_64 (Virtmem.PAGE_SIZE) else 0),
               (if arg4 = MAP_INTO_BOOT_MODULE then Device_Memory_Admission.Read_Only
                else Device_Memory_Admission.Read_Write),
               Why);
        end Refuse_Into;
    begin
        retval := reterr;

        Process.resolveIdentity (Process_Identities.From_Word (arg0), targetPID, targetGeneration);
        if targetPID = Process.NO_PROCESS then
            return;
        elsif arg3 = 0 or else arg3 > MAX_MAP_INTO_PAGES_PER_CALL or else
          arg1 mod Unsigned_64 (Virtmem.PAGE_SIZE) /= 0 or else
          arg2 = 0 or else arg2 mod Unsigned_64 (Virtmem.PAGE_SIZE) /= 0 or else
          arg2 >= USER_ADDRESS_LIMIT or else
          arg3 * Unsigned_64 (Virtmem.PAGE_SIZE) > USER_ADDRESS_LIMIT - arg2
        then
            Refuse_Into ((Why => Device_Memory_Publication.Invalid_Range, others => <>));
            return;
        end if;

        -- What may be shared (CAP-001). Read-only: pages of the boot image
        -- (the initrd), named by offset, the only RAM this call maps. I/O:
        -- device pages the CALLER holds CAP_DEVICE_MEM for. RAM, kernel
        -- memory and other processes' memory are never mapped, whatever the
        -- grant.
        case arg4 is
            when MAP_INTO_BOOT_MODULE =>
                declare
                    valid : Boolean;
                begin
                    Modules.Initrd_Pages
                      (arg1, arg3 * Unsigned_64 (Virtmem.PAGE_SIZE), physFirst, valid);
                    if not valid or else not Physical_Range_Map.Within_Boot_Module
                      (physFirst, arg3 * Unsigned_64 (Virtmem.PAGE_SIZE))
                    then
                        Refuse_Into ((Why => Device_Memory_Publication.Not_Boot_Module,
                                      others => <>));
                        return;
                    end if;
                end;
                pgFlags := Virtmem.PG_USERDATARO;
            when MAP_INTO_DEVICE =>
                if not Device_Memory_Authority.Authorize
                  (deviceMemoryOf (callerPID, arg6), Physical_Range_Map.Kernel_Map,
                   arg1, arg3 * Unsigned_64 (Virtmem.PAGE_SIZE),
                   Device_Memory_Admission.Read_Write)
                then
                    Refuse_Into (Device_Memory_Authority.Explain
                      (deviceMemoryOf (callerPID, arg6), Physical_Range_Map.Kernel_Map,
                       arg1, arg3 * Unsigned_64 (Virtmem.PAGE_SIZE),
                       Device_Memory_Admission.Read_Write));
                    return;
                end if;
                pgFlags := Virtmem.PG_USERIO;
            when others =>
                Refuse_Into ((Why => Device_Memory_Publication.Invalid_Range, others => <>));
                return;
        end case;
        if Memory_Grants.Overlaps_Received_Region
          (arg2, Memory_Grants.Page_Count (arg3))
          or else Owned_Memory_Layout.Conflicts
            (arg2, arg3 * Unsigned_64 (Virtmem.PAGE_SIZE))
        then
            return;
        end if;

        -- arg5 names a CAP_PROCESS with RIGHT_GRANT for the target (or the
        -- wildcard), read under the caller's own mailbox lock; the target's
        -- generation it was checked against is rechecked under the
        -- target's lock below.
        declare
            authority : constant Process_Capabilities.Found :=
              findOwn (callerPID, arg5, Capabilities.CAP_PROCESS,
                       onlyRight (Capabilities.RIGHT_GRANT));
        begin
            hasCap := authority.OK and then
              (authority.Cap.object.ref = 0 or else
               (authority.Cap.object.ref = Unsigned_64 (targetPID) and then
                authority.Cap.gen = targetGeneration));
        end;

        declare
            procedure performLocked is
            begin
                if not Process.proctab(targetPID).admitted or else
                   Process.generationOf (targetPID) /= targetGeneration or else
                   Process_Lifetime.Closing (Process.threadOf (targetPID).lifetime) then
                    retval := reterr;
                    return;
                end if;


                if not hasCap then
                    println ("MAP_INTO: denied, no RIGHT_GRANT");
                    return;
                elsif Process.threadOf (targetPID).state = Process.INVALID then
                    println ("MAP_INTO: target not valid");
                    return;
                end if;

                ok := True;
                Process.Owned_Memory.Lock;
                if Process.Owned_Memory.Physical_Conflict
                  (physFirst, arg3 * Unsigned_64 (Virtmem.PAGE_SIZE)) then
                    Process.Owned_Memory.Unlock;
                    return;
                end if;
                Process.lockAddressSpace (targetPID);
                for i in 0 .. Natural (arg3) - 1 loop
                    mapPage (
                        phys    => Virtmem.PhysAddress (physFirst) +
                            Virtmem.PhysAddress (i * Virtmem.PAGE_SIZE),
                        virt    => Virtmem.VirtAddress (arg2) +
                            Virtmem.VirtAddress (i * Virtmem.PAGE_SIZE),
                        flags   => pgFlags,
                        myP4    => Process.addrtab (targetPID),
                        success => ok);

                    if not ok then
                        Process.unlockAddressSpace (targetPID);
                        Process.Owned_Memory.Unlock;
                        print ("MAP_INTO: map fail page ");
                        println (i);
                        return;
                    end if;
                end loop;
                Process.unlockAddressSpace (targetPID);
                Process.Owned_Memory.Unlock;

                retval := 0;
            end performLocked;
        begin
            Spinlocks.enterCriticalSection (Process.mailtab(targetPID).lock);
            performLocked;
            Spinlocks.exitCriticalSection (Process.mailtab(targetPID).lock);
        end;
    end handleMapInto;

    ---------------------------------------------------------------------------
    -- handleSbrk
    ---------------------------------------------------------------------------
    procedure handleSbrk (callerPID : Process.ProcessID;
                          arg0      : Unsigned_64;
                          retval    : out Unsigned_64) with
        SPARK_Mode => Off
    is
        use type Heap_Admission.Status;
        use type Process.Page_Allocation_Result;
        plan : constant Heap_Admission.Growth_Plan := Heap_Admission.Plan
          (Heap_Start => Unsigned_64 (To_Integer (Process.proctab(callerPID).heapStart)),
           Current_Break => Unsigned_64 (To_Integer (Process.proctab(callerPID).heapEnd)),
           Exclusive_Limit => Unsigned_64
             (Integer_Address'Min
                (Process.GRANT_REGION_BASE,
                 To_Integer (Process.proctab(callerPID).stackBottom)) and Virtmem.PAGE_MASK),
           Increment => arg0,
           Used_Frames => Unsigned_64 (Process.proctab(callerPID).frames.length),
           Tracking_Capacity => Unsigned_64 (Natural'Last),
           Quota => Unsigned_64 (Process.proctab(callerPID).quota.maxFrames));
        frames : Process.FrameLists.List renames Process.proctab(callerPID).frames;
        oldCapacity : constant Natural := frames.capacity;
        newCapacity : Natural;
        success : Boolean;

        procedure Add (Index : Natural; Success : out Boolean) is
            storage : System.Address;
            allocation : Process.Page_Allocation_Result;
        begin
            Process.tryAddPage
              (proc => Process.proctab(callerPID),
               mapTo => To_Address (Integer_Address (plan.First_Page) +
                                    Integer_Address (Index) * Virtmem.PAGE_SIZE),
               storage => storage, result => allocation, flags => Virtmem.PG_USERDATA);
            Success := allocation = Process.Page_Added;
        end Add;
        procedure Unmap (Index : Natural) is
            removed : Boolean;
        begin
            Virtmem.unmapPage
              (Integer_Address (plan.First_Page) + Integer_Address (Index) * Virtmem.PAGE_SIZE,
               Process.addrtab(Process.proctab(callerPID).pgTable), removed);
            if not removed then
                raise Process.ProcessException with "Heap rollback lost a new mapping";
            end if;
        end Unmap;
        procedure Release_Latest is
            frame : constant Virtmem.PhysAddress := Process.FrameLists.front (frames);
        begin
            Process.FrameLists.popFront (frames);
            BuddyAllocator.freeFrame (frame);
        end Release_Latest;
        procedure Grow is new Heap_Growth.Apply
          (Add, Unmap, TLB_Shootdown.Invalidate_All, Release_Latest);
    begin
        retval := reterr;
        if plan.Result /= Heap_Admission.Ready then return; end if;

        newCapacity := Heap_Admission.Expanded_Capacity
          (oldCapacity, Natural (plan.Page_Count));
        if newCapacity = 0 then return; end if;

        --  One executing member per user address space is enforced by create;
        --  its execution pin prevents reaping while this syscall runs. Shared
        --  address spaces will require reservation/serialization before they
        --  can be enabled. No new global allocator lock is introduced here.
        -- List nodes are supplied on demand, not preallocated by capacity.
        -- Representation and policy quota were checked for the whole request;
        -- physical/node/page-table failures are rolled back before returning.
        -- Heap growth must not consume capacity reserved for future stack
        -- faults. The list is node-allocated, so headroom costs no pages here.
        frames.capacity := newCapacity;
        Grow (Natural (plan.Page_Count), success);
        if not success then
            frames.capacity := oldCapacity;
            return;
        end if;
        Process.proctab(callerPID).heapEnd :=
          To_Address (Integer_Address (plan.New_Break));
        retval := plan.Old_Break;
    end handleSbrk;

    ---------------------------------------------------------------------------
    -- handleMapFB
    ---------------------------------------------------------------------------
    procedure handleMapFB (callerPID : Process.ProcessID;
                           arg0      : Unsigned_64;
                           retval    : out Unsigned_64) with
        SPARK_Mode => Off
    is

        procedure mapPageInst is new Virtmem.mapPage
            (BuddyAllocator.allocFrame);

        FB_USER_BASE : constant Integer_Address :=
            16#0000_6000_0000_0000#;

        Item : Boot_Framebuffer.Description;
        numPages : Storage_Count;
        fbPhys : Virtmem.PhysAddress;

        ok : Boolean := True;
        capAllowed : Boolean;
        -- arg0 names the CAP_DEVICE_MEM covering the framebuffer.
        authority : constant Capabilities.Capability := deviceMemoryOf (callerPID, arg0);
    begin
        retval := reterr;
        if not Multiboot.Has_Graphics then
            return;
        end if;
        Item := Multiboot.Framebuffer;
        fbPhys := Virtmem.PhysAddress (Item.Map_First);
        numPages := (Storage_Count (Item.Map_Limit) - Storage_Count (Item.Map_First)) /
          Virtmem.PAGE_SIZE;
        -- The capability must cover the framebuffer's real pages, which
        -- must be device-mappable (CAP-001).
        capAllowed := Device_Memory_Authority.Authorize
          (authority, Physical_Range_Map.Kernel_Map,
           Unsigned_64 (Item.Map_First),
           Unsigned_64 (Item.Map_Limit) - Unsigned_64 (Item.Map_First),
           Device_Memory_Admission.Read_Write);

        if not capAllowed then
            Device_Memory_Diagnostics.Record_Denial
              (callerPID, Device_Memory_Publication.Map_Framebuffer,
               Unsigned_64 (Item.Map_First),
               Unsigned_64 (Item.Map_Limit) - Unsigned_64 (Item.Map_First),
               Device_Memory_Admission.Read_Write,
               Device_Memory_Authority.Explain
                 (authority, Physical_Range_Map.Kernel_Map,
                  Unsigned_64 (Item.Map_First),
                  Unsigned_64 (Item.Map_Limit) - Unsigned_64 (Item.Map_First),
                  Device_Memory_Admission.Read_Write));
            retval := reterr;
        else
            --  Close and drain kernel rendering BEFORE any usable mapping is
            --  published, including a partially successful mapping attempt.
            Boot_Output.Retire;
            TextIO.disableVideo;
            Process.Owned_Memory.Lock;
            if Process.Owned_Memory.Physical_Conflict
              (Unsigned_64 (fbPhys), Unsigned_64 (numPages) * Virtmem.PAGE_SIZE) then
                Process.Owned_Memory.Unlock;
                return;
            end if;
            Process.lockAddressSpace (callerPID);
            for i in 0 .. numPages - 1 loop
                declare
                    pageOk : Boolean;
                begin
                    mapPageInst (
                        phys    => fbPhys +
                            Virtmem.PhysAddress(i * Virtmem.PAGE_SIZE),
                        virt    => FB_USER_BASE +
                            Integer_Address(i * Virtmem.PAGE_SIZE),
                        flags   => Virtmem.PG_USERIO_WC,
                        myP4    => Process.addrtab(callerPID),
                        success => pageOk);
                    if not pageOk then
                        ok := False;
                    end if;
                end;
            end loop;
            Process.unlockAddressSpace (callerPID);
            Process.Owned_Memory.Unlock;

            if ok then
                retval := Unsigned_64(FB_USER_BASE) +
                  Unsigned_64 (Item.Base) - Unsigned_64 (Item.Map_First);
            else
                retval := reterr;
            end if;
        end if;
    end handleMapFB;

    ---------------------------------------------------------------------------
    -- handleReceive
    ---------------------------------------------------------------------------
    procedure handleReceive (arg0   : Unsigned_64;
                             retval : out Unsigned_64) with
        SPARK_Mode => Off
    is
        caller  : constant Process.ProcessID := PerCPUData.getCurrentPID;
        from    : Process.ProcessID;
        recvMsg : Process.Message;
        ok      : Boolean;
    begin
        -- arg0 is a user pointer: checked, never dereferenced (a message
        -- is taken only when it can be delivered).
        if not Process.User_Memory.Message_Writable (caller, arg0) then
            retval := reterr;
            return;
        end if;
        Process.IPC.receive (from, recvMsg);
        Process.User_Memory.Write_Message (caller, arg0, recvMsg, ok);
        retval := (if ok then Process_Identities.To_Word (Process.identityOf (from)) else reterr);
    end handleReceive;

    ---------------------------------------------------------------------------
    -- handleReceiveUntil
    -- arg0 points to the userspace Message, arg1 is an absolute monotonic-ms
    -- deadline. Unsigned_64'Last is returned on timeout; otherwise the
    -- sender's identity, zero for an unsolicited event.
    ---------------------------------------------------------------------------
    procedure handleReceiveUntil (arg0, arg1 : Unsigned_64;
                                  retval     : out Unsigned_64) with
        SPARK_Mode => Off
    is
        caller   : constant Process.ProcessID := PerCPUData.getCurrentPID;
        from     : Process.ProcessID;
        recvMsg  : Process.Message;
        received : Boolean;
        ok       : Boolean;
    begin
        if not Process.User_Memory.Message_Writable (caller, arg0) then
            retval := reterr;
            return;
        end if;
        Process.IPC.receiveUntil (arg1, from, recvMsg, received);
        Process.User_Memory.Write_Message (caller, arg0, recvMsg, ok);
        retval := (if not received or else not ok then Unsigned_64'Last
                   else Process_Identities.To_Word (Process.identityOf (from)));
    end handleReceiveUntil;

    ---------------------------------------------------------------------------
    -- handleReply
    ---------------------------------------------------------------------------
    procedure handleReply (arg0, arg1, arg2, arg3,
                           arg4, arg5 : Unsigned_64;
                           retval     : out Unsigned_64) with
        SPARK_Mode => Off
    is



        replyMsg : constant Process.Message := (
            tag      => u64ToTag (arg1),
            authorityTag => 0,
            words    => (arg2, arg3, arg4, arg5));
    begin
        -- A stale identity names no one: NO_PROCESS, which reply refuses.
        retval := Process.IPC.reply (
            replyTo => Process.processOfIdentity (Process_Identities.From_Word (arg0)),
            msg     => replyMsg);
    end handleReply;

    ---------------------------------------------------------------------------
    -- handleReplyCap
    ---------------------------------------------------------------------------
    procedure handleReplyCap (arg0, arg1, arg2, arg3,
                              arg4, arg5 : Unsigned_64;
                              retval     : out Unsigned_64) with
        SPARK_Mode => Off
    is
        replyMsg : constant Process.Message := (
            tag      => u64ToTag (arg1),
            authorityTag => 0,
            words    => (arg2, arg3, arg4, arg5));
    begin
        -- arg0: a saved reply's handle (MOVE_REPLY_CAPABILITY). The current
        -- reply is answered with REPLY or REPLY_WAIT, by the caller's identity.
        retval := Process.IPC.replyCap (handle => arg0, msg => replyMsg);
    end handleReplyCap;

    ---------------------------------------------------------------------------
    -- handleReplyWithCap (REPLY_WITH_CAPABILITY, CAP-002): arg0 = the
    -- requester's identity, arg1 = address of the reply message, arg2 =
    -- address of a Capability_ABI.Derivation. See Process.IPC.
    ---------------------------------------------------------------------------
    procedure handleReplyWithCap (callerPID : Process.ProcessID;
                                  arg0, arg1, arg2 : Unsigned_64;
                                  retval : out Unsigned_64) with
        SPARK_Mode => Off
    is
        replyMsg   : Process.Message;
        derivation : aliased Capability_ABI.Derivation := [others => 0];
        ok         : Boolean;
    begin
        retval := Capability_ABI.Refusal_Word (Capability_ABI.Invalid_Argument);
        Process.User_Memory.Read_Message (callerPID, arg1, replyMsg, ok);
        if not ok then
            return;
        end if;
        Process.User_Memory.Copy
          (callerPID, arg2, derivation'Address, Capability_ABI.Derivation_Bytes, ok);
        if not ok then
            return;
        end if;
        -- The kernel stamps the reply's authority tag (the new handle).
        replyMsg.authorityTag := 0;
        retval := Process.IPC.replyWithCapability
          (replyTo    => Process.processOfIdentity (Process_Identities.From_Word (arg0)),
           msg        => replyMsg,
           derivation => derivation);
    end handleReplyWithCap;

    procedure handleReplySavedWithCap (callerPID : Process.ProcessID;
                                       arg0, arg1, arg2 : Unsigned_64;
                                       retval : out Unsigned_64) with
        SPARK_Mode => Off
    is
        replyMsg   : Process.Message;
        derivation : aliased Capability_ABI.Derivation := [others => 0];
        ok         : Boolean;
    begin
        retval := Capability_ABI.Refusal_Word (Capability_ABI.Invalid_Argument);
        Process.User_Memory.Read_Message (callerPID, arg1, replyMsg, ok);
        if not ok then
            return;
        end if;
        Process.User_Memory.Copy
          (callerPID, arg2, derivation'Address, Capability_ABI.Derivation_Bytes, ok);
        if not ok then
            return;
        end if;
        replyMsg.authorityTag := 0;
        retval := Process.IPC.replySavedWithCapability (arg0, replyMsg, derivation);
    end handleReplySavedWithCap;

    ---------------------------------------------------------------------------
    -- handleReceiveEventNB
    ---------------------------------------------------------------------------
    procedure handleReceiveEventNB (arg0, arg1 : Unsigned_64;
                                    retval : out Unsigned_64) with
        SPARK_Mode => Off
    is
        caller   : constant Process.ProcessID := PerCPUData.getCurrentPID;
        eventMsg : Process.Message;
        found    : Boolean;
        ok       : Boolean;
        publisher : Process.ProcessID;
        -- arg1, when nonzero: where to write the publisher's identity (0 for
        -- the kernel's own notices), the sender a receiver authenticates.
        identity : aliased Unsigned_64 := 0;
    begin
        if not Process.User_Memory.Message_Writable (caller, arg0) or else
           (arg1 /= 0 and then not Process.User_Memory.Writable_Range
                                     (caller, arg1, Unsigned_64'Size / 8))
        then
            retval := 0;
            return;
        end if;
        Process.IPC.receiveEventNB (eventMsg, found, publisher);
        if found then
            Process.User_Memory.Write_Message (caller, arg0, eventMsg, ok);
            if ok and then arg1 /= 0 then
                if publisher /= Process.NO_PROCESS then
                    identity := Process_Identities.To_Word (Process.identityOf (publisher));
                end if;
                Process.User_Memory.Copy_To_User
                  (caller, arg1, identity'Address, Unsigned_64'Size / 8, ok);
            end if;
            retval := (if ok then 1 else 0);
        else
            retval := 0;
        end if;
    end handleReceiveEventNB;

    ---------------------------------------------------------------------------
    -- handleSendEvent
    ---------------------------------------------------------------------------
    procedure handleSendEvent (callerPID : Process.ProcessID;
                               arg0, arg1, arg2, arg3,
                               arg4, arg5, arg6 : Unsigned_64;
                               retval     : out Unsigned_64) with
        SPARK_Mode => Off
    is
        use type Capabilities.CapabilityType;




        destPID : Process.ProcessID;
        generation : Capabilities.Generation;
        hasCap  : Boolean := False;
        accepted : Boolean;
        authorityTag : Capabilities.Authority_Tag := Capabilities.NO_AUTHORITY_TAG;
        resolvedPID : Unsigned_64;
        resolvedAuthorityTag : Capabilities.Authority_Tag;
        resolveStatus : Capabilities.Operations.OperationStatus;
        publishRights : constant Capabilities.CapabilityRights :=
          (Capabilities.RIGHT_WRITE => True, others => False);
        eventMsg : Process.Message;
    begin
        Process.resolveIdentity (Process_Identities.From_Word (arg0), destPID, generation);
        if destPID = Process.NO_PROCESS then
            retval := reterr;
            return;
        end if;
        eventMsg := (
            tag      => u64ToTag (arg1),
            authorityTag => 0,
            words    => (arg2, arg3, arg4, arg5));

        -- The kernel's own events (child exits, faults, grant lifecycle,
        -- control messages) and flags cannot be forged: receivers act on them.
        if IPC_Labels.Is_Kernel_Event (eventMsg.tag.label)
          or else Capability_ABI.Claims_Kernel_Flags (eventMsg.tag.ipc)
        then
            retval := reterr;
            return;
        end if;

        -- Kernel-mode threads are exempt
        if Process.threadOf (callerPID).mode = Process.KERNEL then
            hasCap := True;
        end if;

        --  A read-only notification capability is narrowly scoped event
        --  publication authority.  Its object is a service/driver class, and
        --  it reaches only the process currently registered for that class.
        --  The complementary RIGHT_WRITE authorizes registration itself.
        -- arg6 names the authority: a notification capability (RIGHT_READ)
        -- covering a role destPID is registered for, or an endpoint
        -- (RIGHT_WRITE) to destPID in its current incarnation.
        if not hasCap then
            declare
                found : Process_Capabilities.Found;
            begin
                Spinlocks.enterCriticalSection (Process.mailtab(callerPID).lock);
                found := Process_Capabilities.Find_Any (Process.proctab(callerPID).caps, arg6);
                Spinlocks.exitCriticalSection (Process.mailtab(callerPID).lock);
                if not found.OK then
                    null;
                elsif found.Cap.capType = Capabilities.CAP_NOTIFICATION then
                    if found.Cap.rights (Capabilities.RIGHT_READ)
                      and then found.Cap.object.param > 0
                      and then found.Cap.object.ref <= Service_Registry.DriverID'Last
                    then
                        for role in found.Cap.object.ref ..
                          (if found.Cap.object.param - 1 >=
                                Service_Registry.DriverID'Last - found.Cap.object.ref
                           then Service_Registry.DriverID'Last
                           else found.Cap.object.ref + (found.Cap.object.param - 1))
                        loop
                            if Service_Registry.Registered (role) = destPID then
                                hasCap := True;
                                authorityTag := found.Cap.authorityTag;
                                exit;
                            end if;
                        end loop;
                    end if;
                elsif found.Cap.object.ref = Unsigned_64 (destPID) then
                    Capabilities.Operations.resolveCurrentEndpoint
                      (cap => found.Cap,
                       rights => publishRights,
                       currentGeneration => Process.generationOf (destPID),
                       destPID => resolvedPID,
                       authorityTag => resolvedAuthorityTag,
                       status => resolveStatus);
                    if resolveStatus = Capabilities.Operations.OP_OK and then
                       resolvedPID = Unsigned_64 (destPID)
                    then
                        hasCap := True;
                        authorityTag := resolvedAuthorityTag;
                    end if;
                end if;
            end;
        end if;

        if not hasCap then
            Process.IPC.notifySupervisor (
                callerPID,
                IPC_Labels.EVENT_CAP_FAULT,
                Unsigned_64 (SyscallNumber'Enum_Rep (
                    SYSCALL_SEND_EVENT)),
                arg0, 0);
            retval := reterr;
        else
            --  The receiver learns which granted authority published the
            --  event. A userspace payload cannot forge this authority tag because the
            --  kernel overwrites it after resolving the caller's capability.
            eventMsg.authorityTag := authorityTag;
            Process.IPC.trySendEvent (
                dest => destPID,
                msg  => eventMsg,
                publisher => callerPID,
                accepted => accepted,
                expectedGeneration => generation);
            retval := (if accepted then 1 else 0);
        end if;
    end handleSendEvent;

    ---------------------------------------------------------------------------
    -- handlePollServiceRequest
    --
    -- Non-blocking service-request receive. This syscall is intentionally
    -- narrow: it accepts client work for a server but will not consume input
    -- events, IRQ-style notifications, or lifecycle events queued in the same
    -- mailbox ring.
    ---------------------------------------------------------------------------
    procedure handlePollServiceRequest (arg0   : Unsigned_64;
                                retval : out Unsigned_64) with
        SPARK_Mode => Off
    is
        caller  : constant Process.ProcessID := PerCPUData.getCurrentPID;
        from    : Process.ProcessID;
        recvMsg : Process.Message;
        found   : Boolean;
        ok      : Boolean;
    begin
        if not Process.User_Memory.Message_Writable (caller, arg0) then
            retval := 0;
            return;
        end if;
        Process.IPC.receiveServiceRequestNB (from, recvMsg, found);
        if found then
            Process.User_Memory.Write_Message (caller, arg0, recvMsg, ok);
            retval := (if ok then Process_Identities.To_Word (Process.identityOf (from)) else 0);
        else
            retval := 0;
        end if;
    end handlePollServiceRequest;

    ---------------------------------------------------------------------------
    -- handlePollAnyIpc
    --
    -- Non-blocking mixed receive. This is the explicit escape hatch for
    -- runtimes that want one central dispatcher for every IPC class.
    ---------------------------------------------------------------------------
    procedure handlePollAnyIpc (arg0   : Unsigned_64;
                                retval : out Unsigned_64) with
        SPARK_Mode => Off
    is
        caller  : constant Process.ProcessID := PerCPUData.getCurrentPID;
        from    : Process.ProcessID;
        recvMsg : Process.Message;
        found   : Boolean;
        ok      : Boolean;
    begin
        if not Process.User_Memory.Message_Writable (caller, arg0) then
            retval := 0;
            return;
        end if;
        Process.IPC.receiveAnyIpcNB (from, recvMsg, found);
        if found then
            Process.User_Memory.Write_Message (caller, arg0, recvMsg, ok);
            retval := (if ok then Process_Identities.To_Word (Process.identityOf (from)) else 0);
        else
            retval := 0;
        end if;
    end handlePollAnyIpc;

    ---------------------------------------------------------------------------
    -- handleWaitCompletion
    ---------------------------------------------------------------------------
    procedure handleWaitCompletion (arg0, arg1, arg2 : Unsigned_64;
                                    retval : out Unsigned_64) with
        SPARK_Mode => Off
    is
        numReturned : Natural;
        effectiveMax : Natural;
        effectiveMin : Natural;
        ok : Boolean;
    begin
        effectiveMax := (if arg1 > Unsigned_64 (Process.COMPLETION_QUEUE_SIZE)
                         then Process.COMPLETION_QUEUE_SIZE else Natural (arg1));
        effectiveMin := (if arg2 > Unsigned_64 (effectiveMax) then effectiveMax
                         else Natural (arg2));
        -- arg0 is a user pointer: written through Process.User_Memory.
        Process.IPC.waitCompletion
          (destination => arg0, maxEntries => effectiveMax, minWait => effectiveMin,
           numReturned => numReturned, ok => ok);
        retval := (if ok then Unsigned_64 (numReturned) else reterr);
    end handleWaitCompletion;

    ---------------------------------------------------------------------------
    -- handlePollCompletion
    ---------------------------------------------------------------------------
    procedure handlePollCompletion (arg0   : Unsigned_64;
                                    retval : out Unsigned_64) with
        SPARK_Mode => Off
    is
        caller     : constant Process.ProcessID := PerCPUData.getCurrentPID;
        found      : Boolean;
        localEntry : Process.CompletionEntry;
        ok         : Boolean;
    begin
        if not Process.User_Memory.Writable_Range
          (caller, arg0, Process.CompletionEntry'Max_Size_In_Storage_Elements)
        then
            retval := 0;
            return;
        end if;
        Process.IPC.pollCompletion (localEntry, found);
        if found then
            Process.User_Memory.Write_Completion (caller, arg0, localEntry, ok);
            retval := (if ok then 1 else 0);
        else
            retval := 0;
        end if;
    end handlePollCompletion;

    ---------------------------------------------------------------------------
    -- handleGrant
    ---------------------------------------------------------------------------
    procedure handleGrant (callerPID : Process.ProcessID;
                           arg0, arg1, arg2, arg3 : Unsigned_64;
                           retval : out Unsigned_64) with
        SPARK_Mode => Off
    is
        use type Capabilities.CapabilityType;


        granteePID : Process.ProcessID;
        generation : Capabilities.Generation;
        replyPID   : Process.ProcessID;
        replyTID   : Process.ThreadID;
        hasCap : Boolean := False;
        gid : Natural;
        perm : Process.GrantPermission;
        ok : Boolean;
    begin
        Process.resolveIdentity (Process_Identities.From_Word (arg0), granteePID, generation);
        if granteePID = Process.NO_PROCESS or else
           not Memory_Grants.Valid_Creation_Request (arg2, arg3) then
            retval := reterr;
            return;
        end if;

        -- Kernel-mode threads exempt
        if Process.threadOf (callerPID).mode = Process.KERNEL then
            hasCap := True;
        else
            -- An endpoint or a saved reply to the grantee is named by
            -- handle: CREATE_SHARED_MEMORY_GRANT_VIA_CAPABILITY. Here only
            -- the calling thread's current reply (answering the grantee) or
            -- the grantee's opt-in below authorize.
            -- The calling thread's current reply authority.
            if not hasCap then
                Process.IPC.replyTargetOf
                  (Process.threadtab (PerCPUData.getCurrentThread).replyCap,
                   replyPID, replyTID);
                hasCap := replyPID = granteePID;
            end if;
        end if;

        -- Reverse: grantee has endpoint to caller
        -- with RIGHT_GRANT (explicit opt-in to
        -- receiving grants from that service).
        -- This looks in the grantee's table, never the caller's: the
        -- grantee consented by holding it (follow-up: name the opt-in
        -- explicitly once channels replace this path).
        if not hasCap then
            Spinlocks.enterCriticalSection (Process.mailtab(granteePID).lock);
            for slot in 0 .. Process_Capabilities.Tables.High_Water
                               (Process.proctab(granteePID).caps) - 1
            loop
                if Process_Capabilities.Tables.Is_Live_At (Process.proctab(granteePID).caps, slot) then
                    declare
                        cap : constant Capabilities.Capability :=
                          Process_Capabilities.Tables.Item_At (Process.proctab(granteePID).caps, slot);
                    begin
                        if cap.capType = Capabilities.CAP_ENDPOINT and then
                           cap.object.ref = Unsigned_64 (callerPID) and then
                           cap.gen = Process.generationOf (callerPID) and then
                           cap.rights (Capabilities.RIGHT_GRANT)
                        then
                            hasCap := True;
                            exit;
                        end if;
                    end;
                end if;
            end loop;
            Spinlocks.exitCriticalSection (Process.mailtab(granteePID).lock);
        end if;

        if not hasCap then
            Process.IPC.notifySupervisor (
                callerPID,
                IPC_Labels.EVENT_CAP_FAULT,
                Unsigned_64
                  (SyscallNumber'Enum_Rep
                     (SYSCALL_CREATE_SHARED_MEMORY_GRANT_FOR_PROCESS_ID)),
                arg0, arg1);
            retval := reterr;
        else
            if (arg3 and 1) /= 0 then
                perm := Process.GRANT_READWRITE;
            else
                perm := Process.GRANT_READ;
            end if;

            Process.IPC.createGrant (
                grantee   => granteePID,
                localAddr => Util.numToAddr(arg1),
                numPages  => Natural(arg2),
                perm      => perm,
                id        => gid,
                success   => ok,
                expectedGeneration => generation,
                forwardable => (arg3 and Grant_Flag_Forwardable) /= 0,
                notify => (arg3 and Grant_Flag_Notify) /= 0);

            if ok then
                retval := Unsigned_64(gid);
            else
                retval := reterr;
            end if;
        end if;
    end handleGrant;

    ---------------------------------------------------------------------------
    -- handleRevoke
    ---------------------------------------------------------------------------
    procedure handleRevoke (callerPID : Process.ProcessID;
                            arg0      : Unsigned_64;
                            retval    : out Unsigned_64) with
        SPARK_Mode => Off
    is
        --  arg0 is the grant's global slot; revokeGrant checks the caller
        --  owns it (KERN-003 step 2: the slot no longer encodes the owner).
        success : Boolean;
    begin
        pragma Unreferenced (callerPID);
        if arg0 = 0 or else arg0 > Unsigned_64 (Memory_Grants.Global_Slot'Last) then
            retval := 0;
            return;
        end if;

        -- Admission and revocation share grantLock. Do not inspect grant
        -- storage outside IPC, especially as records become lazily allocated.
        Process.IPC.revokeGrant (id => Memory_Grants.Global_Slot (arg0), success => success);
        retval := (if success then 1 else 0);
    end handleRevoke;

    procedure handleGetOwnedGrantGeneration
      (arg0   : Unsigned_64;
       retval : out Unsigned_64)
    is
        generation : Memory_Grants.Grant_Generation;
    begin
        -- Slot 0 names no grant: refused, never reported as retired.
        if arg0 < Unsigned_64 (Memory_Grants.Grant_Slot'First) or else
           arg0 > Unsigned_64 (Memory_Grants.Grant_Slot'Last)
        then
            retval := reterr;
            return;
        end if;

        Process.IPC.getOwnedGrantGeneration
          (slot       => Memory_Grants.Grant_Slot (arg0),
           generation => generation);
        retval := Unsigned_64 (generation);
    end handleGetOwnedGrantGeneration;

    procedure handleAcquireGrant
      (arg0, arg1, arg2, arg3, arg4, arg5 : Unsigned_64;
       retval : out Unsigned_64)
    is
        mappedAddress : System.Address;
        success : Boolean;
        -- The owner the grantee expects, by identity: a stale one names no
        -- one, so the acquire fails.
        expectedOwner : constant Process.ProcessID := Process.processOfIdentity (Process_Identities.From_Word (arg2));
    begin
        if arg0 > Unsigned_64 (Memory_Grants.Global_Slot'Last) or else
           not Memory_Grants.Is_Valid_Generation_Field (arg1) or else
           expectedOwner = Process.NO_PROCESS or else
           arg5 > 1
        then
            retval := reterr;
            return;
        end if;

        Process.IPC.acquireGrant
          (reference =>
             (slot       => Memory_Grants.Global_Slot (arg0),
              generation => Memory_Grants.To_Live_Generation (arg1)),
           expectedOwner => expectedOwner,
           byteOffset    => arg3,
           byteLength    => arg4,
           requiredWrite => arg5 = 1,
           mappedAddress => mappedAddress,
           success       => success);
        retval := (if success then Util.addrToNum (mappedAddress) else reterr);
    end handleAcquireGrant;

    procedure handleAcquireGrantViaCap
      (callerPID : Process.ProcessID;
       arg0, arg1, arg2, arg3, arg4, arg5 : Unsigned_64;
       retval : out Unsigned_64)
    is
        use type Capabilities.CapabilityType;
        cap : Capabilities.Capability;
        ownerPID : Process.ProcessID;
        mappedAddress : System.Address;
        success : Boolean;
    begin
        if arg1 > Unsigned_64 (Memory_Grants.Global_Slot'Last) or else
           not Memory_Grants.Is_Valid_Generation_Field (arg2) or else
           arg5 > 1
        then
            retval := reterr;
            return;
        end if;

        cap := ownCap (callerPID, arg0);
        if cap.capType /= Capabilities.CAP_ENDPOINT or else
           not cap.rights (Capabilities.RIGHT_READ) or else
           cap.object.ref = 0 or else
           cap.object.ref > Unsigned_64 (Process.ProcessID'Last)
        then
            retval := reterr;
            return;
        end if;

        ownerPID := Process.ProcessID (cap.object.ref);
        if cap.gen /= Process.generationOf (ownerPID) then
            retval := reterr;
            return;
        end if;

        Process.IPC.acquireGrant
          (reference =>
             (slot       => Memory_Grants.Global_Slot (arg1),
              generation => Memory_Grants.To_Live_Generation (arg2)),
           expectedOwner => ownerPID,
           byteOffset    => arg3,
           byteLength    => arg4,
           requiredWrite => arg5 = 1,
           mappedAddress => mappedAddress,
           success       => success);
        retval := (if success then Util.addrToNum (mappedAddress) else reterr);
    end handleAcquireGrantViaCap;

    procedure handleReturnGrant
      (arg0, arg1 : Unsigned_64;
       retval : out Unsigned_64)
    is
        success : Boolean;
    begin
        if arg0 > Unsigned_64 (Memory_Grants.Global_Slot'Last) or else
           not Memory_Grants.Is_Valid_Generation_Field (arg1)
        then
            retval := 0;
            return;
        end if;

        Process.IPC.returnGrant
          (reference =>
             (slot       => Memory_Grants.Global_Slot (arg0),
              generation => Memory_Grants.To_Live_Generation (arg1)),
           success => success);
        retval := (if success then 1 else 0);
    end handleReturnGrant;

    procedure handleRevokeGrantReference
      (arg0, arg1 : Unsigned_64;
       retval : out Unsigned_64)
    is
        success : Boolean;
    begin
        if arg0 > Unsigned_64 (Memory_Grants.Global_Slot'Last) or else
           not Memory_Grants.Is_Valid_Generation_Field (arg1)
        then
            retval := 0;
            return;
        end if;

        Process.IPC.revokeGrantReference
          (reference =>
             (slot       => Memory_Grants.Global_Slot (arg0),
              generation => Memory_Grants.To_Live_Generation (arg1)),
           success => success);
        retval := (if success then 1 else 0);
    end handleRevokeGrantReference;

    ---------------------------------------------------------------------------
    -- handleReadStatistics (SYSCALL_READ_STATISTICS)
    -- arg0 = address of a System_Publications.Statistics in the caller's own
    -- writable memory, arg1 = its size. Only the caller's own counters and
    -- the system's free memory: callerPID comes from the running thread, so
    -- no argument can select another process.
    ---------------------------------------------------------------------------
    procedure handleReadStatistics (callerPID  : Process.ProcessID;
                                    arg0, arg1 : Unsigned_64;
                                    retval     : out Unsigned_64) with
        SPARK_Mode => Off
    is
        Snapshot : constant System_Publications.Statistics :=
          (Owned_Bytes       => Unsigned_64 (Process.proctab(callerPID).frames.length) *
                                  Unsigned_64 (Virtmem.PAGE_SIZE),
           Event_Drops       => Process.proctab(callerPID).eventDrops,
           Free_Memory_Bytes => Unsigned_64 (BuddyAllocator.getFreeBytes));
        copied : Boolean;
    begin
        retval := reterr;
        if arg1 /= System_Publications.Statistics_Bytes then
            return;
        end if;
        Process.User_Memory.Copy_To_User
          (callerPID, arg0, Snapshot'Address,
           System_Publications.Statistics_Bytes, copied);
        if copied then
            retval := 0;
        end if;
    end handleReadStatistics;

    ---------------------------------------------------------------------------
    -- handleGrantViaCap
    -- Create a shared memory grant using a capability slot to identify the
    -- grantee, instead of a raw PID.
    -- arg0 = cap slot, arg1 = local addr, arg2 = pages,
    -- arg3 = bit0 RW, bit1 explicit owner forwarding opt-in
    ---------------------------------------------------------------------------
    procedure handleGrantViaCap (callerPID : Process.ProcessID;
                                  arg0, arg1, arg2, arg3 : Unsigned_64;
                                  retval : out Unsigned_64) with
        SPARK_Mode => Off
    is
        use type Capabilities.CapabilityType;

        cap : Capabilities.Capability;
        granteePID : Process.ProcessID;
        gid : Natural;
        perm : Process.GrantPermission;
        ok : Boolean;
    begin
        if not Memory_Grants.Valid_Creation_Request (arg2, arg3) then
            println
              ("CREATE_SHARED_MEMORY_GRANT_VIA_CAPABILITY: invalid size or flags");
            retval := reterr;
            return;
        end if;

        cap := ownCap (callerPID, arg0);

        -- A saved reply names the process it answers: a server may share
        -- memory with a client whose request it set aside.
        if cap.capType = Capabilities.CAP_REPLY then
            declare
                replyPID : Process.ProcessID;
                replyTID : Process.ThreadID;
            begin
                Process.IPC.replyTargetOf (cap, replyPID, replyTID);
                if replyPID = Process.NO_PROCESS then
                    retval := reterr;
                    return;
                end if;
                cap := (capType => Capabilities.CAP_ENDPOINT,
                        rights  => Capabilities.READ_WRITE,
                        authorityTag => Capabilities.NO_AUTHORITY_TAG,
                        object  => (ref => Unsigned_64 (replyPID), param => 0),
                        gen     => Process.generationOf (replyPID));
            end;
        end if;

        if cap.capType /= Capabilities.CAP_ENDPOINT then
            println
              ("CREATE_SHARED_MEMORY_GRANT_VIA_CAPABILITY: slot is not " &
               "CAP_ENDPOINT");
            retval := reterr;
            return;
        end if;

        if not cap.rights(Capabilities.RIGHT_READ) then
            println
              ("CREATE_SHARED_MEMORY_GRANT_VIA_CAPABILITY: no RIGHT_READ " &
               "on endpoint");
            retval := reterr;
            return;
        end if;

        if cap.object.ref > Unsigned_64 (Process.ProcessID'Last) or
           cap.object.ref = 0
        then
            println
              ("CREATE_SHARED_MEMORY_GRANT_VIA_CAPABILITY: invalid PID " &
               "in capability");
            retval := reterr;
            return;
        end if;

        granteePID := Process.ProcessID (cap.object.ref);

        -- Validate generation (stale cap check)
        if cap.gen /= Process.generationOf (granteePID) then
            println
              ("CREATE_SHARED_MEMORY_GRANT_VIA_CAPABILITY: stale " &
               "capability");
            retval := reterr;
            return;
        end if;

        if (arg3 and 1) /= 0 then
            perm := Process.GRANT_READWRITE;
        else
            perm := Process.GRANT_READ;
        end if;

        Process.IPC.createGrant (
            grantee   => granteePID,
            localAddr => Util.numToAddr(arg1),
            numPages  => Natural(arg2),
            perm      => perm,
            id        => gid,
            success   => ok,
                expectedGeneration => cap.gen,
                forwardable => (arg3 and Grant_Flag_Forwardable) /= 0,
                notify => (arg3 and Grant_Flag_Notify) /= 0);

        if ok then
            retval := Unsigned_64(gid);
        else
            println ("CREATE_SHARED_MEMORY_GRANT_VIA_CAPABILITY: create failed");
            retval := reterr;
        end if;
    end handleGrantViaCap;

    -- Recipient endpoint, parent slot/generation, page offset/count, RW flag.
    -- Return the child slot+generation atomically in canonical reference form.
    procedure handleDeriveGrantViaCap
      (callerPID : Process.ProcessID;
       arg0, arg1, arg2, arg3, arg4, arg5 : Unsigned_64;
       retval : out Unsigned_64)
    is
        use type Capabilities.CapabilityType;
        cap : Capabilities.Capability;
        derived : Memory_Grants.Reference;
        success : Boolean;
    begin
        retval := reterr;
        if arg1 > Unsigned_64 (Memory_Grants.Global_Slot'Last) or else
           not Memory_Grants.Is_Valid_Generation_Field (arg2) or else
           arg3 > Unsigned_64 (Memory_Grants.Page_Offset'Last) or else
           arg4 not in 1 .. Unsigned_64 (Memory_Grants.Page_Count'Last) or else
           arg5 > 1
        then
            return;
        end if;
        cap := ownCap (callerPID, arg0);
        if cap.capType /= Capabilities.CAP_ENDPOINT or else
           not cap.rights (Capabilities.RIGHT_READ) or else
           cap.object.ref = 0 or else
           cap.object.ref > Unsigned_64 (Process.ProcessID'Last)
        then
            return;
        end if;
        Process.IPC.deriveGrant
          ((Memory_Grants.Global_Slot (arg1), Memory_Grants.To_Live_Generation (arg2)),
           Process.ProcessID (cap.object.ref), cap.gen,
           Memory_Grants.Page_Offset (arg3), Memory_Grants.Page_Count (arg4),
           (if arg5 = 1 then Process.GRANT_READWRITE else Process.GRANT_READ),
           derived, success);
        if success then
            retval := Shift_Left (Unsigned_64 (derived.generation), 32) or
              Unsigned_64 (derived.slot);
        end if;
    end handleDeriveGrantViaCap;

end Syscall.IPC;
