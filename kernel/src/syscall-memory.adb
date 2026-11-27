-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2020 Jon Andrew
--
-- Syscall memory handlers: device memory, DMA, MAP_INTO, the heap break
-- and the framebuffer (moved from Syscall.IPC, IPC revision 2 S0).
-------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Unchecked_Conversion;
with System;
with System.Storage_Elements; use System.Storage_Elements;

with BuddyAllocator;
with Memory_Accounting;
with Process_Memory_Budget;
with Boot_Framebuffer;
with Multiboot;
with Capabilities;
with Capabilities.Operations;
with Process_Capabilities;
with Device_Memory_Admission;
with Device_Memory_Authority;
with Device_Memory_Diagnostics;
with Device_Memory_Publication;
with DMA_Allocation_ABI;
with DMA_Remapping;
with Heap_Admission;
with Heap_Growth;
with Memory_Grants;
with Modules;
with Owned_Memory_Layout;
with Contiguous_Memory;
with Process;
with Process_Frames;
with Process_Pool;
with Process_Identities;
with Process.DMA;
with Process.Owned_Memory;
with Process.User_Memory;
with Spinlocks;
with Process_Lifetime;
with Physical_Range_Map;
with Platform_Pool;
with Platform_Pool_ABI;
with TextIO; use TextIO;
with TLB_Shootdown;
with Boot_Output;
with Virtmem;
with x86;
with Syscall.Lookups; use Syscall.Lookups;

use type Process.MessageTag;
use type Process.ProcessMode;
use type Capabilities.Operations.OperationStatus;

package body Syscall.Memory is
    use type Platform_Pool.Claim_Outcome;
    -- ALLOC_DMA's largest order is the one userspace sizes DMA by
    -- (shared/launch Contiguous_Memory, LIM-001).
    pragma Compile_Time_Error
      (Natural (BuddyAllocator.Order'Last) /= Contiguous_Memory.Maximum_Order,
       "the kernel's largest contiguous block is not the one userspace sizes DMA by");
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
                               arg5      : Unsigned_64;
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
        -- Firmware memory, not device memory; and if so, mapped write-back
        -- (NVS, the FACS page) rather than uncached (PCI configuration).
        firmware  : Boolean;
        writeBack : Boolean;
        ok : Boolean := True;
        -- arg4 names the CAP_DEVICE_MEM this mapping relies on.
        authority : constant Capabilities.Capability := deviceMemoryOf (callerPID, arg4);
    begin
        retval := reterr;

        -- The platform-register pool (ACPI-POOL-001, Platform_Pool_ABI):
        -- checked once here against the pool; arg5 names a refusal record.
        declare
            use type Capabilities.CapabilityType;
            use type Capabilities.Authority_Tag;
        begin
            if authority.capType = Capabilities.CAP_DEVICE_MEM
              and then authority.authorityTag = Capabilities.PLATFORM_POOL_TAG
            then
                declare
                    mapped  : Boolean;
                    why     : Platform_Pool_ABI.Refusal;
                    refusal : aliased Platform_Pool_ABI.Refusal_Record;
                    copied  : Boolean;
                begin
                    if arg3 > Platform_Pool_ABI.Read_Only_Mode
                      or else numPages = 0 or else numPages > Platform_Pool_ABI.Maximum_Pages
                      or else arg1 mod Unsigned_64 (Virtmem.PAGE_SIZE) /= 0
                      or else arg1 >= USER_ADDRESS_LIMIT
                      or else numPages * Unsigned_64 (Virtmem.PAGE_SIZE) > USER_ADDRESS_LIMIT - arg1
                      or else Memory_Grants.Overlaps_Received_Region
                                (arg1, Memory_Grants.Page_Count (numPages))
                      or else Owned_Memory_Layout.Conflicts
                                (arg1, numPages * Unsigned_64 (Virtmem.PAGE_SIZE))
                    then
                        -- Never over a received grant or owned memory.
                        retval := Platform_Pool_ABI.Refusal_Word (Platform_Pool_ABI.Invalid_Request);
                        return;
                    end if;
                    Platform_Pool.Map
                      (callerPID, authority, arg0, arg1, numPages,
                       arg3 = Platform_Pool_ABI.Read_Only_Mode, mapped, why, refusal);
                    retval := (if mapped then 0 else Platform_Pool_ABI.Refusal_Word (why));
                    if not mapped and then arg5 /= 0 then
                        Process.User_Memory.Copy_To_User
                          (callerPID, arg5, refusal'Address,
                           Platform_Pool_ABI.Refusal_Record_Bytes, copied);
                    end if;
                end;
                return;
            end if;
        end;

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
        -- CAP_DEVICE_MEM the caller holds. Or, through a firmware-memory
        -- capability only, whole firmware pages of its kind: ACPI NVS and
        -- the FACS page mapped write-back, PCI segment 0 bus 0 configuration
        -- uncached and read-only (Device_Memory_Authority.Firmware_Mode).
        capAllowed := Device_Memory_Authority.Authorize
          (authority, Physical_Range_Map.Kernel_Map,
           arg0, numPages * Unsigned_64 (Virtmem.PAGE_SIZE),
           (if arg3 = 0 then Device_Memory_Admission.Read_Write
            else Device_Memory_Admission.Read_Only));
        firmware := not capAllowed and then Device_Memory_Authority.Authorize_Firmware
          (authority, Physical_Range_Map.Kernel_Map,
           arg0, numPages * Unsigned_64 (Virtmem.PAGE_SIZE),
           (if arg3 = 0 then Device_Memory_Admission.Read_Write
            else Device_Memory_Admission.Read_Only));
        capAllowed := capAllowed or else firmware;
        writeBack := firmware and then Device_Memory_Authority.Write_Back
          (Device_Memory_Authority.Kind_Of (authority.authorityTag));

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
                    flags   => (if writeBack then
                                  (if arg3 = 1 then Virtmem.PG_USERDATARO
                                   else Virtmem.PG_USERDATA)
                                elsif arg3 = 1 then
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
                              forDevice : Boolean;
                              requester : DMA_Allocation_ABI.Requester_ID;
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
        -- The block's IOMMU domain (IOMMU-001), 0 when unconfined.
        domain : Unsigned_64 := 0;
        use type DMA_Remapping.Grant_Result;

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
        if Natural (order) <= 9 then
            -- From the process pool (KERN-014): outside the kernel's map,
            -- zeroed through the window. dmaAddr is only the allocator's
            -- name for the block (its linear-map address, never accessed).
            declare
                base : Virtmem.PhysAddress;
            begin
                Process_Pool.Alloc_Run (Natural (order), ceiling, base);
                dmaAddr := (if base = 0 then BuddyAllocator.NO_BLOCK_AVAILABLE
                            else Virtmem.P2Va (base));
            end;
        elsif ceiling = 0 then
            -- Larger than one region: still the kernel's lists (KERN-014
            -- follow-up), so the kernel's map still covers these.
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

            -- Into the device's IOMMU domain only, before any mapping is
            -- published (IOMMU-001). Unconfined when the device's unit does
            -- not translate (no IOMMU): the kernel reports that state.
            if forDevice then
                declare
                    result : DMA_Remapping.Grant_Result;
                begin
                    DMA_Remapping.Map_Grant
                      (Requester => requester,
                       Owner     => Process_Identities.To_Word (Process.identityOf (targetPID)),
                       Physical  => Unsigned_64 (dmaPhys),
                       Pages     => Unsigned_64 (numPages),
                       Writable  => True,
                       Domain    => domain,
                       Result    => result);
                    if result = DMA_Remapping.Refused then
                        for page in 0 .. numPages - 1 loop
                            BuddyAllocator.releaseUserFrame
                              (dmaPhys + Virtmem.PhysAddress (page * Virtmem.PAGE_SIZE),
                               BuddyAllocator.Frame_Owner (targetPID));
                        end loop;
                        -- Refused with a domain: the rollback's invalidation
                        -- did not complete, so the block is never reused.
                        if domain = 0 then
                            BuddyAllocator.free (order, dmaAddr);
                        else
                            println ("ALLOC_DMA: IOMMU rollback incomplete; block quarantined");
                        end if;
                        Process.DMA.Cancel (tracking);
                        println ("ALLOC_DMA: refused by the IOMMU");
                        return;
                    elsif result = DMA_Remapping.Unconfined then
                        domain := 0;
                    end if;
                end;
            end if;

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
                    -- Out of the IOMMU domain, invalidated, before reuse.
                    declare
                        released : Boolean := True;
                    begin
                        if domain /= 0 then
                            DMA_Remapping.Unmap_Grant
                              (domain, Unsigned_64 (dmaPhys), Unsigned_64 (numPages), released);
                        end if;
                        if released then
                            BuddyAllocator.free (order, dmaAddr);
                        else
                            println ("ALLOC_DMA: IOMMU rollback incomplete; block quarantined");
                        end if;
                    end;
                    Process.unlockAddressSpace (targetPID);
                    Process.DMA.Cancel (tracking);
                    return;
                end if;
                mappedPages := mappedPages + 1;
            end loop;

            -- Publication cannot allocate metadata: reservation preceded DMA.
            Process.DMA.Commit (tracking, Unsigned_64 (dmaPhys), domain);
            Process.unlockAddressSpace (targetPID);

            retval := Unsigned_64 (dmaPhys);
        end;
    end allocateDma;

    procedure allocateDmaRequest
      (callerPID : Process.ProcessID;
       authority : Unsigned_64;
       arg0, arg1, arg2, arg3, arg4, arg6 : Unsigned_64;
       retval : out Unsigned_64) with SPARK_Mode => Off
    is
        package ABI renames DMA_Allocation_ABI;
        mode      : constant Unsigned_64 := ABI.Mode_Of (arg3);
        forDevice : constant Boolean := ABI.Is_Device (arg3);
        requester : constant ABI.Requester_ID := ABI.Requester_Of (arg3);
    begin
        retval := reterr;
        if not ABI.Well_Formed (arg3) or else
          (not forDevice and then requester /= 0)
        then
            return;
        end if;
        -- A device-bound block names a PCI function's IOMMU domain: arg6 is
        -- the caller's CAP_IOPORT over the PCI configuration mechanism,
        -- with write (it can already program any function's bus mastering).
        -- The caller's mailbox lock is held (handleAllocDma).
        if forDevice then
            declare
                held : constant Process_Capabilities.Found := Process_Capabilities.Find
                  (Process.proctab(callerPID).caps, arg6, Capabilities.CAP_IOPORT,
                   onlyRight (Capabilities.RIGHT_WRITE));
            begin
                if not held.OK or else
                  held.Cap.object.ref > ABI.PCI_Config_First_Port or else
                  ABI.PCI_Config_First_Port - held.Cap.object.ref > held.Cap.object.param or else
                  held.Cap.object.param - (ABI.PCI_Config_First_Port - held.Cap.object.ref)
                    < ABI.PCI_Config_Ports
                then
                    println ("ALLOC_DMA: device binding denied, no PCI configuration authority");
                    return;
                end if;
            end;
        end if;
        if mode = ABI.Mode_Ordinary then
            allocateDma (callerPID, authority, arg0, arg1, arg2, False, False, arg4,
                         forDevice, requester, retval);
            return;
        elsif (mode /= ABI.Mode_Retained and then mode /= ABI.Mode_Retained_Large)
          or else arg1 > 14 or else
          (mode = ABI.Mode_Retained_Large and then arg1 /= 9)
        then
            return;
        end if;
        -- The common ledger reserves global and owner budgets together after
        -- authorization. Bound charges refund only at actual physical free.
        allocateDma (callerPID, authority, arg0, arg1, arg2, True,
                     mode = ABI.Mode_Retained_Large, arg4, forDevice, requester, retval);
    end allocateDmaRequest;

    procedure handleAllocDma
      (callerPID : Process.ProcessID;
       arg0, arg1, arg2, arg3, arg4, arg5, arg6 : Unsigned_64;
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
            Spinlocks.enterCriticalSection (Process.tableLock (First));
            if Last /= First then
                Spinlocks.enterCriticalSection (Process.tableLock (Last));
            end if;
            if Process.proctab(callerPID).admitted and then
              Process.proctab(Target).admitted and then
              Process.generationOf (Target) = resolvedGeneration and then
              not Process_Lifetime.Closing (Process.threadOf (callerPID).lifetime) and then
              not Process_Lifetime.Closing (Process.threadOf (Target).lifetime)
            then
                -- Below this point the target is the resolved slot.
                allocateDmaRequest
                  (callerPID, arg5, Unsigned_64 (Target), arg1, arg2, arg3, arg4, arg6, retval);
            else
                println ("ALLOC_DMA: process closing or unavailable");
            end if;
            if Last /= First then
                Spinlocks.exitCriticalSection (Process.tableLock (Last));
            end if;
            Spinlocks.exitCriticalSection (Process.tableLock (First));
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
                -- Device pages mapped into another process are a claim on
                -- the platform pool, recorded before they are mapped.
                -- No room for the claim: refused (CAPACITY, kernel log).
                if targetPID /= callerPID
                  and then Platform_Pool.Mapping_Into
                             (targetPID, arg1, arg3 * Unsigned_64 (Virtmem.PAGE_SIZE))
                           = Platform_Pool.Refused_Capacity
                then
                    return;
                end if;
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
                -- Boot-image pages: one more mapping each, counted before it
                -- exists (a released page is refused here).
                if arg4 = MAP_INTO_BOOT_MODULE then
                    declare
                        granted : Boolean;
                        acquired : Unsigned_64;
                    begin
                        Modules.Acquire_Initrd_Pages
                          (arg1, arg3 * Unsigned_64 (Virtmem.PAGE_SIZE), acquired, granted);
                        if not granted or else acquired /= physFirst then
                            if granted then
                                Modules.Return_Initrd_Pages
                                  (acquired, arg3 * Unsigned_64 (Virtmem.PAGE_SIZE));
                            end if;
                            Process.unlockAddressSpace (targetPID);
                            Process.Owned_Memory.Unlock;
                            println ("MAP_INTO: boot image pages released or unavailable");
                            return;
                        end if;
                    end;
                end if;
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
                        -- Pages mapped so far stay mapped and counted; the
                        -- rest of the acquired span goes back.
                        if arg4 = MAP_INTO_BOOT_MODULE then
                            Modules.Note_Mapped (targetPID, Unsigned_64 (i));
                            Modules.Return_Initrd_Pages
                              (physFirst + Unsigned_64 (i) * Unsigned_64 (Virtmem.PAGE_SIZE),
                               (arg3 - Unsigned_64 (i)) * Unsigned_64 (Virtmem.PAGE_SIZE));
                        end if;
                        Process.unlockAddressSpace (targetPID);
                        Process.Owned_Memory.Unlock;
                        print ("MAP_INTO: map fail page ");
                        println (i);
                        return;
                    end if;
                end loop;
                if arg4 = MAP_INTO_BOOT_MODULE then
                    Modules.Note_Mapped (targetPID, arg3);
                end if;
                Process.unlockAddressSpace (targetPID);
                Process.Owned_Memory.Unlock;

                retval := 0;
            end performLocked;
        begin
            Spinlocks.enterCriticalSection (Process.tableLock (targetPID));
            performLocked;
            Spinlocks.exitCriticalSection (Process.tableLock (targetPID));
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
                (Integer_Address'Min
                   (Process.GRANT_REGION_BASE,
                    -- The owned-memory aperture lies below the received
                    -- grants (MEM-007): the heap stops at it.
                    Integer_Address (Owned_Memory_Layout.First)),
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
            storage : Process_Frames.Process_Frame;
            allocation : Process.Page_Allocation_Result;
        begin
            Process.tryAddPage
              (proc => Process.proctab(callerPID),
               mapTo => To_Address (Integer_Address (plan.First_Page) +
                                    Integer_Address (Index) * Virtmem.PAGE_SIZE),
               frame => storage, result => allocation, flags => Virtmem.PG_USERDATA);
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
            Process.publishOwnedBytes (callerPID);
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

end Syscall.Memory;
