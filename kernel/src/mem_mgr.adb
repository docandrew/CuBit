-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2019 Jon Andrew
--
-- @summary Virtual Memory Manager
-------------------------------------------------------------------------------

with Interfaces; use Interfaces;
with BootAllocator;
with BuddyAllocator;
with MemoryAreas; use MemoryAreas;
with TextIO; use TextIO;
with x86;

package body Mem_mgr
    with SPARK_Mode => On
is
    -- raise if we fail to re-map any pages
    RemapException : exception;

    -- Switch from bootstrap page table to the primary kernel page table.
    kernelP4 : aliased Virtmem.P4 with alignment => Virtmem.PAGE_SIZE;

    ---------------------------------------------------------------------------
    -- determineFlagsAndMapPage - given a frame number, map it with the 
    -- appropriate page flags, depending on if it is part of the kernel,
    -- etc.
    ---------------------------------------------------------------------------
    procedure determineFlagsAndMapFrame (frame : in Virtmem.PFN) is

        procedure mapPage is new Virtmem.mapPage(allocate);
        
        ok              : Boolean;

        fromPhys        : constant Virtmem.PhysAddress :=
            Virtmem.PFNToAddr(frame);

        toVirtLinear    : constant Virtmem.VirtAddress :=
            Virtmem.P2V(fromPhys);

        toVirtKernel    : constant Virtmem.VirtAddress :=
            fromPhys + Virtmem.KERNEL_BASE;

    begin
        -- We'll map the kernel sections twice - once to the linear-mapped
        -- region (read-only), and again to the kernel-mapped region
        -- (as code or r/w data).

        if frame in kernelTextPages then
            mapPage (fromPhys, 
                     toVirtLinear,
                     Virtmem.PG_KERNELDATARO,
                     kernelP4,
                     ok);

            if not ok then raise RemapException with "Unable to map text page to linear or kernel region"; end if;

            mapPage (fromPhys,
                     toVirtKernel,
                     Virtmem.PG_KERNELCODE,
                     kernelP4,
                     ok);

        elsif frame in kernelROPages then

            mapPage (fromPhys,
                     toVirtLinear,
                     Virtmem.PG_KERNELDATARO,
                     kernelP4,
                     ok);

            if not ok then raise RemapException with "Unable to map R/O page to linear or kernel region"; end if;

            mapPage (fromPhys,
                     toVirtKernel,
                     Virtmem.PG_KERNELDATARO,
                     kernelP4,
                     ok);

        elsif frame in kernelRWPages then

            mapPage (fromPhys,
                     toVirtLinear,
                     Virtmem.PG_KERNELDATARO,
                     kernelP4,
                     ok);

            if not ok then raise RemapException with "Unable to map R/W page to linear or kernel region"; end if;
            
            mapPage (fromPhys,
                     toVirtKernel,
                     Virtmem.PG_KERNELDATA,
                     kernelP4,
                     ok);

        else
            -- everything else gets mapped to the linear area
            mapPage (fromPhys,
                     toVirtLinear,
                     Virtmem.PG_KERNELDATA,
                     kernelP4,
                     ok);

        end if;

        if not ok then
            raise RemapException with "Unable to map page to linear or kernel region";
        end if;
    end determineFlagsAndMapFrame;


    ---------------------------------------------------------------------------
    -- mapBigFrameAsSmallFrames
    ---------------------------------------------------------------------------
    procedure mapBigFrameAsSmallFrames (bigFrame : in Virtmem.BigPFN) is

        procedure determineFlagsAndMapFrame is new
            Mem_mgr.determineFlagsAndMapFrame (allocate);
    
        use type Virtmem.PFN;

        -- each big PFN contains 512 small frames
        startFrame : constant Virtmem.PFN := Virtmem.bigPFNToPFN (bigFrame);
        endFrame   : constant Virtmem.PFN := startFrame + 511;       
    begin
    
        for frame in startFrame..endFrame loop
            determineFlagsAndMapFrame(frame);
        end loop;
    end mapBigFrameAsSmallFrames;


    ---------------------------------------------------------------------------
    -- mapBigFrame 
    ---------------------------------------------------------------------------
    procedure mapBigFrame (bigFrame : in Virtmem.BigPFN;
                           flags    : in Unsigned_64 := Virtmem.PG_KERNELDATA) is

        fromPhys     : constant Virtmem.PhysAddress := Virtmem.BigPFNToAddr (bigFrame);
        toVirtLinear : constant Virtmem.VirtAddress := Virtmem.P2V (fromPhys);
        ok : Boolean;

        procedure mapBigPage is new Virtmem.mapBigPage (allocate);
    begin
        mapBigPage (fromPhys,
                    toVirtLinear,
                    flags,
                    kernelP4,
                    ok);

        if not ok then
            raise RemapException with "Unable to map big page to linear region.";
        end if;
    end mapBigFrame;

    ---------------------------------------------------------------------------
    -- isBigFrameAligned
    ---------------------------------------------------------------------------
    function isBigFrameAligned (addr : in Virtmem.PhysAddress) 
        return Boolean
    is
    begin
        return Unsigned_64(addr) mod Unsigned_64(Virtmem.BIG_FRAME_SIZE) = 0;
    end isBigFrameAligned;


    ---------------------------------------------------------------------------
    -- mapArea - map a memory area
    -- We assume that memory areas from Multiboot do not overlap.
    ---------------------------------------------------------------------------
    procedure mapArea (area : in MemoryAreas.MemoryArea) is

        use type Virtmem.PFN;
        use type Virtmem.BigPFN;

        procedure mapBigFrame is new 
            Mem_mgr.mapBigFrame(allocate);

        procedure determineFlagsAndMapFrame is new 
            Mem_mgr.determineFlagsAndMapFrame(allocate);

        -- big PFN containing the top of the stack.
        kernelEndBigPFN : constant Virtmem.BigPFN :=
            Virtmem.addrToBigPFN (Virtmem.V2P (Virtmem.STACK_TOP - 1));

        -- frame we are considering mapping
        curFrame        : Virtmem.PFN;

        -- address we are considering mapping
        curAddr         : Virtmem.PhysAddress;

        smallPagesMapped : Natural := 0;
        bigPagesMapped : Natural := 0;
    begin
        -- iterate by small frames, trying to map big pages if we can.
        curFrame := Virtmem.addrToPFN (area.startAddr);

        mapFramesInArea: loop

            curAddr := Virtmem.pfnToAddr (curFrame);

            -- If this frame is big-frame aligned, past the kernel, and
            -- there's enough room left in the area to map it as a big
            -- page, do so.
            if isBigFrameAligned (curAddr) and
               area.endAddr - curAddr >= (Virtmem.BIG_FRAME_SIZE - 1) and
               curAddr >= Virtmem.V2P (Virtmem.STACK_TOP)
            then
                mapBigFrame (Virtmem.pfnToBigPFN (curFrame));
                curFrame := curFrame + 512;
                bigPagesMapped := bigPagesMapped + 1;
            else
                determineFlagsAndMapFrame (curFrame);
                curFrame := curFrame + 1;
                smallPagesMapped := smallPagesMapped + 1;
            end if;

            exit mapFramesInArea when
                curFrame > Virtmem.addrToPFN (area.endAddr);

        end loop mapFramesInArea;

    end mapArea;


    ---------------------------------------------------------------------------
    -- mapIOArea - map a memory area with PG_IO flags
    ---------------------------------------------------------------------------
    procedure mapIOArea (area  : in MemoryAreas.MemoryArea;
                         flags : in Unsigned_64 := Virtmem.PG_IO) is
        ok : Boolean;
        startPFN : Virtmem.PFN := Virtmem.addrToPFN (area.startAddr);
        endPFN   : Virtmem.PFN := Virtmem.addrToPFN (area.endAddr);
        
        function mapIOFrame is new Mem_mgr.mapIOFrame(allocate);
    begin

        for frame in startPFN..endPFN loop

            ok := mapIOFrame (Virtmem.PFNToAddr(frame), flags);

            if not ok then
                raise RemapException with "mapIOArea: Unable to map IO area";
            end if;

        end loop;
    end mapIOArea;

    ---------------------------------------------------------------------------
    -- mapBigIOArea - map a memory area with big pages and PG_IO flags.
    ---------------------------------------------------------------------------
    procedure mapBigIOArea (area  : in MemoryAreas.MemoryArea;
                            flags : in Unsigned_64 := Virtmem.PG_IO) is
        use type Virtmem.PFN;
        use type Virtmem.BigPFN;

        curFrame  : Virtmem.PFN;
        curAddr   : Virtmem.PhysAddress;
        bigFrames : Natural := 0;

        procedure mapBigFrame is new Mem_mgr.mapBigFrame(allocate);
    begin
        curFrame := Virtmem.addrToPFN (area.startAddr);

        mapFramesInArea: loop

            curAddr := Virtmem.pfnToAddr (curFrame);

            -- If this frame is big-frame aligned, past the kernel, and
            -- there's enough room left in the area to map it as a big
            -- page, do so.
            if isBigFrameAligned (curAddr) then
                mapBigFrame (Virtmem.pfnToBigPFN (curFrame), flags);
                curFrame := curFrame + 512;
                bigFrames := bigFrames + 1;
            else
                raise RemapException with "mapBigIOArea: part of this area is not aligned with big pages.";
            end if;

            exit mapFramesInArea when curFrame > Virtmem.addrToPFN (area.endAddr);

        end loop mapFramesInArea;

        -- print ("mapBigIOArea: Mapped "); print (bigFrames); println (" big frames");
    end mapBigIOArea;
    ---------------------------------------------------------------------------
    -- setup
    -- map all physical memory into our p4 table.
    ---------------------------------------------------------------------------
    procedure setup (areas : in MemoryAreas.MemoryAreaArray) with
        SPARK_Mode => Off
    is
        procedure mapIOArea is new Mem_mgr.mapIOArea(BootAllocator.allocFrame);
        procedure mapArea is new Mem_mgr.mapArea(BootAllocator.allocFrame);
    begin

        -- Zeroize the top-level page table
        for i in kernelP4'Range loop
            kernelP4(i) := Virtmem.u64ToPTE(0);
        end loop;

        -- Map everything R/W below 0x100000 (except null page) to start.
        mapArea (MemoryArea'(kind        => MemoryAreas.USABLE,
                             startAddr   => 1,
                             endAddr     => 16#FFFFF#));

        -- Map the rest of our memory areas.
        for area of areas loop
            if area.kind = MemoryAreas.BAD then
                null;
            elsif area.kind = MemoryAreas.VIDEO then
                mapIOArea (area, Virtmem.PG_IO_WC);
            else
                mapArea (area);
            end if;
        end loop;

        -- Make these new page tables the active ones.
        switchAddressSpace;

    end setup;


    ---------------------------------------------------------------------------
    -- createGuardPage
    -- Unmap a single 4KB page from the kernel linear map. If the page is
    -- part of a 2MB big page, split the big page into 512 small pages first,
    -- then clear the target P1 entry. Used for kernel stack guard pages.
    ---------------------------------------------------------------------------
    procedure createGuardPage (physAddr : in Virtmem.PhysAddress) with
        SPARK_Mode => Off  -- uses 'Address, Import overlays
    is
        use Virtmem;

        function getP3 is new getNextTable (P4);
        function getP2 is new getNextTable (P3);

        virt    : constant VirtAddress := P2V (physAddr);
        p4Index : constant PageTableIndex := getP4Index (virt);
        p3Index : constant PageTableIndex := getP3Index (virt);
        p2Index : constant PageTableIndex := getP2Index (virt);
        p1Index : constant PageTableIndex := getP1Index (virt);

        p3Addr : PhysAddress;
        p2Addr : PhysAddress;
    begin
        p3Addr := getP3 (kernelP4, p4Index);
        if p3Addr = 0 then
            raise RemapException with
                "createGuardPage: P3 not present";
        end if;

        walkP3 : declare
            myP3 : P3 with Import, Address => To_Address (P2V (p3Addr));
        begin
            p2Addr := getP2 (myP3, p3Index);
            if p2Addr = 0 then
                raise RemapException with
                    "createGuardPage: P2 not present";
            end if;

            walkP2 : declare
                myP2 : P2 with Import, Address => To_Address (P2V (p2Addr));
            begin
                -- If this is a big page, split it into 512 small pages
                if myP2(p2Index).size then
                    splitBigPage : declare
                        -- Physical base of the 2MB region
                        bigBase  : constant PhysAddress :=
                            PFNToAddr (myP2(p2Index).pgNum);

                        -- Preserve flags from big page entry
                        wasWritable      : constant Boolean := myP2(p2Index).writable;
                        wasGlobal        : constant Boolean := myP2(p2Index).global;
                        wasNX            : constant Boolean := myP2(p2Index).NX;
                        wasUser          : constant Boolean := myP2(p2Index).user;
                        wasWriteThrough  : constant Boolean := myP2(p2Index).writeThrough;
                        wasCacheDisabled : constant Boolean := myP2(p2Index).cacheDisabled;

                        -- Allocate a frame for the new P1 table
                        p1Phys : PhysAddress;
                    begin
                        BuddyAllocator.allocFrame (p1Phys);
                        if p1Phys = 0 then
                            raise RemapException with
                                "createGuardPage: cannot allocate P1 frame";
                        end if;

                        fillP1 : declare
                            newP1 : P1 with Import,
                                Address => To_Address (P2V (p1Phys));
                        begin
                            -- Fill 512 P1 entries to map each 4KB sub-page
                            for i in PageTableIndex loop
                                newP1(i).pgNum        := addrToPFN (bigBase) +
                                                          PFN (i);
                                newP1(i).present       := True;
                                newP1(i).writable      := wasWritable;
                                newP1(i).global        := wasGlobal;
                                newP1(i).NX            := wasNX;
                                newP1(i).user          := wasUser;
                                newP1(i).writeThrough  := wasWriteThrough;
                                newP1(i).cacheDisabled := wasCacheDisabled;
                                newP1(i).size          := False;
                            end loop;

                            -- Now clear the guard page entry
                            newP1(p1Index).present := False;
                        end fillP1;

                        -- Update P2 to point to new P1 table instead of big page
                        myP2(p2Index).pgNum   := addrToPFN (p1Phys);
                        myP2(p2Index).size    := False;
                        -- P2 entry pointing to P1 needs USER+WRITABLE+PRESENT
                        -- to be permissive (actual permissions in P1 entries)
                        myP2(p2Index).present  := True;
                        myP2(p2Index).writable := True;
                        myP2(p2Index).user     := False;
                        myP2(p2Index).global   := False;
                        myP2(p2Index).NX       := False;
                    end splitBigPage;
                else
                    -- Already using small pages, just walk to P1
                    clearEntry : declare
                        function getP1 is new getNextTable (P2);
                        p1Addr : constant PhysAddress :=
                            getP1 (myP2, p2Index);
                    begin
                        if p1Addr = 0 then
                            raise RemapException with
                                "createGuardPage: P1 not present";
                        end if;

                        doUnmap : declare
                            myP1 : P1 with Import,
                                Address => To_Address (P2V (p1Addr));
                        begin
                            myP1(p1Index).present := False;
                        end doUnmap;
                    end clearEntry;
                end if;
            end walkP2;
        end walkP3;

        flushTLB;
    end createGuardPage;

    ---------------------------------------------------------------------------
    -- removeGuardPage
    -- Restore a previously unmapped kernel linear page so the buddy
    -- allocator can reuse the frame. Assumes big page was already split.
    ---------------------------------------------------------------------------
    procedure removeGuardPage (physAddr : in Virtmem.PhysAddress) with
        SPARK_Mode => Off  -- uses 'Address, Import overlays
    is
        use Virtmem;

        function getP3 is new getNextTable (P4);
        function getP2 is new getNextTable (P3);
        function getP1 is new getNextTable (P2);

        virt    : constant VirtAddress := P2V (physAddr);
        p4Index : constant PageTableIndex := getP4Index (virt);
        p3Index : constant PageTableIndex := getP3Index (virt);
        p2Index : constant PageTableIndex := getP2Index (virt);
        p1Index : constant PageTableIndex := getP1Index (virt);

        p3Addr : PhysAddress;
        p2Addr : PhysAddress;
        p1Addr : PhysAddress;
    begin
        p3Addr := getP3 (kernelP4, p4Index);
        if p3Addr = 0 then
            raise RemapException with
                "removeGuardPage: P3 not present";
        end if;

        walkP3 : declare
            myP3 : P3 with Import, Address => To_Address (P2V (p3Addr));
        begin
            p2Addr := getP2 (myP3, p3Index);
            if p2Addr = 0 then
                raise RemapException with
                    "removeGuardPage: P2 not present";
            end if;

            walkP2 : declare
                myP2 : P2 with Import, Address => To_Address (P2V (p2Addr));
            begin
                p1Addr := getP1 (myP2, p2Index);
                if p1Addr = 0 then
                    raise RemapException with
                        "removeGuardPage: P1 not present";
                end if;

                restoreEntry : declare
                    myP1 : P1 with Import,
                        Address => To_Address (P2V (p1Addr));
                begin
                    myP1(p1Index) := makePTE (addrToPFN (physAddr),
                                              PG_KERNELDATA);
                end restoreEntry;
            end walkP2;
        end walkP3;

        flushTLB;
    end removeGuardPage;

    ---------------------------------------------------------------------------
    -- switchAddressSpace - make the kernel's page table the active one.
    ---------------------------------------------------------------------------
    procedure switchAddressSpace with
        SPARK_Mode => On
    is
    begin
        -- only switch if its necessary to avoid the TLB flush
        if x86.getCR3 /= Virtmem.K2P(kernelP4'Address) then
            Virtmem.setActiveP4(Virtmem.K2P(kernelP4'Address));
        end if;
    end switchAddressSpace;

    ---------------------------------------------------------------------------
    -- mapKernelMemIntoProcess
    ---------------------------------------------------------------------------
    procedure mapKernelMemIntoProcess (procP4 : in out Virtmem.P4) with
        SPARK_Mode => On
    is
        use Virtmem; -- for PageTableIndex
    begin
        -- Just copy the upper 256 page entries into the process' space.
        for i in PageTableIndex(256) .. PageTableIndex(511) loop
            procP4(i) := kernelP4(i);
        end loop;

        Virtmem.flushTLB;
    end mapKernelMemIntoProcess;

    ---------------------------------------------------------------------------
    -- unmapKernelMemFromProcess
    ---------------------------------------------------------------------------
    procedure unmapKernelMemFromProcess (procP4 : in out Virtmem.P4) with
        SPARK_Mode => On
    is
        use Virtmem; -- for PageTableIndex
    begin
        for i in PageTableIndex(256) .. PageTableIndex(511) loop
            procP4(i) := u64ToPTE(0);
        end loop;

        Virtmem.flushTLB;
    end unmapKernelMemFromProcess;

    ---------------------------------------------------------------------------
    -- Map a single frame of memory-mapped IO region into the higher-half
    -- w/ generic procedure "allocate"
    ---------------------------------------------------------------------------
    function mapIOFrame (addr  : in Virtmem.PhysAddress;
                         flags : in Unsigned_64 := Virtmem.PG_IO) return Boolean
        with SPARK_Mode => On
    is
        procedure mapPage is new Virtmem.mapPage (allocate);
        ok : Boolean;

    begin
        mapPage (addr, virtmem.P2V (addr), flags, kernelP4, ok);
        
        if ok then
            Virtmem.flushTLB;
            return True;
        else
            return False;
        end if;

    end mapIOFrame;

end Mem_mgr;

-- textmode.print(" Mapping ");
-- textmode.println(" pages");
-- textmode.print("srodata: "); textmode.println(srodataPhys);
-- textmode.print("erodata: "); textmode.println(erodataPhys);
-- textmode.print("sdata: "); textmode.println(sdataPhys);
-- textmode.print("edata: "); textmode.println(edataPhys);
-- textmode.print("sbss: "); textmode.println(sbssPhys);
-- textmode.print("ebss: "); textmode.println(ebssPhys);
-- textmode.print("stext: "); textmode.println(stextPhys);
-- textmode.print("etext: "); textmode.println(etextPhys);
-- textmode.print("stack bottom: "); textmode.println(stackBottomPhys);
-- textmode.print("stack top: "); textmode.println(stackTopPhys);