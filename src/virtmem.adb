-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2019 Jon Andrew
--
-- Virtual Memory / Paging Routines
-------------------------------------------------------------------------------
--with System.Address_To_Access_Conversions;
with System.Machine_Code; use System.Machine_Code;

with Textmode; use Textmode;

package body Virtmem
    with SPARK_Mode => On
is

    -- get index into the next level page tables
    function getP4Index(virtAddr : in VirtAddress) return PageTableIndex with
        SPARK_Mode => On is
    begin
        return PageTableIndex(Shift_Right(Unsigned_64(virtAddr), 39) and 16#1FF#);
    end getP4Index;


    function getP3Index(virtAddr : in VirtAddress) return PageTableIndex with
        SPARK_Mode => On is
    begin
        return PageTableIndex(Shift_Right(Unsigned_64(virtAddr), 30) and 16#1FF#);
    end getP3Index;


    function getP2Index(virtAddr : in VirtAddress) return PageTableIndex with
        SPARK_Mode => On is
    begin
        return PageTableIndex(Shift_Right(Unsigned_64(virtAddr), 21) and 16#1FF#);
    end getP2Index;


    function getP1Index(virtAddr : in VirtAddress) return PageTableIndex with
        SPARK_Mode => On is
    begin
        return PageTableIndex(Shift_Right(Unsigned_64(virtAddr), FRAME_SHIFT) and 16#1FF#);
    end getP1Index;


    -- getOffset just returns lowest 12 bits of address
    function getOffset(virtAddr : in VirtAddress) return PageOffset with
        SPARK_Mode => On is
    begin
        return PageOffset(virtAddr and 16#FFF#);
    end getOffset;


    -- see if address is a 48-bit canonical virtual address
    function isCanonical(virtAddr : in VirtAddress) return Boolean with
        SPARK_Mode => On is
        canonical : Boolean;
    begin
        canonical := (virtAddr < 16#0000_8000_0000_0000#) or 
                     (virtAddr >= 16#FFFF_8000_0000_0000#);
        return canonical;
    end isCanonical;


    -- read current PML4 table address from CR3
    function getActiveP4 return PhysAddress with
        SPARK_Mode => Off     -- inline asm
    is
        ret : PhysAddress := 0;
    begin
        Asm("movq %%cr3, %0",
            Outputs => PhysAddress'Asm_Output ("=g", ret),
            Volatile => True);
        return ret;
    end getActiveP4;


    -- load new P4 table address into CR3
    procedure setActiveP4(p4addr : PhysAddress) with
        SPARK_Mode => Off       -- inline asm
    is
        --cr3val : Unsigned_64 := 0;
        --LEFT OFF: Need to get CR3 format right when setting to address
    begin
        Asm("movq %0, %%cr3",
            Inputs => PhysAddress'Asm_Input ("r", p4addr),
            Volatile => True);
    end setActiveP4;


    -- Reload CR3 to flush the TLB
    procedure flushTLB with
        SPARK_Mode => Off       -- inline asm
    is
        use ASCII;
    begin
        Asm("movq %%cr3, %%rax" & LF & HT &
            "movq %%rax, %%cr3",
            Volatile => True,
            Clobber => "rax");
    end flushTLB;


    -- get address from PFN
    function PFNToAddr(page : PFN) return PhysAddress with
        SPARK_Mode => On
    is
    begin
        return PhysAddress(Shift_Left(page, FRAME_SHIFT));
    end PFNToAddr;


    -- get addr from big PFN
    function bigPFNToAddr(page : BigPFN) return PhysAddress with
        SPARK_Mode => On
    is
    begin
        return PhysAddress(Shift_Left(page, BIG_FRAME_SHIFT));
    end bigPFNToAddr;


    -- get PFN from addr
    function addrToPFN(addr : PhysAddress) return PFN with
        SPARK_Mode => On
    is
    begin
        return PFN(Shift_Right(Unsigned_64(addr), FRAME_SHIFT));
    end addrToPFN;


    -- get big PFN from addr
    function addrToBigPFN(addr : PhysAddress) return BigPFN with
        SPARK_Mode => On
    is
    begin
        return BigPFN(Shift_Right(Unsigned_64(addr), BIG_FRAME_SHIFT));
    end addrToBigPFN;


    -- convert between big and small PFNs
    function bigPFNToPFN(bigPage : in BigPFN) return PFN with
        SPARK_Mode => On
    is
        shiftDiff : constant Natural := BIG_FRAME_SHIFT - FRAME_SHIFT;
    begin
        return PFN(Shift_Left(Unsigned_64(bigPage), shiftDiff));
    end bigPFNToPFN;


    -- convert between big and small PFNs
    function PFNToBigPFN(page : in PFN) return BigPFN with
        SPARK_Mode => On
    is
        shiftDiff : constant Natural := BIG_FRAME_SHIFT - FRAME_SHIFT;
    begin
        return BigPFN(Shift_Right(Unsigned_64(page), shiftDiff));
    end PFNToBigPFN;


    -- clear out a new table
    procedure zeroize(table : in out PN) with
        SPARK_Mode => On
    is
    begin
        for i in table'Range loop
            table(i) := u64ToPTE(0);
        end loop;
    end zeroize;


    -- get next level table
    function getNextTable(table : in PN; index : PageTableIndex)
        return PhysAddress
        with SPARK_Mode => On
    is
    begin
        -- if the size bit is 1, then there is no next table.
        if table(index).size = True then
            raise PageTableLevelException with
                "Attempted page table access where parent table maps big or huge pages";
        end if;

        if table(index).present then
            return pfnToAddr(table(index).pgNum);
        end if;

        return 0;
    end getNextTable;


    -- generic discriminants: PN, allocate, flags
    procedure createNextTable(  table       : in out PN;
                                index       : in PageTableIndex;
                                tableAddr   : out PhysAddress)
        with SPARK_Mode => On
    is
        function getPM is new getNextTable(PN);

        addrNext : PhysAddress;
    begin
        -- if the size bit is 1, then there is no next table.
        if table(index).size = True then
            raise PageTableLevelException with
                "Attempted page table access where parent maps big or huge pages";
        end if;

        addrNext := getPM(table, index);

        if (addrNext /= 0) then
            -- table exists, return it.
            tableAddr := addrNext;
        else

            -- table doesn't exist, so allocate it and make a new
            -- PTE to point to it. We assume that we're allocating from
            -- a mapped and accessible linear range.
            allocate(addrNext);
            --print("zeroizing "); printd(Unsigned_32(PN'Size) / 8); print(" bytes at ");
            --println(addrNext);
            Util.memset(To_Address(P2V(addrNext)), 0, PN'Size / 8);

            if(addrNext /= 0) then
                
                table(index) := makePTE(addrToPFN(addrNext), flags);
                tableAddr := addrNext;
                return;
            end if;

            -- uh oh, couldn't allocate memory for this page.
            tableAddr := 0;
        end if;
    end createNextTable;


    -- map 4k page
    procedure mapPage(  phys    : in PhysAddress;
                        virt    : in VirtAddress;
                        flags   : in Unsigned_64;
                        myP4    : in out P4;
                        success : out Boolean)
        with SPARK_Mode => On
    is
        -- We set 
        procedure createP3 is new createNextTable(P4, allocate, PG_USERCODE);
        procedure createP2 is new createNextTable(P3, allocate, PG_USERCODE);
        procedure createP1 is new createNextTable(P2, allocate, PG_USERCODE);

        p4Index : constant PageTableIndex := getP4Index(virt);
        p3Index : constant PageTableIndex := getP3Index(virt);
        p2Index : constant PageTableIndex := getP2Index(virt);
        p1Index : constant PageTableIndex := getP1Index(virt);

        p3Addr : PhysAddress;
        p2Addr : PhysAddress;
        p1Addr : PhysAddress;

        physPFN : constant PFN := addrToPFN(phys);
    begin
        -- if phys > 16#0000000000100000# and phys < 16#000000000011F000# then
        --     print("mapping page from "); print(phys); print(" to "); println(virt);
        -- end if;
        --print("mapping page from "); print(phys); print(" to "); println(virt);

        createP3(myP4, p4Index, p3Addr);

        if p3Addr /= 0 then
            doP3 : declare
                myP3 : P3 with 
                    Import, Address => To_Address(P2V(p3Addr));
            begin
                createP2(myP3, p3Index, p2Addr);
                
                if p2Addr /= 0 then
                    doP2 : declare
                        myP2 : P2 with 
                            Import, Address => To_Address(P2V(p2Addr));
                    begin
                        createP1(myP2, p2Index, p1Addr);

                        if p1Addr /= 0 then
                            addNewEntry : declare
                                myP1 : P1 with 
                                    Import, Address => To_Address(P2V(p1Addr));
                            begin
                                myP1(p1Index) := makePTE(physPFN, flags);

                                success := True;
                                return;
                            end addNewEntry;
                        end if;
                    end doP2;
                end if;
            end doP3;
        end if;

        success := False;
    end mapPage;


    procedure mapBigPage(   phys    : in PhysAddress;
                            virt    : in VirtAddress;
                            flags   : in Unsigned_64;
                            myP4    : in out P4;
                            success : out Boolean)
        with SPARK_Mode => On
    is
        procedure createP3 is new createNextTable(P4, allocate, PG_USERCODE);
        procedure createP2 is new createNextTable(P3, allocate, PG_USERCODE);
        --procedure createP1 is new createNextTable(P2, allocate);

        p4Index : constant PageTableIndex := getP4Index(virt);
        p3Index : constant PageTableIndex := getP3Index(virt);
        p2Index : constant PageTableIndex := getP2Index(virt);
        --p1Index : constant PageTableIndex := getP1Index(virt);

        p3Addr : PhysAddress;
        p2Addr : PhysAddress;
        --p1Addr : PhysAddress;

        physPFN : constant BigPFN := addrToBigPFN(phys);

        -- The P2 table expects "4k" PFNs, where the lower 8 bits of the phys base
        -- address is 0.
        adjPFN  : constant BigPFN := BigPFN(Shift_Left(physPFN, 9));
    begin
        -- if phys > 16#0F00000# and phys < 16#1100000# then
        --     print("mapping big page from "); print(phys); print(" to "); println(virt);
        -- end if;
        createP3(myP4, p4Index, p3Addr);

        if p3Addr /= 0 then
            doP3 : declare
                myP3 : P3 with 
                    Import, Address => To_Address(P2V(p3Addr));
            begin
                createP2(myP3, p3Index, p2Addr);
                
                if p2Addr /= 0 then
                    doP2 : declare
                        myP2 : P2 with 
                            Import, Address => To_Address(P2V(p2Addr));
                    begin
                        --print(" Creating huge page table with flags: "); println(flags or PG_HUGE);
                        myP2(p2Index) := makeBigPTE(adjPFN, flags);
                        
                        success := True;
                        return;
                    end doP2;
                end if;
            end doP3;
        end if;

        success := False;
    end mapBigPage;


    -- Walk the page table and mark it as non-present. This preserves the
    -- address, so later on we can free up the memory used by the entry.
    procedure unmapPage(virt    : in VirtAddress;
                        myP4    : in P4;
                        success : out Boolean)
        with SPARK_Mode => On
    is
        function getP3 is new getNextTable(P4);
        function getP2 is new getNextTable(P3);
        function getP1 is new getNextTable(P2);

        p4Index : constant PageTableIndex := getP4Index(virt);
        p3Index : constant PageTableIndex := getP3Index(virt);
        p2Index : constant PageTableIndex := getP2Index(virt);
        p1Index : constant PageTableIndex := getP1Index(virt);

        p3Addr : PhysAddress;
        p2Addr : PhysAddress;
        p1Addr : PhysAddress;
    begin
        p3Addr := getP3(myP4, p4Index);

        if p3Addr /= 0 then
            doP3 : declare
                myP3 : aliased P3 with Import, 
                            Address => To_Address(P2V(p3Addr));
            begin
                -- TODO: if we get around to supporting huge pages, then check
                -- the .huge flag here, mark p3 as non-present, return.

                p2Addr := getP2(myP3, p3Index);
                
                if p2Addr /= 0 then
                    doP2 : declare
                        myP2 : aliased P2 with Import, 
                                    Address => To_Address(P2V(p2Addr));
                    begin

                        -- If this P2 maps a big page, mark it as non-present,
                        --  no subtable to mess with.
                        if myP2(p2Index).size then
                            myP2(p2Index).present := False;
                            success := True;
                            return;
                        end if;

                        p1Addr := getP1(myP2, p2Index);

                        if p1Addr /= 0 then
                            addNewEntry : declare
                                myP1 : aliased P1 with Import,
                                            Address => To_Address(P2V(p1Addr));
                            begin
                                myP1(p1Index).present := False;

                                success := True;
                                return;
                            end addNewEntry;
                        end if;
                    end doP2;
                end if;
            end doP3;
        end if;

        success := False;
    end unmapPage;


    -- tableWalk, for arbitrarily-mapped process memory
    function tableWalk(virt : in VirtAddress; myP4 : in P4)
        return PhysAddress
        with SPARK_Mode => On
    is
        function getP3 is new getNextTable(P4);
        function getP2 is new getNextTable(P3);
        function getP1 is new getNextTable(P2);

        p4Index : constant PageTableIndex := getP4Index(virt);
        p3Index : constant PageTableIndex := getP3Index(virt);
        p2Index : constant PageTableIndex := getP2Index(virt);
        p1Index : constant PageTableIndex := getP1Index(virt);

        p3Addr : PhysAddress;
        p2Addr : PhysAddress;
        p1Addr : PhysAddress;
    begin
        p3Addr := getP3(myP4, p4Index);
        --print("newMapPage: P3"); println(p3Addr);

        if p3Addr /= 0 then
            doP3 : declare
                myP3 : P3 with
                    Import, Address => To_Address(P2V(p3Addr));
            begin
                p2Addr := getP2(myP3, p3Index);
                
                if p2Addr /= 0 then
                    doP2 : declare
                        myP2 : P2 with
                            Import, Address => To_Address(P2V(p2Addr));
                    begin
                        p1Addr := getP1(myP2, p2Index);

                        if p1Addr /= 0 then
                            addNewEntry : declare
                                myP1 : P1 with
                                    Import, Address => To_Address(P2V(p1Addr));
                            begin
                                return pfnToAddr(myP1(p1Index).pgNum);
                            end addNewEntry;
                        end if;
                    end doP2;
                end if;
            end doP3;
        end if;

        return PhysAddress(0);
    end tableWalk;


    -- zeroize all P1 subtables then free them
    procedure deleteP2(myP2 : in out P2) with
        SPARK_Mode => On
    is
        function getP1      is new getNextTable(P2);
        procedure zeroize   is new Virtmem.zeroize(P1);

        p1Addr : PhysAddress;
    begin
        for p2Index in PageTableIndex'Range loop
            
            -- if this P2 maps a big page, then no P1s under it to free.
            if myP2(p2Index).size = False then
                p1Addr := getP1(myP2, p2Index);

                if p1Addr /= 0 then
                    doP1 : declare
                        myP1 : P1 with
                            Import, Address => To_Address(P2V(p1Addr));
                    begin
                        zeroize(myP1);
                        free(p1Addr);
                    end doP1;
                end if;
            end if;
        end loop;
    end deleteP2;


    -- recursively delete all p2 tables, then zeroize and free them
    procedure deleteP3(myP3 : in out P3) with
        SPARK_Mode => On
    is
        function getP2      is new getNextTable(P3);
        procedure deleteP2  is new Virtmem.deleteP2(free);
        procedure zeroize   is new Virtmem.zeroize(P2);

        p2Addr : PhysAddress;
    begin
        for p3Index in PageTableIndex'Range loop
            
            p2Addr := getP2(myP3, p3Index);

            if p2Addr /= 0 then
                doP2 : declare
                    myP2 : P2 with
                        Import, Address => To_Address(P2V(p2Addr));
                begin
                    deleteP2(myP2);
                    zeroize(myP2);
                    free(p2Addr);
                end doP2;
            end if;
        end loop;
    end deleteP3;


    -- Iterate through full structure, deleting all sub-tables
    procedure deleteP4(myP4 : in out P4) with
        SPARK_Mode => On
    is
        function getP3      is new getNextTable(P4);
        procedure deleteP3  is new Virtmem.deleteP3(free);
        procedure zeroize   is new Virtmem.zeroize(P3);

        p3Addr : PhysAddress;
    begin
        for p4Index in PageTableIndex'Range loop
            
            p3Addr := getP3(myP4, p4Index);

            if p3Addr /= 0 then
                doP3 : declare
                    myP3 : P3 with
                        Import, Address => To_Address(P2V(p3Addr));
                begin
                    deleteP3(myP3); -- recursively delete subtables
                    zeroize(myP3);  -- clear it out
                    free(p3Addr);   -- free the physical mem it lives at
                end doP3;
            end if;
        end loop;
    end deleteP4;


    -- return a PTE with the PFN and flags set
    function makePTE(frame : in PFN; flags : in Unsigned_64) 
        return PageTableEntry 
        with SPARK_Mode => On
    is
        newPTE : PageTableEntry;
    begin
        newPTE.pgNum            := frame;

        newPTE.present          := (flags and PG_PRESENT) /= 0;
        newPTE.writable         := (flags and PG_WRITABLE) /= 0;
        newPTE.user             := (flags and PG_USER) /= 0;
        newPTE.writeThrough     := (flags and PG_WRITETHROUGH) /= 0;
        newPTE.cacheDisabled    := (flags and PG_CACHEDISABLED) /= 0;
        newPTE.accessed         := (flags and PG_ACCESSED) /= 0;
        newPTE.dirty            := (flags and PG_DIRTY) /= 0;
        newPTE.size             := (flags and PG_SIZE) /= 0;
        newPTE.global           := (flags and PG_GLOBAL) /= 0;
        newPTE.NX               := (flags and PG_NXE) /= 0;

        return newPTE;
    end makePTE;


    -- return a Big PTE with the PFN and flags set
    function makeBigPTE(bigFrame : in BigPFN; flags : in Unsigned_64)
        return PageTableEntry 
        with SPARK_Mode => On
    is   
        function toPFN is new Ada.Unchecked_Conversion(BigPFN, PFN);
    begin
        return makePTE(toPFN(bigFrame), flags or PG_SIZE);
    end makeBigPTE;
    
    ---------------------------------------------------------------------------
    -- These next two functions rely on the fact that we "identity-map" all
    -- physical memory into the upper-half with a fixed-offset of LINEAR_BASE.
    ---------------------------------------------------------------------------
    -- return physical address 
    function V2P(virtAddr : in VirtAddress) return PhysAddress
        with SPARK_Mode => On is
    begin
        return virtAddr - LINEAR_BASE;
    end V2P;


    -- return virtual address 
    function P2V(physAddr : in PhysAddress) return VirtAddress
        with SPARK_Mode => On is
    begin
        return physAddr + LINEAR_BASE;
    end P2V;


    ---------------------------------------------------------------------------
    -- To be used to get physical address of any variable we're using here in
    -- the kernel.
    ---------------------------------------------------------------------------
    function K2P(kernAddr : in VirtAddress) return PhysAddress
        with SPARK_Mode => On is
    begin
        return kernAddr - KERNEL_BASE;
    end K2P;
end virtmem;