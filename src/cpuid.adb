-------------------------------------------------------------------------------
-- Fortress OS
-- Copyright (C) 2019 Jon Andrew
--
-- x86-64 CPUID functionality
-------------------------------------------------------------------------------
with System.Machine_Code; use System.Machine_Code;

with textmode; use textmode;
with util;
with x86;

package body cpuid
    with SPARK_Mode => On
is

    maxStdLeaf : Unsigned_32;
    maxExtLeaf : Unsigned_32;

    ---------------------------------------------------------------------------
    -- printCacheInfo
    ---------------------------------------------------------------------------
    procedure printCacheInfo with
        SPARK_Mode => On
    is
        foundAny : Boolean := False;
    begin
        println("Cache Configuration: ");
        for i in systemCaches'Range loop
            if systemCaches(i).Kind = Cache then
                foundAny := True;
                printd(systemCaches(i).Size);
                print("k ");
                case systemCaches(i).Level is
                    when L1DATA =>
                        print("L1 Data Cache");
                    when L1INST =>
                        print("L1 Instruction Cache");
                    when L2 =>
                        print("L2 Cache");
                    when L3 =>
                        print("L3 Cache");
                    when others =>
                        print("?");
                end case;
                println;
            end if;
        end loop;

        if not foundAny then
            println("No caches found (VM?)");
        end if;
    end printCacheInfo;

    ---------------------------------------------------------------------------
    -- Given a cache descriptor # from the table in the manual, determine the
    --  details of the cache. Yes, this is painful.
    ---------------------------------------------------------------------------
    function lookupCacheDescriptor(val : Unsigned_8) return CacheDescriptor
        with SPARK_Mode => On
    is
    begin
        case val is
            when 16#00# =>
                return (Kind => NOTPRESENT, others => <>);
            when 16#01# =>
                return (Kind => TLB, Level => ITLB, Size => 4, Associativity => 4, BlockSize => 32);
            when 16#02# =>
                return (Kind => TLB, Level => ITLB, Size => 4096, Associativity => 0, BlockSize => 2);
            when 16#03# =>
                return (Kind => TLB, Level => DTLB, Size => 4, Associativity => 4, BlockSize => 64);
            when 16#04# =>
                return (Kind => TLB, Level => DTLB, Size => 4096, Associativity => 4, BlockSize => 8);
            when 16#05# =>
                return (Kind => TLB, Level => DTLB, Size => 4096, Associativity => 4, BlockSize => 32);
            when 16#06# =>
                return (Kind => Cache, Level => L1INST, Size => 8, Associativity => 4, BlockSize => 32);
            when 16#08# =>
                return (Kind => Cache, Level => L1INST, Size => 16, Associativity => 4, BlockSize => 32);
            when 16#09# =>
                return (Kind => Cache, Level => L1INST, Size => 32, Associativity => 4, BlockSize => 64);
            when 16#0A# =>
                return (Kind => Cache, Level => L1DATA, Size => 8, Associativity => 2, BlockSize => 32);
            when 16#0B# =>
                return (Kind => TLB, Level => ITLB, Size => 4096, Associativity => 4, BlockSize => 4);
            when 16#0C# =>
                return (Kind => Cache, Level => L1DATA, Size => 16, Associativity => 4, BlockSize => 32);
            when 16#0D# =>
                return (Kind => Cache, Level => L1DATA, Size => 16, Associativity => 4, BlockSize => 64);
            when 16#0E# =>
                return (Kind => Cache, Level => L1DATA, Size => 24, Associativity => 6, BlockSize => 64);
            when 16#1D# =>
                return (Kind => Cache, Level => L2, Size => 128, Associativity => 2, BlockSize => 64);
            when 16#21# =>
                return (Kind => Cache, Level => L2, Size => 256, Associativity => 8, BlockSize => 64);
            when 16#22# =>
                return (Kind => Cache, Level => L3, Size => 512, Associativity => 4, BlockSize => 64);
            when 16#23# =>
                return (Kind => Cache, Level => L3, Size => 1024, Associativity => 8, BlockSize => 64);
            when 16#24# =>
                return (Kind => Cache, Level => L2, Size => 1024, Associativity => 1, BlockSize => 64);
            when 16#25# =>
                return (Kind => Cache, Level => L3, Size => 2048, Associativity => 8, BlockSize => 64);
            when 16#29# =>
                return (Kind => Cache, Level => L3, Size => 4096, Associativity => 8, BlockSize => 64);
            when 16#2C# =>
                return (Kind => Cache, Level => L1DATA, Size => 32, Associativity => 8, BlockSize => 64);
            when 16#30# =>
                return (Kind => Cache, Level => L1INST, Size => 32, Associativity => 8, BlockSize => 64);
            when 16#40# =>      -- No L2, or no L3 if L2 present.
                return (Kind => NOTPRESENT, Level => NA, others => <>);
            when 16#41# =>
                return (Kind => Cache, Level => L2, Size => 128, Associativity => 4, BlockSize => 32);
            when 16#42# =>
                return (Kind => Cache, Level => L2, Size => 256, Associativity => 4, BlockSize => 32);
            when 16#43# =>
                return (Kind => Cache, Level => L2, Size => 512, Associativity => 4, BlockSize => 32);
            when 16#44# =>
                return (Kind => Cache, Level => L2, Size => 1024, Associativity => 4, BlockSize => 32);
            when 16#45# =>
                return (Kind => Cache, Level => L2, Size => 2048, Associativity => 4, BlockSize => 32);
            when 16#46# =>
                return (Kind => Cache, Level => L3, Size => 4096, Associativity => 4, BlockSize => 64);
            when 16#47# =>
                return (Kind => Cache, Level => L3, Size => 8192, Associativity => 8, BlockSize => 64);
            when 16#48# =>
                return (Kind => Cache, Level => L2, Size => 3072, Associativity => 12, BlockSize => 64);
            when 16#049# =>     -- Special case: Xeon MP
                if leaf1eax.cpuFamilyID = 16#0F# and leaf1eax.cpuModel = 16#06# then
                    return (Kind => Cache, Level => L2, Size => 4096,
                            BlockSize => 64, Associativity => 16);
                else
                    return (Kind => Cache, Level => L3, Size => 4096,
                            BlockSize => 64, Associativity => 16);
                end if;
            when 16#4A# =>
                return (Kind => Cache, Level => L3, Size => 6144, Associativity => 12, BlockSize => 64);
            when 16#4B# =>
                return (Kind => Cache, Level => L3, Size => 8192, Associativity => 16, BlockSize => 64);
            when 16#4C# =>
                return (Kind => Cache, Level => L3, Size => 12288, Associativity => 12, BlockSize => 64);
            when 16#4D# =>
                return (Kind => Cache, Level => L3, Size => 16144, Associativity => 16, BlockSize => 64);
            when 16#4E# =>
                return (Kind => Cache, Level => L2, Size => 6144, Associativity => 24, BlockSize => 64);
            when 16#4F# =>
                return (Kind => TLB, Level => ITLB, Size => 4, Associativity => 0, BlockSize => 32);
            when 16#50# =>
                return (Kind => TLB, Level => ITLB, Size => 4, Associativity => 0, BlockSize => 64);
            when 16#51# =>
                return (Kind => TLB, Level => ITLB, Size => 4, Associativity => 0, BlockSize => 128);
            when 16#52# =>
                return (Kind => TLB, Level => ITLB, Size => 4, Associativity => 0, BlockSize => 256);
            when 16#55# =>
                return (Kind => TLB, Level => ITLB, Size => 2048, Associativity => 0, BlockSize => 7);
            when 16#56# =>
                return (Kind => TLB, Level => DTLB0, Size => 4096, Associativity => 4, BlockSize => 16);
            when 16#57# =>
                return (Kind => TLB, Level => DTLB0, Size => 4, Associativity => 4, BlockSize => 16);
            when 16#59# =>
                return (Kind => TLB, Level => DTLB0, Size => 4, Associativity => 0, BlockSize => 16);
            when 16#5A# =>
                return (Kind => TLB, Level => DTLB0, Size => 2048, Associativity => 4, BlockSize => 32);
            when 16#5B# =>
                return (Kind => TLB, Level => DTLB, Size => 4, Associativity => 0, BlockSize => 64);
            when 16#5C# =>
                return (Kind => TLB, Level => DTLB, Size => 4, Associativity => 0, BlockSize => 128);
            when 16#5D# =>
                return (Kind => TLB, Level => DTLB, Size => 4, Associativity => 0, BlockSize => 256);
            when 16#60# =>
                return (Kind => Cache, Level => L1DATA, Size => 16, Associativity => 8, BlockSize => 64);
            when 16#61# =>
                return (Kind => TLB, Level => ITLB, Size => 4, Associativity => 0, BlockSize => 48);
            when 16#63# =>
                return (Kind => TLB, Level => DTLB, Size => 2048, Associativity => 4, BlockSize => 32);
            when 16#64# =>
                return (Kind => TLB, Level => DTLB, Size => 4, Associativity => 4, BlockSize => 512);
            when 16#66# =>
                return (Kind => Cache, Level => L1DATA, Size => 8, Associativity => 4, BlockSize => 64);
            when 16#67# =>
                return (Kind => Cache, Level => L1DATA, Size => 16, Associativity => 4, BlockSize => 64);
            when 16#68# =>
                return (Kind => Cache, Level => L1DATA, Size => 32, Associativity => 4, BlockSize => 64);
            when 16#6A# =>
                return (Kind => Cache, Level => uTLB, Size => 4, Associativity => 8, BlockSize => 64);
            when 16#6B# =>
                return (Kind => Cache, Level => DTLB, Size => 4, Associativity => 8, BlockSize => 256);
            when 16#6C# =>
                return (Kind => Cache, Level => DTLB, Size => 2048, Associativity => 8, BlockSize => 128);
            when 16#6D# =>
                return (Kind => Cache, Level => DTLB, Size => 1048576, Associativity => 0, BlockSize => 16);
            when 16#70# =>
                return (Kind => Trace, Level => Trace, Size => 12, Associativity => 8, BlockSize => 0);
            when 16#71# =>
                return (Kind => Trace, Level => Trace, Size => 16, Associativity => 8, BlockSize => 0);
            when 16#72# =>
                return (Kind => Trace, Level => Trace, Size => 32, Associativity => 8, BlockSize => 0);
            when 16#76# =>
                return (Kind => TLB, Level => ITLB, Size => 2048, Associativity => 0, BlockSize => 8);
            when 16#78# =>
                return (Kind => Cache, Level => L2, Size => 1024, Associativity => 4, BlockSize => 64);
            when 16#79# =>
                return (Kind => Cache, Level => L2, Size => 128, Associativity => 8, BlockSize => 64);
            when 16#7A# =>
                return (Kind => Cache, Level => L2, Size => 256, Associativity => 8, BlockSize => 64);
            when 16#7B# =>
                return (Kind => Cache, Level => L2, Size => 512, Associativity => 8, BlockSize => 64);
            when 16#7C# =>
                return (Kind => Cache, Level => L2, Size => 1024, Associativity => 8, BlockSize => 64);
            when 16#7D# =>
                return (Kind => Cache, Level => L2, Size => 2048, Associativity => 8, BlockSize => 64);
            when 16#7F# =>
                return (Kind => Cache, Level => L2, Size => 512, Associativity => 2, BlockSize => 64);
            when 16#80# =>
                return (Kind => Cache, Level => L2, Size => 512, Associativity => 8, BlockSize => 64);
            when 16#82# =>
                return (Kind => Cache, Level => L2, Size => 256, Associativity => 8, BlockSize => 32);
            when 16#83# =>
                return (Kind => Cache, Level => L2, Size => 512, Associativity => 8, BlockSize => 32);
            when 16#84# =>
                return (Kind => Cache, Level => L2, Size => 1024, Associativity => 8, BlockSize => 32);
            when 16#85# =>
                return (Kind => Cache, Level => L2, Size => 2048, Associativity => 8, BlockSize => 32);
            when 16#86# =>
                return (Kind => Cache, Level => L2, Size => 512, Associativity => 4, BlockSize => 64);
            when 16#87# =>
                return (Kind => Cache, Level => L2, Size => 1024, Associativity => 8, BlockSize => 64);
            when 16#A0# =>
                return (Kind => DTLB, Level => DTLB, Size => 4, Associativity => 0, BlockSize => 32);
            when 16#B0# =>
                return (Kind => TLB, Level => ITLB, Size => 4, Associativity => 4, BlockSize => 128);
            when 16#B1# =>
                return (Kind => TLB, Level => ITLB, Size => 2048, Associativity => 4, BlockSize => 8);
            when 16#B2# =>
                return (Kind => TLB, Level => ITLB, Size => 4, Associativity => 4, BlockSize => 64);
            when 16#B3# =>
                return (Kind => TLB, Level => DTLB, Size => 4, Associativity => 4, BlockSize => 128);
            when 16#B4# =>
                return (Kind => TLB, Level => DTLB, Size => 4, Associativity => 4, BlockSize => 256);
            when 16#B5# =>
                return (Kind => TLB, Level => ITLB, Size => 4, Associativity => 8, BlockSize => 64);
            when 16#B6# =>
                return (Kind => TLB, Level => ITLB, Size => 4, Associativity => 8, BlockSize => 128);
            when 16#BA# =>
                return (Kind => TLB, Level => DTLB, Size => 4, Associativity => 4, BlockSize => 64);
            when 16#C0# =>
                return (Kind => TLB, Level => DTLB, Size => 4, Associativity => 4, BlockSize => 8);
            when 16#C1# =>
                return (Kind => STLB, Level => L2, Size => 4, Associativity => 8, BlockSize => 1024);
            when 16#C2# =>
                return (Kind => DTLB, Level => DTLB, Size => 4, Associativity => 4, BlockSize => 16);
            when 16#C3# =>
                return (Kind => STLB, Level => L2, Size => 4, Associativity => 6, BlockSize => 1536);
            when 16#C4# =>
                return (Kind => DTLB, Level => DTLB, Size => 2048, Associativity => 4, BlockSize => 32);
            when 16#CA# =>
                return (Kind => STLB, Level => L2, Size => 4, Associativity => 4, BlockSize => 512);
            when 16#D0# =>
                return (Kind => Cache, Level => L3, Size => 512, Associativity => 4, BlockSize => 64);
            when 16#D1# =>
                return (Kind => Cache, Level => L3, Size => 1024, Associativity => 4, BlockSize => 64);
            when 16#D2# =>
                return (Kind => Cache, Level => L3, Size => 2048, Associativity => 4, BlockSize => 64);
            when 16#D6# =>
                return (Kind => Cache, Level => L3, Size => 1024, Associativity => 8, BlockSize => 64);
            when 16#D7# =>
                return (Kind => Cache, Level => L3, Size => 2048, Associativity => 8, BlockSize => 64);
            when 16#D8# =>
                return (Kind => Cache, Level => L3, Size => 4096, Associativity => 8, BlockSize => 64);
            when 16#DC# =>
                return (Kind => Cache, Level => L3, Size => 1540, Associativity => 12, BlockSize => 64);
            when 16#DD# =>
                return (Kind => Cache, Level => L3, Size => 3072, Associativity => 12, BlockSize => 64);
            when 16#DE# =>
                return (Kind => Cache, Level => L3, Size => 6144, Associativity => 12, BlockSize => 64);
            when 16#E2# =>
                return (Kind => Cache, Level => L3, Size => 2048, Associativity => 16, BlockSize => 64);
            when 16#E3# =>
                return (Kind => Cache, Level => L3, Size => 4096, Associativity => 16, BlockSize => 64);
            when 16#E4# =>
                return (Kind => Cache, Level => L3, Size => 8192, Associativity => 16, BlockSize => 64);
            when 16#EA# =>
                return (Kind => Cache, Level => L3, Size => 12288, Associativity => 24, BlockSize => 64);
            when 16#EB# =>
                return (Kind => Cache, Level => L3, Size => 18192, Associativity => 24, BlockSize => 64);
            when 16#EC# =>
                return (Kind => Cache, Level => L3, Size => 24576, Associativity => 24, BlockSize => 64);
            when 16#F0# =>
                return (Kind => Prefetch, Size => 64, others => <>);
            when 16#F1# =>
                return (Kind => Prefetch, Size => 128, others => <>);            
            when others =>
                return (Kind => NOTPRESENT, others => <>);
        end case;
    end lookupCacheDescriptor;

    -- Pull cache descriptor info returned from CPUID:02h
    procedure getCacheDescriptors(caches : out CacheDescriptors;
                                  eax, ebx, ecx, edx : in Unsigned_32) 
    with SPARK_Mode => On is
        use util;   -- isBitSet, getByte
    begin
        -- if no cache info is set (like in some VMs), just set null
        -- values initially
        caches := (others => (Kind => NOTPRESENT, Level => NA, others => <>));

        -- Each reg eax, ebx, ecx and edx has potentially 4 descriptors
        -- If bit 31 is clear, then reg contains TLB/Cache data
        if not isBitSet(eax, 31) then
            for i in 0..3 loop
                caches(i) := lookupCacheDescriptor(getByte(eax, i));
            end loop;
        end if;

        if not isBitSet(ebx, 31) then
            for i in 0..3 loop
                caches(i+4) := lookupCacheDescriptor(getByte(ebx, i));
            end loop;
        end if;

        if not isBitSet(ecx, 31) then
            for i in 0..3 loop
                caches(i+8) := lookupCacheDescriptor(getByte(ecx, i));
            end loop;
        end if;

        if not isBitSet(edx, 31) then
            for i in 0..3 loop
                caches(i+12) := lookupCacheDescriptor(getByte(edx, i));
            end loop;
        end if;
    end getCacheDescriptors;

    ---------------------------------------------------------------------------
    -- charAt - helper function to get the ASCII value at position N within
    -- one of the CPUID registers.
    ---------------------------------------------------------------------------
    -- function charAt(val : Unsigned_32; pos : Natural range 0..3) return Character
    -- is
    --     shift : constant Integer := Integer(pos * 8);
    --     mask : constant Unsigned_32 := Shift_Left(16#FF#, shift);
    -- begin
    --     return Character'Val(Shift_Right(val and mask, shift));
    -- end charAt;

    ---------------------------------------------------------------------------
    -- CPUID function 0x0000_0000
    ---------------------------------------------------------------------------
    procedure getVendor(vendor : out CpuVendorString) with
        SPARK_Mode => On
    is
        -- vendor ID string returned in 3 registers, EBX & EDX & ECX
        unused  : Unsigned_32;
        ebx     : Unsigned_32;
        ecx     : Unsigned_32;
        edx     : Unsigned_32;
    begin
        cpuid(0, unused, ebx, ecx, edx);
        
        for i in 0..3 loop
            vendor(i+1) := Character'Val(util.getByte(ebx, i));
            vendor(i+5) := Character'Val(util.getByte(edx, i));
            vendor(i+9) := Character'Val(util.getByte(ecx, i));
        end loop;
    end getVendor;

    ---------------------------------------------------------------------------
    -- CPUID function 0x0000_0000 returns the max CPUID value allowed
    ---------------------------------------------------------------------------
    function getMaxStandardFunction return Unsigned_32 with
        SPARK_Mode => On
    is
        eax     : Unsigned_32;
        ignore1 : Unsigned_32;
        ignore2 : Unsigned_32;
        ignore3 : Unsigned_32;
    begin
        cpuid(0, eax, ignore1, ignore2, ignore3);
        return eax;
    end getMaxStandardFunction;

    ---------------------------------------------------------------------------
    -- CPUID function 0x8000_0000 returns the max extended CPUID value allowed
    ---------------------------------------------------------------------------
    function getMaxExtendedFunction return Unsigned_32 with
        SPARK_Mode => On
    is
        eax     : Unsigned_32;
        ignore1 : Unsigned_32;
        ignore2 : Unsigned_32;
        ignore3 : Unsigned_32;
    begin
        cpuid(16#8000_0000#, eax, ignore1, ignore2, ignore3);
        return eax;
    end getMaxExtendedFunction;

    ---------------------------------------------------------------------------
    -- CPUID function 0x8000_00002-4
    ---------------------------------------------------------------------------
    procedure getBrandString(brand : out CpuBrandString) with
        SPARK_Mode => On
    is
        eax : Unsigned_32;
        ebx : Unsigned_32;
        ecx : Unsigned_32;
        edx : Unsigned_32;
    begin
        for i in 0..2 loop
            cpuid(16#8000_0002# + Unsigned_32(i), eax, ebx, ecx, edx);

            for j in 0..3 loop
                brand(i*16 + j+1)  := Character'Val(util.getByte(eax, j));
                brand(i*16 + j+5)  := Character'Val(util.getByte(ebx, j));
                brand(i*16 + j+9)  := Character'Val(util.getByte(ecx, j));
                brand(i*16 + j+13) := Character'Val(util.getByte(edx, j));
            end loop;
        end loop;
    end getBrandString;

    ---------------------------------------------------------------------------
    -- Get whatever available information we can make use of.
    ---------------------------------------------------------------------------
    procedure setupCPUID with
        SPARK_Mode => On
    is 
        eax : Unsigned_32;
        ebx : Unsigned_32;
        ecx : Unsigned_32;
        edx : Unsigned_32;
        unused : Unsigned_32;
        unused1 : Unsigned_32;
        unused2 : Unsigned_32;
    begin
        -- Get CPU vendor and max supported CPUID data from CPUID:00h
        getVendor(cpuVendor);
        maxStdLeaf := getMaxStandardFunction;
        maxExtLeaf := getMaxExtendedFunction;

        -- Get basic features, CPUID:01h
        cpuid(1, eax, ebx, ecx, edx);
        leaf1eax := toRecord(eax);
        leaf1ebx := toRecord(ebx);
        leaf1ecx := toRecord(ecx);
        leaf1edx := toRecord(edx);

        -- If Leaf 2 was available, get cache descriptors
        if maxStdLeaf >= 2 then
            cpuid(2, eax, ebx, ecx, edx);
            getCacheDescriptors(systemCaches, eax, ebx, ecx, edx);
        end if;

        -- print("Is MISC_ENABLE set? "); println(x86.rdmsr(x86.MSRs.MISC_ENABLE));
        -- println((x86.rdmsr(x86.MSRs.MISC_ENABLE) and x86.MSRs.MISC_ENABLE_FLAG_LIMIT_CPUID) = 0);

        -- Leaves above 2 available if IA32_MISC_ENABLE[bit 22] MSR is _not_ set
        if (x86.rdmsr(x86.MSRs.MISC_ENABLE) and 
            x86.MSRs.MISC_ENABLE_FLAG_LIMIT_CPUID) = 0 then
            -- Skip leaf 3, don't want processor serial number
            -- Skip leaf 4, don't need cache coherency data yet
            -- Skip leaf 5, don't need MONITOR/MWAIT data yet

            -- Thermal/Power Mgmt
            if maxStdLeaf >= 6 then
                cpuid(6, eax, ebx, ecx, unused);
                leaf6eax := toRecord(eax);
                leaf6ebx := toRecord(ebx);
                leaf6ecx := toRecord(ecx);
            end if;

            -- TSC frequency info
            if maxStdLeaf >= 16#15# then
                cpuid(16#15#, eax, ebx, ecx, unused);
                tscRatioDenominator := eax;
                tscRatioNumerator   := ebx;
                tscFreqHz           := ecx;
            end if;

            -- Processor Speeds
            if maxStdLeaf >= 16#16# then
                cpuid(16#16#, eax, ebx, ecx, unused);
                baseCPUFreqMHz      := eax;
                maxCPUFreqMHz       := ebx;
                busFreqMHz          := ecx;
            end if;
        end if;

        if maxExtLeaf >= 16#8000_0004# then
            getBrandString(cpuBrand);
        end if;

        if maxExtLeaf >= 16#8000_0007# then
            cpuid(16#8000_0007#, unused, unused1, unused2, edx);
            hasInvariantTSC := util.isBitSet(edx, 8);
        end if;

        initialized := True;
    end setupCPUID;

    ---------------------------------------------------------------------------
    -- CPUID.07h, stores results in the CPUID_0000_0007_XXX vars
    ---------------------------------------------------------------------------
    -- procedure getExtendedFeatures is
    -- begin
    --     cpuid(1, CPUID_0000_0007_EAX, CPUID_0000_0007_EBX,
    --              CPUID_0000_0007_ECX, CPUID_0000_0007_EDX);
    -- end getExtendedFeatures;

    ---------------------------------------------------------------------------
    -- CPUID function w/ ECX = 0
    ---------------------------------------------------------------------------
    procedure cpuid(leaf : in Unsigned_32; eax, ebx, ecx, edx : out Unsigned_32)
    with
        SPARK_Mode => Off
    is
    begin
        Asm ("cpuid",
        Inputs =>   (Unsigned_32'Asm_Input ("a", leaf)),
        Outputs =>  (Unsigned_32'Asm_Output ("=a", eax),
                     Unsigned_32'Asm_Output ("=b", ebx),
                     Unsigned_32'Asm_Output ("=c", ecx),
                     Unsigned_32'Asm_Output ("=d", edx)));
    end cpuid;

    ---------------------------------------------------------------------------
    -- CPUID function w/ specified ECX
    ---------------------------------------------------------------------------
    procedure cpuid(leaf, ecxIn : in Unsigned_32;
                    eax, ebx, ecx, edx : out Unsigned_32)
    with
        SPARK_Mode => Off
    is
    begin
        Asm ("cpuid",
        Inputs =>   (Unsigned_32'Asm_Input ("a", leaf),
                     Unsigned_32'Asm_Input ("c", ecxIn)),
        Outputs =>  (Unsigned_32'Asm_Output ("=a", eax),
                     Unsigned_32'Asm_Output ("=b", ebx),
                     Unsigned_32'Asm_Output ("=c", ecx),
                     Unsigned_32'Asm_Output ("=d", edx)));
    end cpuid;

end cpuid;