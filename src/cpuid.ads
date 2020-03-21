-------------------------------------------------------------------------------
-- Fortress OS
-- Copyright (C) 2019 Jon Andrew
--
-- x86-64 CPUID functionality
-------------------------------------------------------------------------------
--with System.Machine_Code; use System.Machine_Code;
with Ada.Unchecked_Conversion;
with Interfaces; use Interfaces;

package cpuid with
    SPARK_Mode => On
is
    -- set by call to setupCPUID
    initialized : Boolean := False with Ghost;

    subtype CpuVendorString is String (1..12);
    cpuVendor : CpuVendorString := "Default CPU ";

    subtype CpuBrandString is String(1..48);
    cpuBrand : CpuBrandString := 
        "Processor model wasn't available: check VM setup";

    ---------------------------------------------------------------------------
    -- CPUID:01h - Processor Features
    ---------------------------------------------------------------------------
    -- CPUID:01h[EAX]
    subtype SteppingID      is Natural range 0..15;
    subtype Model           is Natural range 0..15;
    subtype FamilyID        is Natural range 0..15;
    
    type ProcessorType is (
        ORIGINAL_OEM,
        INTEL_OVERDRIVE,
        DUAL_PROCESSOR,
        PROC_TYPE_RESERVED);

    subtype ExtModelID      is Natural range 0..15;
    subtype ExtFamilyID     is Natural range 0..255;

    type CPUID_1_EAX is
    record
        cpuSteppingID       : SteppingID;
        cpuModel            : Model;
        cpuFamilyID         : FamilyID;
        cpuType             : ProcessorType;
        res1                : Natural range 0..3;
        cpuExtModelID       : ExtModelID;
        cpuExtFamilyID      : ExtFamilyID;
        res2                : Natural range 0..15;
    end record with Size => 32;

    for CPUID_1_EAX use
    record
        cpuSteppingID       at 0 range 0..3;
        cpuModel            at 0 range 4..7;
        cpuFamilyID         at 1 range 0..3;
        cpuType             at 1 range 4..5;
        res1                at 1 range 6..7;
        cpuExtModelID       at 2 range 0..3;
        cpuExtFamilyID      at 2 range 4..11;
        res2                at 3 range 4..7;
    end record;
    
    function toRecord is new Ada.Unchecked_Conversion(Unsigned_32, CPUID_1_EAX);

    -- CPUID:01h[EBX]
    type CPUID_1_EBX is
    record
        brandIndex          : Unsigned_8;
        clflushSize         : Unsigned_8;
        numAPICIDs          : Unsigned_8;
        localAPICID         : Unsigned_8;
    end record with Size => 32;

    for CPUID_1_EBX use
    record
        brandIndex          at 0 range 0..7;
        clflushSize         at 1 range 0..7;
        numAPICIDs          at 2 range 0..7;
        localAPICID         at 3 range 0..7;
    end record;

    function toRecord is new Ada.Unchecked_Conversion(Unsigned_32, CPUID_1_EBX);

    -- CPUID:01h[ECX]
    type CPUID_1_ECX is
    record
        hasSSE3             : Boolean;
        hasPCMULQDQ         : Boolean;
        hasDTES64           : Boolean;
        hasMONITOR          : Boolean;
        hasDS_CPL           : Boolean;
        hasVMX              : Boolean;
        hasSMX              : Boolean;    -- safer mode extensions
        hasEST              : Boolean;    -- enhanced speedstep
        hasTM2              : Boolean;    -- thermal monitor 2
        hasSSSE3            : Boolean;
        hasCNXT_ID          : Boolean;    -- L1 context ID
        hasSDBG             : Boolean;    -- Silicon debug
        hasFMA              : Boolean;    -- fused multiply-add (FMA3)
        hasCX16             : Boolean;    -- CMPXCHG16B instruction
        hasXTPR             : Boolean;    -- can disable task prio. msgs
        hasPDCM             : Boolean;    -- perfmon & debug capes
        res16               : Boolean;    -- bit 16 reserved
        hasPCID             : Boolean;    -- Process context IDs
        hasDCA              : Boolean;    -- Direct cache access
        hasSSE4_1           : Boolean;
        hasSSE4_2           : Boolean;
        hasX2APIC           : Boolean;
        hasMOVBE            : Boolean;
        hasPOPCNT           : Boolean;
        hasTSCDeadline      : Boolean;
        hasAES              : Boolean;
        hasXSAVE            : Boolean;
        hasOSXSAVE          : Boolean;
        hasAVX              : Boolean;
        hasF16C             : Boolean;     -- half-precision FP
        hasRDRAND           : Boolean;
        isHypervised        : Boolean;
    end record with Size => 32;

    for CPUID_1_ECX use
    record
        hasSSE3             at 0 range 0..0;
        hasPCMULQDQ         at 0 range 1..1;
        hasDTES64           at 0 range 2..2;
        hasMONITOR          at 0 range 3..3;
        hasDS_CPL           at 0 range 4..4;
        hasVMX              at 0 range 5..5;
        hasSMX              at 0 range 6..6;
        hasEST              at 0 range 7..7;
        hasTM2              at 0 range 8..8;
        hasSSSE3            at 0 range 9..9;
        hasCNXT_ID          at 0 range 10..10;
        hasSDBG             at 0 range 11..11;
        hasFMA              at 0 range 12..12;
        hasCX16             at 0 range 13..13;
        hasXTPR             at 0 range 14..14;
        hasPDCM             at 0 range 15..15;
        res16               at 0 range 16..16;
        hasPCID             at 0 range 17..17;
        hasDCA              at 0 range 18..18;
        hasSSE4_1           at 0 range 19..19;
        hasSSE4_2           at 0 range 20..20;
        hasX2APIC           at 0 range 21..21;
        hasMOVBE            at 0 range 22..22;
        hasPOPCNT           at 0 range 23..23;
        hasTSCDeadline      at 0 range 24..24;
        hasAES              at 0 range 25..25;
        hasXSAVE            at 0 range 26..26;
        hasOSXSAVE          at 0 range 27..27;
        hasAVX              at 0 range 28..28;
        hasF16C             at 0 range 29..29;
        hasRDRAND           at 0 range 30..30;
        isHypervised        at 0 range 31..31;
    end record;

    function toRecord is new Ada.Unchecked_Conversion(Unsigned_32, CPUID_1_ECX);

    -- -- CPUID:01h[EDX]
    type CPUID_1_EDX is
    record
        hasFPU              : Boolean;
        hasVME              : Boolean;
        hasDE               : Boolean;
        hasPSE              : Boolean;
        hasTSC              : Boolean;
        hasMSR              : Boolean;
        hasPAE              : Boolean;
        hasMCE              : Boolean;
        hasCX8              : Boolean;     -- CMPXCHG8
        hasAPIC             : Boolean;
        res10               : Boolean;
        hasSEP              : Boolean;
        hasMTRR             : Boolean;
        hasPGE              : Boolean;
        hasMCA              : Boolean;
        hasCMOV             : Boolean;
        hasPAT              : Boolean;     -- Page Attribute Table
        hasPSE36            : Boolean;
        hasPSN              : Boolean;     -- Proc serial num
        hasCLFSH            : Boolean;
        res20               : Boolean;
        hasDS               : Boolean;     -- Debug store, save jumps
        hasACPI             : Boolean;     -- onboard thermal ctrl MSRs
        hasMMX              : Boolean;
        hasFXSR             : Boolean;     -- FXSAVE/FXSTOR
        hasSSE              : Boolean;
        hasSSE2             : Boolean;
        hasSS               : Boolean;     -- cache self-snoop
        hasHTT              : Boolean;     -- Hyperthreading
        hasTM               : Boolean;     -- Thermal auto-limit temp
        isIA64              : Boolean;     -- IA64 proc emulating x86
        hasPBE              : Boolean;
    end record with Size => 32;

    for CPUID_1_EDX use
    record
        hasFPU              at 0 range 0..0;
        hasVME              at 0 range 1..1;
        hasDE               at 0 range 2..2;
        hasPSE              at 0 range 3..3;
        hasTSC              at 0 range 4..4;
        hasMSR              at 0 range 5..5;
        hasPAE              at 0 range 6..6;
        hasMCE              at 0 range 7..7;
        hasCX8              at 0 range 8..8;
        hasAPIC             at 0 range 9..9;
        res10               at 0 range 10..10;
        hasSEP              at 0 range 11..11;
        hasMTRR             at 0 range 12..12;
        hasPGE              at 0 range 13..13;
        hasMCA              at 0 range 14..14;
        hasCMOV             at 0 range 15..15;
        hasPAT              at 0 range 16..16;
        hasPSE36            at 0 range 17..17;
        hasPSN              at 0 range 18..18;
        hasCLFSH            at 0 range 19..19;
        res20               at 0 range 20..20;
        hasDS               at 0 range 21..21;
        hasACPI             at 0 range 22..22;
        hasMMX              at 0 range 23..23;
        hasFXSR             at 0 range 24..24;
        hasSSE              at 0 range 25..25;
        hasSSE2             at 0 range 26..26;
        hasSS               at 0 range 27..27;
        hasHTT              at 0 range 28..28;
        hasTM               at 0 range 29..29;
        isIA64              at 0 range 30..30;
        hasPBE              at 0 range 31..31;
    end record;

    function toRecord is new Ada.Unchecked_Conversion(Unsigned_32, CPUID_1_EDX);

    ---------------------------------------------------------------------------
    -- CPUID:02h - TLB/Cache/Prefetch Information
    -- Notes:   LSByte of EAX always returns 01h
    --          MSB of each register is valid (0) or reserved (1)
    --
    -- There can be 4 cache descriptors per register, and 4 regs, so up to
    --  16 CacheDescriptors for this call.
    ---------------------------------------------------------------------------
    type CacheType is (TLB, STLB, DTLB, Cache, Trace, Prefetch, NOTPRESENT);
    type CacheLevel is (L1DATA, L1INST, DTLB, ITLB, uTLB, L2, L3, DTLB0, Trace, NA);

    ---------------------------------------------------------------------------
    -- CacheDescriptor
    -- @field Kind - type of cache (TLB, Cache, or Trace Cache)
    -- @field Level - see type CacheLevel
    -- @field Size - in KiB. If Cache type is Prefetch, this describes
    --  the prefetch size in bytes.
    -- @field BlockSize - Either the cache line size in bytes or number of TLB
    --  entries
    -- @field Associativity - the set associativity for this cache. 0 indicates
    --  fully associative if the cache is present.
    -- NOTE: For now, if a TLB can be set up with multiple page sizes, Cubit
    --  considers either the smallest page size for this CacheDescriptor, OR,
    --  if only one is valid for Long Mode, then this record reflects the setup
    --  that's valid for Long Mode. i.e., if the TLB can hold either 4k pages
    --  or 2M
    ---------------------------------------------------------------------------
    type CacheDescriptor is
    record
        Kind                : CacheType := NOTPRESENT;
        Level               : CacheLevel := NA;
        Size                : Unsigned_32 := 0;
        BlockSize           : Unsigned_32 := 0;
        Associativity       : Unsigned_32 := 0;
    end record;

    type CacheDescriptors is array (Natural range 0..15) of CacheDescriptor;

    ---------------------------------------------------------------------------
    -- systemCaches is the array of CacheDescriptors from a CPUID leaf 2, 4 or
    --  0x18 call.
    ---------------------------------------------------------------------------
    systemCaches : CacheDescriptors;

    ---------------------------------------------------------------------------
    -- printCacheInfo
    -- @description print out details of the systemCaches array, previously
    --  set by call to setupCPUID;
    ---------------------------------------------------------------------------
    procedure printCacheInfo with
        Pre => initialized;

    ---------------------------------------------------------------------------
    -- CPUID:03h - Processor Serial Number (NOT SUPPORTED)
    ---------------------------------------------------------------------------

    ---------------------------------------------------------------------------
    -- CPUID:04h - Deterministic Cache features for each level (NOT SUPPORTED)
    ---------------------------------------------------------------------------

    ---------------------------------------------------------------------------
    -- CPUID:05h - Thermal and Power Management
    ---------------------------------------------------------------------------
    type CPUID_6_EAX is
    record
        hasTempSensor           : Boolean;
        hasTurboBoost           : Boolean;
        hasARAT                 : Boolean;  -- Always-Run-APIC-Timer feature
        res3                    : Boolean;  -- bit 3 reserved
        hasPLN                  : Boolean;  -- Power Limit Notification
        hasECMD                 : Boolean;  -- Clock modulation duty cycle
        hasPTM                  : Boolean;  -- Package thermal mgmt
        hasHWP                  : Boolean;  -- HWP base MSRs are supported
        hasHWPNotification      : Boolean;  -- HWP_INTERRUPT MSR supported
        hasHWPActivityWindow    : Boolean;  -- uses HWP_REQUEST bits 41:32
        hasHWPEnergyPrefs       : Boolean;  -- uses HWP_REQUEST bits 31:24
        hasHWPPackageLevel      : Boolean;  -- HWP_REQUEST_PKG MSR supported
        res12                   : Boolean;
        hasHDC                  : Boolean;  -- Support Hardware Duty Cycling
        hasTurboBoostMax3       : Boolean;
        hasHWPCapabilities      : Boolean;
        hasHWPPECI              : Boolean;
        hasFlexibleHWP          : Boolean;
        hasFastHWPRequest       : Boolean;
        res19                   : Boolean;
        hasIdleCPUCanIgnoreHWP  : Boolean;
        res21_31                : Natural range 0..16#7FF#;
        --res21_31              -- bits 21:31 ignored
    end record with Size => 32;

    for CPUID_6_EAX use
    record
        hasTempSensor           at 0 range 0..0;
        hasTurboBoost           at 0 range 1..1;
        hasARAT                 at 0 range 2..2;
        res3                    at 0 range 3..3;
        hasPLN                  at 0 range 4..4;
        hasECMD                 at 0 range 5..5;
        hasPTM                  at 0 range 6..6;
        hasHWP                  at 0 range 7..7;
        hasHWPNotification      at 0 range 8..8;
        hasHWPActivityWindow    at 0 range 9..9;
        hasHWPEnergyPrefs       at 0 range 10..10;
        hasHWPPackageLevel      at 0 range 11..11;
        res12                   at 0 range 12..12;
        hasHDC                  at 0 range 13..13;
        hasTurboBoostMax3       at 0 range 14..14;
        hasHWPCapabilities      at 0 range 15..15;
        hasHWPPECI              at 0 range 16..16;
        hasFlexibleHWP          at 0 range 17..17;
        hasFastHWPRequest       at 0 range 18..18;
        res19                   at 0 range 19..19;
        hasIdleCPUCanIgnoreHWP  at 0 range 20..20;
        res21_31                at 0 range 21..31;
    end record;

    function toRecord is new Ada.Unchecked_Conversion(Unsigned_32, CPUID_6_EAX);

    -- CPUID:06h[EBX]
    type CPUID_6_EBX is
    record
        numThermalINTThresholds : Natural range 0..7;
    end record with Size => 32;

    for CPUID_6_EBX use
    record
        numThermalINTThresholds at 0 range 0..3;
    end record;

    function toRecord is new Ada.Unchecked_Conversion(Unsigned_32, CPUID_6_EBX);

    -- CPUID:06h[ECX]
    type CPUID_6_ECX is
    record
        hasHWPerfFeedback       : Boolean;
        hasPerfEnergyBiasPrefs  : Boolean;
    end record with Size => 32;

    for CPUID_6_ECX use
    record
        hasHWPerfFeedback       at 0 range 0..0;
        hasPerfEnergyBiasPrefs  at 0 range 3..3;
    end record;

    function toRecord is new Ada.Unchecked_Conversion(Unsigned_32, CPUID_6_ECX);

    -- CPUID:06h[EDX] is reserved (0)

    ---------------------------------------------------------------------------
    -- CPUID:07h - Extended Feature Flags
    ---------------------------------------------------------------------------
    -- CPUID:07h[EAX] returns the max valid ECX parameter for each sub-leaf
    
    -- CPUID:07h[EBX]
    type CPUID_7_EBX is
    record
        hasFSGSBase             : Boolean;
        hasTSCAdjust            : Boolean;      -- Has TSC_ADJUST MSR
        hasSGX                  : Boolean; 
        hasBMI1                 : Boolean;      -- bit-manipulation insts
        hasHLE                  : Boolean;      -- TSX hardware lock elision
        hasAVX2                 : Boolean;
        FDP_EXCPTN_ONLY         : Boolean;      -- x87 dp update on exceptions.
        hasSMEP                 : Boolean;      -- supervisor mode exec. prot.
        hasBMI2                 : Boolean; 
        hasERMS                 : Boolean;      -- enhanced REP MOVSB/STOSB
        hasINVPCID              : Boolean; 
        hasRTM                  : Boolean;      -- TSX restr. transact. mem
        hasRDT_M                : Boolean;      -- QoS monitoring
        deprecateFPU_CS_DS      : Boolean;
        hasMPX                  : Boolean;      -- Memory Protection Extensions
        hasRDT_A                : Boolean;      -- QoS enforcement
        hasAVX512f              : Boolean;      -- AVX512 foundation
        hasAVX512dq             : Boolean;      -- AVX512 DWORD/QWORD insts
        hasRDSEED               : Boolean; 
        hasADX                  : Boolean;      -- Multi-precision add/carry
        hasSMAP                 : Boolean;      -- Supervisor mode access prot.
        hasAVX512ifma           : Boolean;      -- Integer-fused mult-add
        res22                   : Boolean;
        hasCLFLUSHOPT           : Boolean; 
        hasCLWB                 : Boolean; 
        hasIntelPT              : Boolean;      -- Intel Processor Trace
        hasAVX512pf             : Boolean;      -- prefetch
        hasAVX512er             : Boolean;      -- exponent & reciprocal insts.
        hasAVX512cd             : Boolean;      -- conflict detection
        hasSHA                  : Boolean; 
        hasAVX512bw             : Boolean;      -- byte/word instructions
        hasAVX512vl             : Boolean;      -- vector length instructions
    end record with Size => 32;

    for CPUID_7_EBX use
    record
        hasFSGSBase             at 0 range 0..0;
        hasTSCAdjust            at 0 range 1..1;
        hasSGX                  at 0 range 2..2;
        hasBMI1                 at 0 range 3..3;
        hasHLE                  at 0 range 4..4;
        hasAVX2                 at 0 range 5..5;
        FDP_EXCPTN_ONLY         at 0 range 6..6;
        hasSMEP                 at 0 range 7..7;
        hasBMI2                 at 0 range 8..8;
        hasERMS                 at 0 range 9..9;
        hasINVPCID              at 0 range 10..10;
        hasRTM                  at 0 range 11..11;
        hasRDT_M                at 0 range 12..12;
        deprecateFPU_CS_DS      at 0 range 13..13;
        hasMPX                  at 0 range 14..14;
        hasRDT_A                at 0 range 15..15;
        hasAVX512f              at 0 range 16..16;
        hasAVX512dq             at 0 range 17..17;
        hasRDSEED               at 0 range 18..18;
        hasADX                  at 0 range 19..19;
        hasSMAP                 at 0 range 20..20;
        hasAVX512ifma           at 0 range 21..21;
        res22                   at 0 range 22..22;
        hasCLFLUSHOPT           at 0 range 23..23;
        hasCLWB                 at 0 range 24..24;
        hasIntelPT              at 0 range 25..25;
        hasAVX512pf             at 0 range 26..26;
        hasAVX512er             at 0 range 27..27;
        hasAVX512cd             at 0 range 28..28;
        hasSHA                  at 0 range 29..29;
        hasAVX512bw             at 0 range 30..30;
        hasAVX512vl             at 0 range 31..31;
    end record;

    function toRecord is new Ada.Unchecked_Conversion(Unsigned_32, CPUID_7_EBX);

    -- CPUID:07h[ECX]
    type CPUID_7_ECX is
    record
        hasPREFETCHWT1          : Boolean;     -- PREFETCHWT1 (Xeon Phi)
        hasAVX512vbmi           : Boolean;     -- vector bit manip insts.
        hasUMIP                 : Boolean;     -- User-mode inst. prevent
        hasPKU                  : Boolean;     -- Mem protect key for UM pages
        hasOSPKE                : Boolean;     -- PKU enabled by OS
        hasWAITPKG              : Boolean;
        res6_7                  : Natural range 0..3;
        hasGFNI                 : Boolean;     -- Galois Field insts. (?)
        res9_13                 : Natural range 0..31;
        hasAVX512VPOPCNTDQ      : Boolean;     -- vector bit manip insts. 2
        res15_16                : Natural range 0..3;
        userMAWA                : Natural range 0..31;
        hasRDPID                : Boolean;
        res23_24                : Natural range 0..3;
        hasCLDEMOTE             : Boolean;
        res26                   : Boolean;
        hasMOVDIRI              : Boolean;
        hasMOVDIR64B            : Boolean;
        res29                   : Boolean;
        hasSGX_LC               : Boolean;
        res31                   : Boolean;
    end record with Size => 32;

    for CPUID_7_ECX use
    record
        hasPREFETCHWT1          at 0 range 0..0;
        hasAVX512vbmi           at 0 range 1..1;
        hasUMIP                 at 0 range 2..2;
        hasPKU                  at 0 range 3..3;
        hasOSPKE                at 0 range 4..4;
        hasWAITPKG              at 0 range 5..5;
        res6_7                  at 0 range 6..7;
        hasGFNI                 at 0 range 8..8;
        res9_13                 at 0 range 9..13;
        hasAVX512VPOPCNTDQ      at 0 range 14..14;
        res15_16                at 0 range 15..16;
        userMAWA                at 0 range 17..21;
        hasRDPID                at 0 range 22..22;
        res23_24                at 0 range 23..24;
        hasCLDEMOTE             at 0 range 25..25;
        res26                   at 0 range 26..26;
        hasMOVDIRI              at 0 range 27..27;
        hasMOVDIR64B            at 0 range 28..28;
        res29                   at 0 range 29..29;
        hasSGX_LC               at 0 range 30..30;
        res31                   at 0 range 31..31;
    end record;

    function toRecord is new Ada.Unchecked_Conversion(Unsigned_32, CPUID_7_ECX);

    -- CPUID:07h[EDX]
    type CPUID_7_EDX is
    record
        res0_1                  : Natural range 0..3;
        hasAVX512_4VNNIW        : Boolean; -- AVX-512 4-reg NN instrs.
        hasAVX512_4FMAPS        : Boolean; -- AVX-512 4-reg mult-acc. SP
        hasFastShortREPMOV      : Boolean;
        res5_14                 : Natural range 0..255;
        isHybridPart            : Boolean;
        res16_19                : Natural range 0..15;
        hasCET_IBT              : Boolean; -- CET indir branch tracking
        res21_25                : Natural range 0..31;
        hasSPEC_CTRL            : Boolean; -- IBRS, IBPB
        hasSTIBP                : Boolean; -- Single-thrd Indir. Branches
        hasL1DFLUSH             : Boolean;
        hasARCH_CAPES_MSR       : Boolean;
        hasCORE_CAPES_MSR       : Boolean;
        hasSSBD                 : Boolean; -- Spec. store bypass disable
    end record with Size => 32;

    for CPUID_7_EDX use
    record
        res0_1                  at 0 range 0..1;
        hasAVX512_4VNNIW        at 0 range 2..2;
        hasAVX512_4FMAPS        at 0 range 3..3;
        hasFastShortREPMOV      at 0 range 4..4;
        res5_14                 at 0 range 5..14;
        isHybridPart            at 0 range 15..15;
        res16_19                at 0 range 16..19;
        hasCET_IBT              at 0 range 20..20;
        res21_25                at 0 range 21..25;
        hasSPEC_CTRL            at 0 range 26..26;
        hasSTIBP                at 0 range 27..27;
        hasL1DFLUSH             at 0 range 28..28;
        hasARCH_CAPES_MSR       at 0 range 29..29;
        hasCORE_CAPES_MSR       at 0 range 30..30;
        hasSSBD                 at 0 range 31..31;
    end record;

    function toRecord is new Ada.Unchecked_Conversion(Unsigned_32, CPUID_7_EDX);

    ---------------------------------------------------------------------------
    -- CPUID:09h - Direct Cache Access Info
    -- Contains the value of the PLATFORM_DCA_CAP MSR (Address 1F8)
    ---------------------------------------------------------------------------
    -- CPUID:09h[EAX]
    subtype CPUID_9_EAX is Unsigned_32;

    ---------------------------------------------------------------------------
    -- CPUID:0Ah - Architectural Performance Monitoring Leaf
    ---------------------------------------------------------------------------
    -- CPUID:0Ah[EAX]
    type CPUID_A_EAX is
    record
        versionID               : Unsigned_8;
        numGPPerfCtrs           : Unsigned_8;
        widthGPPerfCtrs         : Unsigned_8;
        lengthPerfMonEBXVector  : Unsigned_8;
    end record with Size => 32;

    for CPUID_A_EAX use
    record
        versionID               at 0 range 0..7;
        numGPPerfCtrs           at 0 range 8..15;
        widthGPPerfCtrs         at 0 range 16..23;
        lengthPerfMonEBXVector  at 0 range 24..31;
    end record;

    function toRecord is new Ada.Unchecked_Conversion(Unsigned_32, CPUID_A_EAX);

    -- CPUID:0Ah[EBX]
    type CPUID_A_EBX is
    record
        coreCycleEvtNotAvbl     : Boolean;
        instRetEvtNotAvbl       : Boolean;
        refCyclesEvtNotAvbl     : Boolean;
        cacheRefEvtNotAvbl      : Boolean;
        cacheMissEvtNotAvbl     : Boolean;
        branchRetEvtNotAvbl     : Boolean;
        branchMisPRetEvtNotAvbl : Boolean;
        res7_31                 : Natural range 0..16#1FF_FFFF#;
    end record with Size => 32;

    for CPUID_A_EBX use
    record
        coreCycleEvtNotAvbl     at 0 range 0..0;
        instRetEvtNotAvbl       at 0 range 1..1;
        refCyclesEvtNotAvbl     at 0 range 2..2;
        cacheRefEvtNotAvbl      at 0 range 3..3;
        cacheMissEvtNotAvbl     at 0 range 4..4;
        branchRetEvtNotAvbl     at 0 range 5..5;
        branchMisPRetEvtNotAvbl at 0 range 6..6;
        res7_31                 at 0 range 7..31;
    end record;

    -- CPUID:0Ah[ECX] - Reserved

    -- CPUID:0Ah[EDX]
    type CPUID_A_EDX is
    record
        numFFPerfCtrs           : Natural range 0..31;
        widthFFPerfCtrs         : Unsigned_8;
        res13_14                : Natural range 0..3;
        isAnyThreadDeprecated   : Boolean;
        res16_31                : Unsigned_16;
    end record with Size => 32;

    for CPUID_A_EDX use
    record
        numFFPerfCtrs           at 0 range 0..4;
        widthFFPerfCtrs         at 0 range 5..12;
        res13_14                at 0 range 13..14;
        isAnyThreadDeprecated   at 0 range 15..15;
        res16_31                at 0 range 16..31;
    end record;

    ---------------------------------------------------------------------------
    -- CPUID:0Bh - Extended Topology Enumeration (NOT SUPPORTED)
    ---------------------------------------------------------------------------

    ---------------------------------------------------------------------------
    -- CPUID:13h - Extended Topology Enumeration (NOT SUPPORTED)
    ---------------------------------------------------------------------------

    ---------------------------------------------------------------------------
    -- CPUID:14h - Processor Trace Enumeration (NOT SUPPORTED)
    ---------------------------------------------------------------------------

    ---------------------------------------------------------------------------
    -- CPUID:15h - Time Stamp Counter / Nominal Core Crystal Clock
    ---------------------------------------------------------------------------
    tscRatioDenominator         : Unsigned_32 := 0;
    tscRatioNumerator           : Unsigned_32 := 0;
    tscFreqHz                   : Unsigned_32 := 0;

    ---------------------------------------------------------------------------
    -- CPUID:16h - Processor Frequency Info
    ---------------------------------------------------------------------------
    baseCPUFreqMHz              : Unsigned_32 := 0;
    maxCPUFreqMHz               : Unsigned_32 := 0;
    busFreqMHz                  : Unsigned_32 := 0;

    ---------------------------------------------------------------------------
    -- CPUID:17h - SoC Vendor Attr. Enumeration (NOT SUPPORTED)
    ---------------------------------------------------------------------------

    ---------------------------------------------------------------------------
    -- CPUID:18h - Deterministic Address Translation (NOT SUPPORTED)
    ---------------------------------------------------------------------------

    ---------------------------------------------------------------------------
    -- CPUID:1Ah - Hybrid Info Enumeration (NOT SUPPORTED)
    ---------------------------------------------------------------------------

    ---------------------------------------------------------------------------
    -- CPUID:1Fh - V2 Extended Topology Enumeration (NOT SUPPORTED)
    ---------------------------------------------------------------------------

    ---------------------------------------------------------------------------
    -- CPUID:8000_0000h - Extended Function CPUID info
    -- see getMaxExtendedFunction
    ---------------------------------------------------------------------------
    
    ---------------------------------------------------------------------------
    -- CPUID:8000_0001h - Extended Processor Information. Note AMD has more of
    --  these fields supported than Intel
    ---------------------------------------------------------------------------
    -- CPUID:8000_0001h[ECX]
    type CPUID_EXT_1_ECX is
    record
        hasLAHF                 : Boolean;
        hasLegacyCMP            : Boolean;
        hasSVM                  : Boolean;
        hasExtAPIC              : Boolean;
        hasALTMOVCR8            : Boolean;
        hasABM                  : Boolean;  -- LZCNT/POPCNT insts.
        hasSSE4A                : Boolean;
        hasMisalignSSE          : Boolean;
        hasPREFETCH             : Boolean;  -- 3DNow Prefetch inst.
        hasOSVW                 : Boolean;  -- OS-visible workaround
        hasIBS                  : Boolean;
        hasXOP                  : Boolean;
        hasSKInit               : Boolean;
        hasWDT                  : Boolean;
        res14                   : Boolean;
        hasLWP                  : Boolean;  -- lightweight profiling
        hasFMA4                 : Boolean;  -- 4-Operand fused-mult-add
        res17                   : Boolean;
        res18                   : Boolean;
        hasNodeID               : Boolean;
        res20                   : Boolean;
        hasTBM                  : Boolean;  -- Trailing bit manip
        hasTopology             : Boolean;  -- CPUID 8000_001Dh
        res23                   : Boolean;
        res24_31                : Unsigned_8;
    end record with Size => 32;
    
    for CPUID_EXT_1_ECX use
    record
        hasLAHF                 at 0 range 0..0;
        hasLegacyCMP            at 0 range 1..1;
        hasSVM                  at 0 range 2..2;
        hasExtAPIC              at 0 range 3..3;
        hasALTMOVCR8            at 0 range 4..4;
        hasABM                  at 0 range 5..5;
        hasSSE4A                at 0 range 6..6;
        hasMisalignSSE          at 0 range 7..7;
        hasPREFETCH             at 0 range 8..8;
        hasOSVW                 at 0 range 9..9;
        hasIBS                  at 0 range 10..10;
        hasXOP                  at 0 range 11..11;
        hasSKInit               at 0 range 12..12;
        hasWDT                  at 0 range 13..13;
        res14                   at 0 range 14..14;
        hasLWP                  at 0 range 15..15;
        hasFMA4                 at 0 range 16..16;
        res17                   at 0 range 17..17;
        res18                   at 0 range 18..18;
        hasNodeID               at 0 range 19..19;
        res20                   at 0 range 20..20;
        hasTBM                  at 0 range 21..21;
        hasTopology             at 0 range 22..22;
        res23                   at 0 range 23..23;
        res24_31                at 0 range 24..31;
    end record;

    function toRecord is new Ada.Unchecked_Conversion(Unsigned_32, CPUID_EXT_1_ECX);

    -- CPUID:8000_0001h[EDX]
    type CPUID_EXT_1_EDX is
    record
        hasFPU                  : Boolean;
        hasVME                  : Boolean;
        hasDE                   : Boolean;
        hasPSE                  : Boolean;
        hasTSC                  : Boolean;
        hasMSR                  : Boolean;
        hasPAE                  : Boolean;
        hasMCE                  : Boolean;
        hasCX8                  : Boolean;
        hasAPIC                 : Boolean;
        res10                   : Boolean;
        hasSYSCALL              : Boolean;
        hasMTRR                 : Boolean;
        hasPGE                  : Boolean;
        hasMCA                  : Boolean;
        hasCMOV                 : Boolean;
        hasPAT                  : Boolean;
        hasPSE36                : Boolean;
        res18                   : Boolean;
        hasMP                   : Boolean;
        hasNX                   : Boolean;
        res21                   : Boolean;
        hasMMXEXT               : Boolean;
        hasMMX                  : Boolean;
        hasFXSR                 : Boolean;
        hasFXSR_OPT             : Boolean;
        hasPDPE1GB              : Boolean;
        hasRDTSCP               : Boolean;
        res28                   : Boolean;
        hasLM                   : Boolean;
        has3DNowExt             : Boolean;
        has3DNow                : Boolean;
    end record with Size => 32;

    for CPUID_EXT_1_EDX use
    record
        hasFPU                  at 0 range 0..0;
        hasVME                  at 0 range 1..1;
        hasDE                   at 0 range 2..2;
        hasPSE                  at 0 range 3..3;
        hasTSC                  at 0 range 4..4;
        hasMSR                  at 0 range 5..5;
        hasPAE                  at 0 range 6..6;
        hasMCE                  at 0 range 7..7;
        hasCX8                  at 0 range 8..8;
        hasAPIC                 at 0 range 9..9;
        res10                   at 0 range 10..10;
        hasSYSCALL              at 0 range 11..11;
        hasMTRR                 at 0 range 12..12;
        hasPGE                  at 0 range 13..13;
        hasMCA                  at 0 range 14..14;
        hasCMOV                 at 0 range 15..15;
        hasPAT                  at 0 range 16..16;
        hasPSE36                at 0 range 17..17;
        res18                   at 0 range 18..18;
        hasMP                   at 0 range 19..19;
        hasNX                   at 0 range 20..20;
        res21                   at 0 range 21..21;
        hasMMXEXT               at 0 range 22..22;
        hasMMX                  at 0 range 23..23;
        hasFXSR                 at 0 range 24..24;
        hasFXSR_OPT             at 0 range 25..25;
        hasPDPE1GB              at 0 range 26..26;
        hasRDTSCP               at 0 range 27..27;
        res28                   at 0 range 28..28;
        hasLM                   at 0 range 29..29;
        has3DNowExt             at 0 range 30..30;
        has3DNow                at 0 range 31..31;
    end record;

    function toRecord is new Ada.Unchecked_Conversion(Unsigned_32, CPUID_EXT_1_EDX);

    ---------------------------------------------------------------------------
    -- CPUID:8000_0007h - Support for invariant TSC
    ---------------------------------------------------------------------------
    hasInvariantTSC : Boolean := False;
    -- type CPUID_EXT_7_EDX is
    -- record
    --     res0_7                  : Unsigned_8;
    --     hasInvariantTSC         : Boolean;
    --     res9_31                 : Natural range 0..16#7F_FFFF#;
    -- end record with Size => 32;

    -- for CPUID_EXT_7_EDX use
    -- record
    --     res0_7                  at 0 range 0..7;
    --     hasInvariantTSC         at 0 range 8..8;
    --     res9_31                 at 0 range 9..31;
    -- end record;

    -- function toRecord is new Ada.Unchecked_Conversion(Unsigned_32, CPUID_EXT_7_EDX);

    ---------------------------------------------------------------------------
    -- Information available to others after call to setupCPUID
    ---------------------------------------------------------------------------
    leaf1eax : CPUID_1_EAX;
    leaf1ebx : CPUID_1_EBX;
    leaf1ecx : CPUID_1_ECX;
    leaf1edx : CPUID_1_EDX;

    leaf6eax : CPUID_6_EAX;
    leaf6ebx : CPUID_6_EBX;
    leaf6ecx : CPUID_6_ECX;

    -- Leaves above 8000_0000h
    extLeaf1ecx : CPUID_EXT_1_ECX;
    extLeaf1edx : CPUID_EXT_1_EDX;

    ---------------------------------------------------------------------------
    -- setupCPUID
    --
    -- Run CPUID with available leafs and stores the results in this package.
    -- Once this is done, those features can be queried using the hasXYZ()
    -- and getXYZ() functions.
    ---------------------------------------------------------------------------
    procedure setupCPUID with 
        Pre => not initialized,
        Post => initialized;

    ---------------------------------------------------------------------------
    -- cpuid - given the CPUID leaf, get the information returned by CPUID
    --  in the registers eax, ebx, ecx and edx.
    ---------------------------------------------------------------------------
    procedure cpuid(leaf : in Unsigned_32; 
                    eax : out Unsigned_32; ebx : out Unsigned_32;
                    ecx : out Unsigned_32; edx : out Unsigned_32);

    ---------------------------------------------------------------------------
    -- Wrapper around CPUID:00h[EAX]
    ---------------------------------------------------------------------------
    function getMaxStandardFunction return Unsigned_32;

    ---------------------------------------------------------------------------
    -- Wrapper around CPUID:8000_0000h[EAX]
    ---------------------------------------------------------------------------
    function getMaxExtendedFunction return Unsigned_32;

private
    ---------------------------------------------------------------------------
    -- getVendor - called by setupCPUID, get the manufacturer string
    ---------------------------------------------------------------------------
    procedure getVendor(vendor : out CpuVendorString);
    pragma Annotate (GNATProve, Intentional, "might not be initialized", 
        "SPARK does not recognize writes to individual elements as affecting the whole.");

    ---------------------------------------------------------------------------
    -- getBrand - called by setupCPUID, get the cpu brand string, i.e. "Core 2"
    ---------------------------------------------------------------------------
    procedure getBrandString(brand : out CpuBrandString);
    pragma Annotate (GNATProve, Intentional, "might not be initialized", 
        "SPARK does not recognize writes to individual elements as affecting the whole.");
end cpuid;