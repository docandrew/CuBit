-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2019 Jon Andrew
--
-- x86-64 instruction wrappers
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System;
with System.Storage_Elements; use System.Storage_Elements;

with Locks;

package x86 with
    Abstract_State => (IOPortState with External),
    SPARK_Mode => On
is
    type PrivilegeLevel is new Integer range 0..3;
    
    DPL_KERNEL : constant PrivilegeLevel := 0;
    DPL_USER   : constant PrivilegeLevel := 3;

    -- FLAGS (EFLAGS/RFLAGS) register
    -- TODO: add more here
    FLAGS_INTERRUPT : constant Unsigned_64 := 16#0200#;

    ---------------------------------------------------------------------------
    -- panicked - if any CPU calls x86.panic, it will set this flag to indicate
    --  to the scheduler running on other CPUs to cease operation.
    ---------------------------------------------------------------------------
    panicked : Boolean := False;

    ---------------------------------------------------------------------------
    -- Record describing the RFLAGS register.
    ---------------------------------------------------------------------------
    type RFlags is
    record
        carry           : Boolean;
        reserved1       : Boolean;
        parity          : Boolean;
        reserved2       : Boolean;
        auxiliary       : Boolean;
        reserved3       : Boolean;
        zero            : Boolean;
        sign            : Boolean;
        trap            : Boolean;
        interrupt       : Boolean;
        direction       : Boolean;
        overflow        : Boolean;
        IOPrivilege     : Integer range 0..3;
        nestedTask      : Boolean;
        reserved4       : Boolean;
        resume          : Boolean;
        virtual8086     : Boolean;
        alignmentCheck  : Boolean;
        virtInterrupt   : Boolean;
        virtIntPending  : Boolean;
        reserved5       : Natural;
    end record with Size => 64;

    for RFlags use
    record
        carry           at 0 range 0..0;
        reserved1       at 0 range 1..1;
        parity          at 0 range 2..2;
        reserved2       at 0 range 3..3;
        auxiliary       at 0 range 4..4;
        reserved3       at 0 range 5..5;
        zero            at 0 range 6..6;
        sign            at 0 range 7..7;
        trap            at 1 range 0..0;
        interrupt       at 1 range 1..1;
        direction       at 1 range 2..2;
        overflow        at 1 range 3..3;
        IOPrivilege     at 1 range 4..5;
        nestedTask      at 1 range 6..6;
        reserved4       at 1 range 7..7;
        resume          at 2 range 0..0;
        virtual8086     at 2 range 1..1;
        alignmentCheck  at 2 range 2..2;
        virtInterrupt   at 2 range 3..3;
        virtIntPending  at 2 range 4..4;
        reserved5       at 2 range 5..46;
    end record;

    -- I/O Address space
    subtype IOPort is Unsigned_16;
    function "+" (Left : IOPort; Right : Unsigned_8) return IOPort;

    -- Interrupts Enabled/Disabled state, to be used in contracts
    interruptsEnabled : Boolean with Ghost;

    ---------------------------------------------------------------------------
    -- Output a single byte to x86 I/O port
    ---------------------------------------------------------------------------
    procedure out8(port : in IOPort; data : in Unsigned_8) with
        Global => (Output => IOPortState);

    ---------------------------------------------------------------------------
    -- Output a single 16-bit word to x86 I/O port
    ---------------------------------------------------------------------------
    procedure out16(port : in IOPort; data : in Unsigned_16) with
        Global => (Output => IOPortState);

    ---------------------------------------------------------------------------
    -- Output a 32-bit quad to x86 I/O port
    ---------------------------------------------------------------------------
    procedure out32(port : in IOPort; data : in Unsigned_32) with
        Global => (Output => IOPortState);

    ---------------------------------------------------------------------------
    -- Output count words to x86 I/O port, reading from address provided.
    ---------------------------------------------------------------------------
    procedure outs16(port : in IOPort; addr : in System.Address;
                    count : in Unsigned_32);

    ---------------------------------------------------------------------------
    -- Output count dwords to x86 I/O port, reading from address provided.
    ---------------------------------------------------------------------------
    procedure outs32(port : in IOPort; addr : in System.Address;
                    count : in Unsigned_32);

    ---------------------------------------------------------------------------
    -- Input a single byte from x86 I/O port
    ---------------------------------------------------------------------------
    procedure in8(port : in IOPort; val : out Unsigned_8) with
        Global => (In_Out => IOPortState),
        Depends => (IOPortState => port, val => IOPortState);

    ---------------------------------------------------------------------------
    -- Input a short from x86 I/O port
    ---------------------------------------------------------------------------
    procedure in16(port : in IOPort; val : out Unsigned_16) with
        Global => (In_Out => IOPortState),
        Depends => (IOPortState => port, val => IOPortState);

    ---------------------------------------------------------------------------
    -- Input a single byte from x86 I/O port
    ---------------------------------------------------------------------------
    procedure in32(port : in IOPort; val : out Unsigned_32) with
        Global => (In_Out => IOPortState),
        Depends => (IOPortState => port, val => IOPortState);

    ---------------------------------------------------------------------------
    -- Input count words from x86 I/O port, writing at the address provided.
    ---------------------------------------------------------------------------
    procedure ins16(port : in IOPort; addr : in System.Address;
                    count : in Unsigned_32);

    ---------------------------------------------------------------------------
    -- Input count dwords from x86 I/O port, writing at the address provided.
    ---------------------------------------------------------------------------
    procedure ins32(port : in IOPort; addr : in System.Address;
                    count : in Unsigned_32);-- with
        --Global => (In_Out => IOPortState),
        --Depends => (IOPortState => port, addr => IOPortState, null => count);

    ---------------------------------------------------------------------------
    -- Load Interrupt Descriptor Table
    ---------------------------------------------------------------------------
    procedure lidt(idtpPtr : System.Address);

    ---------------------------------------------------------------------------
    -- Load General Descriptor Table
    ---------------------------------------------------------------------------
    procedure lgdt(gdtPtr : System.Address);

    ---------------------------------------------------------------------------
    -- Load Task Register with index of TSS in the GDT
    ---------------------------------------------------------------------------
    procedure ltr(selector : Unsigned_16);

    ---------------------------------------------------------------------------
    -- Get the current value of the rflags register.
    ---------------------------------------------------------------------------
    function getFlags return RFlags;

    ---------------------------------------------------------------------------
    -- Get the current value of the CR2 register (used in page faults)
    ---------------------------------------------------------------------------
    function getCR2 return Unsigned_64;

    ---------------------------------------------------------------------------
    -- Get the current value of the CR3 register (top-level page table ptr)
    ---------------------------------------------------------------------------
    function getCR3 return Integer_Address;

    ---------------------------------------------------------------------------
    -- Get base pointer. In debug mode, calling convention stores the caller
    -- in this register, so we can use this to get a stack trace.
    ---------------------------------------------------------------------------
    function getRBP return Unsigned_64;

    ---------------------------------------------------------------------------
    -- swapgs instruction
    ---------------------------------------------------------------------------
    procedure swapgs;

    ---------------------------------------------------------------------------
    -- Enable Interrupts
    -- TODO: add pre-condition here to make sure IDT is loaded before calling
    ---------------------------------------------------------------------------
    procedure sti with
        Global => (Output => interruptsEnabled),
        Post => (interruptsEnabled);
    
    ---------------------------------------------------------------------------
    -- Disable Interrupts
    ---------------------------------------------------------------------------
    procedure cli with
        Global => (Output => interruptsEnabled),
        Post => (not interruptsEnabled);

    ---------------------------------------------------------------------------
    -- XCHG instruction, used for atomic locking primitive.
    -- Given a variable, swap the value in that variable with newval.
    -- On return, var will contain newval, and oldval will have the old value of
    -- var, possibly set elsewhere.
    ---------------------------------------------------------------------------
    procedure xchg(var : in out Unsigned_32; newval : in Unsigned_32; 
                   oldval : out Unsigned_32)
        with Inline, Convention => C;

    ---------------------------------------------------------------------------
    -- XCHG instruction specialized for locking.
    ---------------------------------------------------------------------------
    procedure lock_xchg(var : in out locks.LockBool; newval : in locks.LockBool;
                        oldval : out locks.LockBool)
        with Inline, Convention => C,
             Post => var = newval and then oldval = var'old;

    ---------------------------------------------------------------------------
    -- Kernel Panic, calls software interrupt
    -- TODO: add precondition for interrupts being set up.
    -- TODO: add IPI or flag to halt the other processors as well.
    ---------------------------------------------------------------------------
    procedure panic with No_Return;

    ---------------------------------------------------------------------------
    -- Halt operation of the CPU
    ---------------------------------------------------------------------------
    procedure halt;

    ---------------------------------------------------------------------------
    -- Model-Specific Registers.
    -- TODO: consider a mixin inheritance structure here to limit wrmsr
    -- to WO and RW MSRs, and limit rdmsr to RO and RW MSRs.
    ---------------------------------------------------------------------------
    subtype MSR is Unsigned_32;
    -- subtype MSR_RO is MSR;
    -- subtype MSR_RW is MSR;
    -- subtype MSR_WO is MSR;

    ---------------------------------------------------------------------------
    -- x86.MSRs package
    -- @summary MSR address definitions. Uses the Intel names minus "IA32".
    -- @description This package enumerates the "architectural" MSRs, that is,
    --  no truly model-specific regs (at least for now).
    --
    -- TODO: use SPEC_CTRL and PRED_CMD to disable certain branch speculation
    --  (need to check recommendations, possibly make this a run-time option)
    ---------------------------------------------------------------------------
    package MSRs is
        P5_MC_ADDR              : constant MSR := 16#0000_0000#;
        P5_MC_TYPE              : constant MSR := 16#0000_0001#;
        MONITOR_FILTER_SIZE     : constant MSR := 16#0000_0006#;
        TSC                     : constant MSR := 16#0000_0010#;
        MSR_PLATFORM_ID         : constant MSR := 16#0000_0017#;

        -----------------------------------------------------------------------
        -- APIC Base address and control flags
        -----------------------------------------------------------------------
        APIC_BASE               : constant MSR := 16#0000_001B#;

        -- BSP Flag (R/W), if set, this is the bootstrap processor
        APIC_BASE_FLAG_BSP      : constant Unsigned_64  := 16#0000_0000_0000_0100#;
        -- Enable x2APIC mode (R/W)
        APIC_BASE_FLAG_X2APIC   : constant Unsigned_64  := 16#0000_0000_0000_0400#;
        -- APIC Global Enable
        APIC_BASE_FLAG_GLOBAL   : constant Unsigned_64  := 16#0000_0000_0000_0800#;
        -- Bits containing APIC base address
        APIC_BASE_MASK          : constant Unsigned_64  := 16#FFFF_FFFF_FFFF_0000#;

        FEATURE_CONTROL         : constant MSR := 16#0000_003A#;
        TSC_ADJUST              : constant MSR := 16#0000_003B#;

        -----------------------------------------------------------------------
        -- Control branch speculation
        -----------------------------------------------------------------------
        SPEC_CTRL               : constant MSR := 16#0000_0048#;
        -- Flags for the SPEC_CTRL MSR.
        -- Restrict indirect branch speculation
        SPEC_CTRL_FLAG_IBRS     : constant Unsigned_64  := 16#0000_0000_0000_0001#;
        -- Limit branch predictions to a single hyperthread core
        SPEC_CTRL_FLAG_STIBP    : constant Unsigned_64  := 16#0000_0000_0000_0002#;
        -- Delay speculative loads until older store addresses are known
        SPEC_CTRL_FLAG_SSBD     : constant Unsigned_64  := 16#0000_0000_0000_0004#;

        -----------------------------------------------------------------------
        -- Control branch prediction barriers
        -----------------------------------------------------------------------
        PRED_CMD                : constant MSR := 16#0000_0049#;
        -- Flag for the PRED_CMD MSR
        -- Enable indirect branch prediction barrier
        PRED_CMD_FLAG_IBPB      : constant Unsigned_64  := 16#0000_0000_0000_0001#;

        -- BIOS update
        BIOS_UPDT_TRIG          : constant MSR := 16#0000_0079#;
        BIOS_SIGN_ID            : constant MSR := 16#0000_008B#;

        -- Intel Secure Guard Extensions
        SGXLEPUBKEYHASH0        : constant MSR := 16#0000_008C#;
        SGXLEPUBKEYHASH1        : constant MSR := 16#0000_008D#;
        SGXLEPUBKEYHASH2        : constant MSR := 16#0000_008E#;
        SGXLEPUBKEYHASH3        : constant MSR := 16#0000_008F#;
        SMM_MONITOR_CTL         : constant MSR := 16#0000_009B#;
        SMBASE                  : constant MSR := 16#0000_009E#;

        -- General Performance Counters
        PMC0                    : constant MSR := 16#0000_00C1#;
        PMC1                    : constant MSR := 16#0000_00C2#;
        PMC2                    : constant MSR := 16#0000_00C3#;
        PMC3                    : constant MSR := 16#0000_00C4#;
        PMC4                    : constant MSR := 16#0000_00C5#;
        PMC5                    : constant MSR := 16#0000_00C6#;
        PMC6                    : constant MSR := 16#0000_00C7#;
        PMC7                    : constant MSR := 16#0000_00C8#;

        CORE_CAPABILITY         : constant MSR := 16#0000_00CF#;
        UMWAIT_CONTROL          : constant MSR := 16#0000_00E1#;

        -- TSC Frequency Clock Counter (R/W to clear)
        MPERF                   : constant MSR := 16#0000_00E7#;

        -- Actual frequency clock count
        APERF                   : constant MSR := 16#0000_00E8#;

        -- MTRR capabilities
        MTRRCAP                 : constant MSR := 16#0000_00FE#;

        -----------------------------------------------------------------------
        -- Arch Capabilities - important safety Spectre/Meltdown mitigation
        --  info here. MSR exists if CPUID.(EAX=07h,ECX=0):EDX[29] = 1
        -----------------------------------------------------------------------
        ARCH_CAPABILITIES       : constant MSR := 16#0000_010A#;

        -- Flag if CPU is not vulnerable to Meltdown attacks, no KPTI needed
        ARCH_CAPES_FLAG_RDCL_NO
                                : constant Unsigned_64  := 16#0000_0000_0000_0001#;
        -- Flag if CPU supports branch speculation limits/enhanced IBRS
        ARCH_CAPES_FLAG_IBRS_ALL
                                : constant Unsigned_64  := 16#0000_0000_0000_0002#;
        -- Flag if CPU supports return stack buffer alternate
        ARCH_CAPES_FLAG_RSBA
                                : constant Unsigned_64  := 16#0000_0000_0000_0004#;
        -- Flag if Hypervisor can avoid flushing L1D on VM entry
        ARCH_CAPES_FLAG_SKIP_L1DFL_VMENTRY
                                : constant Unsigned_64  := 16#0000_0000_0000_0008#;
        -- Flag if Processor not susceptible to Speculative Store Bypass (SSB)
        ARCH_CAPES_FLAG_SSB_NO
                                : constant Unsigned_64  := 16#0000_0000_0000_0010#;

        -----------------------------------------------------------------------
        -- Flush command (WO) - invalidate the L1D cache
        -- A write to bit 0 (L1D_FLUSH) will writeback and invalidate the L1D
        -----------------------------------------------------------------------
        FLUSH_CMD               : constant MSR := 16#0000_010B#;
        L1D_FLUSH               : constant Unsigned_64  := 16#0000_0000_0000_0001#;

        -- Sysenter MSRs
        SYSENTER_CS             : constant MSR := 16#0000_0174#;
        SYSENTER_ESP            : constant MSR := 16#0000_0175#;
        SYSENTER_EIP            : constant MSR := 16#0000_0176#;

        -- Global Machine Check Capability
        MCG_CAP                 : constant MSR := 16#0000_0179#;
        MCG_STATUS              : constant MSR := 16#0000_017A#;
        MCG_CTL                 : constant MSR := 16#0000_017B#;

        -- Performance Event Select
        PERFEVTSEL0             : constant MSR := 16#0000_0186#;
        PERFEVTSEL1             : constant MSR := 16#0000_0187#;
        PERFEVTSEL2             : constant MSR := 16#0000_0188#;
        PERFEVTSEL3             : constant MSR := 16#0000_0189#;

        -- Performance Status
        PERF_STATUS             : constant MSR := 16#0000_0198#;
        PERF_CTL                : constant MSR := 16#0000_0199#;

        -- Clock Modulation
        CLOCK_MODULATION        : constant MSR := 16#0000_019A#;

        -- Thermal Interrupt Control - enable/disable interrupts based on thermals
        THERM_INTERRUPT         : constant MSR := 16#0000_019B#;
        -- Get status from thermal sensors (including temp thresholds, )
        THERM_STATUS            : constant MSR := 16#0000_019C#;

        -----------------------------------------------------------------------
        -- Miscellaneous processor features
        -----------------------------------------------------------------------
        MISC_ENABLE             : constant MSR := 16#0000_01A0#;
        -- Enable fast-strings (default)
        MISC_ENABLE_FLAG_FAST_STRINGS
                                : constant Unsigned_64  := 16#0000_0000_0000_0001#;
        -- Enable automatic thermal control circuit (reduce power consumption
        --  automatically if things get too hot)
        MISC_ENABLE_FLAG_AUTO_THERM
                                : constant Unsigned_64  := 16#0000_0000_0000_0008#;
        -- Enable performance monitoring
        MISC_ENABLE_FLAG_PERF
                                : constant Unsigned_64  := 16#0000_0000_0000_0080#;
        -- If set, processor does not support branch trace storage (read only)
        MISC_ENABLE_FLAG_BTS_NOT_AVAILABLE
                                : constant Unsigned_64  := 16#0000_0000_0000_0800#;
        -- If set, processor does not support event based sampling (PEBS)
        MISC_ENABLE_FLAG_PEBS_NOT_AVAILABLE
                                : constant Unsigned_64  := 16#0000_0000_0000_1000#;
        -- If set, SpeedStep is enabled
        MISC_ENABLE_FLAG_SPEEDSTEP
                                : constant Unsigned_64  := 16#0000_0000_0001_0000#;
        -- If set, MONITOR/MWAIT are supported
        -- Note: Only set this bit if SSE3 feature flag is also set.
        MISC_ENABLE_FLAG_MONITOR
                                : constant Unsigned_64  := 16#0000_0000_0004_0000#;
        -- Limit CPUID maximum value. (a BIOS thing, for broken OSes)
        MISC_ENABLE_FLAG_LIMIT_CPUID
                                : constant Unsigned_64  := 16#0000_0000_0040_0000#;
        -- xTPR message disable
        MISC_ENABLE_FLAG_DISABLE_TPR
                                : constant Unsigned_64  := 16#0000_0000_0080_0000#;
        -- If set, the NX-bit is disabled
        MISC_ENABLE_FLAG_DISABLE_NXE
                                : constant Unsigned_64  := 16#0000_0004_0000_0000#;
        
        -- 0 indicates best performance, 15 indicates best power efficiency
        ENERGY_PERF_BIAS        : constant MSR := 16#0000_01B0#;

        -- Contains status about thermal sensor
        PACKAGE_THERM_STATUS    : constant MSR := 16#0000_01B1#;

        -- Package thermal interrupt control
        PACKAGE_THERM_INTERRUPT : constant MSR := 16#0000_01B2#;

        -- Trace/Profile Resource Control
        DEBUGCTL                : constant MSR := 16#0000_01D9#;

        -- Software Debug (AMD Only?)
        LAST_BRANCH_FROM_IP     : constant MSR := 16#0000_01DB#;
        LAST_BRANCH_TO_IP       : constant MSR := 16#0000_01DC#;
        LAST_INT_FROM_IP        : constant MSR := 16#0000_01DD#;
        LAST_INT_TO_IP          : constant MSR := 16#0000_01DE#;

        -- SMM Memory Range base address
        SMRR_PHYSBASE           : constant MSR := 16#0000_01F2#;

        -- SMRR Range Mask
        SMRR_PHYSMASK           : constant MSR := 16#0000_01F3#;

        -- DCA (Direct Cache Access) Capability
        PLATFORM_DCA_CAP        : constant MSR := 16#0000_01F8#;

        -- If set, CPU supports Prefetch-Hint type
        CPU_DCA_CAP             : constant MSR := 16#0000_01F9#;

        -- DCA type 0 status and control register
        DCA_0_CAP               : constant MSR := 16#0000_01FA#;

        -----------------------------------------------------------------------
        -- MTRR Memory Type Range Registers
        -----------------------------------------------------------------------
        MTRR_PHYSBASE0          : constant MSR := 16#0000_0200#;
        MTRR_PHYSMASK0          : constant MSR := 16#0000_0201#;

        MTRR_PHYSBASE1          : constant MSR := 16#0000_0202#;
        MTRR_PHYSMASK1          : constant MSR := 16#0000_0203#;
        
        MTRR_PHYSBASE2          : constant MSR := 16#0000_0204#;
        MTRR_PHYSMASK2          : constant MSR := 16#0000_0205#;

        MTRR_PHYSBASE3          : constant MSR := 16#0000_0206#;
        MTRR_PHYSMASK3          : constant MSR := 16#0000_0207#;

        MTRR_PHYSBASE4          : constant MSR := 16#0000_0208#;
        MTRR_PHYSMASK4          : constant MSR := 16#0000_0209#;

        MTRR_PHYSBASE5          : constant MSR := 16#0000_020A#;
        MTRR_PHYSMASK5          : constant MSR := 16#0000_020B#;

        MTRR_PHYSBASE6          : constant MSR := 16#0000_020C#;
        MTRR_PHYSMASK6          : constant MSR := 16#0000_020D#;

        MTRR_PHYSBASE7          : constant MSR := 16#0000_020E#;
        MTRR_PHYSMASK7          : constant MSR := 16#0000_020F#;

        MTRR_PHYSBASE8          : constant MSR := 16#0000_0210#;
        MTRR_PHYSMASK8          : constant MSR := 16#0000_0211#;

        MTRR_PHYSBASE9          : constant MSR := 16#0000_0212#;
        MTRR_PHYSMASK9          : constant MSR := 16#0000_0213#;

        MTRR_FIX64K_00000       : constant MSR := 16#0000_0250#;
        MTRR_FIX16K_80000       : constant MSR := 16#0000_0258#;
        MTRR_FIX16K_A0000       : constant MSR := 16#0000_0259#;
        MTRR_FIX4K_C0000        : constant MSR := 16#0000_0268#;
        MTRR_FIX4K_C8000        : constant MSR := 16#0000_0269#;
        MTRR_FIX4K_D0000        : constant MSR := 16#0000_026A#;
        MTRR_FIX4K_D8000        : constant MSR := 16#0000_026B#;
        MTRR_FIX4K_E0000        : constant MSR := 16#0000_026C#;
        MTRR_FIX4K_E8000        : constant MSR := 16#0000_026D#;
        MTRR_FIX4K_F0000        : constant MSR := 16#0000_026E#;
        MTRR_FIX4K_F8000        : constant MSR := 16#0000_026F#;

        -----------------------------------------------------------------------
        -- PAT Page Attribute Table
        -----------------------------------------------------------------------
        PAT                     : constant MSR := 16#0000_0277#;

        -- MTRR Def (default?) type
        MTRR_DEF_TYPE           : constant MSR := 16#0000_02FF#;

        -----------------------------------------------------------------------
        -- Productive Performance Count
        -----------------------------------------------------------------------
        MSR_PPERF               : constant MSR := 16#0000_064E#;

        -----------------------------------------------------------------------
        -- Hardware Duty Cyling - allow CPU to idle internal components
        -----------------------------------------------------------------------
        PKG_HDC_CONFIG_CTL      : constant MSR := 16#0000_0652#;
        CORE_HDC_RESIDENCY      : constant MSR := 16#0000_0653#;
        PKG_HDC_SHALLOW_RES     : constant MSR := 16#0000_0655#;
        PKG_HDC_DEEP_RES        : constant MSR := 16#0000_0656#;

        -----------------------------------------------------------------------
        -- Hardware-Controlled Performance States
        -----------------------------------------------------------------------
        PM_ENABLE               : constant MSR := 16#0000_0770#;
        HWP_CAPABILITIES        : constant MSR := 16#0000_0771#;
        HWP_REQUEST_PKG         : constant MSR := 16#0000_0772#;
        HWP_INTERRUPT           : constant MSR := 16#0000_0773#;
        HWP_REQUEST             : constant MSR := 16#0000_0774#;
        HWP_STATUS              : constant MSR := 16#0000_0777#;
        -----------------------------------------------------------------------
        -- X2 APIC
        -----------------------------------------------------------------------
        X2APIC_APICID           : constant MSR := 16#0000_0802#;
        X2APIC_VERSION          : constant MSR := 16#0000_0803#;
        X2APIC_TPR              : constant MSR := 16#0000_0808#;
        X2APIC_PPR              : constant MSR := 16#0000_080A#;
        X2APIC_EOI              : constant MSR := 16#0000_080B#;
        X2APIC_LDR              : constant MSR := 16#0000_080D#;
        X2APIC_SIVR             : constant MSR := 16#0000_080F#;
        
        X2APIC_ISR0             : constant MSR := 16#0000_0810#;
        X2APIC_ISR1             : constant MSR := 16#0000_0811#;
        X2APIC_ISR2             : constant MSR := 16#0000_0812#;
        X2APIC_ISR3             : constant MSR := 16#0000_0813#;
        X2APIC_ISR4             : constant MSR := 16#0000_0814#;
        X2APIC_ISR5             : constant MSR := 16#0000_0815#;
        X2APIC_ISR6             : constant MSR := 16#0000_0816#;
        X2APIC_ISR7             : constant MSR := 16#0000_0817#;
        
        X2APIC_TMR0             : constant MSR := 16#0000_0818#;
        X2APIC_TMR1             : constant MSR := 16#0000_0819#;
        X2APIC_TMR2             : constant MSR := 16#0000_081A#;
        X2APIC_TMR3             : constant MSR := 16#0000_081B#;
        X2APIC_TMR4             : constant MSR := 16#0000_081C#;
        X2APIC_TMR5             : constant MSR := 16#0000_081D#;
        X2APIC_TMR6             : constant MSR := 16#0000_081E#;
        X2APIC_TMR7             : constant MSR := 16#0000_081F#;

        X2APIC_IRR0             : constant MSR := 16#0000_0820#;
        X2APIC_IRR1             : constant MSR := 16#0000_0821#;
        X2APIC_IRR2             : constant MSR := 16#0000_0822#;
        X2APIC_IRR3             : constant MSR := 16#0000_0823#;
        X2APIC_IRR4             : constant MSR := 16#0000_0824#;
        X2APIC_IRR5             : constant MSR := 16#0000_0825#;
        X2APIC_IRR6             : constant MSR := 16#0000_0826#;
        X2APIC_IRR7             : constant MSR := 16#0000_0827#;

        X2APIC_ESR              : constant MSR := 16#0000_0828#;
        X2APIC_LVT_CMCI         : constant MSR := 16#0000_082F#;
        X2APIC_ICR              : constant MSR := 16#0000_0830#;
        X2APIC_LVT_TIMER        : constant MSR := 16#0000_0832#;
        X2APIC_LVT_THERMAL      : constant MSR := 16#0000_0833#;
        X2APIC_PMI              : constant MSR := 16#0000_0834#;
        X2APIC_LVT_LINT0        : constant MSR := 16#0000_0835#;
        X2APIC_LVT_LINT1        : constant MSR := 16#0000_0836#;
        X2APIC_LVT_ERROR        : constant MSR := 16#0000_0837#;
        X2APIC_INIT_COUNT       : constant MSR := 16#0000_0838#;
        X2APIC_CUR_COUNT        : constant MSR := 16#0000_0839#;
        X2APIC_DIV_CONF         : constant MSR := 16#0000_083E#;
        X2APIC_SELF_IPI         : constant MSR := 16#0000_083F#;

        -----------------------------------------------------------------------
        -- More Hardware duty cycling MSRs
        -----------------------------------------------------------------------
        PKG_HDC_CTL             : constant MSR := 16#0000_0DB0#;
        PM_CTL1                 : constant MSR := 16#0000_0DB1#;
        THREAD_STALL            : constant MSR := 16#0000_0DB2#;

        -- Extended feature enables
        EFER                    : constant MSR := 16#C000_0080#;

        -- System call target addresses (RW)
        STAR                    : constant MSR := 16#C000_0081#;
        LSTAR                   : constant MSR := 16#C000_0082#;
        CSTAR                   : constant MSR := 16#C000_0083#;
        FMASK                   : constant MSR := 16#C000_0084#;

        -- Map of BASE address of FS/GS (RW)
        FS_BASE                 : constant MSR := 16#C000_0100#;
        GS_BASE                 : constant MSR := 16#C000_0101#;

        -- Swap target of BASE address of GS (RW)
        KERNEL_GS_BASE          : constant MSR := 16#C000_0102#;

        -- Auxiliary TSC (RW)
        TSC_AUX                 : constant MSR := 16#C000_0103#;
    end MSRs;

    ---------------------------------------------------------------------------
    -- Read from a model-specific register (MSR)
    ---------------------------------------------------------------------------
    function rdmsr(msraddr : in MSR) return Unsigned_64;

    ---------------------------------------------------------------------------
    -- Write to a model-specific register (MSR)
    ---------------------------------------------------------------------------
    procedure wrmsr(msraddr : in MSR; val : in Unsigned_64);

    ---------------------------------------------------------------------------
    -- Read Time-Stamp Counter
    ---------------------------------------------------------------------------
    function rdtsc return Unsigned_64 with Inline;

    ---------------------------------------------------------------------------
    -- Read Time-Stamp Counter & Processor ID (stronger serialization).
    -- Can be used to get the chip & core a process is running on.
    ---------------------------------------------------------------------------
    function rdtscp(chip : out Unsigned_32; core : out Unsigned_32)
        return Unsigned_64 with Inline, SPARK_Mode => Off;

end x86;