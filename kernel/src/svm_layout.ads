-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary
-- The AMD SVM VMCB layout, intercept bits, exit codes and event encodings
-- (VMM-001 slice 1, docs/hypervisor.md section 4).
--
-- @description
-- AMD APM Vol. 2, Appendix B (VMCB layout) and 15.x; offsets cross-checked
-- against Linux 7.0's asm/svm.h (struct vmcb_control_area, struct
-- vmcb_save_area: control area 1024 bytes, save area at 0x400) and
-- uapi/asm/svm.h (SVM_EXIT_*). Every value is named here; no other unit
-- writes a VMCB offset or an exit code as a number.
-------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package SVM_Layout with
    SPARK_Mode => On,
    Pure
is
    VMCB_Bytes : constant := 4096;
    subtype VMCB_Offset is Natural range 0 .. VMCB_Bytes - 1;

    -- Control area.
    Intercept_CR       : constant VMCB_Offset := 16#000#;  -- u32
    Intercept_DR       : constant VMCB_Offset := 16#004#;  -- u32
    Intercept_Exceptions : constant VMCB_Offset := 16#008#; -- u32
    Intercept_Word_3   : constant VMCB_Offset := 16#00C#;  -- u32
    Intercept_Word_4   : constant VMCB_Offset := 16#010#;  -- u32
    Intercept_Word_5   : constant VMCB_Offset := 16#014#;  -- u32
    Pause_Filter_Threshold : constant VMCB_Offset := 16#03C#; -- u16
    Pause_Filter_Count : constant VMCB_Offset := 16#03E#;  -- u16
    IOPM_Base          : constant VMCB_Offset := 16#040#;  -- u64
    MSRPM_Base         : constant VMCB_Offset := 16#048#;  -- u64
    TSC_Offset         : constant VMCB_Offset := 16#050#;  -- u64
    Guest_ASID         : constant VMCB_Offset := 16#058#;  -- u32
    TLB_Control        : constant VMCB_Offset := 16#05C#;  -- u8
    Interrupt_Control  : constant VMCB_Offset := 16#060#;  -- u32 (int_ctl)
    Interrupt_Vector   : constant VMCB_Offset := 16#064#;  -- u32
    Interrupt_State    : constant VMCB_Offset := 16#068#;  -- u32 (shadow)
    Exit_Code          : constant VMCB_Offset := 16#070#;  -- u64
    Exit_Info_1        : constant VMCB_Offset := 16#078#;  -- u64
    Exit_Info_2        : constant VMCB_Offset := 16#080#;  -- u64
    Exit_Interrupt_Info : constant VMCB_Offset := 16#088#; -- u32
    Exit_Interrupt_Error : constant VMCB_Offset := 16#08C#; -- u32
    Nested_Control     : constant VMCB_Offset := 16#090#;  -- u64
    Event_Inject       : constant VMCB_Offset := 16#0A8#;  -- u32
    Event_Inject_Error : constant VMCB_Offset := 16#0AC#;  -- u32
    Nested_CR3         : constant VMCB_Offset := 16#0B0#;  -- u64
    Virtual_Extensions : constant VMCB_Offset := 16#0B8#;  -- u64
    Clean_Bits         : constant VMCB_Offset := 16#0C0#;  -- u32
    Next_RIP           : constant VMCB_Offset := 16#0C8#;  -- u64
    Instruction_Length : constant VMCB_Offset := 16#0D0#;  -- u8
    Instruction_Bytes  : constant VMCB_Offset := 16#0D1#;  -- 15 bytes

    -- State save area (0x400 + struct vmcb_save_area offset). A segment
    -- is selector (u16), attributes (u16), limit (u32), base (u64).
    Save_Area          : constant VMCB_Offset := 16#400#;
    Segment_Bytes      : constant := 16;
    Segment_Selector   : constant := 0;
    Segment_Attributes : constant := 2;
    Segment_Limit      : constant := 4;
    Segment_Base       : constant := 8;
    Guest_ES   : constant VMCB_Offset := Save_Area + 16#000#;
    Guest_CS   : constant VMCB_Offset := Save_Area + 16#010#;
    Guest_SS   : constant VMCB_Offset := Save_Area + 16#020#;
    Guest_DS   : constant VMCB_Offset := Save_Area + 16#030#;
    Guest_FS   : constant VMCB_Offset := Save_Area + 16#040#;
    Guest_GS   : constant VMCB_Offset := Save_Area + 16#050#;
    Guest_GDTR : constant VMCB_Offset := Save_Area + 16#060#;
    Guest_LDTR : constant VMCB_Offset := Save_Area + 16#070#;
    Guest_IDTR : constant VMCB_Offset := Save_Area + 16#080#;
    Guest_TR   : constant VMCB_Offset := Save_Area + 16#090#;
    Guest_CPL  : constant VMCB_Offset := Save_Area + 16#0CB#;  -- u8
    Guest_EFER : constant VMCB_Offset := Save_Area + 16#0D0#;
    Guest_CR4  : constant VMCB_Offset := Save_Area + 16#148#;
    Guest_CR3  : constant VMCB_Offset := Save_Area + 16#150#;
    Guest_CR0  : constant VMCB_Offset := Save_Area + 16#158#;
    Guest_DR7  : constant VMCB_Offset := Save_Area + 16#160#;
    Guest_DR6  : constant VMCB_Offset := Save_Area + 16#168#;
    Guest_RFLAGS : constant VMCB_Offset := Save_Area + 16#170#;
    Guest_RIP  : constant VMCB_Offset := Save_Area + 16#178#;
    Guest_RSP  : constant VMCB_Offset := Save_Area + 16#1D8#;
    Guest_RAX  : constant VMCB_Offset := Save_Area + 16#1F8#;
    Guest_STAR : constant VMCB_Offset := Save_Area + 16#200#;
    Guest_LSTAR : constant VMCB_Offset := Save_Area + 16#208#;
    Guest_CSTAR : constant VMCB_Offset := Save_Area + 16#210#;
    Guest_SFMASK : constant VMCB_Offset := Save_Area + 16#218#;
    Guest_Kernel_GS_Base : constant VMCB_Offset := Save_Area + 16#220#;
    Guest_Sysenter_CS  : constant VMCB_Offset := Save_Area + 16#228#;
    Guest_Sysenter_ESP : constant VMCB_Offset := Save_Area + 16#230#;
    Guest_Sysenter_EIP : constant VMCB_Offset := Save_Area + 16#238#;
    Guest_CR2  : constant VMCB_Offset := Save_Area + 16#240#;
    Guest_PAT  : constant VMCB_Offset := Save_Area + 16#268#;
    Guest_Debug_Control : constant VMCB_Offset := Save_Area + 16#270#;
    -- The guest's SPEC_CTRL, which VMRUN loads and #VMEXIT saves on a CPU
    -- with V_SPEC_CTRL (CPUID 8000_000Ah EDX bit 20; asm/svm.h struct
    -- vmcb_save_area spec_ctrl).
    Guest_Spec_Ctrl : constant VMCB_Offset := Save_Area + 16#2E0#;

    -- Intercept bit positions (asm/svm.h INTERCEPT_*), as a bit number in
    -- the 32-bit word its name gives.
    subtype Word_Bit is Natural range 0 .. 31;
    -- Word 3.
    Intercept_INTR        : constant Word_Bit := 0;
    Intercept_NMI         : constant Word_Bit := 1;
    Intercept_SMI         : constant Word_Bit := 2;
    Intercept_INIT        : constant Word_Bit := 3;
    Intercept_VINTR       : constant Word_Bit := 4;
    Intercept_CPUID       : constant Word_Bit := 18;
    Intercept_RSM         : constant Word_Bit := 19;
    Intercept_INVD        : constant Word_Bit := 22;
    Intercept_PAUSE       : constant Word_Bit := 23;
    Intercept_HLT         : constant Word_Bit := 24;
    Intercept_INVLPGA     : constant Word_Bit := 26;
    Intercept_IOIO        : constant Word_Bit := 27;
    Intercept_MSR         : constant Word_Bit := 28;
    Intercept_Task_Switch : constant Word_Bit := 29;
    Intercept_FERR_Freeze : constant Word_Bit := 30;
    Intercept_Shutdown    : constant Word_Bit := 31;
    -- Word 4.
    Intercept_VMRUN   : constant Word_Bit := 0;
    Intercept_VMMCALL : constant Word_Bit := 1;
    Intercept_VMLOAD  : constant Word_Bit := 2;
    Intercept_VMSAVE  : constant Word_Bit := 3;
    Intercept_STGI    : constant Word_Bit := 4;
    Intercept_CLGI    : constant Word_Bit := 5;
    Intercept_SKINIT  : constant Word_Bit := 6;
    Intercept_ICEBP   : constant Word_Bit := 8;
    Intercept_WBINVD  : constant Word_Bit := 9;
    Intercept_MONITOR : constant Word_Bit := 10;
    Intercept_MWAIT   : constant Word_Bit := 11;
    Intercept_MWAIT_Conditional : constant Word_Bit := 12;
    Intercept_XSETBV  : constant Word_Bit := 13;
    Intercept_RDPRU   : constant Word_Bit := 14;
    -- Exception vectors intercepted (word 2, bit = vector).
    Vector_Debug             : constant Word_Bit := 1;   -- #DB
    Vector_Alignment_Check   : constant Word_Bit := 17;  -- #AC
    Vector_General_Protection : constant Word_Bit := 13; -- #GP

    -- Exit codes (uapi/asm/svm.h SVM_EXIT_*).
    Exit_Write_CR0    : constant Unsigned_64 := 16#010#;
    Exit_Write_CR4    : constant Unsigned_64 := 16#014#;
    -- EXITINFO1 of a CR write exit as VMX_Run reports it: SVM intercepts no
    -- CR (VM_Objects), so these come only from VMX, decided already.
    CR_Write_Applied  : constant Unsigned_64 := 0;
    CR_Write_Refused  : constant Unsigned_64 := 1;   -- #GP(0) to inject
    Exit_Read_DR0     : constant Unsigned_64 := 16#020#;
    Exit_Write_DR7    : constant Unsigned_64 := 16#037#;
    Exit_Exception_Base : constant Unsigned_64 := 16#040#;
    Exit_Last_Exception : constant Unsigned_64 := 16#05F#;
    Exit_INTR         : constant Unsigned_64 := 16#060#;
    Exit_NMI          : constant Unsigned_64 := 16#061#;
    Exit_SMI          : constant Unsigned_64 := 16#062#;
    Exit_INIT         : constant Unsigned_64 := 16#063#;
    Exit_VINTR        : constant Unsigned_64 := 16#064#;
    Exit_CPUID        : constant Unsigned_64 := 16#072#;
    Exit_PAUSE        : constant Unsigned_64 := 16#077#;
    Exit_HLT          : constant Unsigned_64 := 16#078#;
    Exit_IOIO         : constant Unsigned_64 := 16#07B#;
    Exit_MSR          : constant Unsigned_64 := 16#07C#;
    MSR_Exit_Write    : constant Unsigned_64 := 1;   -- EXITINFO1 of a WRMSR exit
    Exit_Shutdown     : constant Unsigned_64 := 16#07F#;
    Exit_VMRUN        : constant Unsigned_64 := 16#080#;
    Exit_VMMCALL      : constant Unsigned_64 := 16#081#;
    Exit_INVD         : constant Unsigned_64 := 16#076#;
    Exit_WBINVD       : constant Unsigned_64 := 16#089#;
    Exit_XSETBV       : constant Unsigned_64 := 16#08D#;
    Exit_NPF          : constant Unsigned_64 := 16#400#;
    Exit_Invalid      : constant Unsigned_64 := Unsigned_64'Last;  -- -1

    -- TLB_CONTROL values.
    TLB_Do_Nothing  : constant Unsigned_8 := 0;
    TLB_Flush_ASID  : constant Unsigned_8 := 3;

    -- int_ctl bits.
    Virtual_IRQ_Bit            : constant Word_Bit := 8;
    Virtual_Ignore_TPR_Bit     : constant Word_Bit := 20;
    Virtual_Interrupt_Masking_Bit : constant Word_Bit := 24;
    Virtual_Priority_Shift     : constant := 16;

    -- Interrupt shadow (int_state bit 0).
    Interrupt_Shadow_Bit : constant Word_Bit := 0;

    -- EVENTINJ / EXITINTINFO.
    Event_Vector_Mask     : constant Unsigned_32 := 16#FF#;
    Event_Type_Shift      : constant := 8;
    Event_Type_Interrupt  : constant Unsigned_32 := 0;
    Event_Type_NMI        : constant Unsigned_32 := 2;
    Event_Type_Exception  : constant Unsigned_32 := 3;
    Event_Error_Valid_Bit : constant Word_Bit := 11;
    Event_Valid_Bit       : constant Word_Bit := 31;

    -- Nested control: NP_ENABLE.
    Nested_Paging_Enable : constant Unsigned_64 := 1;

    -- IOIO exit information (EXITINFO1).
    IOIO_In_Bit      : constant Word_Bit := 0;   -- 1 = IN
    IOIO_String_Bit  : constant Word_Bit := 2;
    IOIO_Rep_Bit     : constant Word_Bit := 3;
    IOIO_Size_8_Bit  : constant Word_Bit := 4;
    IOIO_Size_16_Bit : constant Word_Bit := 5;
    IOIO_Size_32_Bit : constant Word_Bit := 6;
    IOIO_Port_Shift  : constant := 16;

    -- Segment attributes (selector attribute word).
    Attribute_S_Bit  : constant Word_Bit := 4;
    Attribute_P_Bit  : constant Word_Bit := 7;
    Attribute_L_Bit  : constant Word_Bit := 9;
    Attribute_DB_Bit : constant Word_Bit := 10;
    Attribute_G_Bit  : constant Word_Bit := 11;

    -- MSRs.
    MSR_EFER        : constant := 16#C000_0080#;
    MSR_VM_HSAVE_PA : constant := 16#C001_0117#;
    EFER_SVME_Bit   : constant := 12;
    CR0_Protection_Enable : constant Unsigned_64 := 1;   -- CR0.PE

    -- I/O and MSR permission maps (APM 15.10, 15.11): 12 KiB and 8 KiB.
    IOPM_Bytes  : constant := 3 * 4096;
    MSRPM_Bytes : constant := 2 * 4096;
end SVM_Layout;
