-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary
-- Intel VMX VMCS field encodings, exit reasons and control bits (VMM-001
-- slice 1, the N150's back end; docs/hypervisor.md section 4).
--
-- @description
-- Intel SDM Vol. 3C, Appendix B (field encodings) and Appendix C (exit
-- reasons); cross-checked against Linux 7.0's asm/vmx.h (enum vmcs_field)
-- and uapi/asm/vmx.h (EXIT_REASON_*). Every value is named here.
-------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package VMX_Layout with
    SPARK_Mode => On,
    Pure
is
    subtype Field is Unsigned_64;

    VPID                  : constant Field := 16#0000#;
    Guest_ES_Selector     : constant Field := 16#0800#;
    Guest_CS_Selector     : constant Field := 16#0802#;
    Guest_SS_Selector     : constant Field := 16#0804#;
    Guest_DS_Selector     : constant Field := 16#0806#;
    Guest_FS_Selector     : constant Field := 16#0808#;
    Guest_GS_Selector     : constant Field := 16#080A#;
    Guest_LDTR_Selector   : constant Field := 16#080C#;
    Guest_TR_Selector     : constant Field := 16#080E#;
    Host_ES_Selector      : constant Field := 16#0C00#;
    Host_CS_Selector      : constant Field := 16#0C02#;
    Host_SS_Selector      : constant Field := 16#0C04#;
    Host_DS_Selector      : constant Field := 16#0C06#;
    Host_FS_Selector      : constant Field := 16#0C08#;
    Host_GS_Selector      : constant Field := 16#0C0A#;
    Host_TR_Selector      : constant Field := 16#0C0C#;
    IO_Bitmap_A           : constant Field := 16#2000#;
    IO_Bitmap_B           : constant Field := 16#2002#;
    MSR_Bitmap            : constant Field := 16#2004#;
    Exit_MSR_Store_Address : constant Field := 16#2006#;
    Exit_MSR_Load_Address : constant Field := 16#2008#;
    Entry_MSR_Load_Address : constant Field := 16#200A#;
    TSC_Offset            : constant Field := 16#2010#;
    EPT_Pointer           : constant Field := 16#201A#;
    Guest_Physical_Address : constant Field := 16#2400#;
    VMCS_Link_Pointer     : constant Field := 16#2800#;
    Guest_PAT             : constant Field := 16#2804#;
    Guest_EFER            : constant Field := 16#2806#;
    Host_PAT              : constant Field := 16#2C00#;
    Host_EFER             : constant Field := 16#2C02#;
    Pin_Controls          : constant Field := 16#4000#;
    Processor_Controls    : constant Field := 16#4002#;
    Exception_Bitmap      : constant Field := 16#4004#;
    Exit_Controls         : constant Field := 16#400C#;
    Exit_MSR_Store_Count  : constant Field := 16#400E#;
    Exit_MSR_Load_Count   : constant Field := 16#4010#;
    Entry_Controls        : constant Field := 16#4012#;
    Entry_MSR_Load_Count  : constant Field := 16#4014#;
    Entry_Interruption_Info : constant Field := 16#4016#;
    Entry_Exception_Error : constant Field := 16#4018#;
    Entry_Instruction_Length : constant Field := 16#401A#;
    Secondary_Controls    : constant Field := 16#401E#;
    Instruction_Error     : constant Field := 16#4400#;
    Exit_Reason           : constant Field := 16#4402#;
    Exit_Interruption_Info : constant Field := 16#4404#;
    Exit_Interruption_Error : constant Field := 16#4406#;
    IDT_Vectoring_Info    : constant Field := 16#4408#;
    IDT_Vectoring_Error   : constant Field := 16#440A#;
    Exit_Instruction_Length : constant Field := 16#440C#;
    Guest_ES_Limit        : constant Field := 16#4800#;
    Guest_GDTR_Limit      : constant Field := 16#4810#;
    Guest_IDTR_Limit      : constant Field := 16#4812#;
    Guest_ES_Access       : constant Field := 16#4814#;
    Guest_Interruptibility : constant Field := 16#4824#;
    Guest_Activity        : constant Field := 16#4826#;
    Guest_Sysenter_CS     : constant Field := 16#482A#;
    Host_Sysenter_CS      : constant Field := 16#4C00#;
    CR0_Mask              : constant Field := 16#6000#;
    CR4_Mask              : constant Field := 16#6002#;
    CR0_Read_Shadow       : constant Field := 16#6004#;
    CR4_Read_Shadow       : constant Field := 16#6006#;
    Exit_Qualification    : constant Field := 16#6400#;
    Guest_CR0             : constant Field := 16#6800#;
    Guest_CR3             : constant Field := 16#6802#;
    Guest_CR4             : constant Field := 16#6804#;
    Guest_ES_Base         : constant Field := 16#6806#;
    Guest_GDTR_Base       : constant Field := 16#6816#;
    Guest_IDTR_Base       : constant Field := 16#6818#;
    Guest_DR7             : constant Field := 16#681A#;
    Guest_RSP             : constant Field := 16#681C#;
    Guest_RIP             : constant Field := 16#681E#;
    Guest_RFLAGS          : constant Field := 16#6820#;
    Guest_Pending_Debug   : constant Field := 16#6822#;
    Guest_Sysenter_ESP    : constant Field := 16#6824#;
    Guest_Sysenter_EIP    : constant Field := 16#6826#;
    Host_CR0              : constant Field := 16#6C00#;
    Host_CR3              : constant Field := 16#6C02#;
    Host_CR4              : constant Field := 16#6C04#;
    Host_FS_Base          : constant Field := 16#6C06#;
    Host_GS_Base          : constant Field := 16#6C08#;
    Host_TR_Base          : constant Field := 16#6C0A#;
    Host_GDTR_Base        : constant Field := 16#6C0C#;
    Host_IDTR_Base        : constant Field := 16#6C0E#;
    Host_Sysenter_ESP     : constant Field := 16#6C10#;
    Host_Sysenter_EIP     : constant Field := 16#6C12#;
    Host_RSP              : constant Field := 16#6C14#;
    Host_RIP              : constant Field := 16#6C16#;
    -- Segment fields: ES, CS, SS, DS, FS, GS, LDTR, TR are consecutive by 2
    -- (selector, limit, access rights, base); GDTR and IDTR have only limit
    -- and base.
    Segment_Stride : constant := 2;

    -- Exit reasons (SDM Appendix C).
    Reason_Exception_NMI   : constant := 0;
    Reason_External_Interrupt : constant := 1;
    Reason_Triple_Fault    : constant := 2;
    Reason_INIT            : constant := 3;
    Reason_SIPI            : constant := 4;
    Reason_Interrupt_Window : constant := 7;
    Reason_NMI_Window      : constant := 8;
    Reason_CPUID           : constant := 10;
    Reason_HLT             : constant := 12;
    Reason_INVD            : constant := 13;
    Reason_VMCALL          : constant := 18;
    Reason_First_VMX_Instruction : constant := 19;  -- VMCLEAR
    Reason_Last_VMX_Instruction  : constant := 27;  -- VMXON
    Reason_CR_Access       : constant := 28;
    Reason_DR_Access       : constant := 29;
    Reason_IO              : constant := 30;
    Reason_RDMSR           : constant := 31;
    Reason_WRMSR           : constant := 32;
    Reason_Invalid_Guest_State : constant := 33;
    Reason_MSR_Loading     : constant := 34;
    Reason_Machine_Check   : constant := 41;
    Reason_EPT_Violation   : constant := 48;
    Reason_EPT_Misconfiguration : constant := 49;
    Reason_INVEPT          : constant := 50;
    Reason_INVVPID         : constant := 53;
    Reason_WBINVD          : constant := 54;
    Reason_XSETBV          : constant := 55;
    Reason_Basic_Mask      : constant := 16#FFFF#;
    Entry_Failure_Bit      : constant := 31;

    -- Interruption types (bits 10:8 of the interruption fields).
    Type_External : constant := 0;
    Type_NMI      : constant := 2;
    Type_Hardware_Exception : constant := 3;

    -- Pin, processor, secondary, exit and entry control bits (the
    -- capability MSRs' names: Virtualization_Features).
    Pin_External_Interrupt : constant := 0;
    Pin_NMI                : constant := 3;
    Proc_Interrupt_Window  : constant := 2;
    Proc_TSC_Offsetting    : constant := 3;
    Proc_HLT               : constant := 7;
    Proc_MWAIT             : constant := 10;
    Proc_MOV_DR            : constant := 23;
    Proc_Unconditional_IO  : constant := 24;
    Proc_IO_Bitmaps        : constant := 25;
    Proc_MSR_Bitmaps       : constant := 28;
    Proc_MONITOR           : constant := 29;
    Proc_Secondary         : constant := 31;
    Proc2_EPT              : constant := 1;
    Proc2_RDTSCP           : constant := 3;
    Proc2_VPID             : constant := 5;
    Proc2_Unrestricted     : constant := 7;
    Proc2_INVPCID          : constant := 12;
    Exit_Host_64           : constant := 9;
    Exit_Save_PAT          : constant := 18;
    Exit_Load_PAT          : constant := 19;
    Exit_Save_EFER         : constant := 20;
    Exit_Load_EFER         : constant := 21;
    Entry_IA32E_Guest      : constant := 9;
    Entry_Load_PAT         : constant := 14;
    Entry_Load_EFER        : constant := 15;

    -- MSRs.
    IA32_FEATURE_CONTROL : constant := 16#3A#;
    IA32_VMX_BASIC       : constant := 16#480#;
    IA32_VMX_CR0_FIXED0  : constant := 16#486#;
    IA32_VMX_CR0_FIXED1  : constant := 16#487#;
    IA32_VMX_CR4_FIXED0  : constant := 16#488#;
    IA32_VMX_CR4_FIXED1  : constant := 16#489#;
    Feature_Control_Lock : constant := 1;
    Feature_Control_VMXON_Outside_SMX : constant := 4;
    CR4_VMXE             : constant := 16#2000#;

    -- EPTP: write-back, a 4-level walk (SDM 25.6.11).
    EPTP_Write_Back_Walk_4 : constant := 16#1E#;
end VMX_Layout;
