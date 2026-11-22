-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary
-- What the CPU offers for hardware virtualization (VMM-001 slice 0,
-- docs/hypervisor.md): VMX capability MSRs and SVM CPUID leaves, decoded.
--
-- @description
-- Pure decoding, no hardware access: Virtualization_Report reads the raw
-- values (CPUID, and MSRs through Msr_Safe) and passes them here.
--
-- Sources for every bit (named below, never a bare number):
--  * Intel SDM Vol. 3C, Appendix A (VMX capability reporting) and Vol. 4
--    (MSR numbers); cross-checked against Linux's asm/vmx.h,
--    asm/vmxfeatures.h and asm/msr-index.h;
--  * AMD APM Vol. 2, 15.4 and Appendix E (CPUID 8000_0001h, 8000_000Ah,
--    8000_001Fh); cross-checked against Linux's asm/cpufeatures.h words 6,
--    15 and 19.
--
-- A VMX control MSR reports the settings the CPU allows: bits 31:0 are the
-- allowed 0-settings (a 1 there means the control must be 1), bits 63:32
-- the allowed 1-settings (a 1 means the control may be 1). A control is
-- "offered" when its allowed-1 bit is set. The tertiary controls MSR has
-- only allowed-1 bits, all 64 of them.
--
-- VMX_Verdict and SVM_Verdict name the first requirement of docs/hypervisor.md
-- (decision 1: EPT/NPT, VPID/ASID tags, unrestricted guest, no shadow MMU,
-- no instruction emulation) that the CPU does not meet, or Usable.
-- Proved (gnatprove level 2, tests/virtualization-features): a verdict is
-- Usable exactly when every requirement holds.
-------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package Virtualization_Features with
    SPARK_Mode => On,
    Pure
is
    -- MSR numbers (Intel SDM Vol. 4, Table 2-2).
    IA32_FEATURE_CONTROL       : constant := 16#0000_003A#;
    IA32_VMX_BASIC             : constant := 16#0000_0480#;
    IA32_VMX_PINBASED_CTLS     : constant := 16#0000_0481#;
    IA32_VMX_PROCBASED_CTLS    : constant := 16#0000_0482#;
    IA32_VMX_EXIT_CTLS         : constant := 16#0000_0483#;
    IA32_VMX_ENTRY_CTLS        : constant := 16#0000_0484#;
    IA32_VMX_MISC              : constant := 16#0000_0485#;
    IA32_VMX_PROCBASED_CTLS2   : constant := 16#0000_048B#;
    IA32_VMX_EPT_VPID_CAP      : constant := 16#0000_048C#;
    IA32_VMX_TRUE_PINBASED_CTLS  : constant := 16#0000_048D#;
    IA32_VMX_TRUE_PROCBASED_CTLS : constant := 16#0000_048E#;
    IA32_VMX_TRUE_EXIT_CTLS    : constant := 16#0000_048F#;
    IA32_VMX_TRUE_ENTRY_CTLS   : constant := 16#0000_0490#;
    IA32_VMX_PROCBASED_CTLS3   : constant := 16#0000_0492#;
    -- AMD VM_CR (APM Vol. 2, 15.30.1).
    MSR_VM_CR                  : constant := 16#C001_0114#;

    -- CPUID leaves (AMD APM Vol. 3, Appendix E).
    Extended_Features_Leaf     : constant := 16#8000_0001#;
    SVM_Leaf                   : constant := 16#8000_000A#;
    Encrypted_Memory_Leaf      : constant := 16#8000_001F#;

    subtype Bit_Number is Natural range 0 .. 63;

    function Bit (Value : Unsigned_64; N : Bit_Number) return Boolean is
      ((Value and Shift_Left (1, N)) /= 0);

    function Field (Value : Unsigned_64; Low : Bit_Number; Width : Positive)
      return Unsigned_64 is
      (Shift_Right (Value, Low) and (Shift_Left (1, Width) - 1))
    with Pre => Width <= 32;

    -- A control's allowed-1 bit (the MSR's upper half).
    Allowed_One_Base : constant := 32;
    subtype Control_Bit is Natural range 0 .. 31;
    function Offered (Controls : Unsigned_64; N : Control_Bit) return Boolean is
      (Bit (Controls, Allowed_One_Base + N));

    -- IA32_FEATURE_CONTROL.
    Feature_Control_Lock        : constant Bit_Number := 0;
    Feature_Control_VMX_Outside_SMX : constant Bit_Number := 2;

    -- IA32_VMX_BASIC.
    Basic_Region_Size_Low   : constant Bit_Number := 32;
    Basic_Region_Size_Width : constant := 13;
    Basic_Memory_Type_Low   : constant Bit_Number := 50;
    Basic_Memory_Type_Width : constant := 4;
    Memory_Type_Write_Back  : constant := 6;
    Basic_True_Controls     : constant Bit_Number := 55;

    -- Pin-based controls.
    Pin_External_Interrupt_Exiting : constant Control_Bit := 0;
    Pin_NMI_Exiting                : constant Control_Bit := 3;
    Pin_Virtual_NMIs               : constant Control_Bit := 5;
    Pin_Preemption_Timer           : constant Control_Bit := 6;
    Pin_Posted_Interrupts          : constant Control_Bit := 7;

    -- Primary processor-based controls.
    Proc_Interrupt_Window   : constant Control_Bit := 2;
    Proc_HLT_Exiting        : constant Control_Bit := 7;
    Proc_TPR_Shadow         : constant Control_Bit := 21;
    Proc_NMI_Window         : constant Control_Bit := 22;
    Proc_IO_Bitmaps         : constant Control_Bit := 25;
    Proc_Monitor_Trap_Flag  : constant Control_Bit := 27;
    Proc_MSR_Bitmaps        : constant Control_Bit := 28;
    Proc_Pause_Exiting      : constant Control_Bit := 30;
    Proc_Tertiary_Controls  : constant Control_Bit := 17;
    Proc_Secondary_Controls : constant Control_Bit := 31;

    -- Secondary processor-based controls.
    Proc2_Virtualize_APIC_Accesses : constant Control_Bit := 0;
    Proc2_EPT                      : constant Control_Bit := 1;
    Proc2_Virtualize_X2APIC        : constant Control_Bit := 4;
    Proc2_VPID                     : constant Control_Bit := 5;
    Proc2_Unrestricted_Guest       : constant Control_Bit := 7;
    Proc2_APIC_Register_Virtualization : constant Control_Bit := 8;
    Proc2_Virtual_Interrupt_Delivery : constant Control_Bit := 9;
    Proc2_Pause_Loop_Exiting       : constant Control_Bit := 10;
    Proc2_VMCS_Shadowing           : constant Control_Bit := 14;
    Proc2_Page_Modification_Logging : constant Control_Bit := 17;
    Proc2_XSAVES                   : constant Control_Bit := 20;
    Proc2_Mode_Based_Execute       : constant Control_Bit := 22;
    Proc2_TSC_Scaling              : constant Control_Bit := 25;
    Proc2_Bus_Lock_Detection       : constant Control_Bit := 30;
    Proc2_Notify_Exiting           : constant Control_Bit := 31;

    -- Tertiary processor-based controls (allowed-1 only, all 64 bits).
    Proc3_HLAT              : constant Bit_Number := 1;
    Proc3_IPI_Virtualization : constant Bit_Number := 4;

    -- VM-exit and VM-entry controls.
    Exit_Host_Address_Space_Size : constant Control_Bit := 9;
    Exit_Acknowledge_Interrupt   : constant Control_Bit := 15;
    Exit_Load_EFER               : constant Control_Bit := 21;
    Entry_IA32E_Mode_Guest       : constant Control_Bit := 9;
    Entry_Load_EFER              : constant Control_Bit := 15;

    -- IA32_VMX_MISC.
    Misc_Timer_Rate_Low   : constant Bit_Number := 0;
    Misc_Timer_Rate_Width : constant := 5;
    Misc_Activity_HLT     : constant Bit_Number := 6;

    -- IA32_VMX_EPT_VPID_CAP.
    EPT_Walk_4          : constant Bit_Number := 6;
    EPT_Walk_5          : constant Bit_Number := 7;
    EPT_Write_Back      : constant Bit_Number := 14;
    EPT_Pages_2M        : constant Bit_Number := 16;
    EPT_Pages_1G        : constant Bit_Number := 17;
    EPT_INVEPT          : constant Bit_Number := 20;
    EPT_Accessed_Dirty  : constant Bit_Number := 21;
    EPT_INVEPT_Single   : constant Bit_Number := 25;
    EPT_INVEPT_All      : constant Bit_Number := 26;
    VPID_INVVPID        : constant Bit_Number := 32;
    VPID_INVVPID_Single : constant Bit_Number := 41;
    VPID_INVVPID_All    : constant Bit_Number := 42;

    -- CPUID 8000_0001h ECX, 8000_000Ah EDX, 8000_001Fh EAX; VM_CR.
    Extended_ECX_SVM     : constant Bit_Number := 2;
    SVM_Nested_Paging    : constant Bit_Number := 0;
    SVM_LBR_Virtualization : constant Bit_Number := 1;
    SVM_Lock             : constant Bit_Number := 2;
    SVM_Next_RIP_Save    : constant Bit_Number := 3;
    SVM_TSC_Rate         : constant Bit_Number := 4;
    SVM_VMCB_Clean       : constant Bit_Number := 5;
    SVM_Flush_By_ASID    : constant Bit_Number := 6;
    SVM_Decode_Assists   : constant Bit_Number := 7;
    SVM_Pause_Filter     : constant Bit_Number := 10;
    SVM_Pause_Threshold  : constant Bit_Number := 12;
    SVM_AVIC             : constant Bit_Number := 13;
    SVM_VMSAVE_Virtualization : constant Bit_Number := 15;
    SVM_Virtual_GIF      : constant Bit_Number := 16;
    SVM_GMET             : constant Bit_Number := 17;
    SVM_X2AVIC           : constant Bit_Number := 18;
    SVM_Virtual_Spec_Ctrl : constant Bit_Number := 20;
    SVM_Virtual_NMI      : constant Bit_Number := 25;
    SVM_Bus_Lock_Threshold : constant Bit_Number := 29;
    SVM_Idle_HLT         : constant Bit_Number := 30;
    Memory_SME           : constant Bit_Number := 0;
    Memory_SEV           : constant Bit_Number := 1;
    Memory_SEV_ES        : constant Bit_Number := 3;
    Memory_SEV_SNP       : constant Bit_Number := 4;
    VM_CR_SVM_Disabled   : constant Bit_Number := 4;
    SVM_Revision_Width   : constant := 8;
    -- ASID 0 is the host's; a guest needs at least one more.
    Minimum_ASIDs        : constant := 2;

    ---------------------------------------------------------------------------
    -- VMX
    ---------------------------------------------------------------------------

    -- What the reader got. A MSR the CPU refused (#GP) reads as Ok False
    -- and value 0, so its features decode as absent.
    type VMX_Raw is record
        Feature_Control : Unsigned_64 := 0;
        Basic           : Unsigned_64 := 0;
        Pin             : Unsigned_64 := 0;  -- TRUE_ variant when offered
        Proc            : Unsigned_64 := 0;
        Exit_Controls   : Unsigned_64 := 0;
        Entry_Controls  : Unsigned_64 := 0;
        Misc            : Unsigned_64 := 0;
        Proc2           : Unsigned_64 := 0;  -- 0 when not offered
        EPT_VPID        : Unsigned_64 := 0;  -- 0 without EPT or VPID
        Proc3           : Unsigned_64 := 0;  -- 0 when not offered
    end record;

    subtype Region_Bytes is Unsigned_64 range 0 .. 2 ** Basic_Region_Size_Width - 1;
    subtype Timer_Shift is Unsigned_64 range 0 .. 2 ** Misc_Timer_Rate_Width - 1;

    type VMX_Features is record
        Firmware_Locked, Enabled_Outside_SMX : Boolean := False;
        True_Controls, Write_Back_Regions  : Boolean := False;
        VMCS_Bytes : Region_Bytes := 0;
        -- Pin-based.
        External_Interrupt_Exiting, NMI_Exiting, Virtual_NMIs,
        Preemption_Timer, Posted_Interrupts : Boolean := False;
        -- Primary.
        Interrupt_Window, HLT_Exiting, TPR_Shadow, NMI_Window, IO_Bitmaps,
        Monitor_Trap_Flag, MSR_Bitmaps, Pause_Exiting, Secondary,
        Tertiary : Boolean := False;
        -- Secondary.
        Virtualize_APIC_Accesses, EPT, Virtualize_X2APIC, VPID,
        Unrestricted_Guest, APIC_Register_Virtualization,
        Virtual_Interrupt_Delivery, Pause_Loop_Exiting, VMCS_Shadowing,
        Page_Modification_Logging, XSAVES, Mode_Based_Execute, TSC_Scaling,
        Bus_Lock_Detection, Notify_Exiting : Boolean := False;
        -- Tertiary.
        HLAT, IPI_Virtualization : Boolean := False;
        -- Exit and entry.
        Host_64, Acknowledge_Interrupt, Exit_Load_EFER, Guest_64,
        Entry_Load_EFER : Boolean := False;
        -- Misc.
        Timer_Rate_Shift : Timer_Shift := 0;
        Activity_HLT : Boolean := False;
        -- EPT and VPID.
        EPT_Walk_4, EPT_Walk_5, EPT_Write_Back, EPT_2M, EPT_1G, INVEPT,
        EPT_Accessed_Dirty, INVEPT_Single, INVEPT_All, INVVPID,
        INVVPID_Single, INVVPID_All : Boolean := False;
    end record;

    function Decode_VMX (Raw : VMX_Raw) return VMX_Features
    with Post =>
      Decode_VMX'Result.EPT = (Offered (Raw.Proc, Proc_Secondary_Controls)
                               and then Offered (Raw.Proc2, Proc2_EPT))
      and then Decode_VMX'Result.Unrestricted_Guest =
        (Offered (Raw.Proc, Proc_Secondary_Controls)
         and then Offered (Raw.Proc2, Proc2_Unrestricted_Guest))
      and then Decode_VMX'Result.VPID =
        (Offered (Raw.Proc, Proc_Secondary_Controls)
         and then Offered (Raw.Proc2, Proc2_VPID))
      and then Decode_VMX'Result.Posted_Interrupts =
        Offered (Raw.Pin, Pin_Posted_Interrupts);

    -- The first unmet requirement, in this order, or Usable.
    type VMX_Verdict_Kind is
      (Usable, Disabled_By_Firmware, No_True_Controls, No_Write_Back_Regions,
       No_External_Interrupt_Exiting, No_NMI_Exiting, No_HLT_Exiting,
       No_IO_Bitmaps, No_MSR_Bitmaps, No_Interrupt_Window, No_NMI_Window,
       No_Secondary_Controls, No_EPT, No_Unrestricted_Guest, No_VPID,
       No_EPT_Walk_4, No_EPT_Write_Back, No_INVEPT_Single,
       No_INVVPID_Single, No_Host_64, No_Acknowledge_Interrupt,
       No_Guest_64, No_EFER_Load);

    -- Every requirement of docs/hypervisor.md decision 1 and section 4.
    -- An unlocked IA32_FEATURE_CONTROL is usable: the kernel sets and locks
    -- it before VMXON (slice 1).
    function VMX_Requirements_Met (F : VMX_Features) return Boolean is
      ((not F.Firmware_Locked or else F.Enabled_Outside_SMX)
       and then F.True_Controls and then F.Write_Back_Regions
       and then F.External_Interrupt_Exiting and then F.NMI_Exiting
       and then F.HLT_Exiting and then F.IO_Bitmaps and then F.MSR_Bitmaps
       and then F.Interrupt_Window and then F.NMI_Window
       and then F.Secondary and then F.EPT and then F.Unrestricted_Guest
       and then F.VPID and then F.EPT_Walk_4 and then F.EPT_Write_Back
       and then F.INVEPT_Single and then F.INVVPID_Single
       and then F.Host_64 and then F.Acknowledge_Interrupt
       and then F.Guest_64 and then F.Exit_Load_EFER
       and then F.Entry_Load_EFER);

    function VMX_Verdict (F : VMX_Features) return VMX_Verdict_Kind
    with Post => (VMX_Verdict'Result = Usable) = VMX_Requirements_Met (F);

    ---------------------------------------------------------------------------
    -- SVM
    ---------------------------------------------------------------------------

    type SVM_Raw is record
        Extended_ECX : Unsigned_32 := 0;   -- CPUID 8000_0001h ECX
        Has_SVM_Leaf : Boolean := False;   -- max extended leaf >= 8000_000Ah
        Revision     : Unsigned_32 := 0;   -- CPUID 8000_000Ah EAX
        ASIDs        : Unsigned_32 := 0;   -- EBX
        Features     : Unsigned_32 := 0;   -- EDX
        Has_Memory_Leaf : Boolean := False; -- max extended leaf >= 8000_001Fh
        Memory_Features : Unsigned_32 := 0; -- CPUID 8000_001Fh EAX
        VM_CR        : Unsigned_64 := 0;
        VM_CR_Read   : Boolean := False;
    end record;

    subtype SVM_Revision is Unsigned_32 range 0 .. 2 ** SVM_Revision_Width - 1;

    type SVM_Features is record
        SVM, Disabled_By_Firmware : Boolean := False;
        Revision : SVM_Revision := 0;
        ASIDs : Unsigned_32 := 0;
        Nested_Paging, LBR_Virtualization, Lock, Next_RIP_Save, TSC_Rate,
        VMCB_Clean, Flush_By_ASID, Decode_Assists, Pause_Filter,
        Pause_Threshold, AVIC, VMSAVE_Virtualization, Virtual_GIF, GMET,
        X2AVIC, Virtual_Spec_Ctrl, Virtual_NMI, Bus_Lock_Threshold,
        Idle_HLT : Boolean := False;
        SME, SEV, SEV_ES, SEV_SNP : Boolean := False;
    end record;

    function Decode_SVM (Raw : SVM_Raw) return SVM_Features
    with Post =>
      Decode_SVM'Result.SVM = Bit (Unsigned_64 (Raw.Extended_ECX), Extended_ECX_SVM)
      and then Decode_SVM'Result.Nested_Paging =
        (Decode_SVM'Result.SVM and then Raw.Has_SVM_Leaf
         and then Bit (Unsigned_64 (Raw.Features), SVM_Nested_Paging))
      and then Decode_SVM'Result.Next_RIP_Save =
        (Decode_SVM'Result.SVM and then Raw.Has_SVM_Leaf
         and then Bit (Unsigned_64 (Raw.Features), SVM_Next_RIP_Save));

    type SVM_Verdict_Kind is
      (Usable, No_SVM, Disabled_By_Firmware, No_Nested_Paging,
       No_Next_RIP_Save, No_ASIDs);

    -- NPT for the guest's memory, next-RIP save so the kernel never decodes
    -- an instruction to skip it (K8), and a guest ASID.
    function SVM_Requirements_Met (F : SVM_Features) return Boolean is
      (F.SVM and then not F.Disabled_By_Firmware and then F.Nested_Paging
       and then F.Next_RIP_Save and then F.ASIDs >= Minimum_ASIDs);

    function SVM_Verdict (F : SVM_Features) return SVM_Verdict_Kind
    with Post => (SVM_Verdict'Result = Usable) = SVM_Requirements_Met (F);

end Virtualization_Features;
