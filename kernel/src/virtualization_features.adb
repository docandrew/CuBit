pragma Ada_2022;
package body Virtualization_Features with
    SPARK_Mode => On
is
    function Decode_VMX (Raw : VMX_Raw) return VMX_Features is
        Secondary : constant Boolean := Offered (Raw.Proc, Proc_Secondary_Controls);
        Tertiary  : constant Boolean := Offered (Raw.Proc, Proc_Tertiary_Controls);
        function S2 (N : Control_Bit) return Boolean is
          (Secondary and then Offered (Raw.Proc2, N));
        function S3 (N : Bit_Number) return Boolean is
          (Tertiary and then Bit (Raw.Proc3, N));
        EPT_On  : constant Boolean := S2 (Proc2_EPT);
        VPID_On : constant Boolean := S2 (Proc2_VPID);
        function E (N : Bit_Number) return Boolean is
          (EPT_On and then Bit (Raw.EPT_VPID, N));
        function P (N : Bit_Number) return Boolean is
          (VPID_On and then Bit (Raw.EPT_VPID, N));
    begin
        return
          (Firmware_Locked     => Bit (Raw.Feature_Control, Feature_Control_Lock),
           Enabled_Outside_SMX => Bit (Raw.Feature_Control, Feature_Control_VMX_Outside_SMX),
           True_Controls       => Bit (Raw.Basic, Basic_True_Controls),
           Write_Back_Regions  =>
             Field (Raw.Basic, Basic_Memory_Type_Low, Basic_Memory_Type_Width)
               = Memory_Type_Write_Back,
           VMCS_Bytes => Field (Raw.Basic, Basic_Region_Size_Low, Basic_Region_Size_Width),
           External_Interrupt_Exiting => Offered (Raw.Pin, Pin_External_Interrupt_Exiting),
           NMI_Exiting         => Offered (Raw.Pin, Pin_NMI_Exiting),
           Virtual_NMIs        => Offered (Raw.Pin, Pin_Virtual_NMIs),
           Preemption_Timer    => Offered (Raw.Pin, Pin_Preemption_Timer),
           Posted_Interrupts   => Offered (Raw.Pin, Pin_Posted_Interrupts),
           Interrupt_Window    => Offered (Raw.Proc, Proc_Interrupt_Window),
           HLT_Exiting         => Offered (Raw.Proc, Proc_HLT_Exiting),
           TPR_Shadow          => Offered (Raw.Proc, Proc_TPR_Shadow),
           NMI_Window          => Offered (Raw.Proc, Proc_NMI_Window),
           IO_Bitmaps          => Offered (Raw.Proc, Proc_IO_Bitmaps),
           Monitor_Trap_Flag   => Offered (Raw.Proc, Proc_Monitor_Trap_Flag),
           MSR_Bitmaps         => Offered (Raw.Proc, Proc_MSR_Bitmaps),
           Pause_Exiting       => Offered (Raw.Proc, Proc_Pause_Exiting),
           Secondary           => Secondary,
           Tertiary            => Tertiary,
           Virtualize_APIC_Accesses => S2 (Proc2_Virtualize_APIC_Accesses),
           EPT                 => EPT_On,
           Virtualize_X2APIC   => S2 (Proc2_Virtualize_X2APIC),
           VPID                => VPID_On,
           Unrestricted_Guest  => S2 (Proc2_Unrestricted_Guest),
           APIC_Register_Virtualization => S2 (Proc2_APIC_Register_Virtualization),
           Virtual_Interrupt_Delivery   => S2 (Proc2_Virtual_Interrupt_Delivery),
           Pause_Loop_Exiting  => S2 (Proc2_Pause_Loop_Exiting),
           VMCS_Shadowing      => S2 (Proc2_VMCS_Shadowing),
           Page_Modification_Logging => S2 (Proc2_Page_Modification_Logging),
           XSAVES              => S2 (Proc2_XSAVES),
           Mode_Based_Execute  => S2 (Proc2_Mode_Based_Execute),
           TSC_Scaling         => S2 (Proc2_TSC_Scaling),
           Bus_Lock_Detection  => S2 (Proc2_Bus_Lock_Detection),
           Notify_Exiting      => S2 (Proc2_Notify_Exiting),
           HLAT                => S3 (Proc3_HLAT),
           IPI_Virtualization  => S3 (Proc3_IPI_Virtualization),
           Host_64             => Offered (Raw.Exit_Controls, Exit_Host_Address_Space_Size),
           Acknowledge_Interrupt => Offered (Raw.Exit_Controls, Exit_Acknowledge_Interrupt),
           Exit_Load_EFER      => Offered (Raw.Exit_Controls, Exit_Load_EFER),
           Guest_64            => Offered (Raw.Entry_Controls, Entry_IA32E_Mode_Guest),
           Entry_Load_EFER     => Offered (Raw.Entry_Controls, Entry_Load_EFER),
           Timer_Rate_Shift    => Field (Raw.Misc, Misc_Timer_Rate_Low, Misc_Timer_Rate_Width),
           Activity_HLT        => Bit (Raw.Misc, Misc_Activity_HLT),
           EPT_Walk_4          => E (EPT_Walk_4),
           EPT_Walk_5          => E (EPT_Walk_5),
           EPT_Write_Back      => E (EPT_Write_Back),
           EPT_2M              => E (EPT_Pages_2M),
           EPT_1G              => E (EPT_Pages_1G),
           INVEPT              => E (EPT_INVEPT),
           EPT_Accessed_Dirty  => E (EPT_Accessed_Dirty),
           INVEPT_Single       => E (EPT_INVEPT) and then E (EPT_INVEPT_Single),
           INVEPT_All          => E (EPT_INVEPT) and then E (EPT_INVEPT_All),
           INVVPID             => P (VPID_INVVPID),
           INVVPID_Single      => P (VPID_INVVPID) and then P (VPID_INVVPID_Single),
           INVVPID_All         => P (VPID_INVVPID) and then P (VPID_INVVPID_All));
    end Decode_VMX;

    function VMX_Verdict (F : VMX_Features) return VMX_Verdict_Kind is
    begin
        if F.Firmware_Locked and then not F.Enabled_Outside_SMX then
            return Disabled_By_Firmware;
        elsif not F.True_Controls then
            return No_True_Controls;
        elsif not F.Write_Back_Regions then
            return No_Write_Back_Regions;
        elsif not F.External_Interrupt_Exiting then
            return No_External_Interrupt_Exiting;
        elsif not F.NMI_Exiting then
            return No_NMI_Exiting;
        elsif not F.HLT_Exiting then
            return No_HLT_Exiting;
        elsif not F.IO_Bitmaps then
            return No_IO_Bitmaps;
        elsif not F.MSR_Bitmaps then
            return No_MSR_Bitmaps;
        elsif not F.Interrupt_Window then
            return No_Interrupt_Window;
        elsif not F.NMI_Window then
            return No_NMI_Window;
        elsif not F.Secondary then
            return No_Secondary_Controls;
        elsif not F.EPT then
            return No_EPT;
        elsif not F.Unrestricted_Guest then
            return No_Unrestricted_Guest;
        elsif not F.VPID then
            return No_VPID;
        elsif not F.EPT_Walk_4 then
            return No_EPT_Walk_4;
        elsif not F.EPT_Write_Back then
            return No_EPT_Write_Back;
        elsif not F.INVEPT_Single then
            return No_INVEPT_Single;
        elsif not F.INVVPID_Single then
            return No_INVVPID_Single;
        elsif not F.Host_64 then
            return No_Host_64;
        elsif not F.Acknowledge_Interrupt then
            return No_Acknowledge_Interrupt;
        elsif not F.Guest_64 then
            return No_Guest_64;
        elsif not F.Exit_Load_EFER or else not F.Entry_Load_EFER then
            return No_EFER_Load;
        else
            return Usable;
        end if;
    end VMX_Verdict;

    function Decode_SVM (Raw : SVM_Raw) return SVM_Features is
        SVM_On : constant Boolean :=
          Bit (Unsigned_64 (Raw.Extended_ECX), Extended_ECX_SVM);
        Leaf   : constant Boolean := SVM_On and then Raw.Has_SVM_Leaf;
        function S (N : Bit_Number) return Boolean is
          (Leaf and then Bit (Unsigned_64 (Raw.Features), N));
        function M (N : Bit_Number) return Boolean is
          (SVM_On and then Raw.Has_Memory_Leaf
           and then Bit (Unsigned_64 (Raw.Memory_Features), N));
    begin
        return
          (SVM                  => SVM_On,
           Disabled_By_Firmware =>
             SVM_On and then Raw.VM_CR_Read and then Bit (Raw.VM_CR, VM_CR_SVM_Disabled),
           Revision => (if Leaf then Raw.Revision and (2 ** SVM_Revision_Width - 1) else 0),
           ASIDs    => (if Leaf then Raw.ASIDs else 0),
           Nested_Paging        => S (SVM_Nested_Paging),
           LBR_Virtualization   => S (SVM_LBR_Virtualization),
           Lock                 => S (SVM_Lock),
           Next_RIP_Save        => S (SVM_Next_RIP_Save),
           TSC_Rate             => S (SVM_TSC_Rate),
           VMCB_Clean           => S (SVM_VMCB_Clean),
           Flush_By_ASID        => S (SVM_Flush_By_ASID),
           Decode_Assists       => S (SVM_Decode_Assists),
           Pause_Filter         => S (SVM_Pause_Filter),
           Pause_Threshold      => S (SVM_Pause_Threshold),
           AVIC                 => S (SVM_AVIC),
           VMSAVE_Virtualization => S (SVM_VMSAVE_Virtualization),
           Virtual_GIF          => S (SVM_Virtual_GIF),
           GMET                 => S (SVM_GMET),
           X2AVIC               => S (SVM_X2AVIC),
           Virtual_Spec_Ctrl    => S (SVM_Virtual_Spec_Ctrl),
           Virtual_NMI          => S (SVM_Virtual_NMI),
           Bus_Lock_Threshold   => S (SVM_Bus_Lock_Threshold),
           Idle_HLT             => S (SVM_Idle_HLT),
           SME                  => M (Memory_SME),
           SEV                  => M (Memory_SEV),
           SEV_ES               => M (Memory_SEV_ES),
           SEV_SNP              => M (Memory_SEV_SNP));
    end Decode_SVM;

    function SVM_Verdict (F : SVM_Features) return SVM_Verdict_Kind is
    begin
        if not F.SVM then
            return No_SVM;
        elsif F.Disabled_By_Firmware then
            return Disabled_By_Firmware;
        elsif not F.Nested_Paging then
            return No_Nested_Paging;
        elsif not F.Next_RIP_Save then
            return No_Next_RIP_Save;
        elsif F.ASIDs < Minimum_ASIDs then
            return No_ASIDs;
        else
            return Usable;
        end if;
    end SVM_Verdict;
end Virtualization_Features;
