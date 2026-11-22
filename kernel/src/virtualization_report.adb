with Interfaces; use Interfaces;
with CPUID;
with Msr_Safe;
with TextIO; use TextIO;
with Virtualization_Features; use Virtualization_Features;

package body Virtualization_Report with SPARK_Mode => Off is

    Found_SVM   : Boolean := False;
    Found_ASIDs : Unsigned_32 := 0;

    function SVM_Usable return Boolean is (Found_SVM);
    function SVM_ASIDs return Unsigned_32 is (Found_ASIDs);
    Found_V_Spec_Ctrl : Boolean := False;
    function SVM_Virtual_Spec_Ctrl return Boolean is (Found_V_Spec_Ctrl);

    Found_VMX : Boolean := False;
    Found_VMX_Raw : VMX_Raw;
    function VMX_Usable return Boolean is (Found_VMX);
    function VMX_Values return VMX_Raw is (Found_VMX_Raw);

    function Read (Register : Unsigned_32) return Unsigned_64 is
        Value : Unsigned_64;
        Ok    : Boolean;
    begin
        Msr_Safe.Read (Register, Value, Ok);
        return (if Ok then Value else 0);
    end Read;

    procedure Flag (Name : String; Value : Boolean) is
    begin
        print (" ");
        print (Name);
        print ((if Value then "=yes" else "=no"));
    end Flag;

    function Image (V : VMX_Verdict_Kind) return String is
      (case V is
         when Usable                       => "usable",
         when Disabled_By_Firmware         => "disabled by firmware (IA32_FEATURE_CONTROL locked without VMX)",
         when No_True_Controls             => "no TRUE control MSRs",
         when No_Write_Back_Regions        => "VMCS regions not write-back",
         when No_External_Interrupt_Exiting => "no external-interrupt exiting",
         when No_NMI_Exiting               => "no NMI exiting",
         when No_HLT_Exiting               => "no HLT exiting",
         when No_IO_Bitmaps                => "no I/O bitmaps",
         when No_MSR_Bitmaps               => "no MSR bitmaps",
         when No_Interrupt_Window          => "no interrupt-window exiting",
         when No_NMI_Window                => "no NMI-window exiting",
         when No_Secondary_Controls        => "no secondary controls",
         when No_EPT                       => "no EPT",
         when No_Unrestricted_Guest        => "no unrestricted guest",
         when No_VPID                      => "no VPID",
         when No_EPT_Walk_4                => "no 4-level EPT",
         when No_EPT_Write_Back            => "no write-back EPT",
         when No_INVEPT_Single             => "no single-context INVEPT",
         when No_INVVPID_Single            => "no single-context INVVPID",
         when No_Host_64                   => "no 64-bit host on exit",
         when No_Acknowledge_Interrupt     => "no interrupt acknowledge on exit",
         when No_Guest_64                  => "no 64-bit guest entry",
         when No_EFER_Load                 => "no EFER load on entry and exit");

    function Image (V : SVM_Verdict_Kind) return String is
      (case V is
         when Usable               => "usable",
         when No_SVM               => "no SVM",
         when Disabled_By_Firmware => "disabled by firmware (VM_CR.SVMDIS)",
         when No_Nested_Paging     => "no nested paging",
         when No_Next_RIP_Save     => "no next-RIP save",
         when No_ASIDs             => "no guest ASID");

    procedure Report_VMX is
        Basic : constant Unsigned_64 := Read (IA32_VMX_BASIC);
        True_Ctls : constant Boolean := Bit (Basic, Basic_True_Controls);
        Raw   : VMX_Raw;
        F     : VMX_Features;
    begin
        Raw.Feature_Control := Read (IA32_FEATURE_CONTROL);
        Raw.Basic := Basic;
        Raw.Pin  := Read (if True_Ctls then IA32_VMX_TRUE_PINBASED_CTLS else IA32_VMX_PINBASED_CTLS);
        Raw.Proc := Read (if True_Ctls then IA32_VMX_TRUE_PROCBASED_CTLS else IA32_VMX_PROCBASED_CTLS);
        Raw.Exit_Controls := Read (if True_Ctls then IA32_VMX_TRUE_EXIT_CTLS else IA32_VMX_EXIT_CTLS);
        Raw.Entry_Controls := Read (if True_Ctls then IA32_VMX_TRUE_ENTRY_CTLS else IA32_VMX_ENTRY_CTLS);
        Raw.Misc := Read (IA32_VMX_MISC);
        -- Only the MSRs the primary controls say exist (SDM A.3.3, A.10).
        if Offered (Raw.Proc, Proc_Secondary_Controls) then
            Raw.Proc2 := Read (IA32_VMX_PROCBASED_CTLS2);
            if Offered (Raw.Proc2, Proc2_EPT) or else Offered (Raw.Proc2, Proc2_VPID) then
                Raw.EPT_VPID := Read (IA32_VMX_EPT_VPID_CAP);
            end if;
        end if;
        if Offered (Raw.Proc, Proc_Tertiary_Controls) then
            Raw.Proc3 := Read (IA32_VMX_PROCBASED_CTLS3);
        end if;
        F := Decode_VMX (Raw);
        Found_VMX := VMX_Verdict (F) = Usable;
        Found_VMX_Raw := Raw;

        print ("virt: vmx ");
        println (Image (VMX_Verdict (F)));
        print ("virt: vmx ept");
        Flag ("walk4", F.EPT_Walk_4); Flag ("walk5", F.EPT_Walk_5);
        Flag ("wb", F.EPT_Write_Back); Flag ("2m", F.EPT_2M); Flag ("1g", F.EPT_1G);
        Flag ("invept-single", F.INVEPT_Single); Flag ("ad", F.EPT_Accessed_Dirty);
        Flag ("vpid", F.VPID); Flag ("invvpid-single", F.INVVPID_Single);
        Flag ("unrestricted-guest", F.Unrestricted_Guest);
        Flag ("preemption-timer", F.Preemption_Timer);
        print (" timer-shift=");
        printd (F.Timer_Rate_Shift);
        println;
        print ("virt: vmx apic");
        Flag ("tpr-shadow", F.TPR_Shadow);
        Flag ("apic-accesses", F.Virtualize_APIC_Accesses);
        Flag ("x2apic", F.Virtualize_X2APIC);
        Flag ("register-virt", F.APIC_Register_Virtualization);
        Flag ("interrupt-delivery", F.Virtual_Interrupt_Delivery);
        Flag ("posted-interrupts", F.Posted_Interrupts);
        Flag ("ipi-virt", F.IPI_Virtualization);
        Flag ("virtual-nmis", F.Virtual_NMIs);
        println;
        print ("virt: vmx other");
        Flag ("mbec", F.Mode_Based_Execute);
        Flag ("vmcs-shadowing", F.VMCS_Shadowing);
        Flag ("pml", F.Page_Modification_Logging);
        Flag ("tsc-scaling", F.TSC_Scaling);
        Flag ("bus-lock", F.Bus_Lock_Detection);
        Flag ("notify", F.Notify_Exiting);
        Flag ("pause-loop", F.Pause_Loop_Exiting);
        Flag ("hlat", F.HLAT);
        Flag ("xsaves", F.XSAVES);
        Flag ("feature-control-locked", F.Firmware_Locked);
        Flag ("outside-smx", F.Enabled_Outside_SMX);
        print (" vmcs-bytes=");
        printd (F.VMCS_Bytes);
        println;
        print ("virt: vmx raw basic=");
        print (Raw.Basic);
        print (" pin=");  print (Raw.Pin);
        print (" proc="); print (Raw.Proc);
        print (" proc2="); print (Raw.Proc2);
        print (" proc3="); print (Raw.Proc3);
        println;
        print ("virt: vmx raw exit=");
        print (Raw.Exit_Controls);
        print (" entry="); print (Raw.Entry_Controls);
        print (" misc="); print (Raw.Misc);
        print (" ept-vpid="); print (Raw.EPT_VPID);
        print (" feature-control="); print (Raw.Feature_Control);
        println;
    end Report_VMX;

    procedure Report_SVM is
        Raw : SVM_Raw;
        F   : SVM_Features;
        EAX, EBX, ECX, EDX : Unsigned_32;
        Max_Extended : constant Unsigned_32 := CPUID.getMaxExtendedFunction;
    begin
        CPUID.cpuid (Extended_Features_Leaf, EAX, EBX, ECX, EDX);
        Raw.Extended_ECX := ECX;
        Raw.Has_SVM_Leaf := Max_Extended >= SVM_Leaf;
        if Raw.Has_SVM_Leaf then
            CPUID.cpuid (SVM_Leaf, EAX, EBX, ECX, EDX);
            Raw.Revision := EAX;
            Raw.ASIDs := EBX;
            Raw.Features := EDX;
        end if;
        Raw.Has_Memory_Leaf := Max_Extended >= Encrypted_Memory_Leaf;
        if Raw.Has_Memory_Leaf then
            CPUID.cpuid (Encrypted_Memory_Leaf, EAX, EBX, ECX, EDX);
            Raw.Memory_Features := EAX;
        end if;
        Msr_Safe.Read (MSR_VM_CR, Raw.VM_CR, Raw.VM_CR_Read);
        F := Decode_SVM (Raw);
        Found_SVM := SVM_Verdict (F) = Usable;
        Found_ASIDs := F.ASIDs;
        Found_V_Spec_Ctrl := F.Virtual_Spec_Ctrl;

        print ("virt: svm ");
        println (Image (SVM_Verdict (F)));
        print ("virt: svm revision=");
        printd (F.Revision);
        print (" asids=");
        printd (F.ASIDs);
        Flag ("npt", F.Nested_Paging); Flag ("nrip", F.Next_RIP_Save);
        Flag ("vmcb-clean", F.VMCB_Clean); Flag ("flush-by-asid", F.Flush_By_ASID);
        Flag ("decode-assists", F.Decode_Assists);
        Flag ("tsc-rate", F.TSC_Rate);
        println;
        print ("virt: svm apic");
        Flag ("avic", F.AVIC); Flag ("x2avic", F.X2AVIC);
        Flag ("vnmi", F.Virtual_NMI); Flag ("vgif", F.Virtual_GIF);
        println;
        print ("virt: svm other");
        Flag ("gmet", F.GMET); Flag ("v-spec-ctrl", F.Virtual_Spec_Ctrl);
        Flag ("pause-filter", F.Pause_Filter); Flag ("pause-threshold", F.Pause_Threshold);
        Flag ("vmsave-virt", F.VMSAVE_Virtualization); Flag ("lbr-virt", F.LBR_Virtualization);
        Flag ("bus-lock-threshold", F.Bus_Lock_Threshold); Flag ("idle-hlt", F.Idle_HLT);
        Flag ("sme", F.SME); Flag ("sev", F.SEV); Flag ("sev-es", F.SEV_ES);
        Flag ("sev-snp", F.SEV_SNP);
        println;
        print ("virt: svm raw features=");
        print (Raw.Features);
        print (" memory="); print (Raw.Memory_Features);
        print (" vm-cr="); print (Raw.VM_CR);
        println;
    end Report_SVM;

    procedure Report is
        EAX, EBX, ECX, EDX : Unsigned_32;
        Has_SVM : Boolean := False;
    begin
        if CPUID.getMaxExtendedFunction >= Extended_Features_Leaf then
            CPUID.cpuid (Extended_Features_Leaf, EAX, EBX, ECX, EDX);
            Has_SVM := Bit (Unsigned_64 (ECX), Extended_ECX_SVM);
        end if;
        if CPUID.leaf1ecx.hasVMX then
            Report_VMX;
        elsif Has_SVM then
            Report_SVM;
        else
            println ("virt: none: the CPU reports neither VMX nor SVM");
        end if;
    end Report;
end Virtualization_Report;
