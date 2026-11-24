pragma Ada_2022;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;
with BuddyAllocator;
with Capability_ABI;
with Charged_Metadata_Pages;
with CPUID;
with Debug_View_Walk;
with Guest_Speculation;
with Guest_Tables;
with Ipi;
with PerCPUData;
with Process.User_Memory;
with Process_Capabilities;
with Process_Identities;
with Process_Lifetime;
with Spinlocks;
with SVM_Layout; use SVM_Layout;
with TextIO; use TextIO;
with Time;
with User_Page_Walk;
with Virtmem;
with Virtualization_Report;
with VMX_Run;
with VM_ABI;
with VM_Admission;
with x86;
with Config;

package body VM_Objects with SPARK_Mode => Off is
    use type Process.ProcessID;
    use type Process.ThreadID;
    use type Process.ProcessState;
    use type Capabilities.CapabilityType;
    use type Debug_View_Walk.Verdict;
    use type VM_Admission.Range_Verdict;
    use type VM_ABI.Outcome;
    use type VM_ABI.Exit_Kind;
    use type VM_ABI.Run_Status;
    use type VM_ABI.State_Set;
    use type Guest_Speculation.Write_Verdict;
    package Caps renames Capabilities;
    package Spec renames Guest_Speculation;

    ---------------------------------------------------------------------------
    -- Memory access to kernel-owned pages (VMCBs, nested tables, maps)
    ---------------------------------------------------------------------------
    procedure Put64 (Base : System.Address; Offset : Natural; V : Unsigned_64) is
        W : Unsigned_64 with Import, Volatile, Address => Base + Storage_Offset (Offset);
    begin
        W := V;
    end Put64;

    function Get64 (Base : System.Address; Offset : Natural) return Unsigned_64 is
        W : Unsigned_64 with Import, Volatile, Address => Base + Storage_Offset (Offset);
    begin
        return W;
    end Get64;

    procedure Put32 (Base : System.Address; Offset : Natural; V : Unsigned_32) is
        W : Unsigned_32 with Import, Volatile, Address => Base + Storage_Offset (Offset);
    begin
        W := V;
    end Put32;

    function Get32 (Base : System.Address; Offset : Natural) return Unsigned_32 is
        W : Unsigned_32 with Import, Volatile, Address => Base + Storage_Offset (Offset);
    begin
        return W;
    end Get32;

    procedure Put16 (Base : System.Address; Offset : Natural; V : Unsigned_16) is
        W : Unsigned_16 with Import, Volatile, Address => Base + Storage_Offset (Offset);
    begin
        W := V;
    end Put16;

    function Get16 (Base : System.Address; Offset : Natural) return Unsigned_16 is
        W : Unsigned_16 with Import, Volatile, Address => Base + Storage_Offset (Offset);
    begin
        return W;
    end Get16;

    function Get8 (Base : System.Address; Offset : Natural) return Unsigned_8 is
        W : Unsigned_8 with Import, Volatile, Address => Base + Storage_Offset (Offset);
    begin
        return W;
    end Get8;

    procedure Put8 (Base : System.Address; Offset : Natural; V : Unsigned_8) is
        W : Unsigned_8 with Import, Volatile, Address => Base + Storage_Offset (Offset);
    begin
        W := V;
    end Put8;

    procedure Zero_Page (Base : System.Address) is
    begin
        for I in 0 .. Guest_Tables.Page_Bytes / 8 - 1 loop
            Put64 (Base, I * 8, 0);
        end loop;
    end Zero_Page;

    procedure Fill_Bytes (Base : System.Address; Bytes : Natural; V : Unsigned_8) is
    begin
        for I in 0 .. Bytes - 1 loop
            Put8 (Base, I, V);
        end loop;
    end Fill_Bytes;

    function Bit32 (N : Word_Bit) return Unsigned_32 is (Shift_Left (1, N));
    Low_Half : constant Unsigned_64 := 16#FFFF_FFFF#;   -- EAX/EDX/ECX of an MSR access

    ---------------------------------------------------------------------------
    -- The assembly entry (svm_run.asm)
    ---------------------------------------------------------------------------
    type GPR_Block is array (0 .. 13) of Unsigned_64;  -- rbx rcx rdx rsi rdi rbp r8..r15
    GPR_RBX : constant := 0; GPR_RCX : constant := 1; GPR_RDX : constant := 2;
    GPR_RSI : constant := 3; GPR_RDI : constant := 4; GPR_RBP : constant := 5;
    GPR_R8  : constant := 6;

    -- The vCPU's SPEC_CTRL as the entry and exit code switch it (svm_run.asm,
    -- vmx_entry.asm read and write these three words).
    type Spec_Ctrl_Block is record
        Guest : Unsigned_64 := 0;   -- the guest's value (software switching)
        Host  : Unsigned_64 := 0;   -- this CPU's host value, set before each entry
        Mode  : Unsigned_64 := 0;   -- Guest_Speculation.Switch
    end record with Convention => C;
    for Spec_Ctrl_Block use record
        Guest at 0 range 0 .. 63;
        Host  at 8 range 0 .. 63;
        Mode  at 16 range 0 .. 63;
    end record;

    procedure cubitSvmRun (VMCB, Host_Area : Unsigned_64; GPRs, Spec_Ctrl : System.Address)
      with Import, Convention => C, External_Name => "cubitSvmRun";

    ---------------------------------------------------------------------------
    -- Per-CPU host state and the shared permission maps
    ---------------------------------------------------------------------------
    subtype CPU_Number is Natural range 0 .. Config.MAX_CPUS - 1;
    type CPU_Phys is array (CPU_Number) of Unsigned_64;
    Host_Save   : CPU_Phys := [others => 0];   -- VM_HSAVE_PA
    Host_VMSAVE : CPU_Phys := [others => 0];   -- the host's VMSAVE area
    SVM_On_BSP  : Boolean := False;
    -- Which back end this kernel runs guests with (the boot CPU decides).
    Use_VMX     : Boolean := False;
    CPU_Ready   : array (CPU_Number) of Boolean := [others => False];

    -- SPEC_CTRL (Guest_Speculation): the bits a guest may set, whether
    -- VMRUN switches the MSR itself (V_SPEC_CTRL), and each CPU's host value
    -- (read once at setup; nothing in CuBit writes it after boot, and
    -- whatever does later, SEC-026, must keep Host_Spec_Ctrl current).
    Host_Features  : Spec.Host_CPUID;
    Spec_Mask      : Spec.Control_Mask := 0;
    Spec_By_VMRUN  : Boolean := False;
    Host_Spec_Ctrl : CPU_Phys := [others => 0];

    IOPM_Phys  : Unsigned_64 := 0;
    MSRPM_Phys : Unsigned_64 := 0;        -- SPEC_CTRL writes exit
    MSRPM_Pass_Phys : Unsigned_64 := 0;   -- SPEC_CTRL writes reach the MSR
    Maps_Ready : Boolean := False;
    Maps_Lock  : Spinlocks.Spinlock;

    -- The MSRs a guest reads and writes without an exit: those VMRUN and
    -- VMLOAD/VMSAVE switch with the VMCB (docs/hypervisor.md 4.3).
    type MSR_List is array (Positive range <>) of Unsigned_32;
    Passthrough_MSRs : constant MSR_List :=
      [16#0000_0174#, 16#0000_0175#, 16#0000_0176#,       -- SYSENTER_CS/ESP/EIP
       16#C000_0081#, 16#C000_0082#, 16#C000_0083#, 16#C000_0084#,  -- STAR..SFMASK
       16#C000_0100#, 16#C000_0101#, 16#C000_0102#];      -- FS, GS, KERNEL_GS base

    -- MSRPM: 2 bits (read, write) per MSR in three 2 KiB ranges (APM 15.11).
    MSRPM_Range_Bytes : constant := 2048;
    MSRPM_Read_Bit  : constant := 1;
    MSRPM_Write_Bit : constant := 2;
    procedure Clear_MSR_Intercept (Map : System.Address; MSR : Unsigned_32;
                                   Read : Boolean := True; Write : Boolean := True) is
        Base_Offset : Natural;
        Index : Natural;
    begin
        if MSR <= 16#1FFF# then
            Base_Offset := 0; Index := Natural (MSR);
        elsif MSR in 16#C000_0000# .. 16#C000_1FFF# then
            Base_Offset := MSRPM_Range_Bytes; Index := Natural (MSR - 16#C000_0000#);
        elsif MSR in 16#C001_0000# .. 16#C001_1FFF# then
            Base_Offset := 2 * MSRPM_Range_Bytes; Index := Natural (MSR - 16#C001_0000#);
        else
            return;
        end if;
        declare
            Byte_Offset : constant Natural := Base_Offset + Index / 4;
            Shift : constant Natural := (Index mod 4) * 2;
            B : Unsigned_8 with Import, Volatile, Address => Map + Storage_Offset (Byte_Offset);
        begin
            B := B and not Shift_Left ((if Read then MSRPM_Read_Bit else 0)
                                       or (if Write then MSRPM_Write_Bit else 0), Shift);
        end;
    end Clear_MSR_Intercept;

    procedure Prepare_Maps (OK : out Boolean) is
        IOPM_Address, MSRPM_Address, Pass_Address : System.Address;
    begin
        if Use_VMX then
            Spinlocks.enterCriticalSection (Maps_Lock);
            VMX_Run.Prepare_Maps (OK);
            Spinlocks.exitCriticalSection (Maps_Lock);
            return;
        end if;
        Spinlocks.enterCriticalSection (Maps_Lock);
        if Maps_Ready then
            Spinlocks.exitCriticalSection (Maps_Lock);
            OK := True;
            return;
        end if;
        -- 12 KiB and 8 KiB, physically contiguous: orders 2 and 1.
        BuddyAllocator.alloc (2, IOPM_Address);
        BuddyAllocator.alloc (1, MSRPM_Address);
        BuddyAllocator.alloc (1, Pass_Address);
        if IOPM_Address = BuddyAllocator.NO_BLOCK_AVAILABLE
          or else MSRPM_Address = BuddyAllocator.NO_BLOCK_AVAILABLE
          or else Pass_Address = BuddyAllocator.NO_BLOCK_AVAILABLE
        then
            Spinlocks.exitCriticalSection (Maps_Lock);
            OK := False;
            return;
        end if;
        -- Every port and every MSR exits, but the switched MSRs.
        Fill_Bytes (IOPM_Address, IOPM_Bytes, 16#FF#);
        declare
            type Map_List is array (1 .. 2) of System.Address;
            Maps : constant Map_List := [MSRPM_Address, Pass_Address];
        begin
            for Map of Maps loop
                Fill_Bytes (Map, MSRPM_Bytes, 16#FF#);
                for M of Passthrough_MSRs loop
                    Clear_MSR_Intercept (Map, M);
                end loop;
                -- SPEC_CTRL reads see the guest's value (loaded at entry);
                -- PRED_CMD is write-only and acts on predictor state alone.
                Clear_MSR_Intercept (Map, Spec.IA32_SPEC_CTRL, Read => True, Write => False);
                Clear_MSR_Intercept (Map, Spec.IA32_PRED_CMD, Read => False, Write => True);
            end loop;
        end;
        Clear_MSR_Intercept (Pass_Address, Spec.IA32_SPEC_CTRL, Read => False, Write => True);
        IOPM_Phys := Unsigned_64 (Virtmem.V2P (IOPM_Address));
        MSRPM_Phys := Unsigned_64 (Virtmem.V2P (MSRPM_Address));
        MSRPM_Pass_Phys := Unsigned_64 (Virtmem.V2P (Pass_Address));
        Maps_Ready := True;
        Spinlocks.exitCriticalSection (Maps_Lock);
        OK := True;
    end Prepare_Maps;

    -- What the host CPU reports about SPEC_CTRL (each leaf only when the
    -- CPU has it).
    function Read_Host_Features return Spec.Host_CPUID is
        H : Spec.Host_CPUID;
        EAX, EBX, ECX, EDX : Unsigned_32;
        Max_Standard : constant Unsigned_32 := CPUID.getMaxStandardFunction;
        Max_Extended : constant Unsigned_32 := CPUID.getMaxExtendedFunction;
    begin
        if Max_Standard >= Spec.Structured_Leaf then
            CPUID.cpuid (Spec.Structured_Leaf, 0, EAX, EBX, ECX, EDX);
            H.Leaf_7_EDX := EDX;
            if EAX >= Spec.Structured_Subleaf_2 then   -- EAX: the last subleaf
                CPUID.cpuid (Spec.Structured_Leaf, Spec.Structured_Subleaf_2, EAX, EBX, ECX, EDX);
                H.Leaf_7_2_EDX := EDX;
            end if;
        end if;
        if Max_Extended >= Spec.Extended_Identifiers_Leaf then
            CPUID.cpuid (Spec.Extended_Identifiers_Leaf, EAX, EBX, ECX, EDX);
            H.Extended_EBX := EBX;
        end if;
        if Max_Extended >= Spec.Extended_Features_2_Leaf then
            CPUID.cpuid (Spec.Extended_Features_2_Leaf, 0, EAX, EBX, ECX, EDX);
            H.Extended_2_EAX := EAX;
        end if;
        return H;
    end Read_Host_Features;

    procedure Setup_Spec_Ctrl (CPU : CPU_Number; VMX : Boolean) is
    begin
        Host_Features := Read_Host_Features;
        Spec_Mask := Spec.Writable_Bits (Host_Features);
        Spec_By_VMRUN := not VMX and then Virtualization_Report.SVM_Virtual_Spec_Ctrl;
        if Spec.Present (Spec_Mask) then
            Host_Spec_Ctrl (CPU) := x86.rdmsr (Spec.IA32_SPEC_CTRL);
        end if;
        if CPU = 0 then
            print ("vm: guest SPEC_CTRL: bits "); print (Spec_Mask);
            println (if not Spec.Present (Spec_Mask) then ", the host has none"
                     elsif Spec_By_VMRUN then ", switched by VMRUN (V_SPEC_CTRL)"
                     else ", switched at entry and exit");
        end if;
    end Setup_Spec_Ctrl;

    procedure Setup_This_CPU is
        CPU : constant Natural := PerCPUData.getCPUNumber;
        Save, Area : Virtmem.PhysAddress;
        EFER : Unsigned_64;
        OK : Boolean;
    begin
        if CPU > CPU_Number'Last then
            return;
        end if;
        if Virtualization_Report.VMX_Usable then
            VMX_Run.Enable_This_CPU (CPU, OK);
            CPU_Ready (CPU) := OK;
            if CPU = 0 and then OK then
                Use_VMX := True;
                SVM_On_BSP := True;
                println ("vm: VMX enabled (VMXON, EPT, VPID); guests run as kernel objects");
            end if;
            if OK then
                Setup_Spec_Ctrl (CPU, VMX => True);
            end if;
            return;
        end if;
        if not Virtualization_Report.SVM_Usable then
            return;
        end if;
        BuddyAllocator.allocFrame (Save);
        BuddyAllocator.allocFrame (Area);
        if Save = 0 or else Area = 0 then
            println ("vm: no memory for this CPU's host areas; guests not run here");
            return;
        end if;
        Zero_Page (Virtmem.P2Va (Save));
        Zero_Page (Virtmem.P2Va (Area));
        EFER := x86.rdmsr (MSR_EFER);
        x86.wrmsr (MSR_EFER, EFER or Shift_Left (1, EFER_SVME_Bit));
        x86.wrmsr (MSR_VM_HSAVE_PA, Unsigned_64 (Save));
        Host_Save (CPU) := Unsigned_64 (Save);
        Host_VMSAVE (CPU) := Unsigned_64 (Area);
        CPU_Ready (CPU) := True;
        if CPU = 0 then
            SVM_On_BSP := True;
            println ("vm: SVM enabled (EFER.SVME, host save area); guests run as kernel objects");
        end if;
        Setup_Spec_Ctrl (CPU, VMX => False);
    end Setup_This_CPU;

    function Available return Boolean is (SVM_On_BSP);

    ---------------------------------------------------------------------------
    -- VMs and vCPUs
    ---------------------------------------------------------------------------
    type Phase_Kind is (Gone, Live, Closing);
    subtype VM_Index is Positive range 1 .. VM_ABI.VM_Ceiling;
    Total_VCPUs : constant := VM_ABI.VM_Ceiling * VM_ABI.VCPU_Ceiling;
    subtype VCPU_Index is Positive range 1 .. Total_VCPUs;
    type VCPU_List is array (1 .. VM_ABI.VCPU_Ceiling) of Natural;

    type VM_Record is record
        Phase        : Phase_Kind := Gone;
        Generation   : Caps.Generation := 1;
        Monitor      : Process.ProcessID := Process.NO_PROCESS;
        Identity     : Unsigned_64 := 0;
        Account      : Unsigned_64 := 0;
        Root         : Unsigned_64 := 0;    -- nested root table (physical)
        Runs         : VM_Admission.Run_Table := [others => <>];
        Run_Count    : VM_Admission.Run_Count := 0;
        Pages        : VM_Admission.Page_Total := 0;
        VCPUs        : VCPU_List := [others => 0];
        VCPU_Count   : Natural range 0 .. VM_ABI.VCPU_Ceiling := 0;
        Has_Run      : Boolean := False;
        Busy         : Boolean := False;    -- an Add_RAM in progress
        TSC_Offset   : Unsigned_64 := 0;
    end record;

    type Pending_Event is record
        Valid  : Boolean := False;
        Word   : Unsigned_32 := 0;         -- EVENTINJ without the valid bit
        Error  : Unsigned_32 := 0;
        Has_Error : Boolean := False;
    end record;

    -- Exit kinds for the 5 s evidence line (image 34b: a VMX vCPU exiting
    -- ~1,800 times a second while the guest made no progress).
    type Exit_Bucket is (B_Host_Interrupt, B_Window, B_Halt, B_Port, B_MSR, B_Nested_Fault,
                         B_CPUID, B_CR_Write, B_Exception, B_Other);
    type Exit_Counts is array (Exit_Bucket) of Unsigned_64;
    Bucket_Names : constant array (Exit_Bucket) of String (1 .. 4) :=
      [B_Host_Interrupt => "intr", B_Window => "wind", B_Halt => "halt", B_Port => "port",
       B_MSR => "msr ", B_Nested_Fault => "npf ", B_CPUID => "cpid", B_CR_Write => "cr  ",
       B_Exception => "exc ", B_Other => "othr"];

    type VCPU_Record is record
        Used        : Boolean := False;
        Generation  : Caps.Generation := 1;
        VM          : Natural := 0;
        VMCB        : System.Address := System.Null_Address;
        VMCB_Phys   : Unsigned_64 := 0;
        FPU         : System.Address := System.Null_Address;  -- guest XSAVE image
        VMCS        : System.Address := System.Null_Address;  -- VMX: the VMCS region
        Lists       : System.Address := System.Null_Address;  -- VMX: its MSR lists
        GPRs        : aliased GPR_Block := [others => 0];
        ASID        : Unsigned_32 := 0;
        Last_CPU    : Integer := -1;
        Tag_Stale   : Boolean := True;
        Runner      : Process.ThreadID := Process.NO_THREAD;
        Running_CPU : Integer := -1;       -- in the guest right now
        Halted      : Process.ThreadID := Process.NO_THREAD;
        Halt_Deadline : Unsigned_64 := VM_ABI.Forever;
        Pending     : Pending_Event;
        Kicked      : Boolean := False;
        Next_RIP    : Unsigned_64 := 0;    -- the last exit's next RIP
        Have_Next_RIP : Boolean := False;
        Entries     : Unsigned_64 := 0;
        Flushes     : Unsigned_64 := 0;
        Forwarded   : Unsigned_64 := 0;
        Spec_Ctrl   : aliased Spec_Ctrl_Block;
        Spec_Writes_Passed : Boolean := False;   -- the guest's writes reach the MSR
        Fault_GP    : Boolean := False;          -- a refused write: #GP at the next entry
        -- Stall evidence (no process may hold a CPU): this window's entries,
        -- host-interrupt exits, TSC spent in the guest and
        -- its longest single entry; reported every Report_Window_Ms.
        Window_Start    : Unsigned_64 := 0;      -- msTicks
        Window_Entries  : Unsigned_64 := 0;
        Window_Host     : Unsigned_64 := 0;
        Window_Guest    : Unsigned_64 := 0;      -- TSC ticks
        Window_Longest  : Unsigned_64 := 0;      -- TSC ticks
        Long_Reported   : Unsigned_64 := 0;      -- msTicks of the last long-entry line
        Window_Kinds    : Exit_Counts := [others => 0];  -- exits by kind this window
        Window_Last_Code, Window_Last_Info_1, Window_Last_Info_2 : Unsigned_64 := 0;
    end record;

    -- One guest entry longer than this is reported at once (at most one
    -- line per second per vCPU); every Report_Window_Ms a running vCPU
    -- reports its window.
    Long_Entry_Ms    : constant := 20;
    Report_Window_Ms : constant := 5_000;

    VMs   : array (VM_Index) of VM_Record;
    VCPUs : array (VCPU_Index) of VCPU_Record;
    Table_Lock : Spinlocks.Spinlock;

    -- ASIDs: 1 .. the CPU's count - 1 (0 is the host's).
    Max_ASIDs : constant := 4096;
    ASID_Used : array (1 .. Max_ASIDs - 1) of Boolean := [others => False];

    procedure Answer (Reply : out Process.Message; Outcome : VM_ABI.Outcome;
                      W1, W2, W3 : Unsigned_64 := 0) is
    begin
        Reply := Process.NULL_MESSAGE;
        Reply.tag.length := 4;
        Reply.words := [Unsigned_64 (VM_ABI.Outcome'Enum_Rep (Outcome)), W1, W2, W3];
    end Answer;

    ---------------------------------------------------------------------------
    -- Nested page tables
    ---------------------------------------------------------------------------
    function Entry_Address (Table : Unsigned_64; Index : Guest_Tables.Table_Index)
      return System.Address is
      (Virtmem.P2Va (Virtmem.PhysAddress (Table)) + Storage_Offset (Index * 8));

    function Read_Entry (Table : Unsigned_64; Index : Guest_Tables.Table_Index)
      return Unsigned_64 is
        W : Unsigned_64 with Import, Atomic, Address => Entry_Address (Table, Index);
    begin
        return W;
    end Read_Entry;

    procedure Write_Entry (Table : Unsigned_64; Index : Guest_Tables.Table_Index;
                           Value : Unsigned_64) is
        W : Unsigned_64 with Import, Atomic, Address => Entry_Address (Table, Index);
    begin
        W := Value;
    end Write_Entry;

    -- The back end's entry format: EPT on VMX, NPT (long mode) on SVM.
    function Entry_Present (E : Unsigned_64) return Boolean is
      (if Use_VMX then Guest_Tables.EPT_Present (E) else Guest_Tables.Present (E));
    function Table_Entry (T : Unsigned_64) return Unsigned_64 is
      (if Use_VMX then Guest_Tables.EPT_Table_Entry (T) else Guest_Tables.Table_Entry (T));
    function Leaf_Entry (F : Unsigned_64; W, X : Boolean) return Unsigned_64 is
      (if Use_VMX then Guest_Tables.EPT_Leaf (F, W, X) else Guest_Tables.Leaf (F, W, X));

    procedure New_Table (Account : Unsigned_64; Phys : out Unsigned_64; OK : out Boolean) is
        Page : System.Address;
    begin
        Charged_Metadata_Pages.Allocate (Account, Page, OK);
        if not OK then
            Phys := 0;
            return;
        end if;
        Zero_Page (Page);
        Phys := Unsigned_64 (Virtmem.V2P (Page));
    end New_Table;

    -- Map one 4 KiB leaf; Result is Ok, No_Memory, or Refused (present).
    procedure Map_Leaf (V : VM_Index; GPA, Frame : Unsigned_64; Write, Execute : Boolean;
                        Result : out VM_ABI.Outcome) is
        T : Unsigned_64 := VMs (V).Root;
        E : Unsigned_64;
        Next : Unsigned_64;
        OK : Boolean;
        I : Guest_Tables.Table_Index;
    begin
        for L in reverse 2 .. Guest_Tables.Levels loop
            I := Guest_Tables.Index_Of (GPA, L);
            E := Read_Entry (T, I);
            if Entry_Present (E) then
                T := Guest_Tables.Address (E);
            else
                New_Table (VMs (V).Account, Next, OK);
                if not OK then
                    Result := VM_ABI.No_Memory;
                    return;
                end if;
                Write_Entry (T, I, Table_Entry (Next));
                T := Next;
            end if;
        end loop;
        I := Guest_Tables.Index_Of (GPA, 1);
        if Entry_Present (Read_Entry (T, I)) then
            Result := VM_ABI.Refused;
            return;
        end if;
        Write_Entry (T, I, Leaf_Entry (Frame, Write, Execute));
        Result := VM_ABI.Ok;
    end Map_Leaf;

    -- Clear one leaf and unpin its frame (rollback of a partial Add_RAM).
    procedure Unmap_Leaf (V : VM_Index; GPA : Unsigned_64) is
        T : Unsigned_64 := VMs (V).Root;
        E : Unsigned_64;
        Done : Boolean;
    begin
        for L in reverse 2 .. Guest_Tables.Levels loop
            E := Read_Entry (T, Guest_Tables.Index_Of (GPA, L));
            if not Entry_Present (E) then
                return;
            end if;
            T := Guest_Tables.Address (E);
        end loop;
        E := Read_Entry (T, Guest_Tables.Index_Of (GPA, 1));
        if Entry_Present (E) then
            Write_Entry (T, Guest_Tables.Index_Of (GPA, 1), Guest_Tables.Not_Present);
            BuddyAllocator.unpinFrame (Virtmem.PhysAddress (Guest_Tables.Address (E)), Done);
        end if;
    end Unmap_Leaf;

    -- Free a whole tree: unpin every leaf's frame, release every table.
    procedure Free_Tree (Table : Unsigned_64; L : Guest_Tables.Level) is
        E : Unsigned_64;
        Done : Boolean;
    begin
        for I in Guest_Tables.Table_Index loop
            E := Read_Entry (Table, I);
            if Entry_Present (E) then
                if L = 1 then
                    BuddyAllocator.unpinFrame
                      (Virtmem.PhysAddress (Guest_Tables.Address (E)), Done);
                else
                    Free_Tree (Guest_Tables.Address (E), L - 1);
                end if;
                Write_Entry (Table, I, Guest_Tables.Not_Present);
            end if;
        end loop;
        Charged_Metadata_Pages.Release (Virtmem.P2Va (Virtmem.PhysAddress (Table)));
    end Free_Tree;

    -- FNV-1a over every present leaf (its level-1 slot path and entry).
    procedure Digest_Tree (Table : Unsigned_64; L : Guest_Tables.Level;
                           Hash : in out Unsigned_64) is
        E : Unsigned_64;
        FNV_Prime : constant Unsigned_64 := 16#0000_0100_0000_01B3#;
    begin
        for I in Guest_Tables.Table_Index loop
            E := Read_Entry (Table, I);
            if Entry_Present (E) then
                if L = 1 then
                    -- The translation (frame and rights), not the use bits
                    -- the CPU sets as the guest runs.
                    if not Use_VMX then
                        E := E and not (Guest_Tables.NPT_Accessed_Bit or Guest_Tables.NPT_Dirty_Bit);
                    end if;
                    Hash := (Hash xor E) * FNV_Prime;
                    Hash := (Hash xor Unsigned_64 (I)) * FNV_Prime;
                else
                    Digest_Tree (Guest_Tables.Address (E), L - 1, Hash);
                end if;
            end if;
        end loop;
    end Digest_Tree;

    function Digest (V : VM_Index) return Unsigned_64 is
        Hash : Unsigned_64 := 16#CBF2_9CE4_8422_2325#;
    begin
        if VMs (V).Root /= 0 then
            Digest_Tree (VMs (V).Root, Guest_Tables.Levels, Hash);
        end if;
        return Hash;
    end Digest;

    ---------------------------------------------------------------------------
    -- The monitor's own frames (Add_RAM)
    ---------------------------------------------------------------------------
    procedure Read_Process_Entry
      (Table_Frame : Unsigned_64; Index : Debug_View_Walk.Table_Index;
       Word : out Unsigned_64)
    is
        W : Unsigned_64 with Import, Atomic,
          Address => Virtmem.P2Va (Virtmem.PhysAddress (Table_Frame)) +
                     Storage_Offset (Index * 8);
    begin
        Word := W;
    end Read_Process_Entry;

    function View_Walk is new Debug_View_Walk.Walk (Read_Process_Entry);

    procedure Read_User_Entry
      (Table_Frame : Unsigned_64; Index : User_Page_Walk.Table_Index;
       Word : out Unsigned_64)
    is
        W : Unsigned_64 with Import, Atomic,
          Address => Virtmem.P2Va (Virtmem.PhysAddress (Table_Frame)) +
                     Storage_Offset (Index * 8);
    begin
        Word := W;
    end Read_User_Entry;

    function Writable_Walk is new User_Page_Walk.Writable_Frame (Read_User_Entry);

    -- The frame the monitor maps write-back at Address, pinned, or 0.
    procedure Pin_Monitor_Frame
      (Monitor : Process.ProcessID; Address : Unsigned_64; Write : Boolean;
       Frame : out Unsigned_64)
    is
        Root : constant Unsigned_64 :=
          Unsigned_64 (Process.rootPhysical (Process.proctab (Monitor).pgTable));
        R : Debug_View_Walk.Result;
        Pinned : Boolean := False;
    begin
        Frame := 0;
        R := View_Walk (Root, Address, Unsigned_64 (Virtmem.MAX_PHYS_USABLE));
        if R.Kind /= Debug_View_Walk.Viewable then
            return;
        end if;
        -- A page the guest may write must be one the monitor may write:
        -- never its code or read-only data.
        if Write and then
          Writable_Walk (Root, Address, Unsigned_64 (Virtmem.MAX_PHYS_USABLE)) /= R.Frame
        then
            return;
        end if;
        BuddyAllocator.pinOwnedFrame
          (Virtmem.PhysAddress (R.Frame), BuddyAllocator.Frame_Owner (Monitor), Pinned);
        if Pinned then
            Frame := R.Frame;
        end if;
    end Pin_Monitor_Frame;

    ---------------------------------------------------------------------------
    -- Admission
    ---------------------------------------------------------------------------
    function Identity_Word (Pid : Process.ProcessID) return Unsigned_64 is
      (Process_Identities.To_Word (Process.identityOf (Pid)));

    -- The live VM Cap names, called by its monitor; 0 if not.
    function VM_Of (Caller : Process.ProcessID; Cap : Caps.Capability) return Natural is
    begin
        if Cap.capType /= Caps.CAP_VM or else Cap.object.ref not in 1 .. VM_ABI.VM_Ceiling then
            return 0;
        end if;
        declare
            V : constant VM_Index := VM_Index (Cap.object.ref);
        begin
            if VMs (V).Phase = Live and then VMs (V).Generation = Cap.gen
              and then VMs (V).Monitor = Caller
              and then VMs (V).Identity = Cap.object.param
              and then Identity_Word (Caller) = Cap.object.param
            then
                return V;
            end if;
            return 0;
        end;
    end VM_Of;

    function VCPU_Of (Caller : Process.ProcessID; Cap : Caps.Capability) return Natural is
    begin
        if Cap.capType /= Caps.CAP_VCPU or else Cap.object.ref not in 1 .. Total_VCPUs then
            return 0;
        end if;
        declare
            X : constant VCPU_Index := VCPU_Index (Cap.object.ref);
        begin
            if VCPUs (X).Used and then VCPUs (X).Generation = Cap.gen
              and then VCPUs (X).VM in VM_Index
              and then VMs (VCPUs (X).VM).Phase = Live
              and then VMs (VCPUs (X).VM).Monitor = Caller
              and then VMs (VCPUs (X).VM).Identity = Cap.object.param
              and then Identity_Word (Caller) = Cap.object.param
            then
                return X;
            end if;
            return 0;
        end;
    end VCPU_Of;

    procedure Insert_Handle (Caller : Process.ProcessID; Kind : Caps.CapabilityType;
                             Ref, Param : Unsigned_64; Gen : Caps.Generation;
                             Handle : out Unsigned_64) is
    begin
        Spinlocks.enterCriticalSection (Process.mailtab (Caller).lock);
        Process_Capabilities.Insert
          (Process.proctab (Caller).caps,
           (capType      => Kind,
            rights       => [Caps.RIGHT_READ | Caps.RIGHT_WRITE => True, others => False],
            authorityTag => Caps.NO_AUTHORITY_TAG,
            object       => (ref => Ref, param => Param),
            gen          => Gen), Handle);
        Spinlocks.exitCriticalSection (Process.mailtab (Caller).lock);
    end Insert_Handle;

    ---------------------------------------------------------------------------
    -- Create_VM, Add_RAM, Create_VCPU, Destroy
    ---------------------------------------------------------------------------
    procedure Create_VM (Caller : Process.ProcessID; Reply : out Process.Message) is
        Slot : Natural := 0;
        Root : Unsigned_64;
        OK : Boolean;
        Handle : Unsigned_64;
        Account : constant Unsigned_64 := Process.proctab (Caller).memoryAccount;
    begin
        if not Available then
            Answer (Reply, VM_ABI.Not_Supported);
            return;
        end if;
        Prepare_Maps (OK);
        if not OK then
            Answer (Reply, VM_ABI.No_Memory);
            return;
        end if;
        New_Table (Account, Root, OK);
        if not OK then
            Answer (Reply, VM_ABI.No_Memory);
            return;
        end if;
        Spinlocks.enterCriticalSection (Table_Lock);
        for V in VM_Index loop
            if VMs (V).Phase = Gone and then VMs (V).Generation < Caps.Generation'Last then
                Slot := V;
                VMs (V).Phase := Live;
                VMs (V).Monitor := Caller;
                VMs (V).Identity := Identity_Word (Caller);
                VMs (V).Account := Account;
                VMs (V).Root := Root;
                VMs (V).Run_Count := 0;
                VMs (V).Pages := 0;
                VMs (V).VCPUs := [others => 0];
                VMs (V).VCPU_Count := 0;
                VMs (V).Has_Run := False;
                VMs (V).Busy := False;
                -- The guest's TSC starts near zero, one offset for all its
                -- vCPUs (docs/hypervisor.md 4.8 rule 2).
                VMs (V).TSC_Offset := (not x86.rdtsc) + 1;
                exit;
            end if;
        end loop;
        Spinlocks.exitCriticalSection (Table_Lock);
        if Slot = 0 then
            Charged_Metadata_Pages.Release (Virtmem.P2Va (Virtmem.PhysAddress (Root)));
            Answer (Reply, VM_ABI.Ceiling);
            return;
        end if;
        Insert_Handle (Caller, Caps.CAP_VM, Unsigned_64 (Slot), VMs (Slot).Identity,
                       VMs (Slot).Generation, Handle);
        if not Capability_ABI.Is_Handle_Word (Handle) then
            Spinlocks.enterCriticalSection (Table_Lock);
            VMs (Slot).Phase := Gone;
            VMs (Slot).Generation := VMs (Slot).Generation + 1;
            Spinlocks.exitCriticalSection (Table_Lock);
            Charged_Metadata_Pages.Release (Virtmem.P2Va (Virtmem.PhysAddress (Root)));
            Answer (Reply, VM_ABI.Ceiling);
            return;
        end if;
        print ("vm: VM "); printd (Unsigned_32 (Slot));
        print (" created for pid "); printdln (Unsigned_32 (Caller));
        Answer (Reply, VM_ABI.Ok, W1 => Handle);
    end Create_VM;

    procedure Add_RAM (Caller : Process.ProcessID; V : VM_Index;
                       Request : Process.Message; Reply : out Process.Message) is
        First   : constant Unsigned_64 := Request.words (0);
        Address : constant Unsigned_64 := Request.words (1);
        Bytes   : constant Unsigned_64 := Request.words (2);
        Rights  : constant Unsigned_64 := Request.words (3);
        Write   : constant Boolean := (Rights and VM_ABI.Right_Write) /= 0;
        Execute : constant Boolean := (Rights and VM_ABI.Right_Execute) /= 0;
        Verdict : VM_Admission.Range_Verdict;
        Pages   : Unsigned_64;
        Frame   : Unsigned_64;
        Result  : VM_ABI.Outcome := VM_ABI.Ok;
        Mapped  : Unsigned_64 := 0;
    begin
        if (Rights and not VM_ABI.Rights_Mask) /= 0 or else (Rights and VM_ABI.Right_Read) = 0
          or else Address mod VM_ABI.Page_Bytes /= 0
          or else Address >= User_Page_Walk.User_Limit
          or else Bytes > User_Page_Walk.User_Limit - Address
        then
            Answer (Reply, VM_ABI.Bad_Range);
            return;
        end if;
        Spinlocks.enterCriticalSection (Table_Lock);
        if VMs (V).Has_Run or else VMs (V).Busy then
            Spinlocks.exitCriticalSection (Table_Lock);
            Answer (Reply, VM_ABI.Busy);
            return;
        end if;
        Verdict := VM_Admission.Range_Admission
          (VMs (V).Runs, VMs (V).Run_Count, VMs (V).Pages, First, Bytes);
        if Verdict /= VM_Admission.Admitted then
            Spinlocks.exitCriticalSection (Table_Lock);
            Answer (Reply, (case Verdict is
                              when VM_Admission.Ledger_Full | VM_Admission.Too_Large =>
                                 VM_ABI.Ceiling,
                              when others => VM_ABI.Bad_Range));
            return;
        end if;
        VMs (V).Busy := True;
        Spinlocks.exitCriticalSection (Table_Lock);

        Pages := Bytes / VM_ABI.Page_Bytes;
        for P in 0 .. Pages - 1 loop
            Process.lockAddressSpace (Caller);
            Pin_Monitor_Frame (Caller, Address + P * VM_ABI.Page_Bytes, Write, Frame);
            Process.unlockAddressSpace (Caller);
            if Frame = 0 then
                Result := VM_ABI.Refused;
                exit;
            end if;
            Map_Leaf (V, First + P * VM_ABI.Page_Bytes, Frame, Write, Execute, Result);
            if Result /= VM_ABI.Ok then
                declare
                    Done : Boolean;
                begin
                    BuddyAllocator.unpinFrame (Virtmem.PhysAddress (Frame), Done);
                end;
                exit;
            end if;
            Mapped := Mapped + 1;
        end loop;

        if Result /= VM_ABI.Ok and then Mapped > 0 then
            for P in 0 .. Mapped - 1 loop
                Unmap_Leaf (V, First + P * VM_ABI.Page_Bytes);
            end loop;
        end if;
        Spinlocks.enterCriticalSection (Table_Lock);
        if Result = VM_ABI.Ok then
            VMs (V).Run_Count := VMs (V).Run_Count + 1;
            VMs (V).Runs (VMs (V).Run_Count) := (First => First, Pages => Pages);
            VMs (V).Pages := VMs (V).Pages + Pages;
        end if;
        VMs (V).Busy := False;
        Spinlocks.exitCriticalSection (Table_Lock);
        Answer (Reply, Result, W1 => (if Result = VM_ABI.Ok then Pages else 0));
    end Add_RAM;

    -- The guest's architectural reset state (a vCPU at power-on in real
    -- mode at F000:FFF0, as SVM's VMCB expresses it), with every intercept
    -- the design needs (docs/hypervisor.md 4.3, 4.6).
    procedure Initialize_VMCB (X : VCPU_Index; V : VM_Index) is
        B : constant System.Address := VCPUs (X).VMCB;
        procedure Seg (Off : VMCB_Offset; Selector : Unsigned_16; Attributes : Unsigned_16;
                       Limit : Unsigned_32; Base : Unsigned_64) is
        begin
            Put16 (B, Off + Segment_Selector, Selector);
            Put16 (B, Off + Segment_Attributes, Attributes);
            Put32 (B, Off + Segment_Limit, Limit);
            Put64 (B, Off + Segment_Base, Base);
        end Seg;
        Data_Attributes : constant Unsigned_16 := 16#0093#;  -- P, S, read/write, accessed
        Code_Attributes : constant Unsigned_16 := 16#009B#;  -- P, S, code, read, accessed
        Word3 : constant Unsigned_32 :=
          Bit32 (Intercept_INTR) or Bit32 (Intercept_NMI) or Bit32 (Intercept_SMI)
          or Bit32 (Intercept_INIT) or Bit32 (Intercept_CPUID) or Bit32 (Intercept_RSM)
          or Bit32 (Intercept_INVD) or Bit32 (Intercept_HLT) or Bit32 (Intercept_INVLPGA)
          or Bit32 (Intercept_IOIO) or Bit32 (Intercept_MSR)
          or Bit32 (Intercept_Task_Switch) or Bit32 (Intercept_FERR_Freeze)
          or Bit32 (Intercept_Shutdown);
        Word4 : constant Unsigned_32 :=
          Bit32 (Intercept_VMRUN) or Bit32 (Intercept_VMMCALL) or Bit32 (Intercept_VMLOAD)
          or Bit32 (Intercept_VMSAVE) or Bit32 (Intercept_STGI) or Bit32 (Intercept_CLGI)
          or Bit32 (Intercept_SKINIT) or Bit32 (Intercept_ICEBP) or Bit32 (Intercept_WBINVD)
          or Bit32 (Intercept_MONITOR) or Bit32 (Intercept_MWAIT)
          or Bit32 (Intercept_MWAIT_Conditional) or Bit32 (Intercept_XSETBV)
          or Bit32 (Intercept_RDPRU);
    begin
        Zero_Page (B);
        Put32 (B, Intercept_CR, 0);
        Put32 (B, Intercept_DR, 16#FFFF_FFFF#);   -- every DR read and write
        Put32 (B, Intercept_Exceptions,
               Bit32 (Vector_Debug) or Bit32 (Vector_Alignment_Check));
        Put32 (B, Intercept_Word_3, Word3);
        Put32 (B, Intercept_Word_4, Word4);
        Put64 (B, IOPM_Base, IOPM_Phys);
        Put64 (B, MSRPM_Base, MSRPM_Phys);
        Put64 (B, TSC_Offset, VMs (V).TSC_Offset);
        Put32 (B, Guest_ASID, VCPUs (X).ASID);
        Put8 (B, TLB_Control, TLB_Flush_ASID);
        -- Physical interrupts stay the host's (V_INTR_MASKING).
        Put32 (B, Interrupt_Control, Bit32 (Virtual_Interrupt_Masking_Bit));
        Put64 (B, Nested_Control, Nested_Paging_Enable);
        Put64 (B, Nested_CR3, VMs (V).Root);
        Put32 (B, Clean_Bits, 0);
        -- Reset state: real mode, CS:IP = F000:FFF0 with base FFFF0000.
        Seg (Guest_CS, 16#F000#, Code_Attributes, 16#FFFF#, 16#FFFF_0000#);
        Seg (Guest_DS, 0, Data_Attributes, 16#FFFF#, 0);
        Seg (Guest_ES, 0, Data_Attributes, 16#FFFF#, 0);
        Seg (Guest_SS, 0, Data_Attributes, 16#FFFF#, 0);
        Seg (Guest_FS, 0, Data_Attributes, 16#FFFF#, 0);
        Seg (Guest_GS, 0, Data_Attributes, 16#FFFF#, 0);
        Seg (Guest_GDTR, 0, 0, 16#FFFF#, 0);
        Seg (Guest_IDTR, 0, 0, 16#FFFF#, 0);
        Seg (Guest_LDTR, 0, 16#0082#, 16#FFFF#, 0);
        Seg (Guest_TR, 0, 16#008B#, 16#FFFF#, 0);
        Put64 (B, Guest_CR0, 16#6000_0010#);      -- CD, NW, ET
        Put64 (B, Guest_EFER, Shift_Left (1, EFER_SVME_Bit));  -- SVME, as VMRUN requires
        Put64 (B, Guest_RFLAGS, 2);
        Put64 (B, Guest_RIP, 16#FFF0#);
        Put64 (B, Guest_DR6, 16#FFFF_0FF0#);
        Put64 (B, Guest_DR7, 16#0400#);
        Put64 (B, Guest_PAT, 16#0007_0406_0007_0406#);
    end Initialize_VMCB;

    procedure Create_VCPU (Caller : Process.ProcessID; V : VM_Index;
                           Reply : out Process.Message) is
        Slot : Natural := 0;
        ASID : Natural := 0;
        VMCB, FPU : System.Address;
        VMCS, Lists : System.Address := System.Null_Address;
        OK1, OK2 : Boolean;
        OK3, OK4 : Boolean := True;
        Handle : Unsigned_64;
        Limit : constant Natural :=
          (if Use_VMX then Max_ASIDs   -- VPIDs: 16 bits, this kernel uses 4095
           else Natural'Min (Natural (Virtualization_Report.SVM_ASIDs), Max_ASIDs));
    begin
        Charged_Metadata_Pages.Allocate (VMs (V).Account, VMCB, OK1);
        Charged_Metadata_Pages.Allocate (VMs (V).Account, FPU, OK2);
        if Use_VMX and then OK1 and then OK2 then
            Charged_Metadata_Pages.Allocate (VMs (V).Account, VMCS, OK3);
            Charged_Metadata_Pages.Allocate (VMs (V).Account, Lists, OK4);
        end if;
        if not (OK1 and OK2 and OK3 and OK4) then
            if OK1 then Charged_Metadata_Pages.Release (VMCB); end if;
            if OK2 then Charged_Metadata_Pages.Release (FPU); end if;
            if Use_VMX and then OK3 then Charged_Metadata_Pages.Release (VMCS); end if;
            if Use_VMX and then OK4 then Charged_Metadata_Pages.Release (Lists); end if;
            Answer (Reply, VM_ABI.No_Memory);
            return;
        end if;
        if Use_VMX then
            VMX_Run.Prepare_VMCS (VMCS);
            Zero_Page (Lists);
        end if;
        Zero_Page (FPU);   -- the XSAVE image of the initial (reset) state
        Spinlocks.enterCriticalSection (Table_Lock);
        if VMs (V).VCPU_Count < VM_ABI.VCPU_Ceiling then
            for A in 1 .. Limit - 1 loop
                if not ASID_Used (A) then
                    ASID := A;
                    exit;
                end if;
            end loop;
            if ASID /= 0 then
                for X in VCPU_Index loop
                    if not VCPUs (X).Used and then VCPUs (X).Generation < Caps.Generation'Last then
                        Slot := X;
                        exit;
                    end if;
                end loop;
            end if;
        end if;
        if Slot /= 0 then
            ASID_Used (ASID) := True;
            VCPUs (Slot) :=
              (Used => True, Generation => VCPUs (Slot).Generation, VM => V,
               VMCB => VMCB, VMCB_Phys => Unsigned_64 (Virtmem.V2P (VMCB)), FPU => FPU,
               VMCS => VMCS, Lists => Lists,
               GPRs => [others => 0], ASID => Unsigned_32 (ASID), Last_CPU => -1,
               Tag_Stale => True, Runner => Process.NO_THREAD, Running_CPU => -1,
               Halted => Process.NO_THREAD, Halt_Deadline => VM_ABI.Forever,
               Pending => (others => <>), Kicked => False, Next_RIP => 0,
               Have_Next_RIP => False, Entries => 0, Flushes => 0, Forwarded => 0,
               Spec_Ctrl => (others => 0), Spec_Writes_Passed => False, Fault_GP => False, Window_Kinds => [others => 0],
               others => 0);
            VMs (V).VCPU_Count := VMs (V).VCPU_Count + 1;
            VMs (V).VCPUs (VMs (V).VCPU_Count) := Slot;
            Initialize_VMCB (Slot, V);
        end if;
        Spinlocks.exitCriticalSection (Table_Lock);
        if Slot = 0 then
            Charged_Metadata_Pages.Release (VMCB);
            Charged_Metadata_Pages.Release (FPU);
            if Use_VMX then
                Charged_Metadata_Pages.Release (VMCS);
                Charged_Metadata_Pages.Release (Lists);
            end if;
            Answer (Reply, VM_ABI.Ceiling);
            return;
        end if;
        Insert_Handle (Caller, Caps.CAP_VCPU, Unsigned_64 (Slot), VMs (V).Identity,
                       VCPUs (Slot).Generation, Handle);
        Answer (Reply, (if Capability_ABI.Is_Handle_Word (Handle) then VM_ABI.Ok
                        else VM_ABI.Ceiling), W1 => Handle);
    end Create_VCPU;

    -- Everything the VM holds goes: its vCPUs' pages and ASIDs, every pin
    -- and every nested table. Caller: no vCPU of V is in Run.
    procedure Teardown (V : VM_Index) is
        Root : Unsigned_64;
    begin
        Spinlocks.enterCriticalSection (Table_Lock);
        for N in 1 .. VMs (V).VCPU_Count loop
            declare
                X : constant VCPU_Index := VMs (V).VCPUs (N);
            begin
                Charged_Metadata_Pages.Release (VCPUs (X).VMCB);
                Charged_Metadata_Pages.Release (VCPUs (X).FPU);
                if Use_VMX then
                    Charged_Metadata_Pages.Release (VCPUs (X).VMCS);
                    Charged_Metadata_Pages.Release (VCPUs (X).Lists);
                end if;
                if Natural (VCPUs (X).ASID) in ASID_Used'Range then
                    -- Reused only by a vCPU that flushes it before its
                    -- first entry on each CPU (Tag_Stale, Last_CPU).
                    ASID_Used (Natural (VCPUs (X).ASID)) := False;
                end if;
                VCPUs (X).Used := False;
                VCPUs (X).Generation := VCPUs (X).Generation + 1;
                VCPUs (X).VM := 0;
            end;
        end loop;
        Root := VMs (V).Root;
        VMs (V).Root := 0;
        VMs (V).VCPU_Count := 0;
        Spinlocks.exitCriticalSection (Table_Lock);
        if Root /= 0 then
            Free_Tree (Root, Guest_Tables.Levels);
        end if;
        Spinlocks.enterCriticalSection (Table_Lock);
        VMs (V).Phase := Gone;
        VMs (V).Generation := VMs (V).Generation + 1;
        VMs (V).Monitor := Process.NO_PROCESS;
        VMs (V).Run_Count := 0;
        VMs (V).Pages := 0;
        Spinlocks.exitCriticalSection (Table_Lock);
        print ("vm: VM "); printd (Unsigned_32 (V)); println (" destroyed; every pin and table released");
    end Teardown;

    function Any_Runner (V : VM_Index) return Boolean is
      (for some N in 1 .. VMs (V).VCPU_Count =>
         VCPUs (VMs (V).VCPUs (N)).Runner /= Process.NO_THREAD);

    procedure Wake_Halted (X : VCPU_Index);
    procedure Kick_Running (X : VCPU_Index);

    procedure Destroy (V : VM_Index; Reply : out Process.Message) is
        Now : Boolean;
    begin
        Spinlocks.enterCriticalSection (Table_Lock);
        VMs (V).Phase := Closing;
        Now := not Any_Runner (V);
        Spinlocks.exitCriticalSection (Table_Lock);
        for N in 1 .. VMs (V).VCPU_Count loop
            Wake_Halted (VMs (V).VCPUs (N));
            Kick_Running (VMs (V).VCPUs (N));
        end loop;
        if Now then
            Teardown (V);
        end if;
        Answer (Reply, VM_ABI.Ok);
    end Destroy;

    ---------------------------------------------------------------------------
    -- Halting, waking and kicking
    ---------------------------------------------------------------------------
    procedure Wake_Thread (T : Process.ThreadID) is
    begin
        if T = Process.NO_THREAD then
            return;
        end if;
        Spinlocks.enterCriticalSection (Process.lock);
        if Process.threadtab (T).state = Process.VMHALTED then
            Process.ready (T);
        end if;
        Spinlocks.exitCriticalSection (Process.lock);
    end Wake_Thread;

    procedure Wake_Halted (X : VCPU_Index) is
        T : Process.ThreadID;
    begin
        Spinlocks.enterCriticalSection (Table_Lock);
        T := VCPUs (X).Halted;
        VCPUs (X).Halted := Process.NO_THREAD;
        Spinlocks.exitCriticalSection (Table_Lock);
        Wake_Thread (T);
    end Wake_Halted;

    -- Any IPI makes a running guest exit (the INTR intercept).
    procedure Kick_Running (X : VCPU_Index) is
        CPU : constant Integer := VCPUs (X).Running_CPU;
    begin
        if CPU >= 0 and then CPU /= PerCPUData.getCPUNumber then
            Ipi.sendReschedule (CPU);
        end if;
    end Kick_Running;

    procedure Expire_Halts (Now_Ms : Unsigned_64) is
        T : Process.ThreadID;
    begin
        if not SVM_On_BSP then
            return;
        end if;
        for X in VCPU_Index loop
            if VCPUs (X).Halted /= Process.NO_THREAD
              and then VCPUs (X).Halt_Deadline <= Now_Ms
            then
                Spinlocks.enterCriticalSection (Table_Lock);
                T := VCPUs (X).Halted;
                VCPUs (X).Halted := Process.NO_THREAD;
                Spinlocks.exitCriticalSection (Table_Lock);
                Wake_Thread (T);
            end if;
        end loop;
    end Expire_Halts;

    -- K6: the guest halted with nothing to deliver. Block until an
    -- injection, a kick, the deadline or the VM's end.
    -- One guest entry's evidence (Run, interrupts disabled): the time it
    -- spent in the guest and why it left. Kernel log lines only, so the
    -- laptop's logstore keeps them (no serial needed).
    procedure Note_Entry (X : VCPU_Index; Spent, Code, Info_1, Info_2 : Unsigned_64) is
        -- Time.tscPerDuration is TSC ticks per microsecond (Time.Duration).
        Per_Us : constant Unsigned_64 := Unsigned_64'Max (Time.tscPerDuration, 1);
        Per_Ms : constant Unsigned_64 := Per_Us * 1000;
        Now : constant Unsigned_64 := Time.msTicks;
        C : VCPU_Record renames VCPUs (X);
    begin
        if C.Window_Start = 0 then
            C.Window_Start := Now;
        end if;
        C.Window_Entries := C.Window_Entries + 1;
        if Code = Exit_INTR or else Code = Exit_NMI or else Code = Exit_SMI then
            C.Window_Host := C.Window_Host + 1;
        end if;
        declare
            B : constant Exit_Bucket :=
              (if Code = Exit_INTR or else Code = Exit_NMI or else Code = Exit_SMI then B_Host_Interrupt
               elsif Code = Exit_VINTR then B_Window
               elsif Code = Exit_HLT then B_Halt
               elsif Code = Exit_IOIO then B_Port
               elsif Code = Exit_MSR then B_MSR
               elsif Code = Exit_NPF then B_Nested_Fault
               elsif Code = Exit_CPUID then B_CPUID
               elsif Code = Exit_Write_CR0 or else Code = Exit_Write_CR4 then B_CR_Write
               elsif Code in Exit_Exception_Base .. Exit_Last_Exception then B_Exception
               else B_Other);
        begin
            C.Window_Kinds (B) := C.Window_Kinds (B) + 1;
            if B /= B_Host_Interrupt then
                C.Window_Last_Code := Code;
                C.Window_Last_Info_1 := Info_1;
                C.Window_Last_Info_2 := Info_2;
            end if;
        end;
        C.Window_Guest := C.Window_Guest + Spent;
        C.Window_Longest := Unsigned_64'Max (C.Window_Longest, Spent);
        if Spent / Per_Ms >= Long_Entry_Ms and then Now - C.Long_Reported >= 1_000 then
            C.Long_Reported := Now;
            print ("vm: vCPU "); printd (Unsigned_32 (X));
            print (" stayed in the guest "); printd (Spent / Per_Ms);
            print (" ms; exit code "); println (Code);
        end if;
        if Now - C.Window_Start >= Report_Window_Ms then
            print ("vm: vCPU "); printd (Unsigned_32 (X));
            print (" over "); printd (Now - C.Window_Start);
            print (" ms: entries "); printd (C.Window_Entries);
            print (", host-interrupt exits "); printd (C.Window_Host);
            print (", in guest "); printd (C.Window_Guest / Per_Ms);
            print (" ms, longest entry "); printd (C.Window_Longest / Per_Us);
            print (" us, forwarded total "); printd (C.Forwarded);
            print ("; exits");
            for K in Exit_Bucket loop
                if C.Window_Kinds (K) /= 0 then
                    print (" "); print (Bucket_Names (K)); print ("="); printd (C.Window_Kinds (K));
                end if;
            end loop;
            print ("; last code "); print (C.Window_Last_Code);
            print (" info1 "); print (C.Window_Last_Info_1);
            print (" info2 "); println (C.Window_Last_Info_2);
            C.Window_Kinds := [others => 0];
            C.Window_Start := Now;
            C.Window_Entries := 0; C.Window_Host := 0;
            C.Window_Guest := 0; C.Window_Longest := 0;
        end if;
    end Note_Entry;

    procedure Halt (X : VCPU_Index; Deadline : Unsigned_64) is
        Me : constant Process.ThreadID := PerCPUData.getCurrentThread;
        V : constant Natural := VCPUs (X).VM;
    begin
        Spinlocks.enterCriticalSection (Table_Lock);
        if VCPUs (X).Pending.Valid or else VCPUs (X).Kicked
          or else V not in VM_Index or else VMs (V).Phase /= Live
          or else Time.msTicks >= Deadline
        then
            Spinlocks.exitCriticalSection (Table_Lock);
            return;
        end if;
        VCPUs (X).Halted := Me;
        VCPUs (X).Halt_Deadline := Deadline;
        -- Published before Table_Lock is released: a waker, which needs
        -- that lock, always finds this state (as Process.Futex does).
        Process.threadtab (Me).state := Process.VMHALTED;
        Spinlocks.exitCriticalSection (Table_Lock);
        Process.yield;
        Spinlocks.enterCriticalSection (Table_Lock);
        VCPUs (X).Halted := Process.NO_THREAD;
        Spinlocks.exitCriticalSection (Table_Lock);
    end Halt;

    ---------------------------------------------------------------------------
    -- Injection (docs/hypervisor.md 4.5)
    ---------------------------------------------------------------------------
    procedure Inject (X : VCPU_Index; Request : Process.Message; Reply : out Process.Message) is
        Kind_Word : constant Unsigned_64 := Request.words (0);
        Vector    : constant Unsigned_64 := Request.words (1);
        Error     : constant Unsigned_64 := Request.words (2);
        Has_Error : constant Boolean := (Request.words (3) and VM_ABI.Event_Error_Valid) /= 0;
        Kind      : VM_ABI.Event_Kind;
        Type_Bits : Unsigned_32;
    begin
        if Kind_Word = Unsigned_64 (VM_ABI.Event_Kind'Enum_Rep (VM_ABI.External)) then
            Kind := VM_ABI.External; Type_Bits := Event_Type_Interrupt;
        elsif Kind_Word = Unsigned_64 (VM_ABI.Event_Kind'Enum_Rep (VM_ABI.NMI)) then
            Kind := VM_ABI.NMI; Type_Bits := Event_Type_NMI;
        elsif Kind_Word = Unsigned_64 (VM_ABI.Event_Kind'Enum_Rep (VM_ABI.Hardware_Exception)) then
            Kind := VM_ABI.Hardware_Exception; Type_Bits := Event_Type_Exception;
        else
            Answer (Reply, VM_ABI.Refused);
            return;
        end if;
        if not VM_Admission.Injection_Valid (Kind, Vector, Has_Error)
          or else Error > Unsigned_64 (Unsigned_32'Last)
        then
            Answer (Reply, VM_ABI.Refused);
            return;
        end if;
        Spinlocks.enterCriticalSection (Table_Lock);
        if VCPUs (X).Pending.Valid then
            Spinlocks.exitCriticalSection (Table_Lock);
            Answer (Reply, VM_ABI.Busy);
            return;
        end if;
        VCPUs (X).Pending :=
          (Valid => True,
           Word  => Unsigned_32 (Vector and 16#FF#) or Shift_Left (Type_Bits, Event_Type_Shift),
           Error => Unsigned_32 (Error), Has_Error => Has_Error);
        Spinlocks.exitCriticalSection (Table_Lock);
        Wake_Halted (X);
        Kick_Running (X);
        Answer (Reply, VM_ABI.Ok);
    end Inject;

    ---------------------------------------------------------------------------
    -- The guest's SPEC_CTRL (Guest_Speculation, docs/hypervisor.md 4.3)
    ---------------------------------------------------------------------------
    -- Where the guest's value lives: the VMCB with V_SPEC_CTRL, else the
    -- block the entry and exit code switch.
    function Guest_Spec_Value (X : VCPU_Index) return Unsigned_64 is
      (if Spec_By_VMRUN then Get64 (VCPUs (X).VMCB, Guest_Spec_Ctrl)
       else VCPUs (X).Spec_Ctrl.Guest);

    procedure Set_Guest_Spec_Value (X : VCPU_Index; Value : Unsigned_64) is
    begin
        if Spec_By_VMRUN then
            Put64 (VCPUs (X).VMCB, Guest_Spec_Ctrl, Value);
        else
            VCPUs (X).Spec_Ctrl.Guest := Value;
        end if;
    end Set_Guest_Spec_Value;

    -- From now on the guest's writes reach the MSR (the exit reads it back).
    procedure Pass_Spec_Ctrl_Writes (X : VCPU_Index) is
    begin
        VCPUs (X).Spec_Writes_Passed := True;
        if not Use_VMX then
            Put64 (VCPUs (X).VMCB, MSRPM_Base, MSRPM_Pass_Phys);
        end if;
    end Pass_Spec_Ctrl_Writes;

    -- A guest WRMSR of SPEC_CTRL that exited (its writes are not passed
    -- through yet): kept in the kernel, decided by Guest_Write.
    procedure Guest_Spec_Ctrl_Write (X : VCPU_Index) is
        B : constant System.Address := VCPUs (X).VMCB;
        Value : constant Unsigned_64 :=
          Shift_Left (VCPUs (X).GPRs (GPR_RDX) and Low_Half, 32) or (Get64 (B, Guest_RAX) and Low_Half);
        Verdict : constant Spec.Write_Verdict := Spec.Guest_Write (Spec_Mask, Value);
    begin
        if Verdict = Spec.Fault then
            VCPUs (X).Fault_GP := True;   -- RIP stays on the WRMSR
            return;
        end if;
        Set_Guest_Spec_Value (X, Value);
        if Verdict = Spec.Pass_Writes then
            Pass_Spec_Ctrl_Writes (X);
        end if;
        Put64 (B, Guest_RIP, Get64 (B, Next_RIP));
    end Guest_Spec_Ctrl_Write;

    -- At entry: re-inject what the last exit interrupted, else a #GP the
    -- kernel decided, else deliver the pending event if the guest can take
    -- it, else ask for the window (K5).
    procedure Prepare_Injection (X : VCPU_Index) is
        B : constant System.Address := VCPUs (X).VMCB;
        Interrupted : constant Unsigned_32 := Get32 (B, Exit_Interrupt_Info);
        Control : Unsigned_32 := Get32 (B, Interrupt_Control);
        Word3 : Unsigned_32 := Get32 (B, Intercept_Word_3);
        Shadow : constant Boolean := (Get32 (B, Interrupt_State) and 1) /= 0;
        IF_Set : constant Boolean := (Get64 (B, Guest_RFLAGS) and 16#200#) /= 0;
        P : Pending_Event renames VCPUs (X).Pending;
        Is_External : Boolean;
        Protected_Mode : constant Boolean := (Get64 (B, Guest_CR0) and CR0_Protection_Enable) /= 0;
        procedure Ask_For_Window is
        begin
            -- A virtual interrupt the guest takes when it can; its
            -- delivery exits (VINTR) and is injected then.
            Control := Control or Bit32 (Virtual_IRQ_Bit)
              or Shift_Left (16#F#, Virtual_Priority_Shift)
              or Bit32 (Virtual_Ignore_TPR_Bit);
            Word3 := Word3 or Bit32 (Intercept_VINTR);
        end Ask_For_Window;
    begin
        Put32 (B, Event_Inject, 0);
        Control := Control and not Bit32 (Virtual_IRQ_Bit);
        Word3 := Word3 and not Bit32 (Intercept_VINTR);
        if (Interrupted and Bit32 (Event_Valid_Bit)) /= 0 then
            Put32 (B, Event_Inject, Interrupted);
            Put32 (B, Event_Inject_Error, Get32 (B, Exit_Interrupt_Error));
            Put32 (B, Exit_Interrupt_Info, 0);
        elsif VCPUs (X).Fault_GP then
            -- #GP(0) for a SPEC_CTRL write the kernel refused; real mode
            -- exceptions push no error code (and VMX refuses one there).
            Put32 (B, Event_Inject,
                   Unsigned_32 (Vector_General_Protection)
                   or Shift_Left (Event_Type_Exception, Event_Type_Shift)
                   or Bit32 (Event_Valid_Bit)
                   or (if Protected_Mode then Bit32 (Event_Error_Valid_Bit) else 0));
            Put32 (B, Event_Inject_Error, 0);
            VCPUs (X).Fault_GP := False;
            if P.Valid then
                Ask_For_Window;
            end if;
        elsif P.Valid then
            Is_External := Shift_Right (P.Word, Event_Type_Shift) = Event_Type_Interrupt;
            if not Is_External or else (IF_Set and then not Shadow) then
                Put32 (B, Event_Inject,
                       P.Word or Bit32 (Event_Valid_Bit)
                       or (if P.Has_Error then Bit32 (Event_Error_Valid_Bit) else 0));
                Put32 (B, Event_Inject_Error, P.Error);
                Spinlocks.enterCriticalSection (Table_Lock);
                P.Valid := False;
                Spinlocks.exitCriticalSection (Table_Lock);
            else
                Ask_For_Window;
            end if;
        end if;
        Put32 (B, Interrupt_Control, Control);
        Put32 (B, Intercept_Word_3, Word3);
    end Prepare_Injection;

    ---------------------------------------------------------------------------
    -- Run (docs/hypervisor.md 4.2, 4.3)
    ---------------------------------------------------------------------------
    function Kind_Word (K : VM_ABI.Exit_Kind) return Unsigned_64 is
      (Unsigned_64 (VM_ABI.Exit_Kind'Enum_Rep (K)));

    procedure Run_Reply (Reply : out Process.Message; Status : VM_ABI.Run_Status;
                         Kind : VM_ABI.Exit_Kind := VM_ABI.Other;
                         Code, Info_1, Info_2 : Unsigned_64 := 0) is
    begin
        Reply := Process.NULL_MESSAGE;
        Reply.tag.label := Unsigned_32 (VM_ABI.Run_Status'Enum_Rep (Status));
        Reply.tag.length := 4;
        Reply.words := [Kind_Word (Kind), Code, Info_1, Info_2];
    end Run_Reply;

    procedure Run (X : VCPU_Index; Request : Process.Message; Reply : out Process.Message) is
        Me : constant Process.ThreadID := PerCPUData.getCurrentThread;
        V  : constant VM_Index := VCPUs (X).VM;
        B  : constant System.Address := VCPUs (X).VMCB;
        Deadline : constant Unsigned_64 := Request.words (0);
        Flags    : constant Unsigned_64 := Request.words (1);
        Code, Info_1, Info_2 : Unsigned_64;
        CPU : Natural;
        Kind : VM_ABI.Exit_Kind;
        Done : Boolean := False;
        Status : VM_ABI.Run_Status := VM_ABI.Exited;
        Entry_TSC : Unsigned_64;
    begin
        Spinlocks.enterCriticalSection (Table_Lock);
        if VCPUs (X).Runner /= Process.NO_THREAD then
            Spinlocks.exitCriticalSection (Table_Lock);
            Run_Reply (Reply, VM_ABI.Run_Refused);
            return;
        end if;
        VCPUs (X).Runner := Me;
        VCPUs (X).Kicked := False;
        VMs (V).Has_Run := True;
        Spinlocks.exitCriticalSection (Table_Lock);

        if (Flags and VM_ABI.Resume_Advance_RIP) /= 0 and then VCPUs (X).Have_Next_RIP then
            Put64 (B, Guest_RIP, VCPUs (X).Next_RIP);
        end if;
        if (Flags and VM_ABI.Resume_Set_RAX) /= 0 then
            Put64 (B, Guest_RAX, Request.words (2));
        end if;
        if (Flags and VM_ABI.Resume_Set_RDX) /= 0 then
            VCPUs (X).GPRs (GPR_RDX) := Request.words (3);
        end if;
        VCPUs (X).Have_Next_RIP := False;

        -- The monitor's own FP/SIMD state, saved once; restored after every
        -- exit, so whatever runs on this CPU between exits (an interrupt, a
        -- reschedule) sees the monitor's, never the guest's.
        Process.saveUserCPUState (Me);

        while not Done loop
            if VMs (V).Phase /= Live then
                Status := VM_ABI.VM_Ended; exit;
            elsif VCPUs (X).Kicked then
                Status := VM_ABI.Kicked; exit;
            elsif Deadline /= VM_ABI.Forever and then Time.msTicks >= Deadline then
                Status := VM_ABI.Deadline; exit;
            elsif Process_Lifetime.Closing (Process.threadtab (Me).lifetime) then
                Status := VM_ABI.VM_Ended; exit;
            end if;

            x86.cli;
            CPU := PerCPUData.getCPUNumber;
            if CPU > CPU_Number'Last or else not CPU_Ready (CPU) then
                Status := VM_ABI.Run_Refused; exit;
            end if;
            -- Flush this vCPU's guest TLB tag on first use, on a CPU other
            -- than its last, and after a paging change (4.7a, Tag_Fresh).
            if VCPUs (X).Tag_Stale or else VCPUs (X).Last_CPU /= CPU then
                Put8 (B, TLB_Control, TLB_Flush_ASID);
                VCPUs (X).Flushes := VCPUs (X).Flushes + 1;
                VCPUs (X).Tag_Stale := False;
                VCPUs (X).Last_CPU := CPU;
            else
                Put8 (B, TLB_Control, TLB_Do_Nothing);
            end if;
            Put32 (B, Clean_Bits, 0);
            Prepare_Injection (X);
            VCPUs (X).Spec_Ctrl.Host := Host_Spec_Ctrl (CPU);
            VCPUs (X).Spec_Ctrl.Mode := Unsigned_64 (Spec.Switch'Enum_Rep
              (Spec.Switch_Mode (Spec_Mask, Spec_By_VMRUN, VCPUs (X).Spec_Writes_Passed)));
            x86.xrstor (VCPUs (X).FPU);
            VCPUs (X).Running_CPU := CPU;
            VCPUs (X).Entries := VCPUs (X).Entries + 1;
            Entry_TSC := x86.rdtsc;
            if Use_VMX then
                -- The state page is a software copy on VMX (VMX_Run): one
                -- entry, translated both ways, the VMCS cleared after it.
                VMX_Run.Enter
                  (State => B, VMCS => VCPUs (X).VMCS, Lists => VCPUs (X).Lists,
                   GPRs => VCPUs (X).GPRs'Address, EPT_Root => VMs (V).Root,
                   VPID => Unsigned_64 (VCPUs (X).ASID),
                   Flush_Tag => Get8 (B, TLB_Control) = TLB_Flush_ASID,
                   Spec_Ctrl => VCPUs (X).Spec_Ctrl'Address,
                   Pass_Spec_Ctrl_Writes => VCPUs (X).Spec_Writes_Passed);
            else
                cubitSvmRun (VCPUs (X).VMCB_Phys, Host_VMSAVE (CPU), VCPUs (X).GPRs'Address,
                             VCPUs (X).Spec_Ctrl'Address);
            end if;
            VCPUs (X).Running_CPU := -1;
            Note_Entry (X, x86.rdtsc - Entry_TSC, Get64 (B, Exit_Code),
                        Get64 (B, Exit_Info_1), Get64 (B, Exit_Info_2));
            x86.xsave (VCPUs (X).FPU);
            Process.restoreUserCPUState (Me);
            -- Host interrupts and NMIs that caused the exit are taken here
            -- (K1, K2); a reschedule may switch this thread away and back.
            x86.sti;
            x86.cli;

            Code   := Get64 (B, Exit_Code);
            Info_1 := Get64 (B, Exit_Info_1);
            Info_2 := Get64 (B, Exit_Info_2);
            if Code = Exit_INTR or else Code = Exit_NMI or else Code = Exit_SMI then
                null;                                   -- K1, K2
            elsif Code = Exit_VINTR then
                null;                                   -- K5: the window opened
            elsif Code = Exit_HLT then                  -- K6
                Put64 (B, Guest_RIP, Get64 (B, Next_RIP));
                Halt (X, Deadline);
            elsif Code = Exit_MSR and then Info_1 = MSR_Exit_Write
              and then (VCPUs (X).GPRs (GPR_RCX) and Low_Half) = Spec.IA32_SPEC_CTRL
            then
                Guest_Spec_Ctrl_Write (X);              -- SPEC_CTRL: kernel-decided
            elsif Code = Exit_Write_CR0 or else Code = Exit_Write_CR4 then
                -- K7: a write to a CR bit VMX pins, decided in VMX_Run
                -- (VMX_CR_Access); a refused one is #GP(0).
                if Info_1 = CR_Write_Refused then
                    VCPUs (X).Fault_GP := True;
                end if;
            else
                Done := True;
                VCPUs (X).Forwarded := VCPUs (X).Forwarded + 1;
                VCPUs (X).Next_RIP := Get64 (B, Next_RIP);
                VCPUs (X).Have_Next_RIP := True;
                Kind :=
                  (if Code = Exit_IOIO then VM_ABI.Port_IO
                   elsif Code = Exit_NPF then VM_ABI.Nested_Fault
                   elsif Code = Exit_MSR then VM_ABI.MSR_Access
                   elsif Code = Exit_CPUID then VM_ABI.CPUID
                   elsif Code = Exit_VMMCALL then VM_ABI.Hypercall
                   elsif Code = Exit_XSETBV then VM_ABI.XSETBV
                   elsif Code = Exit_WBINVD or else Code = Exit_INVD then VM_ABI.Cache_Flush
                   elsif Code = Exit_INIT then VM_ABI.Init_Signal
                   elsif Code = Exit_Shutdown then VM_ABI.Shutdown
                   elsif Code = Exit_Invalid then VM_ABI.Entry_Failed
                   elsif Code in Exit_Exception_Base .. Exit_Last_Exception then
                      VM_ABI.Exception_Exit
                   elsif Code in Exit_Read_DR0 .. Exit_Write_DR7 then VM_ABI.Debug_Register
                   else VM_ABI.Other);
                if Kind = VM_ABI.Exception_Exit then
                    Run_Reply (Reply, VM_ABI.Exited, Kind, Code,
                               Code - Exit_Exception_Base, Info_1);
                else
                    Run_Reply (Reply, VM_ABI.Exited, Kind, Code, Info_1, Info_2);
                end if;
            end if;
        end loop;
        x86.cli;

        Spinlocks.enterCriticalSection (Table_Lock);
        VCPUs (X).Runner := Process.NO_THREAD;
        declare
            Last_Out : constant Boolean := VMs (V).Phase = Closing and then not Any_Runner (V);
        begin
            Spinlocks.exitCriticalSection (Table_Lock);
            if Last_Out then
                Teardown (V);
            end if;
        end;
        if Status /= VM_ABI.Exited then
            Run_Reply (Reply, Status);
        end if;
        if Process_Lifetime.Closing (Process.threadtab (Me).lifetime) then
            Process.checkTermination;
        end if;
    end Run;

    ---------------------------------------------------------------------------
    -- State (docs/hypervisor.md 4.6, 4.8 rule 1)
    ---------------------------------------------------------------------------
    Segment_Offsets : constant array (0 .. 9) of VMCB_Offset :=
      [Guest_ES, Guest_CS, Guest_SS, Guest_DS, Guest_FS, Guest_GS,
       Guest_GDTR, Guest_LDTR, Guest_IDTR, Guest_TR];

    -- The EFER bits a monitor may set (SCE, LME, LMA, NXE, SVME, FFXSR,
    -- TCE), plus AutoIBRS where Guest_Speculation allows it.
    Guest_EFER_Base : constant Unsigned_64 := 16#0000_0000_0000_DD01#;

    function Canonical (A : Unsigned_64) return Boolean is
      (A < 16#0000_8000_0000_0000# or else A >= 16#FFFF_8000_0000_0000#);

    procedure State (Caller : Process.ProcessID; X : VCPU_Index; Set_It : Boolean;
                     Request : Process.Message; Reply : out Process.Message) is
        B : constant System.Address := VCPUs (X).VMCB;
        Set_Word : constant Unsigned_64 := Request.words (0);
        Buffer   : constant Unsigned_64 := Request.words (1);
        Bytes    : constant Unsigned_64 := Request.words (2);
        OK : Boolean;
        S  : VM_ABI.State_Set;
        G  : VM_ABI.General_Registers;
        Sg : VM_ABI.Segment_Registers;
        C  : VM_ABI.Control_Registers;
        M  : VM_ABI.Machine_Register_Values;
        E  : VM_ABI.Event_Values;
        Address : System.Address;
        Length  : Unsigned_64;
    begin
        if Set_Word not in 1 .. 5 then
            Answer (Reply, VM_ABI.Refused);
            return;
        end if;
        S := (case Set_Word is
                when 1 => VM_ABI.General, when 2 => VM_ABI.Segments,
                when 3 => VM_ABI.Control, when 4 => VM_ABI.Machine_Registers,
                when others => VM_ABI.Events);
        Length := VM_ABI.Bytes_Of (S);
        if Bytes < Length or else VCPUs (X).Runner /= Process.NO_THREAD then
            Answer (Reply, (if Bytes < Length then VM_ABI.Bad_Range else VM_ABI.Busy));
            return;
        end if;
        Address := (case S is
                      when VM_ABI.General => G'Address,
                      when VM_ABI.Segments => Sg'Address,
                      when VM_ABI.Control => C'Address,
                      when VM_ABI.Machine_Registers => M'Address,
                      when VM_ABI.Events => E'Address);
        if Set_It then
            if S = VM_ABI.Events then
                Answer (Reply, VM_ABI.Refused);   -- events go through Inject
                return;
            end if;
            -- One copy in, then validate (no field is read twice: 4.6).
            Process.User_Memory.Copy (Caller, Buffer, Address, Storage_Count (Length), OK);
            if not OK then
                Answer (Reply, VM_ABI.Bad_Range);
                return;
            end if;
        end if;
        case S is
            when VM_ABI.General =>
                if Set_It then
                    Put64 (B, Guest_RAX, G (VM_ABI.RAX_Index));
                    VCPUs (X).GPRs (GPR_RBX) := G (VM_ABI.RBX_Index);
                    VCPUs (X).GPRs (GPR_RCX) := G (VM_ABI.RCX_Index);
                    VCPUs (X).GPRs (GPR_RDX) := G (VM_ABI.RDX_Index);
                    VCPUs (X).GPRs (GPR_RSI) := G (VM_ABI.RSI_Index);
                    VCPUs (X).GPRs (GPR_RDI) := G (VM_ABI.RDI_Index);
                    VCPUs (X).GPRs (GPR_RBP) := G (VM_ABI.RBP_Index);
                    Put64 (B, Guest_RSP, G (VM_ABI.RSP_Index));
                    for R in 0 .. 7 loop
                        VCPUs (X).GPRs (GPR_R8 + R) := G (VM_ABI.R8_Index + R);
                    end loop;
                    -- A new RIP replaces the last exit's next RIP; the same
                    -- RIP keeps it (a monitor answering CPUID writes the
                    -- registers, then resumes with Resume_Advance_RIP).
                    if G (VM_ABI.RIP_Index) /= Get64 (B, Guest_RIP) then
                        VCPUs (X).Have_Next_RIP := False;
                        VCPUs (X).Fault_GP := False;   -- the refused WRMSR is left
                    end if;
                    Put64 (B, Guest_RIP, G (VM_ABI.RIP_Index));
                    -- RFLAGS bit 1 is fixed to 1; VM, the reserved bits 0.
                    Put64 (B, Guest_RFLAGS, (G (VM_ABI.RFLAGS_Index) and 16#003F_7FD7#) or 2);
                else
                    G := [Get64 (B, Guest_RAX), VCPUs (X).GPRs (GPR_RBX),
                          VCPUs (X).GPRs (GPR_RCX), VCPUs (X).GPRs (GPR_RDX),
                          VCPUs (X).GPRs (GPR_RSI), VCPUs (X).GPRs (GPR_RDI),
                          VCPUs (X).GPRs (GPR_RBP), Get64 (B, Guest_RSP),
                          VCPUs (X).GPRs (GPR_R8), VCPUs (X).GPRs (GPR_R8 + 1),
                          VCPUs (X).GPRs (GPR_R8 + 2), VCPUs (X).GPRs (GPR_R8 + 3),
                          VCPUs (X).GPRs (GPR_R8 + 4), VCPUs (X).GPRs (GPR_R8 + 5),
                          VCPUs (X).GPRs (GPR_R8 + 6), VCPUs (X).GPRs (GPR_R8 + 7),
                          Get64 (B, Guest_RIP), Get64 (B, Guest_RFLAGS)];
                end if;
            when VM_ABI.Segments =>
                if Set_It then
                    for I in Sg'Range loop
                        if not Canonical (Sg (I).Base) then
                            Answer (Reply, VM_ABI.Refused);
                            return;
                        end if;
                    end loop;
                    for I in Sg'Range loop
                        Put16 (B, Segment_Offsets (I) + Segment_Selector, Sg (I).Selector);
                        Put16 (B, Segment_Offsets (I) + Segment_Attributes,
                               Sg (I).Attributes and 16#0FFF#);
                        Put32 (B, Segment_Offsets (I) + Segment_Limit, Sg (I).Limit);
                        Put64 (B, Segment_Offsets (I) + Segment_Base, Sg (I).Base);
                    end loop;
                    -- SVM takes CPL from the save area: SS's DPL.
                    Put8 (B, Guest_CPL, Unsigned_8 (Shift_Right (Sg (VM_ABI.SS_Index).Attributes, 5) and 3));
                else
                    for I in Sg'Range loop
                        Sg (I) := (Selector   => Get16 (B, Segment_Offsets (I) + Segment_Selector),
                                   Attributes => Get16 (B, Segment_Offsets (I) + Segment_Attributes),
                                   Limit      => Get32 (B, Segment_Offsets (I) + Segment_Limit),
                                   Base       => Get64 (B, Segment_Offsets (I) + Segment_Base));
                    end loop;
                end if;
            when VM_ABI.Control =>
                if Set_It then
                    -- The guest never sees or clears EFER.SVME: the kernel
                    -- keeps it (VMRUN requires it). XCR0 stays the host's
                    -- in this slice (the monitor answers XSETBV).
                    if (C (VM_ABI.CR4_Index) and 16#FFFF_FFFF_FF80_0000#) /= 0
                      or else (C (VM_ABI.EFER_Index)
                               and not Spec.Guest_EFER_Bits (Guest_EFER_Base, Host_Features,
                                                             SVM => not Use_VMX)) /= 0
                    then
                        Answer (Reply, VM_ABI.Refused);
                        return;
                    end if;
                    if C (VM_ABI.CR0_Index) /= Get64 (B, Guest_CR0)
                      or else C (VM_ABI.CR3_Index) /= Get64 (B, Guest_CR3)
                      or else C (VM_ABI.CR4_Index) /= Get64 (B, Guest_CR4)
                      or else (C (VM_ABI.EFER_Index) or Shift_Left (1, EFER_SVME_Bit))
                              /= Get64 (B, Guest_EFER)
                    then
                        VCPUs (X).Tag_Stale := True;   -- Flush_On_Arch_Change
                    end if;
                    Put64 (B, Guest_CR0, C (VM_ABI.CR0_Index));
                    Put64 (B, Guest_CR2, C (VM_ABI.CR2_Index));
                    Put64 (B, Guest_CR3, C (VM_ABI.CR3_Index));
                    Put64 (B, Guest_CR4, C (VM_ABI.CR4_Index));
                    Put64 (B, Guest_EFER, C (VM_ABI.EFER_Index) or Shift_Left (1, EFER_SVME_Bit));
                    Put64 (B, Guest_DR6, C (VM_ABI.DR6_Index));
                    Put64 (B, Guest_DR7, C (VM_ABI.DR7_Index));
                else
                    C := [Get64 (B, Guest_CR0), Get64 (B, Guest_CR2), Get64 (B, Guest_CR3),
                          Get64 (B, Guest_CR4),
                          Get64 (B, Guest_EFER) and not Shift_Left (1, EFER_SVME_Bit),
                          Get64 (B, Guest_DR6), Get64 (B, Guest_DR7), x86.getXCR0,
                          Unsigned_64 (Get16 (B, Guest_CPL) and 16#FF#),
                          VM_ABI.Activity_Running];
                end if;
            when VM_ABI.Machine_Registers =>
                if Set_It then
                    if not Canonical (M (VM_ABI.LSTAR_Index))
                      or else not Canonical (M (VM_ABI.CSTAR_Index))
                      or else not Canonical (M (VM_ABI.Kernel_GS_Index))
                      or else not Spec.Value_Allowed (Spec_Mask, M (VM_ABI.SPEC_CTRL_Index))
                    then
                        Answer (Reply, VM_ABI.Refused);
                        return;
                    end if;
                    Put64 (B, Guest_STAR, M (VM_ABI.STAR_Index));
                    Put64 (B, Guest_LSTAR, M (VM_ABI.LSTAR_Index));
                    Put64 (B, Guest_CSTAR, M (VM_ABI.CSTAR_Index));
                    Put64 (B, Guest_SFMASK, M (VM_ABI.SFMASK_Index));
                    Put64 (B, Guest_Kernel_GS_Base, M (VM_ABI.Kernel_GS_Index));
                    Put64 (B, Guest_Sysenter_CS, M (VM_ABI.Sysenter_CS_Index));
                    Put64 (B, Guest_Sysenter_ESP, M (VM_ABI.Sysenter_ESP_Index));
                    Put64 (B, Guest_Sysenter_EIP, M (VM_ABI.Sysenter_EIP_Index));
                    Put64 (B, Guest_PAT, M (VM_ABI.PAT_Index));
                    Put64 (B, TSC_Offset, M (VM_ABI.TSC_Offset_Index));
                    Set_Guest_Spec_Value (X, M (VM_ABI.SPEC_CTRL_Index));
                    if Spec.Guest_Write (Spec_Mask, M (VM_ABI.SPEC_CTRL_Index)) = Spec.Pass_Writes then
                        Pass_Spec_Ctrl_Writes (X);
                    end if;
                else
                    M := [Get64 (B, Guest_STAR), Get64 (B, Guest_LSTAR), Get64 (B, Guest_CSTAR),
                          Get64 (B, Guest_SFMASK), Get64 (B, Guest_Kernel_GS_Base),
                          Get64 (B, Guest_Sysenter_CS), Get64 (B, Guest_Sysenter_ESP),
                          Get64 (B, Guest_Sysenter_EIP), Get64 (B, Guest_PAT),
                          Get64 (B, TSC_Offset), Guest_Spec_Value (X)];
                end if;
            when VM_ABI.Events =>
                E := [(if VCPUs (X).Pending.Valid
                       then Shift_Left (1, 63) or Unsigned_64 (VCPUs (X).Pending.Word) else 0),
                      Unsigned_64 (Get32 (B, Interrupt_State) and 1),
                      Unsigned_64 (Get32 (B, Exit_Interrupt_Info)),
                      Unsigned_64 (Get32 (B, Exit_Interrupt_Error))];
        end case;
        if not Set_It then
            Process.User_Memory.Copy_To_User (Caller, Buffer, Address, Storage_Count (Length), OK);
            if not OK then
                Answer (Reply, VM_ABI.Bad_Range);
                return;
            end if;
        end if;
        Answer (Reply, VM_ABI.Ok, W1 => Length);
    end State;

    ---------------------------------------------------------------------------
    -- Dispatch
    ---------------------------------------------------------------------------
    procedure Call (Caller  : Process.ProcessID;
                    Cap     : Capabilities.Capability;
                    Request : Process.Message;
                    Reply   : out Process.Message)
    is
        Label : constant Unsigned_64 := Unsigned_64 (Request.tag.label);
        V : Natural;
        X : Natural;
    begin
        Reply := Process.NULL_MESSAGE;
        if Cap.capType = Caps.CAP_VM_CREATE then
            if Label = VM_ABI.Create_VM_Label and then Cap.rights (Caps.RIGHT_WRITE) then
                Create_VM (Caller, Reply);
            else
                Answer (Reply, VM_ABI.Refused);
            end if;
        elsif Cap.capType = Caps.CAP_VM then
            V := VM_Of (Caller, Cap);
            if V = 0 or else not Cap.rights (Caps.RIGHT_WRITE) then
                Answer (Reply, VM_ABI.Refused);
            else
            case Label is
                when VM_ABI.Add_RAM_Label     => Add_RAM (Caller, V, Request, Reply);
                when VM_ABI.Create_VCPU_Label => Create_VCPU (Caller, V, Reply);
                when VM_ABI.Describe_Label    =>
                    Answer (Reply, VM_ABI.Ok, W1 => VMs (V).Pages,
                            W2 => Unsigned_64 (VMs (V).VCPU_Count),
                            W3 => Digest (V));
                when VM_ABI.Destroy_Label     => Destroy (V, Reply);
                when others                   => Answer (Reply, VM_ABI.Refused);
            end case;
            end if;
        elsif Cap.capType = Caps.CAP_VCPU then
            X := VCPU_Of (Caller, Cap);
            if X = 0 or else not Cap.rights (Caps.RIGHT_WRITE) then
                if Label = VM_ABI.Run_Label then
                    Run_Reply (Reply, VM_ABI.Run_Refused);
                else
                    Answer (Reply, VM_ABI.Refused);
                end if;
            else
            case Label is
                when VM_ABI.Run_Label       => Run (X, Request, Reply);
                when VM_ABI.Get_State_Label => State (Caller, X, False, Request, Reply);
                when VM_ABI.Set_State_Label => State (Caller, X, True, Request, Reply);
                when VM_ABI.Inject_Label    => Inject (X, Request, Reply);
                when VM_ABI.Statistics_Label =>
                    Answer (Reply, VM_ABI.Ok, W1 => VCPUs (X).Entries,
                            W2 => VCPUs (X).Flushes, W3 => VCPUs (X).Forwarded);
                when VM_ABI.Speculation_Label =>
                    Answer (Reply, VM_ABI.Ok,
                            W1 => (if Spec.Present (Spec_Mask)
                                   then x86.rdmsr (Spec.IA32_SPEC_CTRL) else 0),
                            W2 => Spec_Mask,
                            W3 => (if Spec.Present (Spec_Mask) then VM_ABI.Speculation_Present else 0)
                                  or (if Spec_By_VMRUN then VM_ABI.Speculation_CPU_Switched else 0)
                                  or (if VCPUs (X).Spec_Writes_Passed
                                      then VM_ABI.Speculation_Writes_Passed else 0));
                when VM_ABI.Kick_Label      =>
                    VCPUs (X).Kicked := True;
                    Wake_Halted (X);
                    Kick_Running (X);
                    Answer (Reply, VM_ABI.Ok);
                when others                 => Answer (Reply, VM_ABI.Refused);
            end case;
            end if;
        else
            Answer (Reply, VM_ABI.Refused);
        end if;
        -- Every answer but Run's carries the request's label, as the other
        -- kernel objects' do (Run's label is its Run_Status).
        if Label /= VM_ABI.Run_Label then
            Reply.tag.label := Request.tag.label;
        end if;
    end Call;

    procedure Process_Ended (Pid : Process.ProcessID) is
    begin
        if not SVM_On_BSP then
            return;
        end if;
        for V in VM_Index loop
            if VMs (V).Phase /= Gone and then VMs (V).Monitor = Pid then
                -- Its threads are gone: no Run is live, nothing is halted.
                for N in 1 .. VMs (V).VCPU_Count loop
                    VCPUs (VMs (V).VCPUs (N)).Runner := Process.NO_THREAD;
                    VCPUs (VMs (V).VCPUs (N)).Halted := Process.NO_THREAD;
                end loop;
                VMs (V).Phase := Closing;
                Teardown (V);
            end if;
        end loop;
    end Process_Ended;
end VM_Objects;
