pragma Ada_2022;
with System.Machine_Code; use System.Machine_Code;
with System.Storage_Elements; use System.Storage_Elements;
with BuddyAllocator;
with Config;
with Guest_Speculation;
with SVM_Layout; use SVM_Layout;
with TextIO; use TextIO;
with Virtmem;
with Virtualization_Features;
with Virtualization_Report;
with VMX_Layout; use VMX_Layout;
with x86;

package body VMX_Run with SPARK_Mode => Off is
    use type System.Address;

    ---------------------------------------------------------------------------
    -- Instructions
    ---------------------------------------------------------------------------
    procedure Write (F : VMX_Layout.Field; Value : Unsigned_64) is
    begin
        Asm ("vmwrite %1, %0",
             Inputs => (Unsigned_64'Asm_Input ("r", F), Unsigned_64'Asm_Input ("r", Value)),
             Clobber => "cc, memory", Volatile => True);
    end Write;

    function Read (F : VMX_Layout.Field) return Unsigned_64 is
        Value : Unsigned_64;
    begin
        Asm ("vmread %1, %0",
             Outputs => Unsigned_64'Asm_Output ("=r", Value),
             Inputs => Unsigned_64'Asm_Input ("r", F),
             Clobber => "cc", Volatile => True);
        return Value;
    end Read;

    procedure VMPTRLD (Physical : Unsigned_64) is
        P : aliased Unsigned_64 := Physical;
    begin
        Asm ("vmptrld %0", Inputs => Unsigned_64'Asm_Input ("m", P),
             Clobber => "cc, memory", Volatile => True);
    end VMPTRLD;

    procedure VMCLEAR (Physical : Unsigned_64) is
        P : aliased Unsigned_64 := Physical;
    begin
        Asm ("vmclear %0", Inputs => Unsigned_64'Asm_Input ("m", P),
             Clobber => "cc, memory", Volatile => True);
    end VMCLEAR;

    function VMXON (Physical : Unsigned_64) return Boolean is
        P : aliased Unsigned_64 := Physical;
        Failed : Unsigned_8;
    begin
        Asm ("vmxon %1; setna %0",
             Outputs => Unsigned_8'Asm_Output ("=r", Failed),
             Inputs => Unsigned_64'Asm_Input ("m", P),
             Clobber => "cc, memory", Volatile => True);
        return Failed = 0;
    end VMXON;

    type INVVPID_Descriptor is record
        VPID   : Unsigned_64 := 0;
        Linear : Unsigned_64 := 0;
    end record;
    Single_Context : constant Unsigned_64 := 1;

    procedure INVVPID (VPID : Unsigned_64) is
        D : aliased INVVPID_Descriptor := (VPID => VPID, Linear => 0);
    begin
        Asm ("invvpid %1, %0",
             Inputs => (Unsigned_64'Asm_Input ("r", Single_Context),
                        INVVPID_Descriptor'Asm_Input ("m", D)),
             Clobber => "cc, memory", Volatile => True);
    end INVVPID;

    function cubitVmxRun (Unused : Unsigned_64; GPRs, RAX_Slot, Spec_Ctrl : System.Address)
      return Unsigned_64
      with Import, Convention => C, External_Name => "cubitVmxRun";

    function Read_CR (N : Natural) return Unsigned_64 is
        V : Unsigned_64;
    begin
        case N is
            when 0 => Asm ("mov %%cr0, %0", Outputs => Unsigned_64'Asm_Output ("=r", V), Volatile => True);
            when 2 => Asm ("mov %%cr2, %0", Outputs => Unsigned_64'Asm_Output ("=r", V), Volatile => True);
            when 3 => Asm ("mov %%cr3, %0", Outputs => Unsigned_64'Asm_Output ("=r", V), Volatile => True);
            when others => Asm ("mov %%cr4, %0", Outputs => Unsigned_64'Asm_Output ("=r", V), Volatile => True);
        end case;
        return V;
    end Read_CR;

    procedure Write_CR2 (V : Unsigned_64) is
    begin
        Asm ("mov %0, %%cr2", Inputs => Unsigned_64'Asm_Input ("r", V), Volatile => True);
    end Write_CR2;

    type Descriptor_Table is record
        Limit : Unsigned_16 := 0;
        Base  : Unsigned_64 := 0;
    end record with Size => 80;
    for Descriptor_Table use record
        Limit at 0 range 0 .. 15;
        Base  at 2 range 0 .. 63;
    end record;

    procedure Store_Tables (GDT, IDT : out Descriptor_Table) is
        G, I : aliased Descriptor_Table;
    begin
        Asm ("sgdt %0", Outputs => Descriptor_Table'Asm_Output ("=m", G), Volatile => True);
        Asm ("sidt %0", Outputs => Descriptor_Table'Asm_Output ("=m", I), Volatile => True);
        GDT := G;
        IDT := I;
    end Store_Tables;

    procedure Load_Tables (GDT, IDT : Descriptor_Table) is
        G : aliased Descriptor_Table := GDT;
        I : aliased Descriptor_Table := IDT;
    begin
        Asm ("lgdt %0", Inputs => Descriptor_Table'Asm_Input ("m", G), Volatile => True);
        Asm ("lidt %0", Inputs => Descriptor_Table'Asm_Input ("m", I), Volatile => True);
    end Load_Tables;

    function Selector (Name : String) return Unsigned_64 is
        V : Unsigned_16 := 0;
    begin
        if Name = "cs" then
            Asm ("mov %%cs, %0", Outputs => Unsigned_16'Asm_Output ("=r", V), Volatile => True);
        elsif Name = "ss" then
            Asm ("mov %%ss, %0", Outputs => Unsigned_16'Asm_Output ("=r", V), Volatile => True);
        elsif Name = "ds" then
            Asm ("mov %%ds, %0", Outputs => Unsigned_16'Asm_Output ("=r", V), Volatile => True);
        elsif Name = "es" then
            Asm ("mov %%es, %0", Outputs => Unsigned_16'Asm_Output ("=r", V), Volatile => True);
        elsif Name = "fs" then
            Asm ("mov %%fs, %0", Outputs => Unsigned_16'Asm_Output ("=r", V), Volatile => True);
        elsif Name = "gs" then
            Asm ("mov %%gs, %0", Outputs => Unsigned_16'Asm_Output ("=r", V), Volatile => True);
        else
            Asm ("str %0", Outputs => Unsigned_16'Asm_Output ("=r", V), Volatile => True);
        end if;
        -- The host selectors' RPL and TI must be 0 (SDM 27.2.3).
        return Unsigned_64 (V) and 16#FFF8#;
    end Selector;

    function Byte_At (A : Unsigned_64) return Unsigned_64 is
        B : Unsigned_8 with Import, Address => To_Address (Integer_Address (A));
    begin
        return Unsigned_64 (B);
    end Byte_At;

    -- A 64-bit system descriptor's base (the TSS).
    function Descriptor_Base (GDT_Base, Sel : Unsigned_64) return Unsigned_64 is
        D : constant Unsigned_64 := GDT_Base + Sel;
    begin
        return Byte_At (D + 2) or Shift_Left (Byte_At (D + 3), 8) or Shift_Left (Byte_At (D + 4), 16)
          or Shift_Left (Byte_At (D + 7), 24) or Shift_Left (Byte_At (D + 8), 32)
          or Shift_Left (Byte_At (D + 9), 40) or Shift_Left (Byte_At (D + 10), 48)
          or Shift_Left (Byte_At (D + 11), 56);
    end Descriptor_Base;

    ---------------------------------------------------------------------------
    -- The state page (SVM layout)
    ---------------------------------------------------------------------------
    procedure Put64 (B : System.Address; O : Natural; V : Unsigned_64) is
        W : Unsigned_64 with Import, Volatile, Address => B + Storage_Offset (O);
    begin
        W := V;
    end Put64;
    function Get64 (B : System.Address; O : Natural) return Unsigned_64 is
        W : Unsigned_64 with Import, Volatile, Address => B + Storage_Offset (O);
    begin
        return W;
    end Get64;
    procedure Put32 (B : System.Address; O : Natural; V : Unsigned_32) is
        W : Unsigned_32 with Import, Volatile, Address => B + Storage_Offset (O);
    begin
        W := V;
    end Put32;
    function Get32 (B : System.Address; O : Natural) return Unsigned_32 is
        W : Unsigned_32 with Import, Volatile, Address => B + Storage_Offset (O);
    begin
        return W;
    end Get32;
    procedure Put16 (B : System.Address; O : Natural; V : Unsigned_16) is
        W : Unsigned_16 with Import, Volatile, Address => B + Storage_Offset (O);
    begin
        W := V;
    end Put16;
    function Get16 (B : System.Address; O : Natural) return Unsigned_16 is
        W : Unsigned_16 with Import, Volatile, Address => B + Storage_Offset (O);
    begin
        return W;
    end Get16;
    function Get8 (B : System.Address; O : Natural) return Unsigned_8 is
        W : Unsigned_8 with Import, Volatile, Address => B + Storage_Offset (O);
    begin
        return W;
    end Get8;

    ---------------------------------------------------------------------------
    -- Per-CPU VMXON regions, the maps, the controls
    ---------------------------------------------------------------------------
    subtype CPU_Number is Natural range 0 .. Config.MAX_CPUS - 1;
    VMXON_Region : array (CPU_Number) of Unsigned_64 := [others => 0];
    IO_A, IO_B, MSR_Map, MSR_Map_Pass : Unsigned_64 := 0;
    Revision : Unsigned_32 := 0;

    function Revision_ID return Unsigned_32 is
      (Unsigned_32 (Virtualization_Report.VMX_Values.Basic and 16#7FFF_FFFF#));

    -- A control word the CPU accepts: what is wanted, plus what must be 1,
    -- minus what may not be (the TRUE capability MSRs' halves).
    function Adjust (Wanted : Unsigned_64; Capability : Unsigned_64) return Unsigned_64 is
      ((Wanted or (Capability and 16#FFFF_FFFF#)) and Shift_Right (Capability, 32));

    function Bit (N : Natural) return Unsigned_64 is (Shift_Left (1, N));

    procedure Enable_This_CPU (CPU : Natural; OK : out Boolean) is
        Region : Virtmem.PhysAddress;
        Control : Unsigned_64;
        CR0, CR4 : Unsigned_64;
    begin
        OK := False;
        if not Virtualization_Report.VMX_Usable or else CPU > CPU_Number'Last then
            return;
        end if;
        Control := x86.rdmsr (IA32_FEATURE_CONTROL);
        if (Control and Feature_Control_Lock) = 0 then
            x86.wrmsr (IA32_FEATURE_CONTROL,
                       Control or Feature_Control_Lock or Feature_Control_VMXON_Outside_SMX);
        elsif (Control and Feature_Control_VMXON_Outside_SMX) = 0 then
            return;
        end if;
        BuddyAllocator.allocFrame (Region);
        if Region = 0 then
            return;
        end if;
        declare
            Base : constant System.Address := Virtmem.P2Va (Region);
        begin
            for I in 0 .. 511 loop
                Put64 (Base, I * 8, 0);
            end loop;
            Revision := Revision_ID;
            Put32 (Base, 0, Revision);
        end;
        -- CR0 and CR4 as VMX operation requires (SDM 24.8).
        CR0 := (Read_CR (0) or x86.rdmsr (IA32_VMX_CR0_FIXED0)) and x86.rdmsr (IA32_VMX_CR0_FIXED1);
        Asm ("mov %0, %%cr0", Inputs => Unsigned_64'Asm_Input ("r", CR0), Volatile => True);
        CR4 := (Read_CR (4) or CR4_VMXE or x86.rdmsr (IA32_VMX_CR4_FIXED0))
               and x86.rdmsr (IA32_VMX_CR4_FIXED1);
        x86.setCR4 (CR4);
        if not VMXON (Unsigned_64 (Region)) then
            println ("vm: VMXON refused on this CPU");
            return;
        end if;
        VMXON_Region (CPU) := Unsigned_64 (Region);
        OK := True;
    end Enable_This_CPU;

    -- The MSRs the guest reaches without an exit: those the VMCS switches
    -- (FS and GS base, SYSENTER) and those the MSR lists switch, for reads
    -- and writes; SPEC_CTRL and PRED_CMD as Guest_Speculation says.
    procedure Clear_MSR (Map : System.Address; MSR : Unsigned_32;
                         Read : Boolean := True; Write : Boolean := True) is
        procedure Clear_Bit (Offset : Natural; Index : Natural) is
            B : Unsigned_8 with Import, Volatile,
              Address => Map + Storage_Offset (Offset + Index / 8);
        begin
            B := B and not Shift_Left (1, Index mod 8);
        end Clear_Bit;
    begin
        if MSR <= 16#1FFF# then
            if Read then Clear_Bit (16#000#, Natural (MSR)); end if;   -- read low
            if Write then Clear_Bit (16#800#, Natural (MSR)); end if;  -- write low
        elsif MSR in 16#C000_0000# .. 16#C000_1FFF# then
            if Read then Clear_Bit (16#400#, Natural (MSR - 16#C000_0000#)); end if;
            if Write then Clear_Bit (16#C00#, Natural (MSR - 16#C000_0000#)); end if;
        end if;
    end Clear_MSR;

    -- The MSRs the VM-entry and VM-exit lists switch (STAR .. SFMASK and
    -- KERNEL_GS_BASE are not in the VMCS) and their state-page offsets.
    type MSR_Entry is record
        Index  : Unsigned_32;
        Offset : Natural;
    end record;
    List_MSRs : constant array (1 .. 5) of MSR_Entry :=
      [(16#C000_0081#, Guest_STAR), (16#C000_0082#, Guest_LSTAR),
       (16#C000_0083#, Guest_CSTAR), (16#C000_0084#, Guest_SFMASK),
       (16#C000_0102#, Guest_Kernel_GS_Base)];
    type MSR_Numbers is array (Positive range <>) of Unsigned_32;
    VMCS_Switched_MSRs : constant MSR_Numbers :=
      [16#0000_0174#, 16#0000_0175#, 16#0000_0176#, 16#C000_0100#, 16#C000_0101#];
    Host_List_Offset : constant := 2048;
    List_Entry_Bytes : constant := 16;

    procedure Prepare_Maps (OK : out Boolean) is
        A, B, M, P : Virtmem.PhysAddress;
    begin
        if IO_A /= 0 then
            OK := True;
            return;
        end if;
        BuddyAllocator.allocFrame (A);
        BuddyAllocator.allocFrame (B);
        BuddyAllocator.allocFrame (M);
        BuddyAllocator.allocFrame (P);
        OK := A /= 0 and then B /= 0 and then M /= 0 and then P /= 0;
        if not OK then
            return;
        end if;
        for I in 0 .. 4095 loop
            declare
                PA : Unsigned_8 with Import, Volatile, Address => Virtmem.P2Va (A) + Storage_Offset (I);
                PB : Unsigned_8 with Import, Volatile, Address => Virtmem.P2Va (B) + Storage_Offset (I);
                PM : Unsigned_8 with Import, Volatile, Address => Virtmem.P2Va (M) + Storage_Offset (I);
                PP : Unsigned_8 with Import, Volatile, Address => Virtmem.P2Va (P) + Storage_Offset (I);
            begin
                PA := 16#FF#; PB := 16#FF#; PM := 16#FF#; PP := 16#FF#;
            end;
        end loop;
        declare
            type Map_List is array (1 .. 2) of System.Address;
            Maps : constant Map_List := [Virtmem.P2Va (M), Virtmem.P2Va (P)];
        begin
            for Map of Maps loop
                for R of VMCS_Switched_MSRs loop
                    Clear_MSR (Map, R);
                end loop;
                for E of List_MSRs loop
                    Clear_MSR (Map, E.Index);
                end loop;
                -- SPEC_CTRL reads see the guest's value (loaded at entry);
                -- PRED_CMD is write-only and acts on predictor state alone.
                Clear_MSR (Map, Guest_Speculation.IA32_SPEC_CTRL, Read => True, Write => False);
                Clear_MSR (Map, Guest_Speculation.IA32_PRED_CMD, Read => False, Write => True);
            end loop;
        end;
        Clear_MSR (Virtmem.P2Va (P), Guest_Speculation.IA32_SPEC_CTRL, Read => False, Write => True);
        IO_A := Unsigned_64 (A); IO_B := Unsigned_64 (B); MSR_Map := Unsigned_64 (M);
        MSR_Map_Pass := Unsigned_64 (P);
    end Prepare_Maps;

    procedure Prepare_VMCS (VMCS : System.Address) is
    begin
        for I in 0 .. 511 loop
            Put64 (VMCS, I * 8, 0);
        end loop;
        Put32 (VMCS, 0, Revision_ID);
        VMCLEAR (Unsigned_64 (Virtmem.V2P (VMCS)));
    end Prepare_VMCS;

    ---------------------------------------------------------------------------
    -- Segments: SVM's 12-bit attributes to VMX access rights and back
    ---------------------------------------------------------------------------
    Unusable_Bit : constant Unsigned_64 := Bit (16);
    function To_Access (A : Unsigned_16) return Unsigned_64 is
      ((Unsigned_64 (A) and 16#FF#) or Shift_Left (Shift_Right (Unsigned_64 (A), 8) and 16#F#, 12)
       or (if (A and 16#80#) = 0 then Unusable_Bit else 0));
    function From_Access (R : Unsigned_64) return Unsigned_16 is
      (if (R and Unusable_Bit) /= 0 then 0
       else Unsigned_16 ((R and 16#FF#) or Shift_Left (Shift_Right (R, 12) and 16#F#, 8)));

    -- The eight segments with all four VMCS fields, in VMCS order (ES, CS,
    -- SS, DS, FS, GS, LDTR, TR), and their state-page offsets.
    Segment_State : constant array (0 .. 7) of VMCB_Offset :=
      [Guest_ES, Guest_CS, Guest_SS, Guest_DS, Guest_FS, Guest_GS, Guest_LDTR, Guest_TR];

    ---------------------------------------------------------------------------
    -- Enter
    ---------------------------------------------------------------------------
    Logged_Failures : Natural := 0;

    procedure Enter
      (State       : System.Address;
       VMCS        : System.Address;
       Lists       : System.Address;
       GPRs        : System.Address;
       EPT_Root    : Unsigned_64;
       VPID        : Unsigned_64;
       Flush_Tag   : Boolean;
       Spec_Ctrl   : System.Address;
       Pass_Spec_Ctrl_Writes : Boolean)
    is
        Raw : constant Virtualization_Features.VMX_Raw := Virtualization_Report.VMX_Values;
        VMCS_Phys : constant Unsigned_64 := Unsigned_64 (Virtmem.V2P (VMCS));
        Lists_Phys : constant Unsigned_64 := Unsigned_64 (Virtmem.V2P (Lists));
        GDT, IDT : Descriptor_Table;
        CR0_Fixed0 : constant Unsigned_64 := x86.rdmsr (IA32_VMX_CR0_FIXED0)
                                             and not 16#8000_0001#;  -- PE, PG: unrestricted
        CR0_Fixed1 : constant Unsigned_64 := x86.rdmsr (IA32_VMX_CR0_FIXED1);
        CR4_Fixed0 : constant Unsigned_64 := x86.rdmsr (IA32_VMX_CR4_FIXED0);
        CR4_Fixed1 : constant Unsigned_64 := x86.rdmsr (IA32_VMX_CR4_FIXED1);
        CR0_View : constant Unsigned_64 := Get64 (State, SVM_Layout.Guest_CR0);
        CR4_View : constant Unsigned_64 := Get64 (State, SVM_Layout.Guest_CR4);
        EFER : constant Unsigned_64 := Get64 (State, SVM_Layout.Guest_EFER) and not Shift_Left (1, EFER_SVME_Bit);
        Word3 : constant Unsigned_32 := Get32 (State, Intercept_Word_3);
        Inject : constant Unsigned_32 := Get32 (State, Event_Inject);
        Failed : Unsigned_64;
        Reason, Qualification, RIP : Unsigned_64;
        Host_Table : Unsigned_64;
    begin
        Store_Tables (GDT, IDT);
        VMPTRLD (VMCS_Phys);

        -- Controls (Required_Controls, docs/hypervisor.md 4.6).
        Write (Pin_Controls, Adjust (Bit (Pin_External_Interrupt) or Bit (Pin_NMI), Raw.Pin));
        Write (Processor_Controls,
               Adjust (Bit (Proc_HLT) or Bit (Proc_MWAIT) or Bit (Proc_MONITOR) or Bit (Proc_MOV_DR)
                       or Bit (Proc_IO_Bitmaps) or Bit (Proc_MSR_Bitmaps) or Bit (Proc_Secondary)
                       or Bit (Proc_TSC_Offsetting)
                       or (if (Word3 and Shift_Left (1, Intercept_VINTR)) /= 0
                           then Bit (Proc_Interrupt_Window) else 0),
                       Raw.Proc));
        Write (Secondary_Controls,
               Adjust (Bit (Proc2_EPT) or Bit (Proc2_VPID) or Bit (Proc2_Unrestricted)
                       or Bit (Proc2_RDTSCP) or Bit (Proc2_INVPCID), Raw.Proc2));
        Write (Exit_Controls,
               Adjust (Bit (VMX_Layout.Exit_Host_64) or Bit (Exit_Save_EFER) or Bit (VMX_Layout.Exit_Load_EFER)
                       or Bit (Exit_Save_PAT) or Bit (Exit_Load_PAT), Raw.Exit_Controls));
        Write (Entry_Controls,
               Adjust (Bit (Entry_Load_EFER) or Bit (VMX_Layout.Entry_Load_PAT)
                       or (if (EFER and 16#400#) /= 0 then Bit (Entry_IA32E_Guest) else 0),
                       Raw.Entry_Controls));
        Write (Exception_Bitmap, Unsigned_64 (Get32 (State, Intercept_Exceptions)));
        Write (IO_Bitmap_A, IO_A);
        Write (IO_Bitmap_B, IO_B);
        Write (MSR_Bitmap, (if Pass_Spec_Ctrl_Writes then MSR_Map_Pass else MSR_Map));
        Write (EPT_Pointer, EPT_Root or EPTP_Write_Back_Walk_4);
        Write (VMX_Layout.VPID, VPID);
        Write (VMX_Layout.TSC_Offset, Get64 (State, SVM_Layout.TSC_Offset));
        Write (VMCS_Link_Pointer, Unsigned_64'Last);
        -- The MSR lists: guest values stored at exit and loaded at entry in
        -- the first half of the page, the host's loaded at exit in the second.
        for I in List_MSRs'Range loop
            declare
                G : constant Natural := (I - 1) * List_Entry_Bytes;
                H : constant Natural := Host_List_Offset + (I - 1) * List_Entry_Bytes;
            begin
                Put32 (Lists, G, List_MSRs (I).Index);
                Put64 (Lists, G + 8, Get64 (State, List_MSRs (I).Offset));
                Put32 (Lists, H, List_MSRs (I).Index);
                Put64 (Lists, H + 8, x86.rdmsr (List_MSRs (I).Index));
            end;
        end loop;
        Write (Entry_MSR_Load_Address, Lists_Phys);
        Write (Entry_MSR_Load_Count, List_MSRs'Length);
        Write (Exit_MSR_Store_Address, Lists_Phys);
        Write (Exit_MSR_Store_Count, List_MSRs'Length);
        Write (Exit_MSR_Load_Address, Lists_Phys + Host_List_Offset);
        Write (Exit_MSR_Load_Count, List_MSRs'Length);

        -- Guest state from the state page.
        for I in Segment_State'Range loop
            declare
                O : constant VMCB_Offset := Segment_State (I);
                F : constant Unsigned_64 := Unsigned_64 (I) * Segment_Stride;
            begin
                Write (Guest_ES_Selector + F, Unsigned_64 (Get16 (State, O + Segment_Selector)));
                Write (Guest_ES_Limit + F, Unsigned_64 (Get32 (State, O + Segment_Limit)));
                Write (Guest_ES_Access + F, To_Access (Get16 (State, O + Segment_Attributes)));
                Write (Guest_ES_Base + F, Get64 (State, O + Segment_Base));
            end;
        end loop;
        Write (Guest_GDTR_Limit, Unsigned_64 (Get32 (State, Guest_GDTR + Segment_Limit)));
        Write (Guest_GDTR_Base, Get64 (State, Guest_GDTR + Segment_Base));
        Write (Guest_IDTR_Limit, Unsigned_64 (Get32 (State, Guest_IDTR + Segment_Limit)));
        Write (Guest_IDTR_Base, Get64 (State, Guest_IDTR + Segment_Base));
        Write (VMX_Layout.Guest_CR0, (CR0_View or CR0_Fixed0) and CR0_Fixed1);
        Write (CR0_Mask, CR0_Fixed0);
        Write (CR0_Read_Shadow, CR0_View);
        Write (VMX_Layout.Guest_CR4, (CR4_View or CR4_Fixed0) and CR4_Fixed1);
        Write (CR4_Mask, CR4_Fixed0);
        Write (CR4_Read_Shadow, CR4_View);
        Write (VMX_Layout.Guest_CR3, Get64 (State, SVM_Layout.Guest_CR3));
        Write (VMX_Layout.Guest_EFER, EFER);
        Write (VMX_Layout.Guest_PAT, Get64 (State, SVM_Layout.Guest_PAT));
        Write (VMX_Layout.Guest_DR7, Get64 (State, SVM_Layout.Guest_DR7));
        Write (VMX_Layout.Guest_RSP, Get64 (State, SVM_Layout.Guest_RSP));
        Write (VMX_Layout.Guest_RIP, Get64 (State, SVM_Layout.Guest_RIP));
        Write (VMX_Layout.Guest_RFLAGS, Get64 (State, SVM_Layout.Guest_RFLAGS));
        Write (VMX_Layout.Guest_Sysenter_CS, Get64 (State, SVM_Layout.Guest_Sysenter_CS));
        Write (VMX_Layout.Guest_Sysenter_ESP, Get64 (State, SVM_Layout.Guest_Sysenter_ESP));
        Write (VMX_Layout.Guest_Sysenter_EIP, Get64 (State, SVM_Layout.Guest_Sysenter_EIP));
        Write (Guest_Pending_Debug, 0);
        Write (Guest_Activity, 0);
        declare
            Shadow : constant Boolean := (Get32 (State, Interrupt_State) and 1) /= 0;
            IF_Set : constant Boolean := (Get64 (State, SVM_Layout.Guest_RFLAGS) and 16#200#) /= 0;
        begin
            Write (Guest_Interruptibility,
                   (if not Shadow then 0 elsif IF_Set then 1 else 2));   -- STI, else MOV SS
        end;
        -- The event to deliver (SVM's EVENTINJ and VMX's interruption field
        -- share the encoding: vector, type << 8, error-valid bit 11, valid 31).
        Write (Entry_Interruption_Info, Unsigned_64 (Inject));
        Write (Entry_Exception_Error, Unsigned_64 (Get32 (State, Event_Inject_Error)));
        Write (Entry_Instruction_Length, 0);

        -- Host state.
        Write (VMX_Layout.Host_CR0, Read_CR (0));
        Write (VMX_Layout.Host_CR3, Read_CR (3));
        Write (VMX_Layout.Host_CR4, Read_CR (4));
        Write (Host_CS_Selector, Selector ("cs"));
        Write (Host_SS_Selector, Selector ("ss"));
        Write (Host_DS_Selector, Selector ("ds"));
        Write (Host_ES_Selector, Selector ("es"));
        Write (Host_FS_Selector, Selector ("fs"));
        Write (Host_GS_Selector, Selector ("gs"));
        Write (Host_TR_Selector, Selector ("tr"));
        Write (VMX_Layout.Host_FS_Base, x86.rdmsr (16#C000_0100#));
        Write (VMX_Layout.Host_GS_Base, x86.rdmsr (16#C000_0101#));
        Host_Table := GDT.Base;
        Write (Host_TR_Base, Descriptor_Base (Host_Table, Selector ("tr")));
        Write (Host_GDTR_Base, GDT.Base);
        Write (Host_IDTR_Base, IDT.Base);
        Write (Host_EFER, x86.rdmsr (MSR_EFER));
        Write (Host_PAT, x86.rdmsr (16#277#));
        Write (Host_Sysenter_CS, x86.rdmsr (16#174#));
        Write (Host_Sysenter_ESP, x86.rdmsr (16#175#));
        Write (Host_Sysenter_EIP, x86.rdmsr (16#176#));

        if Flush_Tag then
            INVVPID (VPID);
        end if;
        Write_CR2 (Get64 (State, SVM_Layout.Guest_CR2));
        Failed := cubitVmxRun (0, GPRs, State + Storage_Offset (SVM_Layout.Guest_RAX), Spec_Ctrl);
        -- A VM exit leaves the GDTR and IDTR limits at 0xFFFF (SDM 28.5.2).
        Load_Tables (GDT, IDT);

        if Failed /= 0 then
            if Logged_Failures < 4 then
                Logged_Failures := Logged_Failures + 1;
                print ("vm: VMLAUNCH failed, VM-instruction error ");
                printdln (Read (Instruction_Error));
            end if;
            Put64 (State, Exit_Code, Exit_Invalid);
            VMCLEAR (VMCS_Phys);
            return;
        end if;
        Put64 (State, SVM_Layout.Guest_CR2, Read_CR (2));

        -- Guest state back to the state page.
        for I in Segment_State'Range loop
            declare
                O : constant VMCB_Offset := Segment_State (I);
                F : constant Unsigned_64 := Unsigned_64 (I) * Segment_Stride;
            begin
                Put16 (State, O + Segment_Selector, Unsigned_16 (Read (Guest_ES_Selector + F) and 16#FFFF#));
                Put32 (State, O + Segment_Limit, Unsigned_32 (Read (Guest_ES_Limit + F) and 16#FFFF_FFFF#));
                Put16 (State, O + Segment_Attributes, From_Access (Read (Guest_ES_Access + F)));
                Put64 (State, O + Segment_Base, Read (Guest_ES_Base + F));
            end;
        end loop;
        Put32 (State, Guest_GDTR + Segment_Limit, Unsigned_32 (Read (Guest_GDTR_Limit) and 16#FFFF#));
        Put64 (State, Guest_GDTR + Segment_Base, Read (Guest_GDTR_Base));
        Put32 (State, Guest_IDTR + Segment_Limit, Unsigned_32 (Read (Guest_IDTR_Limit) and 16#FFFF#));
        Put64 (State, Guest_IDTR + Segment_Base, Read (Guest_IDTR_Base));
        Put64 (State, SVM_Layout.Guest_CR0,
               (Read (VMX_Layout.Guest_CR0) and not CR0_Fixed0) or (CR0_View and CR0_Fixed0));
        Put64 (State, SVM_Layout.Guest_CR4,
               (Read (VMX_Layout.Guest_CR4) and not CR4_Fixed0) or (CR4_View and CR4_Fixed0));
        Put64 (State, SVM_Layout.Guest_CR3, Read (VMX_Layout.Guest_CR3));
        Put64 (State, SVM_Layout.Guest_EFER, Read (VMX_Layout.Guest_EFER) or Shift_Left (1, EFER_SVME_Bit));
        Put64 (State, SVM_Layout.Guest_PAT, Read (VMX_Layout.Guest_PAT));
        Put64 (State, SVM_Layout.Guest_DR7, Read (VMX_Layout.Guest_DR7));
        Put64 (State, SVM_Layout.Guest_RSP, Read (VMX_Layout.Guest_RSP));
        RIP := Read (VMX_Layout.Guest_RIP);
        Put64 (State, SVM_Layout.Guest_RIP, RIP);
        Put64 (State, SVM_Layout.Guest_RFLAGS, Read (VMX_Layout.Guest_RFLAGS));
        Put64 (State, SVM_Layout.Guest_Sysenter_CS, Read (VMX_Layout.Guest_Sysenter_CS));
        Put64 (State, SVM_Layout.Guest_Sysenter_ESP, Read (VMX_Layout.Guest_Sysenter_ESP));
        Put64 (State, SVM_Layout.Guest_Sysenter_EIP, Read (VMX_Layout.Guest_Sysenter_EIP));
        Put8_Shadow : declare
            CPL : constant Unsigned_8 :=
              Unsigned_8 (Shift_Right (Read (Guest_ES_Access + 2 * Segment_Stride), 5) and 3);
            B : Unsigned_8 with Import, Volatile, Address => State + Storage_Offset (Guest_CPL);
        begin
            B := CPL;
        end Put8_Shadow;
        for I in List_MSRs'Range loop
            Put64 (State, List_MSRs (I).Offset, Get64 (Lists, (I - 1) * List_Entry_Bytes + 8));
        end loop;
        Put32 (State, Interrupt_State,
               (if (Read (Guest_Interruptibility) and 3) /= 0 then 1 else 0));
        Put32 (State, Event_Inject, 0);

        -- The exit, as SVM codes and information words.
        Reason := Read (Exit_Reason) and Reason_Basic_Mask;
        Qualification := Read (Exit_Qualification);
        Put64 (State, Next_RIP, RIP + Read (Exit_Instruction_Length));
        Put32 (State, Exit_Interrupt_Info, Unsigned_32 (Read (IDT_Vectoring_Info) and 16#FFFF_FFFF#));
        Put32 (State, Exit_Interrupt_Error, Unsigned_32 (Read (IDT_Vectoring_Error) and 16#FFFF_FFFF#));
        Put64 (State, Exit_Info_1, 0);
        Put64 (State, Exit_Info_2, 0);
        declare
            Code : Unsigned_64 := 16#FFF0#;   -- not modelled: the monitor answers #UD
        begin
            case Reason is
                when Reason_External_Interrupt => Code := Exit_INTR;
                when Reason_Exception_NMI =>
                    declare
                        Info : constant Unsigned_64 := Read (Exit_Interruption_Info);
                        Kind : constant Unsigned_64 := Shift_Right (Info, 8) and 7;
                    begin
                        if Kind = Type_NMI then
                            Code := Exit_NMI;
                            -- The host's NMI, taken now (NMI exiting).
                            Asm ("int $2", Volatile => True);
                        else
                            Code := Exit_Exception_Base + (Info and 16#FF#);
                            Put64 (State, Exit_Info_1, Read (Exit_Interruption_Error));
                        end if;
                    end;
                when Reason_Triple_Fault => Code := Exit_Shutdown;
                when Reason_INIT => Code := Exit_INIT;
                when Reason_Interrupt_Window => Code := Exit_VINTR;
                when Reason_CPUID => Code := Exit_CPUID;
                when Reason_HLT => Code := Exit_HLT;
                when Reason_INVD => Code := Exit_INVD;
                when Reason_WBINVD => Code := Exit_WBINVD;
                when Reason_VMCALL => Code := Exit_VMMCALL;
                when Reason_First_VMX_Instruction .. Reason_Last_VMX_Instruction
                   | Reason_INVEPT | Reason_INVVPID => Code := Exit_VMRUN;
                when Reason_DR_Access => Code := Exit_Read_DR0;
                when Reason_IO =>
                    Code := Exit_IOIO;
                    declare
                        Size : constant Unsigned_64 := (Qualification and 7) + 1;
                    begin
                        Put64 (State, Exit_Info_1,
                               (if (Qualification and 8) /= 0 then 1 else 0)
                               or (if (Qualification and 16#10#) /= 0 then 4 else 0)
                               or (if (Qualification and 16#20#) /= 0 then 8 else 0)
                               or (if Size = 1 then 16#10# elsif Size = 2 then 16#20# else 16#40#)
                               or Shift_Left (Shift_Right (Qualification, 16) and 16#FFFF#, IOIO_Port_Shift));
                        Put64 (State, Exit_Info_2, RIP + Read (Exit_Instruction_Length));
                    end;
                when Reason_RDMSR => Code := Exit_MSR;
                when Reason_WRMSR =>
                    Code := Exit_MSR;
                    Put64 (State, Exit_Info_1, 1);
                when Reason_EPT_Violation =>
                    Code := Exit_NPF;
                    Put64 (State, Exit_Info_1,
                           (if (Qualification and 16#38#) /= 0 then 1 else 0)
                           or (if (Qualification and 2) /= 0 then 2 else 0)
                           or (if (Qualification and 4) /= 0 then 16#10# else 0));
                    Put64 (State, Exit_Info_2, Read (Guest_Physical_Address));
                when Reason_XSETBV => Code := Exit_XSETBV;
                when Reason_Invalid_Guest_State | Reason_MSR_Loading | Reason_Machine_Check
                   | Reason_EPT_Misconfiguration =>
                    Code := Exit_Invalid;
                    if Logged_Failures < 4 then
                        Logged_Failures := Logged_Failures + 1;
                        print ("vm: VM entry or EPT failure, VMX exit reason ");
                        printdln (Reason);
                    end if;
                when others => null;
            end case;
            Put64 (State, Exit_Code, Code);
        end;
        VMCLEAR (VMCS_Phys);
    end Enter;
end VMX_Run;
