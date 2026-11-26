pragma Ada_2022;
with System.Machine_Code; use System.Machine_Code;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Messages;
with CuBit.Monotonic;
with CuBit.Owned_Reservations;
with Console_Line;
with Guest_UART;
with VM_ABI;

package body Vmm_Machine is
   package V renames CuBit.Handles.VMs;
   package A renames VM_ABI;
   use type A.Outcome;
   use type A.Run_Status;
   use type System.Address;

   ---------------------------------------------------------------------------
   --  Guest physical layout (docs/hypervisor.md 6.2, the 64-bit boot
   --  protocol: Documentation/arch/x86/boot.rst and zero-page.rst)
   ---------------------------------------------------------------------------
   GDT_At         : constant := 16#0000_1000#;
   Zero_Page_At   : constant := 16#0000_7000#;
   PML4_At        : constant := 16#0000_9000#;
   PDPT_At        : constant := 16#0000_A000#;
   PD_At          : constant := 16#0000_B000#;
   Command_At     : constant := 16#0002_0000#;
   Command_Limit  : constant := 4096;
   Low_RAM_End    : constant := 16#0009_FC00#;   --  below the EBDA
   MP_Floating_At : constant := 16#000F_0000#;
   MP_Config_At   : constant := 16#000F_0100#;
   High_RAM_At    : constant := 16#0010_0000#;
   Kernel_At      : constant := 16#0100_0000#;   --  the bzImage's pref_address
   Kernel_Entry_Offset : constant := 16#200#;   --  startup_64
   Page_Bytes     : constant := 4096;
   Two_MiB        : constant := 16#20_0000#;

   LAPIC_Base  : constant := 16#FEE0_0000#;
   IOAPIC_Base : constant := 16#FEC0_0000#;
   MMIO_Page   : constant := 16#1000#;
   IOAPIC_ID   : constant := Max_VCPUs;  --  above every vCPU's LAPIC ID
   IOAPIC_Pins : constant := 24;
   UART_IRQ    : constant := 4;          --  COM1, ISA IRQ 4

   --  Boot-params (zero page) offsets.
   BP_E820_Entries  : constant := 16#1E8#;
   BP_Setup_Header  : constant := 16#1F1#;
   BP_Type_Of_Loader : constant := 16#210#;
   BP_Load_Flags    : constant := 16#211#;
   BP_Ramdisk_Image : constant := 16#218#;
   BP_Ramdisk_Size  : constant := 16#21C#;
   BP_Heap_End      : constant := 16#224#;
   BP_Command_Line  : constant := 16#228#;
   BP_E820_Table    : constant := 16#2D0#;
   Header_Magic_At  : constant := 16#202#;   --  "HdrS"
   Header_Jump_At   : constant := 16#201#;   --  the jump's offset: header end - 0x202
   Setup_Sects_At   : constant := 16#1F1#;
   Loader_Undefined : constant := 16#FF#;
   Loaded_High      : constant := 16#01#;
   Can_Use_Heap     : constant := 16#80#;
   E820_RAM         : constant := 1;
   E820_Reserved    : constant := 2;

   ---------------------------------------------------------------------------
   --  Guest RAM (this process's own memory)
   ---------------------------------------------------------------------------
   RAM_Base  : Unsigned_64 := 0;
   RAM_Bytes : Unsigned_64 := 0;

   function In_RAM (GPA, Bytes : Unsigned_64) return Boolean is
     (Bytes <= RAM_Bytes and then GPA <= RAM_Bytes - Bytes);

   function Host (GPA : Unsigned_64) return System.Address is
     (To_Address (Integer_Address (RAM_Base + GPA)));

   function Read8 (GPA : Unsigned_64) return Unsigned_8 is
      B : Unsigned_8 with Import, Volatile, Address => Host (GPA);
   begin
      return B;
   end Read8;

   function Read64 (GPA : Unsigned_64) return Unsigned_64 is
      W : Unsigned_64 with Import, Volatile, Address => Host (GPA);
   begin
      return W;
   end Read64;

   procedure Write8 (GPA : Unsigned_64; Value : Unsigned_8) is
      B : Unsigned_8 with Import, Volatile, Address => Host (GPA);
   begin
      B := Value;
   end Write8;

   procedure Write16 (GPA : Unsigned_64; Value : Unsigned_16) is
      W : Unsigned_16 with Import, Volatile, Address => Host (GPA);
   begin
      W := Value;
   end Write16;

   procedure Write32 (GPA : Unsigned_64; Value : Unsigned_32) is
      W : Unsigned_32 with Import, Volatile, Address => Host (GPA);
   begin
      W := Value;
   end Write32;

   procedure Write64 (GPA : Unsigned_64; Value : Unsigned_64) is
      W : Unsigned_64 with Import, Volatile, Address => Host (GPA);
   begin
      W := Value;
   end Write64;

   type Byte_Block is array (Unsigned_64 range <>) of Unsigned_8;

   procedure Copy_In (GPA : Unsigned_64; Source : System.Address; Bytes : Unsigned_64) is
      From : Byte_Block (1 .. Bytes) with Import, Address => Source;
      To   : Byte_Block (1 .. Bytes) with Import, Address => Host (GPA);
   begin
      To := From;
   end Copy_In;

   function Image_Byte (Base : System.Address; Offset : Unsigned_64) return Unsigned_8 is
      B : Unsigned_8 with Import, Address => Base + Storage_Offset (Offset);
   begin
      return B;
   end Image_Byte;

   function Image_U32 (Base : System.Address; Offset : Unsigned_64) return Unsigned_32 is
     (Unsigned_32 (Image_Byte (Base, Offset))
      or Shift_Left (Unsigned_32 (Image_Byte (Base, Offset + 1)), 8)
      or Shift_Left (Unsigned_32 (Image_Byte (Base, Offset + 2)), 16)
      or Shift_Left (Unsigned_32 (Image_Byte (Base, Offset + 3)), 24));

   ---------------------------------------------------------------------------
   --  Timing
   ---------------------------------------------------------------------------
   Stages : array (Stage) of Unsigned_64 := [others => 0];
   Started_Us : Unsigned_64 := 0;
   Returned : Unsigned_64 := 0;

   function Stage_Us (S : Stage) return Unsigned_64 is (Stages (S));
   function Exits_Returned return Unsigned_64 is (Returned);

   procedure Mark (S : Stage) is
   begin
      if Stages (S) = 0 then
         Stages (S) := CuBit.Monotonic.Microseconds - Started_Us;
      end if;
   end Mark;

   function Rdtsc return Unsigned_64 is
      Lo, Hi : Unsigned_32;
   begin
      Asm ("rdtsc", Outputs => (Unsigned_32'Asm_Output ("=a", Lo),
                                Unsigned_32'Asm_Output ("=d", Hi)), Volatile => True);
      return Shift_Left (Unsigned_64 (Hi), 32) or Unsigned_64 (Lo);
   end Rdtsc;

   --  The host TSC's rate, measured against the monotonic clock (the
   --  guest's TSC runs at the host's: no scaling in this slice).
   function Measure_TSC_kHz return Unsigned_64 is
      Window_Us : constant := 20_000;
      T0 : constant Unsigned_64 := CuBit.Monotonic.Microseconds;
      C0 : constant Unsigned_64 := Rdtsc;
      T1 : Unsigned_64;
   begin
      loop
         T1 := CuBit.Monotonic.Microseconds;
         exit when T1 - T0 >= Window_Us;
      end loop;
      return (Rdtsc - C0) * 1000 / (T1 - T0);
   end Measure_TSC_kHz;

   ---------------------------------------------------------------------------
   --  CPUID (the monitor runs it itself, then filters: 6.2)
   ---------------------------------------------------------------------------
   procedure Host_CPUID (Leaf, Sub : Unsigned_32; EAX, EBX, ECX, EDX : out Unsigned_32) is
   begin
      Asm ("cpuid",
           Outputs => (Unsigned_32'Asm_Output ("=a", EAX), Unsigned_32'Asm_Output ("=b", EBX),
                       Unsigned_32'Asm_Output ("=c", ECX), Unsigned_32'Asm_Output ("=d", EDX)),
           Inputs => (Unsigned_32'Asm_Input ("a", Leaf), Unsigned_32'Asm_Input ("c", Sub)),
           Volatile => True);
   end Host_CPUID;

   function Bit (N : Natural) return Unsigned_32 is (Shift_Left (1, N));

   --  "CuBitCuBitCu": the hypervisor leaf names CuBit, never another
   --  hypervisor (no paravirtual interface is offered in this slice).
   Signature_EBX : constant Unsigned_32 := 16#69427543#;  --  "CuBi"
   Signature_ECX : constant Unsigned_32 := 16#42754374#;  --  "tCuB"
   Signature_EDX : constant Unsigned_32 := 16#75437469#;  --  "itCu"
   Hypervisor_Leaf : constant := 16#4000_0000#;

   --  XCR0 stays the host's in this slice (the kernel runs the guest with
   --  it), so the guest is offered exactly those state components: leaf
   --  0xD lists them, and the features whose state lies elsewhere are off.
   XCR0_AVX512 : constant := 16#E0#;    --  opmask, ZMM_Hi256, Hi16_ZMM
   XCR0_PKRU   : constant := 16#200#;
   XCR0_AMX    : constant := 16#6_0000#;

   --  Speculation controls. The kernel switches IA32_SPEC_CTRL per vCPU and
   --  passes PRED_CMD writes through (kernel Guest_Speculation,
   --  docs/hypervisor.md 4.3), so the guest is offered them as the host
   --  has them; the kernel refuses (#GP) any SPEC_CTRL bit the host lacks.
   --  Still hidden: the MSRs nothing models.
   Leaf_6_ARAT              : constant := 2;    --  CPUID.6:EAX, APIC timer always running
   Leaf_7_CET_IBT           : constant := 20;   --  CPUID.(7,0):EDX
   Leaf_7_L1D_Flush         : constant := 28;   --  IA32_FLUSH_CMD: not passed through
   Leaf_7_Arch_Capabilities : constant := 29;   --  IA32_ARCH_CAPABILITIES: not answered
   --  CPUID.(7,2):EDX bits 0 .. 5: PSFD, IPRED_CTRL, RRSBA_CTRL, DDPD_U,
   --  BHI_CTRL (SPEC_CTRL bits) and MCDT_NO (a property of the CPU).
   Leaf_7_2_Speculation : constant Unsigned_32 := 16#3F#;
   Extended_IBRS_Always_On : constant := 16;   --  CPUID 8000_0008h EBX, as KVM
   Extended_IBRS_Preferred : constant := 18;   --  hides them
   Extended_VIRT_SSBD      : constant := 25;   --  VIRT_SPEC_CTRL: not modelled
   Extended_Max_Leaf        : constant := 16#8000_0000#;   --  EAX: the last extended leaf
   Extended_Features_2_Leaf : constant := 16#8000_0021#;
   Extended_2_AutoIBRS      : constant := 8;   --  EFER.AIBRSE, the guest's own on SVM

   --  EFER bits a guest may write (SCE, LME, LMA, NXE, FFXSR), and
   --  EFER.AIBRSE when the guest is offered AutoIBRS.
   EFER_Writable : constant Unsigned_64 := 16#4D01#;
   EFER_AutoIBRS : constant Unsigned_64 := 2 ** 21;

   function Host_Has_AutoIBRS return Boolean is
      EAX, EBX, ECX, EDX : Unsigned_32;
   begin
      Host_CPUID (Extended_Max_Leaf, 0, EAX, EBX, ECX, EDX);
      if EAX < Extended_Features_2_Leaf then
         return False;
      end if;
      Host_CPUID (Extended_Features_2_Leaf, 0, EAX, EBX, ECX, EDX);
      return (EAX and Bit (Extended_2_AutoIBRS)) /= 0;
   end Host_Has_AutoIBRS;

   procedure Guest_CPUID (Leaf, Sub : Unsigned_32; Guest_CR4 : Unsigned_64; APIC_ID : Unsigned_32;
                          XCR0 : Unsigned_64; EAX, EBX, ECX, EDX : out Unsigned_32) is
      Has_AVX512 : constant Boolean := (XCR0 and XCR0_AVX512) = XCR0_AVX512;
      Has_PKRU : constant Boolean := (XCR0 and XCR0_PKRU) /= 0;
      Has_AMX : constant Boolean := (XCR0 and XCR0_AMX) = XCR0_AMX;
   begin
      if Leaf in Hypervisor_Leaf .. Hypervisor_Leaf + 16#FF# then
         if Leaf = Hypervisor_Leaf then
            EAX := Hypervisor_Leaf; EBX := Signature_EBX; ECX := Signature_ECX; EDX := Signature_EDX;
         else
            EAX := 0; EBX := 0; ECX := 0; EDX := 0;
         end if;
         return;
      end if;
      Host_CPUID (Leaf, Sub, EAX, EBX, ECX, EDX);
      case Leaf is
         when 1 =>
            --  This vCPU's APIC ID, one logical processor in its package
            --  (each vCPU is a package of its own: no HTT, no core leaves).
            EBX := (EBX and 16#0000_FFFF#) or Shift_Left (1, 16) or Shift_Left (APIC_ID, 24);
            --  No VMX/SMX/EST/TM2/xTPR/PDCM/DCA/x2APIC/MONITOR; TSC-deadline
            --  (the monitor's LAPIC timer); OSXSAVE as the guest's CR4 says;
            --  the hypervisor bit.
            ECX := ECX and not (Bit (3) or Bit (5) or Bit (6) or Bit (7) or Bit (8)
                                or Bit (14) or Bit (15) or Bit (18) or Bit (21) or Bit (27));
            ECX := ECX or Bit (24) or Bit (31);
            if (Guest_CR4 and 16#4_0000#) /= 0 then
               ECX := ECX or Bit (27);
            end if;
            --  No MCE/MCA (the monitor models no machine-check banks), ACPI
            --  thermal, TM or PBE.
            EDX := EDX and not (Bit (7) or Bit (14) or Bit (22) or Bit (28) or Bit (29) or Bit (31));
         when 4 =>
            EAX := EAX and 16#0000_3FFF#;   --  one core and one thread per cache
         when 6 =>
            --  ARAT only (as KVM): the monitor's LAPIC timer is software and
            --  never stops in a C-state. Without it Linux on an Intel host
            --  (AMD sets ARAT itself) marks the timer C3STOP and wants a
            --  broadcast timer this machine does not have. No thermal or
            --  power management.
            EAX := Bit (Leaf_6_ARAT); EBX := 0; ECX := 0; EDX := 0;
         when 16#A# | 16#B# | 16#1F# =>
            EAX := 0; EBX := 0; ECX := 0; EDX := 0;   --  no PMU or topology
         when 7 =>
            if Sub = 0 then
               EBX := EBX and not (Bit (1) or Bit (4) or Bit (11));   --  no TSC_ADJUST, HLE/RTM
               --  No WAITPKG, LA57, CET shadow stacks.
               ECX := ECX and not (Bit (5) or Bit (7) or Bit (16));
               --  SPEC_CTRL (IBRS, IBPB), STIBP and SSBD as the host has them;
               --  no L1D_FLUSH or ARCH_CAPABILITIES (MSRs nothing models: the
               --  guest is told it lacks them rather than given MSRs that do
               --  nothing), no CET IBT.
               EDX := EDX and not (Bit (Leaf_7_CET_IBT) or Bit (Leaf_7_L1D_Flush)
                                   or Bit (Leaf_7_Arch_Capabilities));
               if not Has_AVX512 then
                  EBX := EBX and not (Bit (16) or Bit (17) or Bit (21) or Bit (26) or Bit (27)
                                      or Bit (28) or Bit (30) or Bit (31));
                  ECX := ECX and not (Bit (1) or Bit (6) or Bit (11) or Bit (12) or Bit (14));
                  EDX := EDX and not (Bit (2) or Bit (3) or Bit (8) or Bit (23));
               end if;
               if not Has_PKRU then
                  ECX := ECX and not (Bit (3) or Bit (4));          --  PKU, OSPKE
               end if;
               if not Has_AMX then
                  EDX := EDX and not (Bit (22) or Bit (24) or Bit (25));
               end if;
            elsif Sub = 1 then
               if not Has_AVX512 then
                  EAX := EAX and not Bit (5);                       --  AVX512_BF16
               end if;
            elsif Sub = 2 then
               EDX := EDX and Leaf_7_2_Speculation;   --  PSFD, IPRED/RRSBA/BHI controls
            end if;
         when 16#D# =>
            if Sub = 0 then
               EAX := EAX and Unsigned_32 (XCR0 and 16#FFFF_FFFF#);
               EDX := EDX and Unsigned_32 (Shift_Right (XCR0, 32));
               ECX := EBX;                     --  the size for XCR0 (hardware's, it runs with it)
            elsif Sub = 1 then
               EAX := EAX and not Bit (3);     --  no XSAVES (no IA32_XSS)
               ECX := 0; EDX := 0;
            elsif Sub < 64 and then (Shift_Right (XCR0, Natural (Sub)) and 1) = 0 then
               EAX := 0; EBX := 0; ECX := 0; EDX := 0;
            end if;
         when 16#8000_0001# =>
            --  No CmpLegacy, SVM, SKINIT, WDT or TOPOEXT (no core topology).
            ECX := ECX and not (Bit (1) or Bit (2) or Bit (12) or Bit (13) or Bit (22));
            EDX := EDX and not (Bit (7) or Bit (14));               --  no MCE/MCA
         when 16#8000_0007# =>
            EAX := 0; EBX := 0; ECX := 0; EDX := EDX and Bit (8);  --  invariant TSC only
         when 16#8000_0008# =>
            ECX := 0;                                 --  one core per package
            --  IBPB, IBRS, STIBP, STIBP always on and SSBD as the host has
            --  them; not IBRS always on or preferred (as KVM), nor VIRT_SSBD.
            EBX := EBX and not (Bit (Extended_IBRS_Always_On) or Bit (Extended_IBRS_Preferred)
                                or Bit (Extended_VIRT_SSBD));
         when Extended_Features_2_Leaf =>
            null;   --  AutoIBRS (EFER below), SBPB and IBPB_BRTYPE (PRED_CMD) as the host has them
         when 16#8000_000A# | 16#8000_001E# | 16#8000_001F# | 16#8000_0026# =>
            EAX := 0; EBX := 0; ECX := 0; EDX := 0;   --  no SVM, topology, SEV
         when others =>
            null;
      end case;
   end Guest_CPUID;

   ---------------------------------------------------------------------------
   --  Devices
   ---------------------------------------------------------------------------
   --  UART 16550A at COM1 (guest_uart.ads).
   COM1 : constant := 16#3F8#;
   COM1_Last : constant := COM1 + Unsigned_64 (Guest_UART.Register'Last);
   COM1_UART : Guest_UART.UART;
   --  The line the guest is writing (console_line.ads), and a count of its
   --  changes for a window to repaint by.
   Line : Console_Line.Line;
   Generation : Unsigned_64 := 0;
   --  The time the first input byte not yet answered by guest output went
   --  into the FIFO (0: none), and the input-to-output times so far.
   Input_Waiting_Since : Unsigned_64 := 0;
   Echoes : Unsigned_64 := 0;
   Last_Echo : Unsigned_64 := 0;
   Console_Out : Console_Writer := null;
   Log_Out : Log_Writer := null;

   procedure Log (Text : String) is
   begin
      if Log_Out /= null then
         Log_Out (Text);
      end if;
   end Log;

   Shell_Ready : constant String := "CuBit guest: shell ready";

   procedure Flush_Line is
      Text : constant String := Line.Text (1 .. Line.Length);
   begin
      if Text'Length > 0 then
         if Console_Out /= null then
            Console_Out (Text);
         end if;
         if Text'Length >= Shell_Ready'Length
           and then (for some I in Text'First .. Text'Last - Shell_Ready'Length + 1 =>
                       Text (I .. I + Shell_Ready'Length - 1) = Shell_Ready)
         then
            Mark (Prompt);
         end if;
      end if;
      Console_Line.Clear (Line);
   end Flush_Line;

   function Partial_Line return String is (Line.Text (1 .. Line.Length));
   function Cursor_Column return Natural is (Line.Cursor);
   function Console_Generation return Unsigned_64 is (Generation);
   function Echo_Count return Unsigned_64 is (Echoes);
   function Last_Echo_Us return Unsigned_64 is (Last_Echo);

   procedure Guest_Byte (B : Unsigned_8) is
      Done : Boolean;
   begin
      Mark (First_Output);
      if Input_Waiting_Since /= 0 then
         Last_Echo := CuBit.Monotonic.Microseconds - Input_Waiting_Since;
         Echoes := Echoes + 1;
         Input_Waiting_Since := 0;
      end if;
      Console_Line.Put (Line, B, Done);
      if Done then
         Flush_Line;
      end if;
      Generation := Generation + 1;
   end Guest_Byte;

   --  PIC: only its presence (an IMR that reads back); it never delivers.
   PIC_IMR_Master, PIC_IMR_Slave : Unsigned_8 := 16#FF#;
   CMOS_Index : Unsigned_8 := 0;

   --  IOAPIC.
   IOAPIC_Select : Unsigned_32 := 0;
   type Redirections is array (0 .. IOAPIC_Pins - 1) of Unsigned_64;
   Redirection : Redirections := [others => 16#1_0000#];   --  masked
   Mask_Bit : constant Unsigned_64 := 16#1_0000#;

   --  LAPIC (xAPIC registers by offset / 16), one per vCPU.
   type APIC_Registers is array (0 .. 63) of Unsigned_32;
   type Vector_Set is array (0 .. 255) of Boolean;
   type Debug_Shadow is array (0 .. 7) of Unsigned_64;
   --  Absent: no such vCPU; Waiting_For_SIPI: an AP after reset or INIT;
   --  Running: the monitor runs it.
   type Core_Mode is (Absent, Waiting_For_SIPI, Running);
   type Core is record
      Handle : V.VCPU;
      Mode : Core_Mode := Absent;
      APIC : APIC_Registers := [others => 0];
      IRR, ISR : Vector_Set := [others => False];
      NMI_Pending : Boolean := False;
      Timer_Deadline : Unsigned_64 := 0;     --  guest TSC, 0 = disarmed
      DR_Shadow : Debug_Shadow := [6 => 16#FFFF_0FF0#, 7 => 16#400#, others => 0];
      --  The resume of this vCPU's last exit, kept while another runs.
      Flags, RAX, RDX : Unsigned_64 := 0;
      Started : Boolean := False;            --  an AP the guest started
   end record;
   subtype Core_Index is Natural range 0 .. Max_VCPUs - 1;
   type Core_Table is array (Core_Index) of Core;
   Cores : Core_Table;
   Core_Count : Natural range 1 .. Max_VCPUs := 1;
   Current : Core_Index := 0;
   --  An IPI's target, which the run loop switches to next (or -1).
   Yield_To : Integer range -1 .. Max_VCPUs - 1 := -1;
   Machine : V.VM;
   Digest_At_INIT : Unsigned_64 := 0;
   Have_Digest : Boolean := False;
   Started_APs : Natural := 0;
   Tables_Held : Boolean := True;
   function APs_Started return Natural is (Started_APs);
   function AP_Tables_Unchanged return Boolean is (Tables_Held);
   Guest_TSC_Offset : Unsigned_64 := 0;
   TSC_kHz : Unsigned_64 := 1;
   APIC_ID_Reg : constant := 16#020# / 16;  APIC_Version_Reg : constant := 16#030# / 16;
   APIC_TPR_Reg : constant := 16#080# / 16; APIC_PPR_Reg : constant := 16#0A0# / 16;
   APIC_EOI_Reg : constant := 16#0B0# / 16; APIC_LDR_Reg : constant := 16#0D0# / 16;
   APIC_DFR_Reg : constant := 16#0E0# / 16; APIC_SVR_Reg : constant := 16#0F0# / 16;
   APIC_ISR_Reg : constant := 16#100# / 16; APIC_TMR_Reg : constant := 16#180# / 16;
   APIC_IRR_Reg : constant := 16#200# / 16; APIC_ESR_Reg : constant := 16#280# / 16;
   APIC_ICR_Low_Reg : constant := 16#300# / 16; APIC_ICR_High_Reg : constant := 16#310# / 16;
   APIC_LVT_Timer_Reg : constant := 16#320# / 16; APIC_LVT_Error_Reg : constant := 16#370# / 16;
   APIC_Timer_Initial_Reg : constant := 16#380# / 16;
   APIC_Timer_Current_Reg : constant := 16#390# / 16;
   LVT_Mask : constant Unsigned_32 := 16#1_0000#;

   function Guest_TSC return Unsigned_64 is (Rdtsc + Guest_TSC_Offset);

   procedure Raise_Vector (To : Core_Index; Vector : Unsigned_64) is
   begin
      if Vector in 16 .. 255 then
         Cores (To).IRR (Natural (Vector)) := True;
      end if;
   end Raise_Vector;

   --  Whether vCPU T is a destination of Dest: physical (its APIC ID, or
   --  the broadcast 0xFF) or logical (flat or cluster model, from its LDR
   --  and DFR).
   function Accepts (T : Core_Index; Dest : Unsigned_32; Logical : Boolean) return Boolean is
      LDR : constant Unsigned_32 := Shift_Right (Cores (T).APIC (APIC_LDR_Reg), 24);
   begin
      if Cores (T).Mode = Absent then
         return False;
      elsif not Logical then
         return Dest = 16#FF# or else Dest = Unsigned_32 (T);
      elsif Shift_Right (Cores (T).APIC (APIC_DFR_Reg), 28) = 16#F# then
         return (Dest and LDR) /= 0;
      end if;
      return Shift_Right (Dest, 4) = Shift_Right (LDR, 4) and then (Dest and LDR and 16#F#) /= 0;
   end Accepts;

   --  Raise IOAPIC pin Pin (an edge); Target is the vCPU it went to, or -1
   --  while the pin is masked.
   procedure Raise_Pin (Pin : Natural; Target : out Integer) is
      Dest : Unsigned_32;
      Logical : Boolean;
   begin
      Target := -1;
      if Pin < IOAPIC_Pins and then (Redirection (Pin) and Mask_Bit) = 0 then
         Dest := Unsigned_32 (Shift_Right (Redirection (Pin), 56));
         Logical := (Redirection (Pin) and 16#800#) /= 0;
         --  Fixed or lowest priority: the first vCPU the destination names.
         Target := 0;
         for T in 0 .. Core_Count - 1 loop
            if Cores (T).Mode = Running and then Accepts (T, Dest, Logical) then
               Target := T;
               exit;
            end if;
         end loop;
         Raise_Vector (Target, Redirection (Pin) and 16#FF#);
      end if;
   end Raise_Pin;

   procedure Raise_Pin (Pin : Natural) is
      Target : Integer;
   begin
      Raise_Pin (Pin, Target);
   end Raise_Pin;

   function Input_Room return Natural is (Guest_UART.Room (COM1_UART));

   procedure Send_Input (B : Unsigned_8) is
      Raise_IRQ : Boolean;
      Target : Integer;
   begin
      Guest_UART.Receive (COM1_UART, B, Raise_IRQ);
      if Input_Waiting_Since = 0 then
         Input_Waiting_Since := CuBit.Monotonic.Microseconds;
      end if;
      if Raise_IRQ then
         Raise_Pin (UART_IRQ, Target);
         --  The vCPU that takes the interrupt runs next.
         if Target >= 0 and then Target /= Current then
            Yield_To := Target;
         end if;
      end if;
   end Send_Input;

   function Highest (S : Vector_Set) return Integer is
   begin
      for I in reverse S'Range loop
         if S (I) then
            return I;
         end if;
      end loop;
      return -1;
   end Highest;

   function Port_Read (Port : Unsigned_64; Size : Natural) return Unsigned_64 is
   begin
      case Port is
         when COM1 .. COM1_Last =>
            declare
               Value : Unsigned_8;
            begin
               Guest_UART.Read (COM1_UART, Guest_UART.Register (Port - COM1), Value);
               return Unsigned_64 (Value);
            end;
         when 16#21# => return Unsigned_64 (PIC_IMR_Master);
         when 16#A1# => return Unsigned_64 (PIC_IMR_Slave);
         when 16#20# | 16#A0# => return 0;
         when 16#71# =>
            --  CMOS: register A with no update in progress, everything else 0.
            return 0;
         when 16#61# => return 0;
         when 16#CFC# .. 16#CFF# | 16#CF8# =>
            return (if Size = 4 then 16#FFFF_FFFF# elsif Size = 2 then 16#FFFF# else 16#FF#);
         when others =>
            return (if Size = 4 then 16#FFFF_FFFF# elsif Size = 2 then 16#FFFF# else 16#FF#);
      end case;
   end Port_Read;

   procedure Port_Write (Port : Unsigned_64; Value : Unsigned_64) is
      B : constant Unsigned_8 := Unsigned_8 (Value and 16#FF#);
   begin
      case Port is
         when COM1 .. COM1_Last =>
            declare
               Effect : Guest_UART.Write_Effect;
            begin
               Guest_UART.Write (COM1_UART, Guest_UART.Register (Port - COM1), B, Effect);
               if Effect.Transmitted then
                  Guest_Byte (Effect.Byte);
               end if;
               if Effect.Discarded > 0 then
                  --  The guest reset its receive FIFO (FCR): never silent.
                  Log ("the guest's FIFO reset discarded" & Natural'Image (Effect.Discarded)
                       & " received byte" & (if Effect.Discarded > 1 then "s" else ""));
               end if;
               if Effect.Raise_IRQ then
                  Raise_Pin (UART_IRQ);
               end if;
            end;
         when 16#21# => PIC_IMR_Master := B;
         when 16#A1# => PIC_IMR_Slave := B;
         when 16#70# => CMOS_Index := B and 16#7F#;
         when others => null;
      end case;
   end Port_Write;

   function IOAPIC_Read return Unsigned_32 is
   begin
      case IOAPIC_Select is
         when 0 => return Shift_Left (Unsigned_32 (IOAPIC_ID), 24);
         when 1 => return 16#0017_0011#;   --  24 pins (max index 23), version 0x11
         when 2 => return Shift_Left (Unsigned_32 (IOAPIC_ID), 24);
         when 16#10# .. 16#10# + 2 * IOAPIC_Pins - 1 =>
            declare
               Pin : constant Natural := Natural (IOAPIC_Select - 16#10#) / 2;
            begin
               if IOAPIC_Select mod 2 = 0 then
                  return Unsigned_32 (Redirection (Pin) and 16#FFFF_FFFF#);
               end if;
               return Unsigned_32 (Shift_Right (Redirection (Pin), 32));
            end;
         when others => return 0;
      end case;
   end IOAPIC_Read;

   procedure IOAPIC_Write (Value : Unsigned_32) is
   begin
      if IOAPIC_Select in 16#10# .. 16#10# + 2 * IOAPIC_Pins - 1 then
         declare
            Pin : constant Natural := Natural (IOAPIC_Select - 16#10#) / 2;
         begin
            if IOAPIC_Select mod 2 = 0 then
               Redirection (Pin) := (Redirection (Pin) and 16#FFFF_FFFF_0000_0000#)
                 or Unsigned_64 (Value and not 16#5000#);   --  delivery status, remote IRR read-only
            else
               Redirection (Pin) := (Redirection (Pin) and 16#FFFF_FFFF#)
                 or Shift_Left (Unsigned_64 (Value), 32);
            end if;
         end;
      end if;
   end IOAPIC_Write;

   function APIC_Read (Offset : Unsigned_64) return Unsigned_32 is
      R : constant Natural := Natural (Offset / 16) mod 64;
      K : Core renames Cores (Current);
   begin
      case R is
         when APIC_ID_Reg => return Shift_Left (Unsigned_32 (Current), 24);
         when APIC_Version_Reg => return 16#0005_0014#;    --  6 LVTs, version 0x14
         when APIC_PPR_Reg =>
            return Unsigned_32'Max (K.APIC (APIC_TPR_Reg) and 16#F0#,
                                    Unsigned_32 (Integer'Max (Highest (K.ISR), 0)) and 16#F0#);
         when APIC_ISR_Reg .. APIC_ISR_Reg + 7 | APIC_IRR_Reg .. APIC_IRR_Reg + 7 =>
            declare
               Base : constant Natural := (if R >= APIC_IRR_Reg then R - APIC_IRR_Reg
                                           else R - APIC_ISR_Reg) * 32;
               W : Unsigned_32 := 0;
            begin
               for I in 0 .. 31 loop
                  if (if R >= APIC_IRR_Reg then K.IRR (Base + I) else K.ISR (Base + I)) then
                     W := W or Bit (I);
                  end if;
               end loop;
               return W;
            end;
         when APIC_TMR_Reg .. APIC_TMR_Reg + 7 => return 0;
         when APIC_Timer_Current_Reg => return 0;
         when others => return K.APIC (R);
      end case;
   end APIC_Read;

   --  A vCPU's architectural state after INIT (Intel SDM 10.4.7.3, AMD APM
   --  2 14.1.3): real mode at F000:FFF0, CR0 0x60000010, everything else
   --  reset. Its LAPIC keeps its ID and the registers reset.
   procedure Reset_Registers (T : Core_Index) is
      S : A.Segment_Registers;
      C : A.Control_Registers;
      G : A.General_Registers := [others => 0];
      R : V.Reply;
      Real_Data : constant Unsigned_16 := 16#0093#;   --  P S data-RW accessed
      Real_Code : constant Unsigned_16 := 16#009B#;   --  P S code-RX accessed
   begin
      S := [others => (Selector => 0, Attributes => Real_Data, Limit => 16#FFFF#, Base => 0)];
      S (A.CS_Index) := (Selector => 16#F000#, Attributes => Real_Code, Limit => 16#FFFF#,
                         Base => 16#FFFF_0000#);
      S (A.GDTR_Index) := (Selector => 0, Attributes => 0, Limit => 16#FFFF#, Base => 0);
      S (A.IDTR_Index) := (Selector => 0, Attributes => 0, Limit => 16#FFFF#, Base => 0);
      S (A.LDTR_Index) := (Selector => 0, Attributes => 16#0082#, Limit => 16#FFFF#, Base => 0);
      S (A.TR_Index) := (Selector => 0, Attributes => 16#008B#, Limit => 16#FFFF#, Base => 0);
      R := V.Set_State (Cores (T).Handle, A.Segments, S'Address, A.Segments_Bytes);
      R := V.Get_State (Cores (T).Handle, A.Control, C'Address, A.Control_Bytes);
      declare
         XCR0 : constant Unsigned_64 := C (A.XCR0_Index);
      begin
         C := [others => 0];
         C (A.CR0_Index) := 16#6000_0010#;
         C (A.DR6_Index) := 16#FFFF_0FF0#;
         C (A.DR7_Index) := 16#400#;
         C (A.XCR0_Index) := XCR0;
      end;
      R := V.Set_State (Cores (T).Handle, A.Control, C'Address, A.Control_Bytes);
      G (A.RIP_Index) := 16#FFF0#;
      G (A.RFLAGS_Index) := 2;
      R := V.Set_State (Cores (T).Handle, A.General, G'Address, A.General_Bytes);
   end Reset_Registers;

   procedure Reset_APIC (K : in out Core) is
   begin
      K.APIC := [others => 0];
      K.APIC (APIC_Version_Reg) := 16#0005_0014#;
      K.APIC (APIC_DFR_Reg) := 16#FFFF_FFFF#;
      K.APIC (APIC_SVR_Reg) := 16#FF#;
      for L in APIC_LVT_Timer_Reg .. APIC_LVT_Error_Reg loop
         K.APIC (L) := LVT_Mask;
      end loop;
      K.IRR := [others => False];
      K.ISR := [others => False];
      K.NMI_Pending := False;
      K.Timer_Deadline := 0;
   end Reset_APIC;

   --  An IPI from the current vCPU's ICR (fixed, lowest priority, NMI,
   --  INIT, SIPI; a shorthand or a physical or logical destination).
   procedure Send_IPI (Low, High : Unsigned_32) is
      Vector : constant Unsigned_64 := Unsigned_64 (Low and 16#FF#);
      Delivery : constant Unsigned_32 := Shift_Right (Low, 8) and 7;
      Logical : constant Boolean := (Low and 16#800#) /= 0;
      Deassert : constant Boolean := (Low and 16#C000#) = 16#8000#;   --  level, not asserted
      Shorthand : constant Unsigned_32 := Shift_Right (Low, 18) and 3;
      Dest : constant Unsigned_32 := Shift_Right (High, 24);
      Hit : Boolean;
      Delivery_Fixed : constant := 0; Delivery_Lowest : constant := 1;
      Delivery_NMI : constant := 4; Delivery_INIT : constant := 5; Delivery_SIPI : constant := 6;
   begin
      for T in 0 .. Core_Count - 1 loop
         Hit := (case Shorthand is
                    when 0 => Accepts (T, Dest, Logical),
                    when 1 => T = Current,
                    when 2 => True,
                    when others => T /= Current);
         if Hit and then Cores (T).Mode /= Absent then
            case Delivery is
               when Delivery_Fixed | Delivery_Lowest =>
                  if Cores (T).Mode = Running then
                     Raise_Vector (T, Vector);
                  end if;
               when Delivery_NMI =>
                  Cores (T).NMI_Pending := Cores (T).Mode = Running;
               when Delivery_INIT =>
                  if not Deassert and then T /= 0 then
                     if not Have_Digest then
                        Digest_At_INIT := V.Describe (Machine).W3;
                        Have_Digest := True;
                     end if;
                     Reset_Registers (T);
                     Reset_APIC (Cores (T));
                     Cores (T).Mode := Waiting_For_SIPI;
                  end if;
               when Delivery_SIPI =>
                  if Cores (T).Mode = Waiting_For_SIPI then
                     declare
                        S : A.Segment_Registers;
                        G : A.General_Registers := [others => 0];
                        R : V.Reply;
                     begin
                        R := V.Get_State (Cores (T).Handle, A.Segments, S'Address, A.Segments_Bytes);
                        S (A.CS_Index).Selector := Unsigned_16 (Vector) * 256;
                        S (A.CS_Index).Base := Vector * Page_Bytes;
                        R := V.Set_State (Cores (T).Handle, A.Segments, S'Address, A.Segments_Bytes);
                        G (A.RFLAGS_Index) := 2;
                        R := V.Set_State (Cores (T).Handle, A.General, G'Address, A.General_Bytes);
                     end;
                     Cores (T).Mode := Running;
                     Cores (T).Flags := 0;
                     if not Cores (T).Started then
                        Cores (T).Started := True;
                        Started_APs := Started_APs + 1;
                     end if;
                  end if;
               when others => null;   --  SMI, ExtINT: not modelled
            end case;
            if T /= Current then
               Yield_To := T;
            end if;
            exit when Delivery = Delivery_Lowest;
         end if;
      end loop;
   end Send_IPI;

   procedure APIC_Write (Offset : Unsigned_64; Value : Unsigned_32) is
      R : constant Natural := Natural (Offset / 16) mod 64;
      K : Core renames Cores (Current);
      H : Integer;
   begin
      case R is
         when APIC_EOI_Reg =>
            H := Highest (K.ISR);
            if H >= 0 then
               K.ISR (H) := False;
            end if;
         when APIC_ID_Reg | APIC_Version_Reg | APIC_PPR_Reg | APIC_ISR_Reg .. APIC_IRR_Reg + 7 =>
            null;   --  read-only
         when APIC_ESR_Reg => K.APIC (R) := 0;
         when APIC_ICR_Low_Reg =>
            K.APIC (R) := Value and not 16#1000#;   --  delivery status: always idle
            Send_IPI (Value, K.APIC (APIC_ICR_High_Reg));
         when others => K.APIC (R) := Value;
      end case;
   end APIC_Write;

   --  The timers (TSC-deadline mode only): fire those whose deadline passed.
   procedure Timer_Check is
   begin
      for T in 0 .. Core_Count - 1 loop
         declare
            K : Core renames Cores (T);
         begin
            if K.Mode = Running and then K.Timer_Deadline /= 0 and then Guest_TSC >= K.Timer_Deadline then
               K.Timer_Deadline := 0;
               if (K.APIC (APIC_LVT_Timer_Reg) and LVT_Mask) = 0 then
                  Raise_Vector (T, Unsigned_64 (K.APIC (APIC_LVT_Timer_Reg) and 16#FF#));
               end if;
            end if;
         end;
      end loop;
   end Timer_Check;

   ---------------------------------------------------------------------------
   --  MMIO: fetch and decode the faulting instruction (6.2, "MMIO decode")
   ---------------------------------------------------------------------------
   --  Guest virtual to guest physical through the guest's own tables
   --  (4-level long mode, 4 KiB / 2 MiB / 1 GiB leaves), or 2 ** 64 - 1.
   No_Address : constant Unsigned_64 := Unsigned_64'Last;
   function Translate (CR3, VA : Unsigned_64) return Unsigned_64 is
      Table : Unsigned_64 := CR3 and 16#000F_FFFF_FFFF_F000#;
      E : Unsigned_64;
   begin
      for Level in reverse 1 .. 4 loop
         declare
            Index : constant Unsigned_64 := Shift_Right (VA, 12 + 9 * (Level - 1)) and 511;
         begin
            if not In_RAM (Table + Index * 8, 8) then
               return No_Address;
            end if;
            E := Read64 (Table + Index * 8);
            if (E and 1) = 0 then
               return No_Address;
            end if;
            if Level in 2 .. 3 and then (E and 16#80#) /= 0 then
               declare
                  Span : constant Unsigned_64 := Shift_Left (1, 12 + 9 * (Level - 1));
               begin
                  return (E and 16#000F_FFFF_FFFF_F000# and not (Span - 1)) + (VA and (Span - 1));
               end;
            end if;
            Table := E and 16#000F_FFFF_FFFF_F000#;
         end;
      end loop;
      return Table + (VA and 16#FFF#);
   end Translate;

   type Instruction is record
      Valid    : Boolean := False;
      Length   : Unsigned_64 := 0;
      Is_Write : Boolean := False;   --  register to memory
      Register : Natural := 0;       --  GPR index (VM_ABI order)
      Size     : Natural := 4;       --  bytes
      Has_Immediate : Boolean := False;
      Immediate : Unsigned_64 := 0;
      Zero_Extend_Byte : Boolean := False;
   end record;

   --  ModRM.reg (with REX.R) to VM_ABI's general-register order.
   type Register_Map is array (0 .. 15) of Natural;
   GPR_Of : constant Register_Map :=
     [A.RAX_Index, A.RCX_Index, A.RDX_Index, A.RBX_Index, A.RSP_Index, A.RBP_Index,
      A.RSI_Index, A.RDI_Index, 8, 9, 10, 11, 12, 13, 14, 15];

   --  The MOV forms Linux uses for device registers: 89/8B (r/m32/64), 88/8A
   --  (8-bit), C7 /0 (imm32), 0F B6/B7 (MOVZX); with 66, REX and a ModRM
   --  memory operand of any form. Anything else is not decoded.
   function Decode (Code_Address : Unsigned_64) return Instruction is
      Result : Instruction;
      Pos : Unsigned_64 := Code_Address;
      Byte : Unsigned_8;
      REX_W, REX_R : Boolean := False;
      Operand_16 : Boolean := False;
      Opcode : Unsigned_8;
      Two_Byte : Boolean := False;
      ModRM, Mode, RM : Unsigned_8;
      function Fetch return Unsigned_8 is
         B : Unsigned_8;
      begin
         if not In_RAM (Pos, 1) then
            return 16#F4#;
         end if;
         B := Read8 (Pos);
         Pos := Pos + 1;
         return B;
      end Fetch;
   begin
      loop
         Byte := Fetch;
         exit when Pos - Code_Address > 15;
         if Byte = 16#66# then
            Operand_16 := True;
         elsif Byte in 16#40# .. 16#4F# then
            REX_W := (Byte and 8) /= 0;
            REX_R := (Byte and 4) /= 0;
         else
            exit;
         end if;
      end loop;
      Opcode := Byte;
      if Opcode = 16#0F# then
         Two_Byte := True;
         Opcode := Fetch;
      end if;
      ModRM := Fetch;
      Mode := Shift_Right (ModRM, 6);
      RM := ModRM and 7;
      Result.Register := GPR_Of (Natural (Shift_Right (ModRM, 3) and 7) + (if REX_R then 8 else 0));
      Result.Size := (if REX_W then 8 elsif Operand_16 then 2 else 4);
      if Mode = 3 then
         return (others => <>);
      end if;
      if RM = 4 then
         declare
            SIB : constant Unsigned_8 := Fetch;
         begin
            if Mode = 0 and then (SIB and 7) = 5 then
               Pos := Pos + 4;
            end if;
         end;
      elsif Mode = 0 and then RM = 5 then
         Pos := Pos + 4;                   --  RIP-relative disp32
      end if;
      if Mode = 1 then
         Pos := Pos + 1;
      elsif Mode = 2 then
         Pos := Pos + 4;
      end if;
      if not Two_Byte and then Opcode = 16#89# then
         Result.Is_Write := True;
      elsif not Two_Byte and then Opcode = 16#8B# then
         Result.Is_Write := False;
      elsif not Two_Byte and then Opcode = 16#88# then
         Result.Is_Write := True; Result.Size := 1;
      elsif not Two_Byte and then Opcode = 16#8A# then
         Result.Is_Write := False; Result.Size := 1;
      elsif not Two_Byte and then Opcode = 16#C7# then
         Result.Is_Write := True;
         Result.Has_Immediate := True;
         Result.Immediate := Unsigned_64 (Read8 (Pos)) or Shift_Left (Unsigned_64 (Read8 (Pos + 1)), 8)
           or Shift_Left (Unsigned_64 (Read8 (Pos + 2)), 16) or Shift_Left (Unsigned_64 (Read8 (Pos + 3)), 24);
         Pos := Pos + (if Result.Size = 2 then 2 else 4);
      elsif Two_Byte and then Opcode in 16#B6# | 16#B7# then
         Result.Is_Write := False;
         Result.Zero_Extend_Byte := True;
         Result.Size := (if Opcode = 16#B6# then 1 else 2);
      else
         return (others => <>);
      end if;
      Result.Valid := True;
      Result.Length := Pos - Code_Address;
      return Result;
   end Decode;

   ---------------------------------------------------------------------------
   --  Boot: zero page, e820, command line, tables, MP tables, GDT
   ---------------------------------------------------------------------------
   procedure Checksum (First, Bytes : Unsigned_64; At_Offset : Unsigned_64) is
      Sum : Unsigned_8 := 0;
   begin
      Write8 (First + At_Offset, 0);
      for I in 0 .. Bytes - 1 loop
         Sum := Sum + Read8 (First + I);
      end loop;
      Write8 (First + At_Offset, 0 - Sum);
   end Checksum;

   procedure Write_Text (GPA : Unsigned_64; Text : String) is
   begin
      for I in Text'Range loop
         Write8 (GPA + Unsigned_64 (I - Text'First), Character'Pos (Text (I)));
      end loop;
   end Write_Text;

   --  Intel MP Specification 1.4: the floating pointer and a configuration
   --  table with a processor per vCPU, the ISA bus, the IOAPIC and ISA IRQs 0 .. 15
   --  routed to IOAPIC pins 0 .. 15, LINT0 ExtINT and LINT1 NMI.
   procedure Write_MP_Tables is
      P : Unsigned_64 := MP_Config_At + 44;
      Count : Unsigned_16 := 0;
      procedure Entry_Done (Bytes : Unsigned_64) is
      begin
         P := P + Bytes;
         Count := Count + 1;
      end Entry_Done;
   begin
      for I in 0 .. 511 loop
         Write8 (MP_Floating_At + Unsigned_64 (I), 0);
      end loop;
      Write_Text (MP_Floating_At, "_MP_");
      Write32 (MP_Floating_At + 4, MP_Config_At);
      Write8 (MP_Floating_At + 8, 1);       --  length in 16-byte units
      Write8 (MP_Floating_At + 9, 4);       --  spec revision 1.4
      Checksum (MP_Floating_At, 16, 10);
      --  One processor per vCPU: enabled; vCPU 0 the bootstrap processor.
      for T in 0 .. Core_Count - 1 loop
         Write8 (P, 0); Write8 (P + 1, Unsigned_8 (T)); Write8 (P + 2, 16#14#);
         Write8 (P + 3, (if T = 0 then 3 else 1));
         Write32 (P + 4, 16#0000_0F00#); Write32 (P + 8, 16#0000_0201#);
         Write32 (P + 12, 0); Write32 (P + 16, 0);
         Entry_Done (20);
      end loop;
      --  Bus 0: ISA.
      Write8 (P, 1); Write8 (P + 1, 0); Write_Text (P + 2, "ISA   ");
      Entry_Done (8);
      --  The IOAPIC.
      Write8 (P, 2); Write8 (P + 1, IOAPIC_ID); Write8 (P + 2, 16#11#); Write8 (P + 3, 1);
      Write32 (P + 4, IOAPIC_Base);
      Entry_Done (8);
      for IRQ in 0 .. 15 loop
         Write8 (P, 3); Write8 (P + 1, 0); Write16 (P + 2, 0); Write8 (P + 4, 0);
         Write8 (P + 5, Unsigned_8 (IRQ)); Write8 (P + 6, IOAPIC_ID); Write8 (P + 7, Unsigned_8 (IRQ));
         Entry_Done (8);
      end loop;
      Write8 (P, 4); Write8 (P + 1, 3); Write16 (P + 2, 0); Write8 (P + 4, 0); Write8 (P + 5, 0);
      Write8 (P + 6, 16#FF#); Write8 (P + 7, 0);
      Entry_Done (8);
      Write8 (P, 4); Write8 (P + 1, 1); Write16 (P + 2, 0); Write8 (P + 4, 0); Write8 (P + 5, 0);
      Write8 (P + 6, 16#FF#); Write8 (P + 7, 1);
      Entry_Done (8);
      Write_Text (MP_Config_At, "PCMP");
      Write16 (MP_Config_At + 4, Unsigned_16 (P - MP_Config_At));
      Write8 (MP_Config_At + 6, 4);
      Write_Text (MP_Config_At + 8, "CUBIT   ");
      Write_Text (MP_Config_At + 16, "CUBIT-VMM   ");
      Write32 (MP_Config_At + 28, 0); Write16 (MP_Config_At + 32, 0);
      Write16 (MP_Config_At + 34, Count);
      Write32 (MP_Config_At + 36, LAPIC_Base);
      Write16 (MP_Config_At + 40, 0); Write8 (MP_Config_At + 42, 0); Write8 (MP_Config_At + 43, 0);
      Checksum (MP_Config_At, P - MP_Config_At, 7);
   end Write_MP_Tables;

   procedure Load_Linux (Guest : Image; Command_Line : String; OK : out Boolean) is
      Setup_Sects : Unsigned_64 := Unsigned_64 (Image_Byte (Guest.Kernel, Setup_Sects_At));
      Payload_At, Payload_Bytes, Header_End : Unsigned_64;
      Initrd_At : Unsigned_64;
      E820 : Unsigned_64 := Zero_Page_At + BP_E820_Table;
      procedure E820_Entry (First, Bytes : Unsigned_64; Kind : Unsigned_32) is
      begin
         Write64 (E820, First); Write64 (E820 + 8, Bytes); Write32 (E820 + 16, Kind);
         E820 := E820 + 20;
      end E820_Entry;
   begin
      OK := False;
      if Guest.Kernel_Bytes < 16#1000# or else Image_U32 (Guest.Kernel, Header_Magic_At) /= 16#5372_6448#
      then
         Log ("the kernel image is not a bzImage (no HdrS)");
         return;
      end if;
      if Setup_Sects = 0 then
         Setup_Sects := 4;
      end if;
      Payload_At := (Setup_Sects + 1) * 512;
      Payload_Bytes := Guest.Kernel_Bytes - Payload_At;
      Header_End := 16#202# + Unsigned_64 (Image_Byte (Guest.Kernel, Header_Jump_At));
      if not In_RAM (Kernel_At, Payload_Bytes) or else Guest.Initrd_Bytes > RAM_Bytes / 2
        or else Command_Line'Length >= Command_Limit
      then
         Log ("the image does not fit the guest's RAM");
         return;
      end if;
      Copy_In (Kernel_At, Guest.Kernel + Storage_Offset (Payload_At), Payload_Bytes);
      --  The initrd at the top of RAM, page-aligned.
      Initrd_At := (RAM_Bytes - Guest.Initrd_Bytes) / Page_Bytes * Page_Bytes;
      Copy_In (Initrd_At, Guest.Initrd, Guest.Initrd_Bytes);

      --  Zero page: the setup header from the image, then the loader's fields.
      for I in 0 .. Unsigned_64 (Page_Bytes) - 1 loop
         Write8 (Zero_Page_At + I, 0);
      end loop;
      for I in BP_Setup_Header .. Header_End - 1 loop
         Write8 (Zero_Page_At + I, Image_Byte (Guest.Kernel, I));
      end loop;
      Write8 (Zero_Page_At + BP_Type_Of_Loader, Loader_Undefined);
      Write8 (Zero_Page_At + BP_Load_Flags,
              Read8 (Zero_Page_At + BP_Load_Flags) or Loaded_High or Can_Use_Heap);
      Write16 (Zero_Page_At + BP_Heap_End, 16#FE00#);
      Write32 (Zero_Page_At + BP_Ramdisk_Image, Unsigned_32 (Initrd_At));
      Write32 (Zero_Page_At + BP_Ramdisk_Size, Unsigned_32 (Guest.Initrd_Bytes));
      Write32 (Zero_Page_At + BP_Command_Line, Command_At);
      Write_Text (Command_At, Command_Line);
      Write8 (Command_At + Command_Line'Length, 0);
      E820_Entry (0, Low_RAM_End, E820_RAM);
      E820_Entry (Low_RAM_End, High_RAM_At - Low_RAM_End, E820_Reserved);
      E820_Entry (High_RAM_At, RAM_Bytes - High_RAM_At, E820_RAM);
      Write8 (Zero_Page_At + BP_E820_Entries, 3);

      --  Identity map of the first GiB with 2 MiB leaves.
      for I in 0 .. 511 loop
         Write64 (PML4_At + Unsigned_64 (I) * 8, 0);
         Write64 (PDPT_At + Unsigned_64 (I) * 8, 0);
         Write64 (PD_At + Unsigned_64 (I) * 8, Unsigned_64 (I) * Two_MiB or 16#83#);
      end loop;
      Write64 (PML4_At, PDPT_At or 3);
      Write64 (PDPT_At, PD_At or 3);
      --  GDT: 0x10 the 64-bit code segment, 0x18 data (boot.rst, "64-bit
      --  boot protocol").
      Write64 (GDT_At, 0); Write64 (GDT_At + 8, 0);
      Write64 (GDT_At + 16#10#, 16#00AF_9A00_0000_FFFF#);
      Write64 (GDT_At + 16#18#, 16#00CF_9200_0000_FFFF#);
      Write_MP_Tables;
      OK := True;
   end Load_Linux;

   ---------------------------------------------------------------------------
   --  The vCPU's 64-bit entry state
   ---------------------------------------------------------------------------
   procedure Enter_Long_Mode (CPU : V.VCPU; OK : out Boolean) is
      G : A.General_Registers := [others => 0];
      S : A.Segment_Registers;
      C : A.Control_Registers := [others => 0];
      R : V.Reply;
      Code_64 : constant Unsigned_16 := 16#0A9B#;   --  P S code-RX accessed, L, G
      Data_32 : constant Unsigned_16 := 16#0C93#;   --  P S data-RW accessed, DB, G
   begin
      S := [others => (Selector => 16#18#, Attributes => Data_32, Limit => 16#FFFF_FFFF#, Base => 0)];
      S (A.CS_Index) := (Selector => 16#10#, Attributes => Code_64, Limit => 16#FFFF_FFFF#, Base => 0);
      S (A.GDTR_Index) := (Selector => 0, Attributes => 0, Limit => 16#1F#, Base => GDT_At);
      S (A.IDTR_Index) := (Selector => 0, Attributes => 0, Limit => 0, Base => 0);
      S (A.LDTR_Index) := (Selector => 0, Attributes => 16#0082#, Limit => 16#FFFF#, Base => 0);
      S (A.TR_Index) := (Selector => 0, Attributes => 16#008B#, Limit => 16#FFFF#, Base => 0);
      R := V.Set_State (CPU, A.Segments, S'Address, A.Segments_Bytes);
      OK := R.Outcome = A.Ok;
      C (A.CR0_Index) := 16#8000_0033#;          --  PG, NE, ET, MP, PE
      C (A.CR3_Index) := PML4_At;
      C (A.CR4_Index) := 16#20#;                 --  PAE
      C (A.EFER_Index) := 16#500#;               --  LME, LMA
      C (A.DR6_Index) := 16#FFFF_0FF0#;
      C (A.DR7_Index) := 16#400#;
      R := V.Set_State (CPU, A.Control, C'Address, A.Control_Bytes);
      OK := OK and then R.Outcome = A.Ok;
      G (A.RSI_Index) := Zero_Page_At;
      G (A.RIP_Index) := Kernel_At + Kernel_Entry_Offset;
      G (A.RFLAGS_Index) := 2;
      R := V.Set_State (CPU, A.General, G'Address, A.General_Bytes);
      OK := OK and then R.Outcome = A.Ok;
   end Enter_Long_Mode;

   ---------------------------------------------------------------------------
   --  The run loop
   ---------------------------------------------------------------------------
   function Image_Of (N : Unsigned_64) return String is
      S : constant String := Unsigned_64'Image (N);
   begin
      return S (S'First + 1 .. S'Last);
   end Image_Of;

   function Hex (N : Unsigned_64) return String is
      D : constant String := "0123456789abcdef";
      R : String (1 .. 16);
      W : Unsigned_64 := N;
   begin
      for I in reverse R'Range loop
         R (I) := D (Natural (W and 15) + 1);
         W := Shift_Right (W, 4);
      end loop;
      return R;
   end Hex;

   --  Every device and vCPU model back to its power-on state (a monitor
   --  may boot more than one guest).
   procedure Reset_Devices is
   begin
      COM1_UART := (others => <>);
      Line := (others => <>);
      Generation := Generation + 1;
      Input_Waiting_Since := 0;
      PIC_IMR_Master := 16#FF#; PIC_IMR_Slave := 16#FF#;
      CMOS_Index := 0;
      IOAPIC_Select := 0;
      Redirection := [others => Mask_Bit];
      Cores := [others => <>];
      Current := 0;
      Yield_To := -1;
      Have_Digest := False;
      Digest_At_INIT := 0;
      Started_APs := 0;
      Tables_Held := True;
   end Reset_Devices;

   procedure Boot_Linux
     (Authority    : CuBit.Handles.VM_Create;
      Guest        : Image;
      RAM_MiB      : Unsigned_64;
      Command_Line : String;
      Run_Ms       : Unsigned_64;
      Console      : Console_Writer;
      Log          : Log_Writer;
      Result       : out Boot_Result;
      Pump         : Pump_Callback := null;
      VCPUs        : VCPU_Count := 1)
   is
      R : V.Reply;
      E : V.Exit_Report;
      OK : Boolean;
      G : A.General_Registers;
      C : A.Control_Registers;
      Flags, RAX, RDX : Unsigned_64 := 0;
      End_Ms : Unsigned_64;
      Unknown_Exits : Natural := 0;
      Last_Pump : Unsigned_64 := 0;
      Stop : Boolean := False;
      --  A vCPU runs for a slice while another is runnable.
      Slice_Ms : constant := 2;
      Slice_End : Unsigned_64 := 0;

      function CPU return V.VCPU is (Cores (Current).Handle);

      function Running_Cores return Natural is
         N : Natural := 0;
      begin
         for T in 0 .. Core_Count - 1 loop
            if Cores (T).Mode = Running then
               N := N + 1;
            end if;
         end loop;
         return N;
      end Running_Cores;

      --  Keep the current vCPU's resume, and run T next for a slice.
      procedure Switch_To (T : Core_Index) is
      begin
         Cores (Current).Flags := Flags;
         Cores (Current).RAX := RAX;
         Cores (Current).RDX := RDX;
         Current := T;
         Flags := Cores (T).Flags;
         RAX := Cores (T).RAX;
         RDX := Cores (T).RDX;
         Slice_End := CuBit.Messages.Deadline_After (Slice_Ms);
      end Switch_To;

      function Next_Running return Core_Index is
         T : Core_Index := Current;
      begin
         for Step in 1 .. Core_Count loop
            T := (Current + Step) mod Core_Count;
            exit when Cores (T).Mode = Running;
         end loop;
         return T;
      end Next_Running;

      procedure Get_General is
      begin
         R := V.Get_State (CPU, A.General, G'Address, A.General_Bytes);
      end Get_General;
      procedure Put_General is
      begin
         R := V.Set_State (CPU, A.General, G'Address, A.General_Bytes);
      end Put_General;
      procedure Get_Control is
      begin
         R := V.Get_State (CPU, A.Control, C'Address, A.Control_Bytes);
      end Get_Control;
      procedure Reflect (Vector : Unsigned_64; Error : Unsigned_64 := 0; Has_Error : Boolean := False) is
      begin
         R := V.Inject (CPU, A.Hardware_Exception, Vector, Error, Has_Error);
      end Reflect;

      --  Deliver the current vCPU's NMI, or the highest pending vector its
      --  LAPIC would take.
      procedure Deliver is
         K : Core renames Cores (Current);
         H : constant Integer := Highest (K.IRR);
         S : constant Integer := Highest (K.ISR);
         NMI_Vector : constant := 2;
      begin
         if K.NMI_Pending then
            R := V.Inject (CPU, A.NMI, NMI_Vector);
            if R.Outcome = A.Ok then
               K.NMI_Pending := False;
            end if;
         elsif H >= 0 and then (S < 0 or else H / 16 > S / 16)
           and then Unsigned_32 (H / 16) > Shift_Right (K.APIC (APIC_TPR_Reg) and 16#F0#, 4)
           and then (K.APIC (APIC_SVR_Reg) and 16#100#) /= 0
         then
            R := V.Inject (CPU, A.External, Unsigned_64 (H));
            if R.Outcome = A.Ok then
               K.IRR (H) := False;
               K.ISR (H) := True;
            end if;
         end if;
      end Deliver;

      function Next_Deadline return Unsigned_64 is
         Now_TSC : Unsigned_64;
         Ms : Unsigned_64 := 20;
         Timer : constant Unsigned_64 := Cores (Current).Timer_Deadline;
         D : Unsigned_64;
      begin
         if Timer /= 0 then
            Now_TSC := Guest_TSC;
            Ms := (if Timer <= Now_TSC then 0 else (Timer - Now_TSC) / TSC_kHz);
            Ms := Unsigned_64'Min (Ms, 20);
         end if;
         --  With a pump (a window's keys), a halted guest wakes for it
         --  every Pump_Interval_Ms: input reaches the guest within that.
         if Pump /= null then
            Ms := Unsigned_64'Min (Ms, Pump_Interval_Ms);
         end if;
         D := CuBit.Messages.Deadline_After (Ms);
         if Running_Cores > 1 then
            D := Unsigned_64'Min (D, Slice_End);
         end if;
         return D;
      end Next_Deadline;

      procedure MMIO (GPA : Unsigned_64) is
         Code : Unsigned_64;
         I : Instruction;
         Value : Unsigned_64 := 0;
      begin
         Get_General;
         Get_Control;
         Code := Translate (C (A.CR3_Index), G (A.RIP_Index));
         I := (if Code = No_Address then (others => <>) else Decode (Code));
         if not I.Valid then
            Log ("unsupported MMIO instruction at " & Hex (G (A.RIP_Index)) & " (GPA " & Hex (GPA) & ")");
            Reflect (6);
            return;
         end if;
         if I.Is_Write then
            Value := (if I.Has_Immediate then I.Immediate else G (I.Register));
            if GPA in LAPIC_Base .. LAPIC_Base + MMIO_Page - 1 then
               APIC_Write (GPA - LAPIC_Base, Unsigned_32 (Value and 16#FFFF_FFFF#));
            elsif GPA = IOAPIC_Base then
               IOAPIC_Select := Unsigned_32 (Value and 16#FF#);
            elsif GPA = IOAPIC_Base + 16#10# then
               IOAPIC_Write (Unsigned_32 (Value and 16#FFFF_FFFF#));
            end if;
         else
            if GPA in LAPIC_Base .. LAPIC_Base + MMIO_Page - 1 then
               Value := Unsigned_64 (APIC_Read (GPA - LAPIC_Base));
            elsif GPA = IOAPIC_Base then
               Value := Unsigned_64 (IOAPIC_Select);
            elsif GPA = IOAPIC_Base + 16#10# then
               Value := Unsigned_64 (IOAPIC_Read);
            else
               Value := Unsigned_64'Last;
            end if;
            if I.Size = 1 then
               Value := Value and 16#FF#;
            elsif I.Size = 2 then
               Value := Value and 16#FFFF#;
            elsif I.Size = 4 then
               Value := Value and 16#FFFF_FFFF#;   --  a 32-bit load zero-extends
            end if;
            if I.Size >= 4 or else I.Zero_Extend_Byte then
               G (I.Register) := Value;
            else
               G (I.Register) := (G (I.Register) and not (Shift_Left (1, 8 * I.Size) - 1)) or Value;
            end if;
         end if;
         G (A.RIP_Index) := G (A.RIP_Index) + I.Length;
         Put_General;
      end MMIO;

      procedure MSR (Write : Boolean) is
         Index : Unsigned_64;
         Value : Unsigned_64 := 0;
         Known : Boolean := True;
      begin
         Get_General;
         Index := G (A.RCX_Index) and 16#FFFF_FFFF#;
         if Write then
            Value := Shift_Left (G (A.RDX_Index) and 16#FFFF_FFFF#, 32)
                     or (G (A.RAX_Index) and 16#FFFF_FFFF#);
            case Index is
               when 16#6E0# =>                      --  IA32_TSC_DEADLINE
                  Cores (Current).Timer_Deadline := Value;
               when 16#C000_0080# =>                --  EFER: SCE, LME, LMA, NXE, FFXSR, AIBRSE
                  if (Value and not (EFER_Writable
                                     or (if Host_Has_AutoIBRS then EFER_AutoIBRS else 0))) /= 0
                  then
                     Reflect (13, 0, True);
                     return;
                  end if;
                  Get_Control;
                  C (A.EFER_Index) := Value;
                  R := V.Set_State (CPU, A.Control, C'Address, A.Control_Bytes);
               when 16#277# =>                      --  PAT
                  declare
                     M : A.Machine_Register_Values;
                  begin
                     R := V.Get_State (CPU, A.Machine_Registers, M'Address, A.Machine_Bytes);
                     M (A.PAT_Index) := Value;
                     R := V.Set_State (CPU, A.Machine_Registers, M'Address, A.Machine_Bytes);
                  end;
               when others => null;                 --  ignored (APIC base, MTRRs...)
            end case;
            Flags := A.Resume_Advance_RIP;
         else
            case Index is
               when 16#1B# =>                                  --  enabled; BSP on vCPU 0
                  Value := LAPIC_Base or 16#800# or (if Current = 0 then 16#100# else 0);
               when 16#6E0# => Value := Cores (Current).Timer_Deadline;
               when 16#10# => Value := Guest_TSC;
               when 16#C000_0080# =>
                  Get_Control;
                  Value := C (A.EFER_Index);
               when 16#277# => Value := 16#0007_0406_0007_0406#;
               when 16#1A0# => Value := 1;                     --  fast strings
               when 16#2FF# => Value := 16#806#;               --  MTRRs on, default WB
               when 16#8B# | 16#FE# | 16#17# | 16#34# | 16#3A# | 16#140# | 16#C001_0010#
                  | 16#C001_0015# | 16#C001_001F# | 16#C001_1029# | 16#C001_0114#
                  | 16#C000_0103# | 16#C001_1020# .. 16#C001_1023# =>
                  Value := 0;
               when others =>
                  Known := False;
            end case;
            if not Known then
               Reflect (13, 0, True);
               return;
            end if;
            Flags := A.Resume_Advance_RIP or A.Resume_Set_RAX or A.Resume_Set_RDX;
            RAX := Value and 16#FFFF_FFFF#;
            RDX := Shift_Right (Value, 32);
         end if;
      end MSR;

      procedure Port (Info : Unsigned_64) is
         Port_Number : constant Unsigned_64 := Shift_Right (Info, A.Port_Number_Shift) and 16#FFFF#;
         Size : constant Natural := (if (Info and 16#40#) /= 0 then 4
                                     elsif (Info and 16#20#) /= 0 then 2 else 1);
         Value : Unsigned_64;
      begin
         if (Info and Shift_Left (1, A.Port_String_Bit)) /= 0 then
            Log ("string port I/O is not modelled (port " & Hex (Port_Number) & ")");
            Flags := A.Resume_Advance_RIP;
            return;
         end if;
         if (Info and 1) /= 0 then
            Get_General;
            Value := Port_Read (Port_Number, Size);
            RAX := (G (A.RAX_Index) and not (Shift_Left (1, 8 * Size) - 1)) or Value;
            if Size = 4 then
               RAX := Value;
            end if;
            Flags := A.Resume_Advance_RIP or A.Resume_Set_RAX;
         else
            Get_General;
            Port_Write (Port_Number, G (A.RAX_Index) and (Shift_Left (1, 8 * Size) - 1));
            Flags := A.Resume_Advance_RIP;
         end if;
      end Port;

      procedure Answer_CPUID is
         EAX, EBX, ECX, EDX : Unsigned_32;
      begin
         Get_General;
         Get_Control;
         Guest_CPUID (Unsigned_32 (G (A.RAX_Index) and 16#FFFF_FFFF#),
                      Unsigned_32 (G (A.RCX_Index) and 16#FFFF_FFFF#),
                      C (A.CR4_Index), Unsigned_32 (Current), C (A.XCR0_Index), EAX, EBX, ECX, EDX);
         G (A.RAX_Index) := Unsigned_64 (EAX);
         G (A.RBX_Index) := Unsigned_64 (EBX);
         G (A.RCX_Index) := Unsigned_64 (ECX);
         G (A.RDX_Index) := Unsigned_64 (EDX);
         Put_General;
         Flags := A.Resume_Advance_RIP;
      end Answer_CPUID;

      --  MOV to or from a debug register (0F 21 /r, 0F 23 /r): a shadow,
      --  never the hardware's (the kernel intercepts every DR access).
      procedure Debug_Register is
         Code : Unsigned_64;
         ModRM : Unsigned_8;
         Op : Unsigned_8;
         P : Unsigned_64;
         REX_B : Boolean := False;
         GPR : Natural;
      begin
         Get_General;
         Get_Control;
         Code := Translate (C (A.CR3_Index), G (A.RIP_Index));
         if Code = No_Address or else not In_RAM (Code, 4) then
            Reflect (6);
            return;
         end if;
         P := Code;
         if Read8 (P) in 16#40# .. 16#4F# then
            REX_B := (Read8 (P) and 1) /= 0;
            P := P + 1;
         end if;
         Op := Read8 (P + 1);
         ModRM := Read8 (P + 2);
         GPR := GPR_Of (Natural (ModRM and 7) + (if REX_B then 8 else 0));
         if Op = 16#21# then
            G (GPR) := Cores (Current).DR_Shadow (Natural (Shift_Right (ModRM, 3) and 7));
         elsif Op = 16#23# then
            Cores (Current).DR_Shadow (Natural (Shift_Right (ModRM, 3) and 7)) := G (GPR);
         end if;
         G (A.RIP_Index) := G (A.RIP_Index) + (P + 3 - Code);
         Put_General;
      end Debug_Register;
   begin
      Result := Setup_Failed;
      Console_Out := Console;
      Log_Out := Log;
      Stages := [others => 0];
      Returned := 0;
      Started_Us := CuBit.Monotonic.Microseconds;
      Reset_Devices;
      Core_Count := VCPUs;

      R := V.Create_VM (Authority, Machine);
      if R.Outcome /= A.Ok then
         Vmm_Machine.Log ("Create_VM refused");
         return;
      end if;
      Mark (VM_Created);
      RAM_Bytes := RAM_MiB * 16#10_0000#;
      declare
         Base : constant Unsigned_64 := CuBit.Owned_Reservations.Reserve (RAM_Bytes, Two_MiB);
      begin
         if Base = 0 then
            Vmm_Machine.Log ("no address space for" & Unsigned_64'Image (RAM_MiB) & " MiB of guest RAM");
            return;
         elsif not CuBit.Owned_Reservations.Change (Base, RAM_Bytes, CuBit.Owned_Reservations.Commit) then
            Vmm_Machine.Log ("committing" & Unsigned_64'Image (RAM_MiB)
                             & " MiB of guest RAM was refused (the monitor's memory limit?)");
            return;
         end if;
         RAM_Base := Base;
      end;
      --  Every page present before the kernel pins it.
      for P in 0 .. RAM_Bytes / Page_Bytes - 1 loop
         Write8 (P * Page_Bytes, 0);
      end loop;
      R := V.Add_RAM (Machine, 0, RAM_Base, RAM_Bytes,
                      A.Right_Read or A.Right_Write or A.Right_Execute);
      if R.Outcome /= A.Ok then
         Vmm_Machine.Log ("Add_RAM refused");
         return;
      end if;
      Mark (RAM_Mapped);
      TSC_kHz := Measure_TSC_kHz;
      Load_Linux
        (Guest, Command_Line & " tsc_early_khz=" & Image_Of (TSC_kHz)
         & " lpj=" & Image_Of (TSC_kHz) & " console=ttyS0,115200 earlyprintk=serial,ttyS0,115200"
         & " acpi=off no_timer_check tsc=reliable pci=off reboot=t panic=-1 nokaslr",
         OK);
      if not OK then
         return;
      end if;
      Mark (Image_Loaded);
      for T in 0 .. Core_Count - 1 loop
         R := V.Create_VCPU (Machine, Cores (T).Handle);
         if R.Outcome /= A.Ok then
            Vmm_Machine.Log ("Create_VCPU refused");
            R := V.Destroy (Machine);
            return;
         end if;
         Reset_APIC (Cores (T));
         Cores (T).Mode := (if T = 0 then Running else Waiting_For_SIPI);
      end loop;
      Current := 0;
      Enter_Long_Mode (CPU, OK);
      if not OK then
         Vmm_Machine.Log ("the vCPU refused the 64-bit entry state");
         return;
      end if;
      declare
         M : A.Machine_Register_Values;
      begin
         R := V.Get_State (CPU, A.Machine_Registers, M'Address, A.Machine_Bytes);
         Guest_TSC_Offset := M (A.TSC_Offset_Index);
      end;
      Vmm_Machine.Log ("entering the guest at " & Hex (Kernel_At + Kernel_Entry_Offset)
                       & ", TSC " & Image_Of (TSC_kHz) & " kHz," & Natural'Image (Core_Count) & " vCPU"
                       & (if Core_Count > 1 then "s" else ""));
      Slice_End := CuBit.Messages.Deadline_After (Slice_Ms);
      End_Ms := CuBit.Messages.Deadline_After (Run_Ms);
      Result := Booted_To_Deadline;

      loop
         exit when CuBit.Messages.Deadline_After (0) >= End_Ms;
         if Pump /= null and then CuBit.Monotonic.Milliseconds - Last_Pump >= Pump_Interval_Ms then
            Last_Pump := CuBit.Monotonic.Milliseconds;
            Pump (Stop);
            exit when Stop;
         end if;
         if Yield_To >= 0 and then Cores (Yield_To).Mode = Running then
            Switch_To (Yield_To);
            Yield_To := -1;
         elsif Running_Cores > 1 and then CuBit.Messages.Deadline_After (0) >= Slice_End then
            Switch_To (Next_Running);
         end if;
         Yield_To := -1;
         Timer_Check;
         Deliver;
         E := V.Run (CPU, Next_Deadline, Flags, RAX, RDX);
         Mark (First_Entry);
         Flags := 0; RAX := 0; RDX := 0;
         case E.Status is
            when A.Deadline | A.Kicked => null;
            when A.VM_Ended | A.Run_Refused =>
               Result := Guest_Failed;
               exit;
            when A.Exited =>
               Returned := Returned + 1;
               --  The first exits, for bring-up.
               if Returned <= 40 or else Returned mod 1_000_000 = 0 then
                  Get_General;
                  Vmm_Machine.Log ("vCPU" & Natural'Image (Current)
                                   & " exit" & Unsigned_64'Image (Returned) & " kind" & Unsigned_64'Image (E.Kind)
                                   & " code " & Hex (E.Code) & " info1 " & Hex (E.Info_1)
                                   & " info2 " & Hex (E.Info_2) & " rip " & Hex (G (A.RIP_Index)));
               end if;
               if V.Is_Kind (E, A.Port_IO) then
                  Port (E.Info_1);
               elsif V.Is_Kind (E, A.Nested_Fault) then
                  MMIO (E.Info_2);
               elsif V.Is_Kind (E, A.MSR_Access) then
                  MSR (E.Info_1 = 1);
               elsif V.Is_Kind (E, A.CPUID) then
                  Answer_CPUID;
               elsif V.Is_Kind (E, A.XSETBV) then
                  --  XCR0 stays the host's in this slice; a guest that asks
                  --  for the same value is answered, anything else is #GP.
                  Get_General;
                  Get_Control;
                  if (Shift_Left (G (A.RDX_Index) and 16#FFFF_FFFF#, 32)
                      or (G (A.RAX_Index) and 16#FFFF_FFFF#)) = C (A.XCR0_Index)
                  then
                     Flags := A.Resume_Advance_RIP;
                  else
                     Vmm_Machine.Log ("the guest asked for a different XCR0");
                     Reflect (13, 0, True);
                  end if;
               elsif V.Is_Kind (E, A.Debug_Register) then
                  Debug_Register;
               elsif V.Is_Kind (E, A.Exception_Exit) then
                  --  #DB and #AC are intercepted (CVE-2015-8104, -5307) and
                  --  reflected.
                  Reflect (E.Info_1, E.Info_2, E.Info_1 = 17);
               elsif V.Is_Kind (E, A.Shutdown) then
                  Vmm_Machine.Log ("the guest shut down (triple fault or reboot)");
                  Result := Guest_Shutdown;
                  exit;
               elsif V.Is_Kind (E, A.Entry_Failed) then
                  Vmm_Machine.Log ("VM entry failed (invalid guest state)");
                  Result := Guest_Failed;
                  exit;
               elsif V.Is_Kind (E, A.Cache_Flush) then
                  Flags := A.Resume_Advance_RIP;        --  WBINVD, INVD: nothing to write back
               else
                  --  VMRUN, VMMCALL, MONITOR/MWAIT, and anything else: #UD.
                  Unknown_Exits := Unknown_Exits + 1;
                  if Unknown_Exits <= 8 then
                     Vmm_Machine.Log ("exit " & Hex (E.Code) & " answered #UD");
                  end if;
                  Reflect (6);
               end if;
         end case;
      end loop;
      Flush_Line;
      --  4.7a in a real guest: INIT, SIPI and the APs' mode changes left
      --  the nested tables as they were, and each AP's tag was flushed
      --  before it first ran.
      Tables_Held := not Have_Digest or else V.Describe (Machine).W3 = Digest_At_INIT;
      for T in 1 .. Core_Count - 1 loop
         if Cores (T).Started then
            declare
               S : constant V.Reply := V.Statistics (Cores (T).Handle);
            begin
               Vmm_Machine.Log ("vCPU" & Natural'Image (T) & ":" & Unsigned_64'Image (S.W1)
                                & " entries," & Unsigned_64'Image (S.W2) & " tag flushes");
               Tables_Held := Tables_Held and then S.W2 >= 1;
            end;
         end if;
      end loop;
      if Have_Digest then
         Vmm_Machine.Log ("nested-table digest at the first INIT " & Hex (Digest_At_INIT) & ", at the end "
                          & Hex (V.Describe (Machine).W3));
      end if;
      R := V.Destroy (Machine);
   end Boot_Linux;
end Vmm_Machine;
