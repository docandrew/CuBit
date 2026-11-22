------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Virtual machines (VMM-001, docs/hypervisor.md section 4): the VM
--  creation authority, the VM and the vCPU kernel objects, reached by Call
--  on their handles (ABI-002; no new system calls). Shared by the kernel
--  (VM_Objects) and the runtime.
--
--  @description
--  Kinds (Capability_ABI.Kind): VM_Create (procmgr's root issuer; a
--  program receives a Write-only copy only when its manifest declares
--  vm-create and its startup entry approves it), VM, VCPU.
--
--  VM_Create:
--    Create_VM   reply (0) = Outcome, (1) = the VM handle (RIGHT_WRITE:
--                control and memory). The caller is the VM's monitor, for
--                life: only it runs, views and changes this VM.
--  VM:
--    Add_RAM     (0) = guest-physical base, (1) = the monitor's own address
--                of the memory (committed, its own, page-aligned), (2) =
--                bytes (pages), (3) = Rights mask (Right_*). The kernel
--                pins each frame and maps it into the VM's nested tables;
--                the monitor keeps its own mapping as the guest view.
--                Reply (0) = Outcome, (1) = pages mapped.
--    Create_VCPU reply (0) = Outcome, (1) = the vCPU handle.
--    Describe    reply (1) = pages mapped, (2) = vCPUs, (3) = a digest of
--                every nested-table leaf (a test sees that an architectural
--                state change leaves them untouched: docs/hypervisor.md 4.7a).
--    Destroy     stops every vCPU, unmaps and unpins everything.
--  VCPU:
--    Run         (0) = deadline (monotonic ms, or Forever), (1) = Resume
--                flags, (2) = RAX, (3) = RDX (each applied only with its
--                flag). Runs the guest on the calling thread until an exit
--                the kernel does not keep (docs/hypervisor.md 4.3), the
--                deadline, a kick, or the VM's end. Reply tag label =
--                Run_Status; (0) = Exit_Kind, (1) = the back end's raw exit
--                code, (2) = exit information 1, (3) = exit information 2.
--    Get_State / Set_State
--                (0) = State_Set, (1) = buffer address in the caller,
--                (2) = buffer bytes. Reply (0) = Outcome, (1) = bytes.
--    Inject      (0) = Event_Kind, (1) = vector, (2) = error code (with
--                Event_Error_Valid in (3)). One pending event at a time:
--                Busy until the kernel has delivered it.
--    Kick        makes the vCPU's Run return Kicked at its next boundary.
--    Statistics  reply (1) = guest entries, (2) = guest tag (ASID or VPID)
--                flushes, (3) = exits returned to the monitor.
--    Speculation reply (1) = the host's IA32_SPEC_CTRL on the calling CPU,
--                read now; (2) = the SPEC_CTRL bits a guest may set (the
--                host CPU's); (3) = Speculation_* flags. The guest's own
--                value is a machine register (SPEC_CTRL_Index).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package VM_ABI with Pure, SPARK_Mode is

   --  Labels (one block, like Debug_ABI's).
   Create_VM_Label   : constant := 16#0E01#;
   Add_RAM_Label     : constant := 16#0E02#;
   Create_VCPU_Label : constant := 16#0E03#;
   Describe_Label    : constant := 16#0E04#;
   Destroy_Label     : constant := 16#0E05#;
   Run_Label         : constant := 16#0E10#;
   Get_State_Label   : constant := 16#0E11#;
   Set_State_Label   : constant := 16#0E12#;
   Inject_Label      : constant := 16#0E13#;
   Kick_Label        : constant := 16#0E14#;
   Statistics_Label  : constant := 16#0E15#;
   Speculation_Label : constant := 16#0E16#;

   --  Speculation's flags (word 3).
   Speculation_Present        : constant Unsigned_64 := 2 ** 0;  --  the host has SPEC_CTRL
   Speculation_CPU_Switched   : constant Unsigned_64 := 2 ** 1;  --  V_SPEC_CTRL: VMRUN switches it
   Speculation_Writes_Passed  : constant Unsigned_64 := 2 ** 2;  --  guest writes no longer exit

   type Outcome is
     (Ok, Refused, Not_Supported, Busy, No_Memory, Ended, Bad_Range, Ceiling);
   for Outcome use
     (Ok => 0, Refused => 1, Not_Supported => 2, Busy => 3, No_Memory => 4,
      Ended => 5, Bad_Range => 6, Ceiling => 7);

   type Back_End is (None, SVM, VMX);
   for Back_End use (None => 0, SVM => 1, VMX => 2);

   --  Guest memory rights (Add_RAM word 3).
   Right_Read    : constant Unsigned_64 := 2 ** 0;
   Right_Write   : constant Unsigned_64 := 2 ** 1;
   Right_Execute : constant Unsigned_64 := 2 ** 2;
   Rights_Mask   : constant Unsigned_64 := Right_Read or Right_Write or Right_Execute;

   Page_Bytes : constant := 4096;
   --  Named ceilings (LIM-001).
   VM_Ceiling          : constant := 16;
   VCPU_Ceiling        : constant := 8;     --  per VM
   RAM_Run_Ceiling     : constant := 32;    --  Add_RAM runs per VM
   Guest_Page_Ceiling  : constant := 2 ** 20;  --  4 GiB of guest RAM per VM
   --  Guest-physical space: 48 bits (4-level nested tables).
   Guest_Physical_Limit : constant := 2 ** 48;

   --  No deadline: Run returns only on an exit, a kick or the VM's end.
   Forever : constant Unsigned_64 := Unsigned_64'Last;

   --  Run's resume flags (word 1).
   Resume_Advance_RIP : constant Unsigned_64 := 2 ** 0;  --  to the last exit's next RIP
   Resume_Set_RAX     : constant Unsigned_64 := 2 ** 1;
   Resume_Set_RDX     : constant Unsigned_64 := 2 ** 2;

   type Run_Status is (Exited, Deadline, Kicked, VM_Ended, Run_Refused);
   for Run_Status use
     (Exited => 1, Deadline => 2, Kicked => 3, VM_Ended => 4, Run_Refused => 5);

   --  Arch-neutral exit kinds; the back end's raw code is in word 1.
   type Exit_Kind is
     (Port_IO,          --  info1: port, size, direction (Port_* below); info2: next RIP
      Nested_Fault,     --  info1: access (Fault_*); info2: guest-physical address
      MSR_Access,       --  info1: 1 = write, 0 = read; ECX/EDX:EAX via Get_State
      CPUID,
      Halted_Too_Long,  --  reserved (HLT is kept in the kernel)
      Hypercall,
      Exception_Exit,   --  info1: vector; info2: error code
      XSETBV,
      Init_Signal,
      Shutdown,         --  triple fault: the vCPU is stopped
      Entry_Failed,     --  invalid guest state: the vCPU is stopped
      Debug_Register,   --  info1: register and direction
      Other,            --  any other intercept: the monitor answers #UD
      Cache_Flush);     --  WBINVD or INVD: the monitor advances past it
   for Exit_Kind use
     (Port_IO => 1, Nested_Fault => 2, MSR_Access => 3, CPUID => 4,
      Halted_Too_Long => 5, Hypercall => 6, Exception_Exit => 7, XSETBV => 8,
      Init_Signal => 9, Shutdown => 10, Entry_Failed => 11,
      Debug_Register => 12, Other => 13, Cache_Flush => 14);

   --  Port_IO information (word 2), as SVM's EXITINFO1 lays it out.
   Port_In_Bit     : constant := 0;
   Port_String_Bit : constant := 2;
   Port_Rep_Bit    : constant := 3;
   Port_Size_Shift : constant := 4;   --  bits 4..6: 1, 2, 4 bytes as one bit each
   Port_Number_Shift : constant := 16;

   --  Nested_Fault access (word 2), as SVM's NPF EXITINFO1 lays it out.
   Fault_Present_Bit : constant := 0;
   Fault_Write_Bit   : constant := 1;
   Fault_Fetch_Bit   : constant := 4;

   --  Inject.
   type Event_Kind is (External, NMI, Hardware_Exception);
   for Event_Kind use (External => 0, NMI => 2, Hardware_Exception => 3);
   Event_Error_Valid : constant Unsigned_64 := 1;

   --  State sets for Get_State and Set_State.
   type State_Set is (General, Segments, Control, Machine_Registers, Events);
   for State_Set use
     (General => 1, Segments => 2, Control => 3, Machine_Registers => 4, Events => 5);

   --  General: RAX, RBX, RCX, RDX, RSI, RDI, RBP, RSP, R8 .. R15, RIP,
   --  RFLAGS.
   type General_Registers is array (0 .. 17) of Unsigned_64;
   General_Bytes : constant := 18 * 8;
   RAX_Index : constant := 0;   RBX_Index : constant := 1;
   RCX_Index : constant := 2;   RDX_Index : constant := 3;
   RSI_Index : constant := 4;   RDI_Index : constant := 5;
   RBP_Index : constant := 6;   RSP_Index : constant := 7;
   R8_Index  : constant := 8;   R15_Index : constant := 15;
   RIP_Index : constant := 16;  RFLAGS_Index : constant := 17;

   --  Segments: ES, CS, SS, DS, FS, GS, GDTR, LDTR, IDTR, TR, each
   --  selector, attributes (SVM's 12-bit form), limit, base.
   type Segment is record
      Selector   : Unsigned_16 := 0;
      Attributes : Unsigned_16 := 0;
      Limit      : Unsigned_32 := 0;
      Base       : Unsigned_64 := 0;
   end record with Size => 128;
   for Segment use record
      Selector   at 0 range 0 .. 15;
      Attributes at 2 range 0 .. 15;
      Limit      at 4 range 0 .. 31;
      Base       at 8 range 0 .. 63;
   end record;
   type Segment_Registers is array (0 .. 9) of Segment;
   Segments_Bytes : constant := 10 * 16;
   ES_Index : constant := 0; CS_Index : constant := 1; SS_Index : constant := 2;
   DS_Index : constant := 3; FS_Index : constant := 4; GS_Index : constant := 5;
   GDTR_Index : constant := 6; LDTR_Index : constant := 7; IDTR_Index : constant := 8;
   TR_Index : constant := 9;

   --  Control: CR0, CR2, CR3, CR4, EFER, DR6, DR7, XCR0, CPL, Activity.
   type Control_Registers is array (0 .. 9) of Unsigned_64;
   Control_Bytes : constant := 10 * 8;
   CR0_Index : constant := 0; CR2_Index : constant := 1; CR3_Index : constant := 2;
   CR4_Index : constant := 3; EFER_Index : constant := 4; DR6_Index : constant := 5;
   DR7_Index : constant := 6; XCR0_Index : constant := 7; CPL_Index : constant := 8;
   Activity_Index : constant := 9;
   --  Activity states (the monitor's INIT/SIPI model): an AP waits for a
   --  SIPI by not being Run; the kernel keeps no such state.
   Activity_Running : constant := 0;

   --  Machine registers: STAR, LSTAR, CSTAR, SFMASK, KERNEL_GS_BASE,
   --  SYSENTER_CS, SYSENTER_ESP, SYSENTER_EIP, PAT, TSC offset, and the
   --  guest's IA32_SPEC_CTRL (Set_State refuses a bit the host lacks).
   type Machine_Register_Values is array (0 .. 10) of Unsigned_64;
   Machine_Bytes : constant := 11 * 8;
   STAR_Index : constant := 0; LSTAR_Index : constant := 1; CSTAR_Index : constant := 2;
   SFMASK_Index : constant := 3; Kernel_GS_Index : constant := 4;
   Sysenter_CS_Index : constant := 5; Sysenter_ESP_Index : constant := 6;
   Sysenter_EIP_Index : constant := 7; PAT_Index : constant := 8;
   TSC_Offset_Index : constant := 9; SPEC_CTRL_Index : constant := 10;

   --  Events: the pending injection (valid, kind, vector, error), the
   --  interrupt shadow, and the event the last exit interrupted.
   type Event_Values is array (0 .. 3) of Unsigned_64;
   Events_Bytes : constant := 4 * 8;
   Pending_Index : constant := 0;   --  bit 63 valid; else the event word
   Shadow_Index  : constant := 1;
   Interrupted_Index : constant := 2;
   Interrupted_Error_Index : constant := 3;

   function Bytes_Of (S : State_Set) return Unsigned_64 is
     (case S is
        when General => General_Bytes,
        when Segments => Segments_Bytes,
        when Control => Control_Bytes,
        when Machine_Registers => Machine_Bytes,
        when Events => Events_Bytes);
end VM_ABI;
