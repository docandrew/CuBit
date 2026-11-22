--  VMM-001 slice 1: the VM mechanism, checked from procmgr's place. A tiny
--  monitor runs a real-mode guest of a few bytes and checks what the
--  kernel keeps and what it returns (docs/hypervisor.md 4.3, 10.1 slice 1):
--  port output, a nested fault outside RAM, VMRUN in the guest, HLT kept
--  in the kernel, a guest spinning with interrupts off still bounded by the
--  deadline, an architectural reset that leaves the nested tables
--  untouched and flushes the guest's tag, a vCPU state round trip, a
--  triple fault, Add_RAM refused after the first run, and handles that go
--  stale with the VM. The guest's own SPEC_CTRL: what it writes it reads
--  back across exits, the host's value is untouched, and a reserved bit is
--  #GP (decided by the kernel while writes exit, by the CPU once they are
--  passed through). It also measures the cost of one exit to the monitor.
pragma Ada_2022;
with Interfaces; use Interfaces;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;
with VM_ABI;
with CuBit.Handles; use CuBit.Handles;
with CuBit.Handles.Boot_Roots;
with CuBit.Handles.Test_Consoles;
with CuBit.Handles.VMs;
with CuBit.Messages;
with CuBit.Owned_Reservations;
with CuBit.Monotonic;

procedure Main is
   package V renames CuBit.Handles.VMs;
   package A renames VM_ABI;
   use type A.Outcome;
   use type A.Run_Status;

   Roots : constant CuBit.Handles.Boot_Roots.Launcher_Handles :=
     CuBit.Handles.Boot_Roots.Process_Manager;
   Passed : Boolean := True;

   procedure Say (Text : String) is
      Answer : CuBit.Handles.Test_Consoles.Outcome;
      pragma Unreferenced (Answer);
   begin
      Answer := CuBit.Handles.Test_Consoles.Write
        (Roots.Test_Console_Object, "vm-hello: " & Text & ASCII.LF);
   end Say;

   procedure Check (Condition : Boolean; Name : String) is
   begin
      Say (Name & (if Condition then " PASS" else " FAIL"));
      Passed := Passed and then Condition;
   end Check;

   function Hex (Value : Unsigned_64) return String is
      Digits_Text : constant String := "0123456789abcdef";
      Result : String (1 .. 16);
      W : Unsigned_64 := Value;
   begin
      for I in reverse Result'Range loop
         Result (I) := Digits_Text (Natural (W and 15) + 1);
         W := Shift_Right (W, 4);
      end loop;
      return Result;
   end Hex;

   --  Guest RAM: 64 KiB of this process's own memory at guest-physical 0.
   RAM_Bytes : constant := 16#1_0000#;
   Code_At   : constant := 16#1000#;
   Text_At   : constant := 16#1100#;
   Store_At  : constant := 16#1010#;   --  the store to GPA 0x20000
   VMRUN_At  : constant := 16#1020#;
   Spin_At   : constant := 16#1030#;
   Fault_At  : constant := 16#1040#;
   CPUID_At  : constant := 16#1050#;
   CPUID_Loop_At : constant := 16#1090#;
   Spec_At   : constant := 16#1060#;
   GP_Vector_Entry : constant := 13 * 4;   --  the real-mode IVT entry of #GP
   Outside   : constant := 16#2_0000#;

   type Byte_Array is array (Natural range <>) of Unsigned_8;
   --  Real mode, CS = DS = 0 (Intel SDM / AMD APM encodings):
   --  1000 mov dx, 0x3F8; mov si, 0x1100
   --  1006 lodsb; test al, al; jz 100E; out dx, al; jmp 1006
   --  100E hlt; jmp 100E
   Hello : constant Byte_Array :=
     [16#BA#, 16#F8#, 16#03#, 16#BE#, 16#00#, 16#11#,
      16#AC#, 16#84#, 16#C0#, 16#74#, 16#03#, 16#EE#, 16#EB#, 16#F8#,
      16#F4#, 16#EB#, 16#FD#];
   --  1010 mov ax, 0x2000; mov ds, ax; mov byte [0], 0x55; hlt
   Store : constant Byte_Array :=
     [16#B8#, 16#00#, 16#20#, 16#8E#, 16#D8#, 16#C6#, 16#06#, 16#00#, 16#00#, 16#55#, 16#F4#];
   --  1020 vmmcall; hlt
   VMRUN_Code : constant Byte_Array := [16#0F#, 16#01#, 16#D9#, 16#F4#];
   --  1030 cli; jmp 1031
   Spin : constant Byte_Array := [16#FA#, 16#EB#, 16#FE#];
   --  1040 int3
   Fault : constant Byte_Array := [16#CC#];
   --  1050 cpuid; hlt
   CPUID_Code : constant Byte_Array := [16#0F#, 16#A2#, 16#F4#];
   --  1060 wrmsr; out dx, al; out dx, al; rdmsr; out dx, al; hlt
   Spec_Code : constant Byte_Array :=
     [16#0F#, 16#30#, 16#EE#, 16#EE#, 16#0F#, 16#32#, 16#EE#, 16#F4#];
   --  #GP goes to 0000:1020 (vmmcall): a write the guest is refused exits
   --  as a hypercall, one it is not falls through to the port output.
   GP_Entry : constant Byte_Array := [16#20#, 16#10#, 16#00#, 16#00#];
   --  1090 cpuid; jmp 1090
   CPUID_Loop : constant Byte_Array := [16#0F#, 16#A2#, 16#EB#, 16#FC#];
   Message : constant String := "Hello from a CuBit guest";

   RAM : Unsigned_64;
   Machine : V.VM;
   CPU : V.VCPU;
   R : V.Reply;
   E : V.Exit_Report;
   Output : String (1 .. 64);
   Output_Length : Natural := 0;

   procedure Poke (At_Offset : Natural; Bytes : Byte_Array) is
   begin
      for I in Bytes'Range loop
         declare
            B : Unsigned_8 with Import, Volatile,
              Address => To_Address (Integer_Address (RAM) + Integer_Address (At_Offset + I));
         begin
            B := Bytes (I);
         end;
      end loop;
   end Poke;

   function Deadline_After (Ms : Unsigned_64) return Unsigned_64 is
     (CuBit.Messages.Deadline_After (Ms));

   G : A.General_Registers;
   Sg : A.Segment_Registers;
   C : A.Control_Registers;

   procedure Get_General is
   begin
      R := V.Get_State (CPU, A.General, G'Address, A.General_Bytes);
   end Get_General;

   procedure Set_RIP (RIP : Unsigned_64) is
   begin
      Get_General;
      G (A.RIP_Index) := RIP;
      R := V.Set_State (CPU, A.General, G'Address, A.General_Bytes);
   end Set_RIP;

   --  The cost of one exit to the monitor and back (docs/hypervisor.md
   --  8.2): the guest loops on CPUID, each one an exit that Run returns,
   --  answered by resuming past it. A measurement, not a check.
   Warm_Up_Exits : constant := 1_000;
   Timed_Exits   : constant := 20_000;
   procedure Exit_Cost (Label : String) is
      Start, Stop : Unsigned_64;
      Exits : Natural := 0;
   begin
      Set_RIP (CPUID_Loop_At);
      for I in 1 .. Warm_Up_Exits loop
         E := V.Run (CPU, Deadline_After (500), A.Resume_Advance_RIP);
      end loop;
      Start := CuBit.Monotonic.Microseconds;
      for I in 1 .. Timed_Exits loop
         E := V.Run (CPU, Deadline_After (500), A.Resume_Advance_RIP);
         exit when E.Status /= A.Exited or else not V.Is_Kind (E, A.CPUID);
         Exits := Exits + 1;
      end loop;
      Stop := CuBit.Monotonic.Microseconds;
      if Exits = Timed_Exits then
         Say ("exit round trip (" & Label & "):"
              & Unsigned_64'Image ((Stop - Start) * 1_000 / Timed_Exits) & " ns per CPUID exit, over"
              & Natural'Image (Timed_Exits));
      else
         Check (False, "a CPUID loop exits on every CPUID (" & Label & ")");
      end if;
   end Exit_Cost;
begin
   Say ("start");
   if not Valid (Roots.VM_Create_Authority) then
      Check (False, "procmgr's place holds the VM creation authority");
      Say ("done FAIL");
      return;
   end if;
   R := V.Create_VM (Roots.VM_Create_Authority, Machine);
   Check (R.Outcome = A.Ok and then V.Valid (Machine), "Create_VM gives a VM handle");
   if not V.Valid (Machine) then
      Say ("done FAIL");
      return;
   end if;

   RAM := CuBit.Owned_Reservations.Allocate (RAM_Bytes);
   --  Touch every page: committed and present before the kernel pins it.
   for P in 0 .. RAM_Bytes / 4096 - 1 loop
      Poke (P * 4096, [0]);
   end loop;
   Poke (Code_At, Hello);
   Poke (Store_At, Store);
   Poke (VMRUN_At, VMRUN_Code);
   Poke (Spin_At, Spin);
   Poke (Fault_At, Fault);
   Poke (CPUID_At, CPUID_Code);
   Poke (CPUID_Loop_At, CPUID_Loop);
   Poke (Spec_At, Spec_Code);
   Poke (GP_Vector_Entry, GP_Entry);
   for I in Message'Range loop
      Poke (Text_At + I - 1, [Character'Pos (Message (I))]);
   end loop;
   Poke (Text_At + Message'Length, [0]);

   R := V.Add_RAM (Machine, 0, RAM, RAM_Bytes, A.Right_Read or A.Right_Write or A.Right_Execute);
   Check (R.Outcome = A.Ok and then R.W1 = RAM_Bytes / 4096, "Add_RAM pins and maps 16 pages");
   R := V.Add_RAM (Machine, 16#8000#, RAM, 4096, A.Right_Read);
   Check (R.Outcome = A.Bad_Range, "an overlapping Add_RAM is refused");
   R := V.Add_RAM (Machine, 16#3_0000#, 16#1000#, 4096, A.Right_Read);
   Check (R.Outcome = A.Refused, "Add_RAM of memory the monitor does not own is refused");

   R := V.Create_VCPU (Machine, CPU);
   Check (R.Outcome = A.Ok and then V.Valid (CPU), "Create_VCPU gives a vCPU handle");

   --  Start in real mode at 0:1000.
   R := V.Get_State (CPU, A.Segments, Sg'Address, A.Segments_Bytes);
   Sg (A.CS_Index).Selector := 0;
   Sg (A.CS_Index).Base := 0;
   R := V.Set_State (CPU, A.Segments, Sg'Address, A.Segments_Bytes);
   Check (R.Outcome = A.Ok, "Set_State segments");
   Set_RIP (Code_At);
   Check (R.Outcome = A.Ok, "Set_State general (RIP)");

   --  The guest's port output, one exit per byte, until it halts.
   loop
      E := V.Run (CPU, Deadline_After (500), A.Resume_Advance_RIP);
      exit when E.Status /= A.Exited or else not V.Is_Kind (E, A.Port_IO);
      Get_General;
      if Shift_Right (E.Info_1, A.Port_Number_Shift) = 16#3F8#
        and then (E.Info_1 and 1) = 0 and then Output_Length < Output'Last
      then
         Output_Length := Output_Length + 1;
         Output (Output_Length) := Character'Val (Natural (G (A.RAX_Index) and 16#FF#));
      end if;
   end loop;
   Say ("guest serial: " & Output (1 .. Output_Length));
   Check (Output (1 .. Output_Length) = Message, "the guest's port output reaches the monitor");
   Check (E.Status = A.Deadline, "HLT stays in the kernel until the deadline");

   R := V.Add_RAM (Machine, 16#2_0000#, RAM, 4096, A.Right_Read);
   Check (R.Outcome = A.Busy, "Add_RAM after the first run is refused");

   --  A store outside guest RAM: a nested fault with its address.
   Set_RIP (Store_At);
   E := V.Run (CPU, Deadline_After (500));
   Check (E.Status = A.Exited and then V.Is_Kind (E, A.Nested_Fault)
          and then E.Info_2 = Outside and then (E.Info_1 and 2) /= 0,
          "a store outside guest RAM is a nested fault at its address " & Hex (E.Info_2));

   --  A hypercall exits to the monitor (the kernel defines none).
   Set_RIP (VMRUN_At);
   E := V.Run (CPU, Deadline_After (500));
   Check (E.Status = A.Exited and then V.Is_Kind (E, A.Hypercall) and then E.Code = 16#81#,
          "VMMCALL exits to the monitor");

   --  A guest spinning with interrupts off is still bounded by Run's
   --  deadline: host interrupts always exit.
   Set_RIP (Spin_At);
   declare
      Before : constant Unsigned_64 := CuBit.Monotonic.Milliseconds;
   begin
      E := V.Run (CPU, Deadline_After (200));
      Check (E.Status = A.Deadline and then CuBit.Monotonic.Milliseconds - Before < 2_000,
             "a guest spinning with interrupts off returns at the deadline");
   end;

   --  INIT-style reset of the architectural state (CR0.PG and PE toggled):
   --  the nested tables are untouched and the guest's tag is flushed.
   declare
      Digest_Before : constant Unsigned_64 := V.Describe (Machine).W3;
      Flushes_Before : constant Unsigned_64 := V.Statistics (CPU).W2;
   begin
      R := V.Get_State (CPU, A.Control, C'Address, A.Control_Bytes);
      C (A.CR0_Index) := C (A.CR0_Index) or 16#8000_0001#;   --  PG and PE on
      C (A.CR4_Index) := 16#20#;                              --  PAE
      R := V.Set_State (CPU, A.Control, C'Address, A.Control_Bytes);
      C (A.CR0_Index) := 16#6000_0010#;                       --  the reset value
      C (A.CR4_Index) := 0;
      R := V.Set_State (CPU, A.Control, C'Address, A.Control_Bytes);
      Check (R.Outcome = A.Ok and then V.Describe (Machine).W3 = Digest_Before,
             "an architectural reset leaves every nested-table leaf unchanged");
      Set_RIP (Spin_At);
      E := V.Run (CPU, Deadline_After (50));
      Check (V.Statistics (CPU).W2 > Flushes_Before,
             "the guest's tag is flushed before it runs again");
   end;

   --  A monitor answering CPUID writes the registers back (same RIP) and
   --  resumes with Resume_Advance_RIP: the guest moves on (regression:
   --  Set_State General used to drop the exit's next RIP, so Linux's first
   --  CPUID looped forever).
   Set_RIP (CPUID_At);
   E := V.Run (CPU, Deadline_After (500));
   if E.Status = A.Exited and then V.Is_Kind (E, A.CPUID) then
      Get_General;
      G (A.RAX_Index) := 16#1234#;
      R := V.Set_State (CPU, A.General, G'Address, A.General_Bytes);
      E := V.Run (CPU, Deadline_After (200), A.Resume_Advance_RIP);
      Check (E.Status = A.Deadline, "a CPUID answered through Set_State advances past the instruction");
   else
      Check (False, "a CPUID answered through Set_State advances past the instruction");
   end if;

   Exit_Cost ("SPEC_CTRL never written");

   --  The guest's own SPEC_CTRL (docs/hypervisor.md 4.3).
   declare
      SPEC_CTRL : constant := 16#48#;
      SSBD : constant Unsigned_64 := 2 ** 2;
      Reserved : constant Unsigned_64 := 2 ** 63;   --  no SPEC_CTRL bit
      S : constant V.Reply := V.Speculation (CPU);
      Host_Before : constant Unsigned_64 := S.W1;
      Mask : constant Unsigned_64 := S.W2;
      Value : Unsigned_64 := 0;
      M : A.Machine_Register_Values;

      procedure Write_From (Written : Unsigned_64) is
      begin
         Get_General;
         G (A.RCX_Index) := SPEC_CTRL;
         G (A.RAX_Index) := Written and 16#FFFF_FFFF#;
         G (A.RDX_Index) := Shift_Right (Written, 32);
         G (A.RIP_Index) := Spec_At;
         R := V.Set_State (CPU, A.General, G'Address, A.General_Bytes);
         E := V.Run (CPU, Deadline_After (500));
      end Write_From;

      function Refused_With_GP return Boolean is
        (E.Status = A.Exited and then V.Is_Kind (E, A.Hypercall));
      function Passed return Boolean is
        ((V.Speculation (CPU).W3 and A.Speculation_Writes_Passed) /= 0);
      function Host_Now return Unsigned_64 is (V.Speculation (CPU).W1);
   begin
      Say ("SPEC_CTRL: host " & Hex (Host_Before) & ", a guest may set " & Hex (Mask)
           & ((if (S.W3 and A.Speculation_CPU_Switched) /= 0 then ", switched by VMRUN"
               else ", switched by the kernel")));
      Write_From (Reserved);
      Check (Refused_With_GP and then not Passed,
             "a SPEC_CTRL write of a reserved bit is #GP in the guest (the kernel decides)");
      for B in 0 .. 63 loop
         exit when Value /= 0;
         if (Mask and Shift_Left (1, B)) /= 0 and then (Host_Before and Shift_Left (1, B)) = 0 then
            Value := Shift_Left (1, B);
         end if;
      end loop;
      if (Mask and SSBD) /= 0 and then (Host_Before and SSBD) = 0 then
         Value := SSBD;
      end if;
      if Value = 0 then
         Say ("SPEC_CTRL: no bit the guest may set differs from the host's; the rest is skipped");
      else
         Say ("SPEC_CTRL: the guest writes " & Hex (Value));
         Write_From (Value);
         R := V.Get_State (CPU, A.Machine_Registers, M'Address, A.Machine_Bytes);
         Check (E.Status = A.Exited and then V.Is_Kind (E, A.Port_IO)
                and then M (A.SPEC_CTRL_Index) = Value and then Passed,
                "a SPEC_CTRL write the host allows is kept and its later writes pass through");
         Check (Host_Now = Host_Before, "the host's SPEC_CTRL is back after the exit");
         --  The monitor clears EAX and EDX; two more exits and re-entries,
         --  then the guest reads the MSR.
         Get_General;
         G (A.RAX_Index) := 0;
         G (A.RDX_Index) := 0;
         R := V.Set_State (CPU, A.General, G'Address, A.General_Bytes);
         E := V.Run (CPU, Deadline_After (500), A.Resume_Advance_RIP);
         E := V.Run (CPU, Deadline_After (500), A.Resume_Advance_RIP);
         Get_General;
         Check (E.Status = A.Exited and then V.Is_Kind (E, A.Port_IO)
                and then G (A.RAX_Index) = (Value and 16#FFFF_FFFF#)
                and then G (A.RDX_Index) = Shift_Right (Value, 32),
                "the guest reads back its SPEC_CTRL after exits and re-entries");
         Check (Host_Now = Host_Before,
                "the host's SPEC_CTRL is unchanged after the guest ran with a different value");
         Exit_Cost ("the guest's SPEC_CTRL differs from the host's");
         Check (Host_Now = Host_Before, "the host's SPEC_CTRL is unchanged after the timed exits");
         Write_From (Reserved);
         R := V.Get_State (CPU, A.Machine_Registers, M'Address, A.Machine_Bytes);
         Check (Refused_With_GP and then M (A.SPEC_CTRL_Index) = Value,
                "with writes passed through, the CPU refuses a reserved bit (#GP) and the value stays");
         M (A.SPEC_CTRL_Index) := Reserved;
         R := V.Set_State (CPU, A.Machine_Registers, M'Address, A.Machine_Bytes);
         Check (R.Outcome = A.Refused, "Set_State refuses a SPEC_CTRL bit the host lacks");
      end if;
   end;

   --  vCPU state round trip: read everything, write it back, still runs.
   --  (DS back to 0: the store test left it at 0x2000.)
   R := V.Get_State (CPU, A.Segments, Sg'Address, A.Segments_Bytes);
   Sg (A.DS_Index).Selector := 0;
   Sg (A.DS_Index).Base := 0;
   R := V.Set_State (CPU, A.Segments, Sg'Address, A.Segments_Bytes);
   declare
      M : A.Machine_Register_Values;
   begin
      R := V.Get_State (CPU, A.Machine_Registers, M'Address, A.Machine_Bytes);
      R := V.Set_State (CPU, A.Machine_Registers, M'Address, A.Machine_Bytes);
      Get_General;
      R := V.Set_State (CPU, A.General, G'Address, A.General_Bytes);
      Set_RIP (Code_At);
      Output_Length := 0;
      E := V.Run (CPU, Deadline_After (500), A.Resume_Advance_RIP);
      Check (E.Status = A.Exited and then V.Is_Kind (E, A.Port_IO),
             "after a state round trip the guest runs on");
   end;

   --  A triple fault (int3 with an empty IDT): the vCPU stops.
   R := V.Get_State (CPU, A.Segments, Sg'Address, A.Segments_Bytes);
   Sg (A.IDTR_Index).Limit := 0;
   R := V.Set_State (CPU, A.Segments, Sg'Address, A.Segments_Bytes);
   Set_RIP (Fault_At);
   E := V.Run (CPU, Deadline_After (500));
   Check (E.Status = A.Exited and then V.Is_Kind (E, A.Shutdown),
          "a guest shutdown (int3 with an empty IDT) stops the vCPU and returns Shutdown");

   --  Bad injections are refused; a good one is accepted once.
   R := V.Inject (CPU, A.External, 7);
   Check (R.Outcome = A.Refused, "an external interrupt below 32 is refused");
   R := V.Inject (CPU, A.Hardware_Exception, 13);
   Check (R.Outcome = A.Refused, "#GP without its error code is refused");
   R := V.Inject (CPU, A.External, 48);
   Check (R.Outcome = A.Ok, "an external interrupt 48 is accepted");
   R := V.Inject (CPU, A.External, 49);
   Check (R.Outcome = A.Busy, "a second pending event is Busy");

   R := V.Destroy (Machine);
   Check (R.Outcome = A.Ok, "Destroy");
   E := V.Run (CPU, Deadline_After (50));
   Check (E.Status = A.Run_Refused, "the vCPU handle is stale after Destroy");
   R := V.Describe (Machine);
   Check (R.Outcome = A.Refused, "the VM handle is stale after Destroy");

   --  A second VM is then left for the reaper: this process exits with it.
   R := V.Create_VM (Roots.VM_Create_Authority, Machine);
   Check (R.Outcome = A.Ok, "a second VM, left for the reaper");
   R := V.Add_RAM (Machine, 0, RAM, 4096, A.Right_Read);

   Say (if Passed then "done PASS" else "done FAIL");
end Main;
