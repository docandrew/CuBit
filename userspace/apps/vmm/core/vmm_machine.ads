------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The per-VM monitor's core (VMM-001 slice 1, docs/hypervisor.md section
--  6): direct Linux boot, the guest's CPUID and MSR answers, a 16550 UART,
--  the xAPIC and IOAPIC registers, the PIC's presence, CMOS, and the MMIO
--  decoder, on top of the kernel's VM and vCPU objects (CuBit.Handles.VMs).
--
--  @description
--  Everything here runs in the deprivileged monitor process: the kernel
--  decodes nothing and models no device (docs/hypervisor.md 4.7). Guest
--  memory is this process's own committed memory, which the kernel pins
--  and maps into the guest (Add_RAM); every read of it is a single fetch
--  into a local value, then checked (docs/hypervisor.md 6.1).
--
--  Slice 1 deviations from 6.2, stated: the guest gets an xAPIC (MMIO,
--  decoded here) rather than x2APIC, because Linux enables x2APIC without
--  interrupt remapping only under hypervisors it knows; MP tables rather
--  than ACPI; a minimal IOAPIC and the PIC's presence (an IMR that reads
--  back) so Linux routes the UART's ISA IRQ 4 through the IOAPIC. All of it
--  is monitor code; none of it is in the kernel.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with System;
with CuBit.Handles;
with CuBit.Handles.VMs;
with VM_ABI;

package Vmm_Machine is

   --  What the guest prints on its serial port goes here, a line or a byte
   --  run at a time.
   type Console_Writer is access procedure (Text : String);
   --  Progress and diagnostics of the monitor itself.
   type Log_Writer is access procedure (Text : String);

   type Image is record
      Kernel        : System.Address := System.Null_Address;  --  a bzImage
      Kernel_Bytes  : Unsigned_64 := 0;
      Initrd        : System.Address := System.Null_Address;
      Initrd_Bytes  : Unsigned_64 := 0;
   end record;

   --  Called between guest runs, about every Pump_Interval_Ms, so a
   --  single-threaded monitor can serve its window and pass the guest its
   --  keys (Send_Input); Stop ends the guest. With a pump, a halted guest
   --  is woken that often too, so a key reaches it within the interval.
   type Pump_Callback is access procedure (Stop : out Boolean);
   Pump_Interval_Ms : constant := 1;

   --  The guest's processors: the boot vCPU, and application vCPUs that
   --  wait for INIT and SIPI from the guest. One monitor thread runs them
   --  in turn (time slices; an IPI switches to its target at once).
   Max_VCPUs : constant := VM_ABI.VCPU_Ceiling;
   subtype VCPU_Count is Positive range 1 .. Max_VCPUs;

   type Boot_Result is (Booted_To_Deadline, Guest_Shutdown, Guest_Failed, Setup_Failed);

   --  Boot Linux from Guest in a VM of RAM_MiB and VCPUs, with Command_Line (to which
   --  the monitor appends its timing and console arguments), and run it
   --  until Run_Ms passes. Console and Log may be null.
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
      VCPUs        : VCPU_Count := 1);

   --  The time from Boot_Linux's call to each stage (docs/hypervisor.md
   --  12.2), in microseconds, for the last boot.
   type Stage is (VM_Created, RAM_Mapped, Image_Loaded, First_Entry, First_Output, Prompt);
   function Stage_Us (S : Stage) return Unsigned_64;
   --  Exits returned to the monitor during the last boot, by kind.
   function Exits_Returned return Unsigned_64;
   --  Application vCPUs the guest started (INIT, then SIPI) in the last boot.
   function APs_Started return Natural;
   --  docs/hypervisor.md 4.7a in a real guest: the VM's nested-table digest
   --  at the first INIT equals the digest when the boot ended (INIT, SIPI,
   --  the AP's real-to-long-mode CR0.PG toggle changed no nested entry), and
   --  every started AP's tag was flushed before it ran.
   function AP_Tables_Unchanged return Boolean;
   --  What the guest printed after its last line feed (a prompt, keys it
   --  echoed), as a terminal shows the line (console_line.ads), and the
   --  column of its cursor (0-based).
   function Partial_Line return String;
   function Cursor_Column return Natural;
   --  Changes with every byte the guest prints: a window repaints when it
   --  differs from the one it painted.
   function Console_Generation return Unsigned_64;

   --  The guest's serial input (COM1's receive FIFO, guest_uart.ads), from
   --  a pump. Input_Room is how many bytes Send_Input may take now; a
   --  caller holds the rest (nothing is dropped). Send_Input raises the
   --  UART's interrupt when the guest enabled it, and the vCPU it goes to
   --  runs next.
   function Input_Room return Natural;
   procedure Send_Input (B : Unsigned_8) with Pre => Input_Room > 0;
   --  Input to output: how many times the guest printed after input it was
   --  sent (an echo), and the time from the input to that output, the last
   --  time, in microseconds.
   function Echo_Count return Unsigned_64;
   function Last_Echo_Us return Unsigned_64;
end Vmm_Machine;
