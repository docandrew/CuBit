------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The generic PCI launch's fixture driver (HW-002): QEMU's educational
--  device, driven from what devmgr's generic launch gives it and nothing
--  else. Every step is a bring-up line through logstore (the convention in
--  coordination/release-stack.md): its Generic_PCI configuration, its
--  register window, the device's identification and liveness registers,
--  its DMA region, a DMA round trip through its DMA region, one read DMA
--  from an address outside it (IOMMU-001: with an IOMMU the kernel reports
--  exactly one typed DMA fault and nothing is read), and one MSI raised by
--  the device after OP_READY.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with System;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Launch_Configurations;
with Launch_Configuration;
with CuBit.Handles.Devices;
with CuBit.Device_Interrupts;
with CuBit.Driver_Readiness;
with CuBit.Logging;
with CuBit.Log_Records;
with Startup;

procedure Main is
   package LC renames Launch_Configuration;
   use type LC.Interrupt_Delivery;

   --  QEMU's edu device (docs/specs/edu.rst): registers in BAR0.
   EDU_IDENTIFICATION   : constant := 16#00#;
   EDU_LIVENESS         : constant := 16#04#;
   EDU_INTERRUPT_STATUS : constant := 16#24#;
   EDU_INTERRUPT_RAISE  : constant := 16#60#;
   EDU_INTERRUPT_ACK    : constant := 16#64#;
   EDU_ID_LOW_BYTE      : constant := 16#ED#;
   LIVENESS_PATTERN     : constant Unsigned_32 := 16#5A5A_1234#;
   INTERRUPT_PATTERN    : constant Unsigned_32 := 16#0000_0ED1#;
   --  Where it maps its register window (its own choice).
   REGISTER_VIRTUAL_BASE : constant := 16#0000_6800_0000_0000#;
   PAGE_BYTES : constant := 4096;
   INTERRUPT_WAIT_MS : constant := 2_000;
   TOTAL_STAGES : constant := 7;

   --  edu's DMA engine: source, destination, count, command; the command's
   --  start bit (clear when done) and direction (set: device to RAM). The
   --  device's side is its 4 KiB buffer at 40000h. edu runs a DMA 100 ms
   --  after the command (virtual clock).
   EDU_DMA_SOURCE       : constant := 16#80#;
   EDU_DMA_DESTINATION  : constant := 16#88#;
   EDU_DMA_COUNT        : constant := 16#90#;
   EDU_DMA_COMMAND      : constant := 16#98#;
   EDU_DMA_START        : constant Unsigned_64 := 1;
   EDU_DMA_TO_RAM       : constant Unsigned_64 := 2;
   EDU_BUFFER           : constant Unsigned_64 := 16#4_0000#;
   --  edu's default DMA mask is 28 bits; above it the device clamps the
   --  address (it would reach the wrong RAM), so the round trip needs the
   --  block below it.
   EDU_DMA_LIMIT        : constant Unsigned_64 := 2 ** 28;
   DMA_PROBE_BYTES      : constant := 64;
   DMA_RETURN_OFFSET    : constant := 2048;   --  where the copy comes back
   DMA_WAIT_MS          : constant := 2_000;
   DMA_POLL_MS          : constant := 10;
   PROBE_PATTERN        : constant Unsigned_8 := 16#A5#;
   --  A physical page outside the driver's DMA block: 1 MiB, the kernel
   --  image on every CuBit machine. Only ever read (RAM to the device):
   --  without an IOMMU nothing is written anywhere; with one the read is
   --  refused and the kernel logs one typed DMA fault.
   UNMAPPED_PROBE_ADDRESS : constant Unsigned_64 := 16#10_0000#;

   Log : CuBit.Logging.Publisher := CuBit.Logging.Bound_To (Startup.Logstore);
   Config : LC.Generic_PCI_Configuration;
   Valid : Boolean;
   Reached : Natural := 0;

   function Hex (Value : Unsigned_64) return String is
      Digits_Text : constant String := "0123456789ABCDEF";
      Result : String (1 .. 16);
      V : Unsigned_64 := Value;
   begin
      for I in reverse Result'Range loop
         Result (I) := Digits_Text (Natural (V and 15) + 1);
         V := Shift_Right (V, 4);
      end loop;
      return "0x" & Result;
   end Hex;

   procedure Line (Text : String; Level : CuBit.Log_Records.Severity) is
      Made : constant CuBit.Log_Records.Decoded := CuBit.Log_Records.Make (Text, Level);
      Submitted : Boolean;
   begin
      debugPrint (Text & ASCII.LF);
      if Made.Success then
         CuBit.Logging.Emit (Log, Made.Value, Submitted);
      end if;
   end Line;

   --  One stage line; False stops the bring-up with its summary.
   function Stage (Name : String; OK : Boolean; Details : String) return Boolean is
   begin
      Line ("generic-check: stage " & Name & (if OK then " ok " else " fail ") & Details,
            (if OK then CuBit.Log_Records.Information else CuBit.Log_Records.Warning));
      if OK then
         Reached := Reached + 1;
      else
         Line ("generic-check: bring-up reached" & Natural'Image (Reached) & "/" &
               Natural'Image (TOTAL_STAGES), CuBit.Log_Records.Warning);
      end if;
      return OK;
   end Stage;

   function Register (Offset : Storage_Offset) return Unsigned_32 is
      Value : Unsigned_32 with Import, Volatile,
        Address => System'To_Address (REGISTER_VIRTUAL_BASE) + Offset;
   begin
      return Value;
   end Register;
   procedure Set_Register (Offset : Storage_Offset; Value : Unsigned_32) is
      Target : Unsigned_32 with Import, Volatile,
        Address => System'To_Address (REGISTER_VIRTUAL_BASE) + Offset;
   begin
      Target := Value;
   end Set_Register;

   function Register_64 (Offset : Storage_Offset) return Unsigned_64 is
      Value : Unsigned_64 with Import, Volatile,
        Address => System'To_Address (REGISTER_VIRTUAL_BASE) + Offset;
   begin
      return Value;
   end Register_64;
   procedure Set_Register_64 (Offset : Storage_Offset; Value : Unsigned_64) is
      Target : Unsigned_64 with Import, Volatile,
        Address => System'To_Address (REGISTER_VIRTUAL_BASE) + Offset;
   begin
      Target := Value;
   end Set_Register_64;

   --  One edu DMA of Count bytes, waited for (bounded): True when the
   --  engine reported it done.
   function Run_DMA (Source, Destination, Count, Direction : Unsigned_64) return Boolean is
      Deadline : constant Unsigned_64 := syscall (SYSCALL_GETTIME) + DMA_WAIT_MS;
      Ignore : Activity_Result;
   begin
      Set_Register_64 (EDU_DMA_SOURCE, Source);
      Set_Register_64 (EDU_DMA_DESTINATION, Destination);
      Set_Register_64 (EDU_DMA_COUNT, Count);
      Set_Register_64 (EDU_DMA_COMMAND, EDU_DMA_START or Direction);
      while (Register_64 (EDU_DMA_COMMAND) and EDU_DMA_START) /= 0 loop
         if syscall (SYSCALL_GETTIME) >= Deadline then
            return False;
         end if;
         Ignore := Wait_For_Activity_Until (syscall (SYSCALL_GETTIME) + DMA_POLL_MS);
      end loop;
      return True;
   end Run_DMA;

   procedure Report (Label : Unsigned_32) is
      Ignore : MessageTag;
   begin
      Ignore := capSend (Startup.Ready,
        (tag => (label => Label, length => 0, protocol => 0, ipc => <>, reserved => 0),
         authorityTag => 0, words => [others => 0]), CuBit.Messages.Wait_Forever);
   end Report;

   procedure Decline is
   begin
      Report (CuBit.Driver_Readiness.Not_Present_Label);
   end Decline;
begin
   CuBit.Launch_Configurations.Get (Config, Valid);
   if not Stage ("identify", Valid and then Config.Window_Total >= 1,
                 "device=" & Hex (Config.Vendor) & ":" & Hex (Config.Device) &
                 " revision=" & Hex (Config.Revision) & " class=" & Hex (Config.Class_Code) &
                 " windows=" & Natural'Image (Natural (Config.Window_Total)) &
                 " bar0=" & Hex (Config.Windows (1).First) & " bytes=" & Hex (Config.Windows (1).Bytes) &
                 " delivery=" & Natural'Image (LC.Interrupt_Delivery'Enum_Rep (Config.Delivery)) &
                 " vectors=" & Hex (Config.Vectors))
   then
      Decline;
      return;
   end if;
   if not Stage ("map", CuBit.Handles.Devices.Map_Device
                   (Startup.Registers, Config.Windows (1).First, REGISTER_VIRTUAL_BASE,
                    Config.Windows (1).Bytes / PAGE_BYTES, Read_Only => False),
                 "va=" & Hex (REGISTER_VIRTUAL_BASE))
   then
      Decline;
      return;
   end if;
   Set_Register (EDU_LIVENESS, LIVENESS_PATTERN);
   if not Stage ("registers",
                 (Register (EDU_IDENTIFICATION) and 16#FF#) = EDU_ID_LOW_BYTE
                 and then Register (EDU_LIVENESS) = not LIVENESS_PATTERN,
                 "id=" & Hex (Unsigned_64 (Register (EDU_IDENTIFICATION))) &
                 " liveness=" & Hex (Unsigned_64 (Register (EDU_LIVENESS))))
   then
      Decline;
      return;
   end if;
   if not Stage ("dma", Config.DMA.Bytes >= PAGE_BYTES and then Config.DMA_Virtual /= 0,
                 "first=" & Hex (Config.DMA.First) & " bytes=" & Hex (Config.DMA.Bytes) &
                 " va=" & Hex (Config.DMA_Virtual))
   then
      Decline;
      return;
   end if;
   --  RAM to the device and back through the DMA block: the device reaches
   --  its block (with an IOMMU: through its domain).
   declare
      Block : Storage_Array (0 .. DMA_RETURN_OFFSET + DMA_PROBE_BYTES - 1)
        with Import, Volatile, Address => System'To_Address (Config.DMA_Virtual);
      Below_Mask : constant Boolean :=
        Config.DMA.First < EDU_DMA_LIMIT
        and then Config.DMA.Bytes <= EDU_DMA_LIMIT - Config.DMA.First;
      Same : Boolean := True;
      Done : Boolean := True;
   begin
      if Below_Mask then
         for I in 0 .. Storage_Offset (DMA_PROBE_BYTES - 1) loop
            Block (I) := Storage_Element (Unsigned_8 (PROBE_PATTERN) xor Unsigned_8 (I));
            Block (DMA_RETURN_OFFSET + I) := 0;
         end loop;
         Done := Run_DMA (Config.DMA.First, EDU_BUFFER, DMA_PROBE_BYTES, 0)
           and then Run_DMA (EDU_BUFFER, Config.DMA.First + DMA_RETURN_OFFSET,
                             DMA_PROBE_BYTES, EDU_DMA_TO_RAM);
         for I in 0 .. Storage_Offset (DMA_PROBE_BYTES - 1) loop
            Same := Same and then Block (DMA_RETURN_OFFSET + I) = Block (I);
         end loop;
      end if;
      if not Stage ("dma-round-trip", Done and then Same,
                    (if Below_Mask then "bytes=" & Natural'Image (DMA_PROBE_BYTES) &
                       (if not Done then " engine timed out"
                        elsif not Same then " data differs" else " match")
                     else "skipped: block above the device's 28-bit DMA mask"))
      then
         Decline;
         return;
      end if;
   end;
   --  A read DMA from outside the block (the IOMMU fault probe).
   declare
      Probe : constant Unsigned_64 :=
        (if UNMAPPED_PROBE_ADDRESS >= Config.DMA.First
           and then UNMAPPED_PROBE_ADDRESS - Config.DMA.First < Config.DMA.Bytes
         then Config.DMA.First + Config.DMA.Bytes else UNMAPPED_PROBE_ADDRESS);
      Done : constant Boolean :=
        Probe < EDU_DMA_LIMIT
        and then Run_DMA (Probe, EDU_BUFFER + DMA_RETURN_OFFSET, DMA_PROBE_BYTES, 0);
   begin
      if not Stage ("dma-unmapped", Done,
                    "probe=" & Hex (Probe) &
                    " (with an IOMMU the kernel reports one DMA fault)")
      then
         Decline;
         return;
      end if;
   end;
   --  Ready: devmgr unmasks (MSI-X) and the device may interrupt now.
   Report (CuBit.Driver_Readiness.Ready_Label);
   declare
      Block : constant CuBit.Device_Interrupts.Interrupt_Block :=
        CuBit.Device_Interrupts.Block_Of (Startup.Device);
      Deadline : constant Unsigned_64 := syscall (SYSCALL_GETTIME) + INTERRUPT_WAIT_MS;
      Event : Message;
      Fired : Boolean := False;
      Status : Unsigned_32;
      Ignore : Activity_Result;
   begin
      Set_Register (EDU_INTERRUPT_RAISE, INTERRUPT_PATTERN);
      while not Fired and then syscall (SYSCALL_GETTIME) < Deadline loop
         while Poll_Event (Event) loop
            Fired := Fired or else
              (Block.Valid and then CuBit.Device_Interrupts.Is_Doorbell (Event)
               and then CuBit.Device_Interrupts.Fired (Event, Block, 0));
         end loop;
         if not Fired then
            Ignore := Wait_For_Activity_Until (Deadline);
         end if;
      end loop;
      Status := Register (EDU_INTERRUPT_STATUS);
      Set_Register (EDU_INTERRUPT_ACK, Status);
      if Stage ("irq", Fired and then Config.Delivery /= LC.Polled,
                "status=" & Hex (Unsigned_64 (Status)) & " vector=" &
                (if Block.Valid then Natural'Image (Natural (Block.Head)) else " none"))
      then
         Line ("generic-check: bring-up complete", CuBit.Log_Records.Information);
      end if;
   end;
   loop
      declare
         Ignore : constant Activity_Result := Wait_For_Activity_Until (Unsigned_64'Last);
      begin
         null;
      end;
   end loop;
end Main;
