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
--  its DMA region, and one MSI raised by the device after OP_READY.
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
   TOTAL_STAGES : constant := 5;

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
