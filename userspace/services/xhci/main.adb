------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  xHCI userspace driver entry point.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;

with CuBit.Messages; use CuBit.Messages;
with XHCI;

procedure main is
   use ASCII;
   use type XHCI.Init_Result;

   OP_XHCI_CONFIGURE : constant Unsigned_32 := 16#0220#;
   REPLY_OK           : constant Unsigned_32 := 16#F000#;
   REPLY_ERR          : constant Unsigned_32 := 16#F001#;

   sender : ProcessID;
   msg    : Message;
   initResult : XHCI.Init_Result;
   ignore : Unsigned_64;
   mouseConsumer : Unsigned_64;
   buttons : Unsigned_8;
   deltaX  : Integer;
   deltaY  : Integer;
   reportReady : Boolean;
   eventAvailable : Boolean;
   interruptMode : XHCI.Runtime_Interrupt_Mode := XHCI.INTERRUPT_POLLING;
   interruptVector : Unsigned_8 := 0;
   interruptTableOffset : Unsigned_64 := 0;
   interruptEnabled : Boolean := False;
   interruptDriven : Boolean := False;
   lastButtons : Unsigned_8 := 0;
   packed : Unsigned_64;

   function Pack_Signed_12 (value : Integer) return Unsigned_64 is
   begin
      if value < 0 then
         return Unsigned_64 (4096 + value) and 16#FFF#;
      end if;
      return Unsigned_64 (value) and 16#FFF#;
   end Pack_Signed_12;

   procedure Reply_With
     (label : Unsigned_32;
      word0 : Unsigned_64 := 0;
      word1 : Unsigned_64 := 0)
   is
   begin
      ignore := replyCap
        (CapabilitySlot'Last,
         (tag => (label => label, length => 2, flags => 0, badge => 0),
          capBadge => 0,
          words => (0 => word0, 1 => word1, others => 0)));
   end Reply_With;

begin
   debugPrint ("xhci: awaiting bounded controller authority" & LF);
   receive (sender, msg);

   if msg.tag.label /= OP_XHCI_CONFIGURE or else msg.tag.length < 4 then
      debugPrint ("xhci: invalid configuration message" & LF);
      Reply_With (REPLY_ERR);
      ignore := syscall (SYSCALL_EXIT);
      return;
   end if;

   case msg.words (3) and 16#FF# is
      when 0 =>
         interruptMode := XHCI.INTERRUPT_POLLING;
      when 1 =>
         interruptMode := XHCI.INTERRUPT_MSI;
      when 2 =>
         interruptMode := XHCI.INTERRUPT_MSIX;
      when others =>
         debugPrint ("xhci: invalid interrupt mode" & LF);
         Reply_With (REPLY_ERR);
         ignore := syscall (SYSCALL_EXIT);
         return;
   end case;
   interruptVector :=
     Unsigned_8 (Shift_Right (msg.words (3), 8) and 16#FF#);
   interruptTableOffset := Shift_Right (msg.words (3), 16);

   XHCI.Initialize
     (barPhys  => msg.words (0),
      barPages => msg.words (1),
      dmaPhys  => msg.words (2),
      result   => initResult);

   if initResult /= XHCI.INIT_OK then
      debugPrint ("xhci: controller initialization failed: ");
      case initResult is
         when XHCI.INIT_OK =>
            debugPrint ("unexpected-success-state");
         when XHCI.INIT_MAP_FAILED =>
            debugPrint ("map-failed");
         when XHCI.INIT_BAD_CAPABILITY =>
            debugPrint ("bad-capability-registers");
         when XHCI.INIT_PAGE_SIZE_UNSUPPORTED =>
            debugPrint ("4k-page-size-unsupported");
         when XHCI.INIT_SCRATCHPAD_LIMIT =>
            debugPrint ("scratchpad-limit");
         when XHCI.INIT_STOP_TIMEOUT =>
            debugPrint ("stop-timeout");
         when XHCI.INIT_RESET_TIMEOUT =>
            debugPrint ("reset-timeout");
         when XHCI.INIT_START_TIMEOUT =>
            debugPrint ("start-timeout");
         when XHCI.INIT_NO_DEVICE =>
            debugPrint ("no-connected-device");
         when XHCI.INIT_PORT_RESET_TIMEOUT =>
            debugPrint ("port-reset-timeout");
         when XHCI.INIT_COMMAND_TIMEOUT =>
            debugPrint ("command-timeout");
         when XHCI.INIT_COMMAND_FAILED =>
            debugPrint ("command-failed");
         when XHCI.INIT_ADDRESS_FAILED =>
            debugPrint ("address-device-failed");
         when XHCI.INIT_DESCRIPTOR_FAILED =>
            debugPrint ("descriptor-failed");
         when XHCI.INIT_NOT_BOOT_MOUSE =>
            debugPrint ("not-a-boot-mouse");
         when XHCI.INIT_CONFIGURE_FAILED =>
            debugPrint ("configure-endpoint-failed");
      end case;
      debugPrint (LF & "");
      Reply_With
        (REPLY_ERR,
         Unsigned_64 (XHCI.Init_Result'Pos (initResult)));
      ignore := syscall (SYSCALL_EXIT);
      return;
   end if;

   debugPrint ("xhci: controller running; root ports=");
   declare
      digit : constant Character :=
        Character'Val (Character'Pos ('0') + XHCI.Port_Count mod 10);
   begin
      debugPrint (String'(1 => digit));
   end;
   debugPrint (" connected=");
   declare
      digit : constant Character :=
        Character'Val
          (Character'Pos ('0') + XHCI.Connected_Port_Count mod 10);
   begin
      debugPrint (String'(1 => digit));
   end;
   debugPrint (LF & "");
   debugPrint ("xhci: enabled slot=");
   declare
      digit : constant Character :=
        Character'Val (Character'Pos ('0') + XHCI.Device_Slot mod 10);
   begin
      debugPrint (String'(1 => digit));
   end;
   debugPrint (LF & "");

   XHCI.Start_Boot_Mouse_Transfers;
   XHCI.Enable_Runtime_Interrupts
     (interruptMode,
      interruptVector,
      interruptTableOffset,
      interruptEnabled);
   interruptDriven := interruptEnabled;
   if interruptEnabled then
      debugPrint ("xhci: interrupt-driven HID input enabled" & LF);
   else
      debugPrint ("xhci: HID input using queued polling fallback" & LF);
   end if;

   --  For MSI-X this reply tells devmgr that the table entry and xHCI
   --  interrupter are ready, so it may safely release the PCI function mask.
   Reply_With
     (REPLY_OK,
      Unsigned_64 (XHCI.Port_Count),
      Unsigned_64 (XHCI.Connected_Port_Count));

   --  Keep the controller's MMIO and DMA authority private.  For this first
   --  vertical slice, translate boot reports to the existing desktop mouse
   --  event ABI.  A dedicated typed usb-hid service endpoint will replace
   --  this legacy driver lookup as the service boundary is split out.
   loop
      XHCI.Poll_Boot_Mouse
        (buttons, deltaX, deltaY, reportReady, eventAvailable);
      if reportReady then
         mouseConsumer := getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_MOUSE);
         if mouseConsumer /= 0 and then
            (deltaX /= 0 or else deltaY /= 0 or else buttons /= lastButtons)
         then
            --  The desktop's existing event ABI uses PS/2 Y orientation
            --  (positive upward); USB HID uses positive downward.
            packed := Unsigned_64 (buttons) or
              Shift_Left (Pack_Signed_12 (deltaX), 8) or
              Shift_Left (Pack_Signed_12 (-deltaY), 20);
            sendEvent
              (mouseConsumer,
               (tag => (label => 2, length => 1, flags => 0, badge => 0),
                capBadge => 0,
                words => (0 => packed, others => 0)));
            lastButtons := buttons;
         end if;
      end if;

      if not eventAvailable then
         if interruptDriven then
            --  IRQ delivery and controller acknowledgement are separate:
            --  Wait_Event consumes only the kernel notification, while xHCI
            --  event-ring dequeue pointers are advanced by Poll_Boot_Mouse.
            msg := Wait_Event;
            XHCI.Acknowledge_Runtime_Interrupt;
         else
            ignore := syscall (SYSCALL_SLEEP, 1);
         end if;
      end if;
   end loop;
end main;
