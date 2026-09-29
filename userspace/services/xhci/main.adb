with Boot_Log;
------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  xHCI userspace driver entry point.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;

with CuBit.Messages; use CuBit.Messages;
with CuBit.Input; use CuBit.Input;
with CuBit.Devices;
with XHCI;
with XHCI_Capabilities;
with Optical_Service;
with USB_Keyboards;

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
   deltaZ  : Integer;
   reportReady : Boolean;
   eventAvailable : Boolean;
   interruptMode : XHCI.Runtime_Interrupt_Mode := XHCI.INTERRUPT_POLLING;
   interruptVector : Unsigned_8 := 0;
   interruptTableOffset : Unsigned_64 := 0;
   interruptEnabled : Boolean := False;
   interruptDriven : Boolean := False;
   lastButtons : Unsigned_8 := 0;
   pointerSequence : Source_Sequence := 0;
   pointerResyncPending : Boolean := False;
   packed : Unsigned_64;
   accepted : Boolean;
   inputReport : Source_Report;
   diagnostics : XHCI.Boot_Mouse_Diagnostics;
   diagnosticsStartMs : Unsigned_64 := 0;
   diagnosticsCountdown : Natural := 64;
   CAP_SLOT_DEVMGR : constant CapabilitySlot := 15;
   storageProgress, irqAvailable : Boolean;
   activity : Activity_Result;
   keyboardState : USB_Keyboards.State;
   keyboardData : USB_Keyboards.Report;
   keyboardChanges : USB_Keyboards.Changes;
   keyboardResult : USB_Keyboards.Decode_Result;
   keyboardReady, keyboardProgress : Boolean;
   keyboardSequence : Source_Sequence := 0;
   keyboardResync : Boolean := False;
   use type USB_Keyboards.Decode_Result;

   procedure Send_Key (Usage : Unsigned_8; Released : Boolean) is
      -- HID usage to existing desktop set-1 boundary; bit 8 means E0 prefix.
      Codes : constant array (Unsigned_8) of Unsigned_16 :=
        [4 => 30, 5 => 48, 6 => 46, 7 => 32, 8 => 18, 9 => 33, 10 => 34,
         11 => 35, 12 => 23, 13 => 36, 14 => 37, 15 => 38, 16 => 50, 17 => 49,
         18 => 24, 19 => 25, 20 => 16, 21 => 19, 22 => 31, 23 => 20, 24 => 22,
         25 => 47, 26 => 17, 27 => 45, 28 => 21, 29 => 44,
         30 => 2, 31 => 3, 32 => 4, 33 => 5, 34 => 6, 35 => 7, 36 => 8,
         37 => 9, 38 => 10, 39 => 11, 40 => 28, 41 => 1, 42 => 14, 43 => 15,
         44 => 57, 45 => 12, 46 => 13, 47 => 26, 48 => 27, 49 => 43,
         51 => 39, 52 => 40, 53 => 41, 54 => 51, 55 => 52, 56 => 53, 57 => 58,
         58 => 59, 59 => 60, 60 => 61, 61 => 62, 62 => 63, 63 => 64,
         64 => 65, 65 => 66, 66 => 67, 67 => 68, 68 => 87, 69 => 88,
         73 => 16#152#, 74 => 16#147#, 75 => 16#149#, 76 => 16#153#,
         77 => 16#14F#, 78 => 16#151#, 79 => 16#14D#, 80 => 16#14B#,
         81 => 16#150#, 82 => 16#148#,
         224 => 29, 225 => 42, 226 => 56, 227 => 16#15B#,
         228 => 16#11D#, 229 => 54, 230 => 16#138#, 231 => 16#15C#, others => 0];
      Code : constant Unsigned_16 := Codes (Usage);
      Consumer : constant Unsigned_64 := getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_KEYBOARD);
      procedure Send_Byte (Byte : Unsigned_8) is
         OK : Boolean;
      begin
         keyboardSequence := Next_Sequence (keyboardSequence);
         OK := trySendEvent (Consumer, Encode
           (Source_Report'(sourceAuthorityTag => 0, sequence => keyboardSequence,
             generation => 1, device => KEYBOARD, delivery => ORDERED_TRANSITION,
             flags => [RESYNCHRONIZE => keyboardResync],
             payload => Unsigned_64 (Byte), snapshot => 0)));
         keyboardResync := not OK;
      end Send_Byte;
   begin
      if Code = 0 or else Consumer = 0 then return; end if;
      if Code > 255 then Send_Byte (16#E0#); end if;
      Send_Byte (Unsigned_8 (Code and 255) or (if Released then 16#80# else 0));
   end Send_Key;

   procedure Print_Decimal (value : Unsigned_64) is
      text : String (1 .. 20);
      first : Natural := text'Last;
      remaining : Unsigned_64 := value;
   begin
      if remaining = 0 then
         Boot_Log.Write ("0");
         return;
      end if;
      while remaining > 0 loop
         text (first) := Character'Val
           (Character'Pos ('0') + Natural (remaining mod 10));
         remaining := remaining / 10;
         first := first - 1;
      end loop;
      Boot_Log.Write (text (first + 1 .. text'Last));
   end Print_Decimal;

   procedure Print_Hex32 (value : Unsigned_32) is
      hexChars : constant String := "0123456789ABCDEF";
      text : String (1 .. 8);
   begin
      for i in text'Range loop
         text (i) := hexChars
           (Natural (Shift_Right (value, (text'Last - i) * 4) and 16#F#) + 1);
      end loop;
      Boot_Log.Write (text);
   end Print_Hex32;

   procedure Maybe_Print_Diagnostics is
      now : constant Unsigned_64 := syscall (SYSCALL_GETTIME);
      publish : Message;
   begin
      if now = Unsigned_64'Last then
         return;
      elsif diagnosticsStartMs = 0 then
         diagnosticsStartMs := now;
         return;
      elsif now < diagnosticsStartMs or else now - diagnosticsStartMs < 1000
      then
         return;
      end if;

      diagnostics := XHCI.Mouse_Diagnostics;
      Boot_Log.Write ("xhci: stats events=");
      Print_Decimal (diagnostics.transferEvents);
      Boot_Log.Write (" reports=");
      Print_Decimal (diagnostics.decodedReports);
      Boot_Log.Write (" motion=");
      Print_Decimal (diagnostics.motionReports);
      Boot_Log.Write (" buttons=");
      Print_Decimal (diagnostics.buttonTransitions);
      Boot_Log.Write (" errors=");
      Print_Decimal (diagnostics.completionErrors);
      Boot_Log.Write (" short=");
      Print_Decimal (diagnostics.shortReports);
      Boot_Log.Write (" other=");
      Print_Decimal (diagnostics.unexpectedEvents);
      Boot_Log.Write (" raw=");
      Print_Hex32 (diagnostics.lastReport);
      Boot_Log.Write (" len=");
      Print_Decimal (Unsigned_64 (diagnostics.lastLength));
      Boot_Log.Write (" cc=");
      Print_Decimal (Unsigned_64 (diagnostics.lastCompletion));
      Boot_Log.Write (LF & "");
      publish :=
        (tag => (label => CuBit.Devices.OP_PUBLISH_XHCI_STATS,
                 length => 3, flags => 0, reserved => 0),
         authorityTag => 0,
         words =>
           [0 => (diagnostics.decodedReports and 16#FFFF_FFFF#) or
              Shift_Left (diagnostics.motionReports and 16#FFFF_FFFF#, 32),
            1 => (diagnostics.buttonTransitions and 16#FFFF_FFFF#) or
              Shift_Left (diagnostics.completionErrors and 16#FFFF_FFFF#, 32),
            2 => Unsigned_64 (diagnostics.lastReport) or
              Shift_Left (Unsigned_64 (diagnostics.lastLength), 32) or
              Shift_Left (Unsigned_64 (diagnostics.lastCompletion), 40) or
              Shift_Left
                (Unsigned_64
                   (XHCI.Runtime_Interrupt_Mode'Enum_Rep (interruptMode)),
                 48),
            3 => 0]);
      --  Input diagnostics must never wait for devmgr. A full destination
      --  ring merely loses this replaceable snapshot; the next one follows.
      if not capSubmit
        (CAP_SLOT_DEVMGR, publish, NO_COMPLETION_TOKEN)
      then
         null;
      end if;
      diagnosticsStartMs := now;
   end Maybe_Print_Diagnostics;

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
         (tag => (label => label, length => 2, flags => 0, reserved => 0),
          authorityTag => 0,
          words => [0 => word0, 1 => word1, others => 0]));
   end Reply_With;

begin
   Boot_Log.Write ("xhci: awaiting bounded controller authority" & LF);
   receive (sender, msg);

   if sender = 0 or else
      sender /= getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_DEVMGR) or else
      msg.tag.label /= OP_XHCI_CONFIGURE or else msg.tag.length /= 4 or else
      Shift_Right (msg.words (1), 32) >
        Unsigned_64 (XHCI_Capabilities.Scratchpad_Buffer_Count'Last) then
      Boot_Log.Write ("xhci: invalid configuration message" & LF);
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
         Boot_Log.Write ("xhci: invalid interrupt mode" & LF);
         Reply_With (REPLY_ERR);
         ignore := syscall (SYSCALL_EXIT);
         return;
   end case;
   interruptVector :=
     Unsigned_8 (Shift_Right (msg.words (3), 8) and 16#FF#);
   interruptTableOffset := Shift_Right (msg.words (3), 16);

   XHCI.Initialize
     (barPhys  => msg.words (0),
      barPages => msg.words (1) and 16#FFFF_FFFF#,
      dmaPhys  => msg.words (2),
      expectedScratchpads => Natural (Shift_Right (msg.words (1), 32)),
      result   => initResult);

   if initResult /= XHCI.INIT_OK then
      Boot_Log.Write ("xhci: controller initialization failed: ");
      case initResult is
         when XHCI.INIT_OK =>
            Boot_Log.Write ("unexpected-success-state");
         when XHCI.INIT_MAP_FAILED =>
            Boot_Log.Write ("map-failed");
         when XHCI.INIT_BAD_CAPABILITY =>
            Boot_Log.Write ("bad-capability-registers");
         when XHCI.INIT_PAGE_SIZE_UNSUPPORTED =>
            Boot_Log.Write ("4k-page-size-unsupported");
         when XHCI.INIT_DMA_LAYOUT_MISMATCH =>
            Boot_Log.Write ("dma-layout-mismatch");
         when XHCI.INIT_FIRMWARE_HANDOFF_FAILED =>
            Boot_Log.Write ("firmware-handoff-failed");
         when XHCI.INIT_STOP_TIMEOUT =>
            Boot_Log.Write ("stop-timeout");
         when XHCI.INIT_RESET_TIMEOUT =>
            Boot_Log.Write ("reset-timeout");
         when XHCI.INIT_START_TIMEOUT =>
            Boot_Log.Write ("start-timeout");
         when XHCI.INIT_NO_DEVICE =>
            Boot_Log.Write ("no-connected-device");
         when XHCI.INIT_PORT_RESET_TIMEOUT =>
            Boot_Log.Write ("port-reset-timeout");
         when XHCI.INIT_COMMAND_TIMEOUT =>
            Boot_Log.Write ("command-timeout");
         when XHCI.INIT_COMMAND_FAILED =>
            Boot_Log.Write ("command-failed");
         when XHCI.INIT_ADDRESS_FAILED =>
            Boot_Log.Write ("address-device-failed");
         when XHCI.INIT_DESCRIPTOR_FAILED =>
            Boot_Log.Write ("descriptor-failed");
         when XHCI.INIT_NOT_BOOT_MOUSE =>
            Boot_Log.Write ("not-a-boot-mouse");
         when XHCI.INIT_CONFIGURE_FAILED =>
            Boot_Log.Write ("configure-endpoint-failed");
      end case;
      Boot_Log.Write (LF & "");
      Reply_With
        (REPLY_ERR,
         Unsigned_64 (XHCI.Init_Result'Pos (initResult)));
      ignore := syscall (SYSCALL_EXIT);
      return;
   end if;

   Boot_Log.Write ("xhci: controller running; root ports=");
   Print_Decimal (Unsigned_64 (XHCI.Port_Count));
   Boot_Log.Write (" connected=");
   Print_Decimal (Unsigned_64 (XHCI.Connected_Port_Count));
   Boot_Log.Write (LF & "");
   Boot_Log.Write ("xhci: enabled slot=");
   declare
      digit : constant Character :=
        Character'Val (Character'Pos ('0') + XHCI.Device_Slot mod 10);
   begin
      Boot_Log.Write (String'(1 => digit));
   end;
   Boot_Log.Write (LF & "");

   XHCI.Probe_Optical;
   XHCI.Start_Boot_Mouse_Transfers;
   XHCI.Start_Boot_Keyboard_Transfers;
   XHCI.Enable_Runtime_Interrupts
     (interruptMode,
      interruptVector,
      interruptTableOffset,
      interruptEnabled);
   interruptDriven := interruptEnabled;
   if interruptEnabled then
      Boot_Log.Write ("xhci: interrupt-driven HID input enabled" & LF);
   else
      Boot_Log.Write ("xhci: HID input using queued polling fallback" & LF);
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
      Boot_Log.Poll;
      Optical_Service.Poll (storageProgress);
      XHCI.Poll_Boot_Keyboard (keyboardData, keyboardReady, keyboardProgress);
      if keyboardReady then
         USB_Keyboards.Update (keyboardState, keyboardData, keyboardChanges, keyboardResult);
         if keyboardResult = USB_Keyboards.Decoded then
            for K in Unsigned_8 loop
               if keyboardChanges.Released (K) then Send_Key (K, True); end if;
            end loop;
            for K in Unsigned_8 range 224 .. 231 loop
               if keyboardChanges.Pressed (K) then Send_Key (K, False); end if;
            end loop;
            for K in Unsigned_8 range 4 .. 223 loop
               if keyboardChanges.Pressed (K) then Send_Key (K, False); end if;
            end loop;
         end if;
      end if;
      XHCI.Poll_Boot_Mouse
        (buttons, deltaX, deltaY, deltaZ, reportReady, eventAvailable);
      if reportReady then
         mouseConsumer := getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_MOUSE);
         if mouseConsumer /= 0 and then
            (deltaX /= 0 or else deltaY /= 0 or else deltaZ /= 0 or else
             buttons /= lastButtons)
         then
            --  The desktop's existing event ABI uses PS/2 Y orientation
            --  (positive upward); USB HID uses positive downward.
            packed := Unsigned_64 (buttons) or
              Shift_Left (Pack_Signed_12 (deltaX), 8) or
              Shift_Left (Pack_Signed_12 (-deltaY), 20) or
              Shift_Left (Unsigned_64 (deltaZ mod 256), 32);
            pointerSequence := Next_Sequence (pointerSequence);
            inputReport :=
              (sourceAuthorityTag => 0,
               sequence    => pointerSequence,
               generation  => 1,
               device      => RELATIVE_POINTER,
               delivery    => ACCUMULABLE_DISPLACEMENT,
               flags       => [RESYNCHRONIZE => pointerResyncPending],
               payload     => packed,
               snapshot    => Unsigned_64 (buttons));
            accepted := trySendEvent
              (mouseConsumer, Encode (inputReport));
            pointerResyncPending := not accepted;
            lastButtons := buttons;
         end if;
      end if;

      irqAvailable := Poll_Event (msg);
      if irqAvailable then
         XHCI.Acknowledge_Runtime_Interrupt;
      end if;
      if not eventAvailable and then not keyboardProgress and then not storageProgress and then
         not irqAvailable
      then
         if interruptDriven then
            --  Wake for either a storage request or an IRQ, with a bounded
            --  timeout for the outstanding USB command. Never poll on a timer.
            declare
               D : Unsigned_64 := XHCI.Optical_Deadline;
            begin
               if Boot_Log.Deadline /= 0 and then (D = 0 or else Boot_Log.Deadline < D) then
                  D := Boot_Log.Deadline;
               end if;
               activity := Wait_For_Activity_Until (D);
            end;
         else
            ignore := syscall (SYSCALL_SLEEP, 1);
         end if;
      end if;
      --  Keep time queries and formatted diagnostics out of the report hot
      --  path.  At 125 Hz this checks roughly twice per second; at 1000 Hz it
      --  checks often enough to retain one-second aggregate visibility.
      if eventAvailable then
         if diagnosticsCountdown = 0 then
            Maybe_Print_Diagnostics;
            diagnosticsCountdown := 64;
         else
            diagnosticsCountdown := diagnosticsCountdown - 1;
         end if;
      end if;
   end loop;
end main;
