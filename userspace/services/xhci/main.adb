------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  xHCI userspace driver entry point. devmgr starts one instance per
--  controller; each serves only its own controller's devices and publishes
--  their input under its own process identity (Desktop tells sources apart
--  by publisher, CuBit.Input.Decode).
------------------------------------------------------------------------------
with Interfaces; use Interfaces;

with CuBit.Messages; use CuBit.Messages;
with CuBit.Input; use CuBit.Input;
with CuBit.Devices;
with CuBit.Device_Interrupts;
with CuBit.Monotonic;
with Startup;
with CuBit.Launch_Configurations;
with Launch_Configuration;
with Boot_Log;
with XHCI;
with XHCI_Capabilities;
with XHCI_Configure_Reply;
with XHCI_Controller_Identity;
with Optical_Service;
with USB_Keyboards;
with Input_Pending;
with Pointer_Pending;
with Keyboard_Pending;

procedure main is
   use type XHCI.Init_Result;
   package CI renames XHCI_Controller_Identity;

   OP_XHCI_CONFIGURE : constant Unsigned_32 := 16#0220#;
   CONFIGURE_WORDS   : constant := 4;
   REPLY_OK          : constant Unsigned_32 := 16#F000#;
   REPLY_ERR         : constant Unsigned_32 := 16#F001#;
   PAGES_MASK        : constant Unsigned_64 := 16#FFFF_FFFF#;
   SCRATCHPAD_SHIFT  : constant := 32;
   STATISTICS_INTERVAL_MS : constant Unsigned_64 := 1_000;
   --  Report events between statistics checks (keeps clock reads out of
   --  the report hot path).
   STATISTICS_EVERY  : constant := 64;
   PS2_RELEASED      : constant Unsigned_8 := 16#80#;
   PS2_EXTENDED      : constant Unsigned_8 := 16#E0#;
   PS2_EXTENDED_FLAG : constant Unsigned_16 := 16#100#;
   USAGE_FIRST_KEY   : constant Unsigned_8 := 4;
   USAGE_LAST_KEY    : constant Unsigned_8 := 223;
   USAGE_FIRST_MODIFIER : constant Unsigned_8 := 224;
   USAGE_LAST_MODIFIER  : constant Unsigned_8 := 231;

   --  This controller's PCI location, "??:??.?" until devmgr says.
   Location : Launch_Configuration.Location_Text := "??:??.?";

   sender : Process_ID;
   msg    : Message;
   initResult : XHCI.Init_Result;
   ignore : Unsigned_64;
   mouseConsumer : Process_ID := No_Process;
   buttons : Unsigned_8;
   deltaX, deltaY, deltaZ : Integer;
   reportReady, eventAvailable : Boolean;
   interruptMode : XHCI.Runtime_Interrupt_Mode := XHCI.INTERRUPT_POLLING;
   interruptEnabled : Boolean := False;
   lastButtons : Unsigned_8 := 0;
   pointerPending : Pointer_Pending.State;
   pointerOverflowReported : Boolean := False;
   pointerNeedsSnapshot : Boolean := True;
   pointerOutcome : Pointer_Pending.Append_Outcome;
   --  Publication accounting (stats line): reports the kernel accepted for
   --  the registered consumer, merged by agreement, true retention
   --  overflows, and credit refusals (busy: kept and retried).
   pointerPublished, keyboardPublished : Unsigned_64 := 0;
   pointerCoalesced : Unsigned_64 := 0;
   pointerOverflows : Unsigned_64 := 0;
   publishBusy : Unsigned_64 := 0;
   diagnostics : XHCI.Boot_Mouse_Diagnostics;
   diagnosticsDue : Unsigned_64 := 0;
   diagnosticsCountdown : Natural := STATISTICS_EVERY;
   lastPublished : Unsigned_64 := Unsigned_64'Last;
   storageProgress, irqAvailable, serviceProgress : Boolean;
   activity : Activity_Result;
   keyboardState : USB_Keyboards.State;
   keyboardData : USB_Keyboards.Report;
   keyboardChanges : USB_Keyboards.Changes;
   keyboardResult : USB_Keyboards.Decode_Result;
   keyboardReady, keyboardProgress : Boolean;
   keyboardPending : Keyboard_Pending.State;
   keyboardConsumer : Process_ID := No_Process;
   keyboardOverflowReported : Boolean := False;
   keyboardPresent : Boolean := False;
   use type USB_Keyboards.Decode_Result;

   function Image (Value : Unsigned_64) return String is
      Text : constant String := Unsigned_64'Image (Value);
   begin
      return Text (Text'First + 1 .. Text'Last);
   end Image;

   procedure Refresh_Pointer_Consumer is
      previous : constant Process_ID := mouseConsumer;
   begin
      mouseConsumer := Registered_Driver (DRIVER_MOUSE);
      if previous /= mouseConsumer then
         Pointer_Pending.Reset (pointerPending);
         pointerNeedsSnapshot := True;
      end if;
   end Refresh_Pointer_Consumer;

   procedure Flush_Pointer is
      pending : Input_Pending.Item;
      report : Source_Report;
   begin
      for Attempt in 1 .. Input_Pending.Capacity loop
         exit when mouseConsumer = No_Process or else
           Pointer_Pending.Count (pointerPending) = 0;
         pending := Pointer_Pending.Element (pointerPending, 0);
         report :=
           (sourceIdentity => 0, sequence => pending.Sequence,
            generation => 1, device => RELATIVE_POINTER,
            delivery => ACCUMULABLE_DISPLACEMENT,
            flags => [RESYNCHRONIZE => pending.Recover],
            payload => pending.Payload,
            snapshot => Pointer_Snapshot (pending.Payload, pending.Observed_Ms));
         if not trySendEvent (mouseConsumer, Encode (report), Startup.Mouse_Events) then
            publishBusy := publishBusy + 1;
            exit;
         end if;
         pointerPublished := pointerPublished + 1;
         Pointer_Pending.Acknowledge (pointerPending);
      end loop;
   end Flush_Pointer;

   procedure Refresh_Keyboard_Consumer is
      Previous : constant Process_ID := keyboardConsumer;
   begin
      keyboardConsumer := Registered_Driver (DRIVER_KEYBOARD);
      if Previous /= keyboardConsumer then
         Keyboard_Pending.Reset_Consumer (keyboardPending);
      end if;
   end Refresh_Keyboard_Consumer;

   procedure Flush_Keyboard is
      Pending : Input_Pending.Item;
   begin
      for Attempt in 1 .. Input_Pending.Capacity loop
         exit when keyboardConsumer = No_Process or else Keyboard_Pending.Count (keyboardPending) = 0;
         Pending := Keyboard_Pending.Element (keyboardPending, 0);
         if not trySendEvent (keyboardConsumer, Encode
           (Source_Report'(sourceIdentity => 0, sequence => Pending.Sequence,
             generation => 1, device => KEYBOARD, delivery => ORDERED_TRANSITION,
             flags => [RESYNCHRONIZE => Pending.Recover], payload => Pending.Payload, snapshot => 0)),
           Startup.Keyboard_Events)
         then
            publishBusy := publishBusy + 1;
            exit;
         end if;
         keyboardPublished := keyboardPublished + 1;
         Keyboard_Pending.Acknowledge (keyboardPending);
      end loop;
   end Flush_Keyboard;

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
      procedure Send_Byte (Byte : Unsigned_8) is
         Added : Keyboard_Pending.Frame_Length;
         Lost : Boolean;
      begin
         Keyboard_Pending.Append_Byte (keyboardPending, Byte, Added, Lost);
         if Lost and then not keyboardOverflowReported then
            Boot_Log.Line ("xhci: " & Location & " keyboard retention overflow; resynchronizing", True);
            keyboardOverflowReported := True;
         end if;
      end Send_Byte;
   begin
      if Code = 0 then return; end if;
      if Code >= PS2_EXTENDED_FLAG then Send_Byte (PS2_EXTENDED); end if;
      Send_Byte (Unsigned_8 (Code and 16#FF#) or (if Released then PS2_RELEASED else 0));
      if keyboardConsumer = No_Process then Keyboard_Pending.Reset_Consumer (keyboardPending);
      else Flush_Keyboard; end if;
   end Send_Key;

   procedure Send_Changes is
   begin
      for K in Unsigned_8 loop
         if keyboardChanges.Released (K) then Send_Key (K, True); end if;
      end loop;
      for K in USAGE_FIRST_MODIFIER .. USAGE_LAST_MODIFIER loop
         if keyboardChanges.Pressed (K) then Send_Key (K, False); end if;
      end loop;
      for K in USAGE_FIRST_KEY .. USAGE_LAST_KEY loop
         if keyboardChanges.Pressed (K) then Send_Key (K, False); end if;
      end loop;
   end Send_Changes;

   --  Serial statistics once a second while reports flow (the QEMU HID
   --  regressions read them), and the replaceable snapshot for devmgr.
   procedure Maybe_Print_Diagnostics is
      now : constant Unsigned_64 := CuBit.Monotonic.Milliseconds;
      publish : Message;
   begin
      if now < diagnosticsDue then
         return;
      end if;
      diagnosticsDue := now + STATISTICS_INTERVAL_MS;
      diagnostics := XHCI.Mouse_Diagnostics;
      if pointerPublished + keyboardPublished = lastPublished then
         return;
      end if;
      lastPublished := pointerPublished + keyboardPublished;
      Boot_Log.Serial
        ("xhci: " & Location & " stats events=" & Image (diagnostics.transferEvents) &
         " reports=" & Image (diagnostics.decodedReports) &
         " motion=" & Image (diagnostics.motionReports) &
         " buttons=" & Image (diagnostics.buttonTransitions) &
         " errors=" & Image (diagnostics.completionErrors) &
         " short=" & Image (diagnostics.shortReports) &
         " other=" & Image (diagnostics.unexpectedEvents) &
         " cc=" & Image (Unsigned_64 (diagnostics.lastCompletion)) &
         " published=" & Image (pointerPublished) &
         " keys-published=" & Image (keyboardPublished) &
         " coalesced=" & Image (pointerCoalesced) &
         " overflow=" & Image (pointerOverflows) &
         " busy=" & Image (publishBusy) &
         " consumer=" & (if mouseConsumer = No_Process then "none" else "registered"));
      publish :=
        (tag => (label => CuBit.Devices.OP_PUBLISH_XHCI_STATS,
                 length => 3, protocol => 0, ipc => <>, reserved => 0),
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
      if not capSubmit (Startup.Ready, publish, NO_COMPLETION_TOKEN) then
         null;
      end if;
   end Maybe_Print_Diagnostics;

   procedure Reply_With
     (label : Unsigned_32;
      word0 : Unsigned_64 := 0;
      word1 : Unsigned_64 := 0;
      word2 : Unsigned_64 := 0)
   is
   begin
      ignore := reply
        (sender,
         (tag => (label => label, length => XHCI_Configure_Reply.Reply_Words,
                  protocol => 0, ipc => <>, reserved => 0),
          authorityTag => 0,
          words => [0 => word0, 1 => word1, 2 => word2, others => 0]));
   end Reply_With;

   function Earliest (A, B : Unsigned_64) return Unsigned_64 is (Unsigned_64'Min (A, B));

begin
   declare
      Configuration : Launch_Configuration.Controller_Configuration;
      Configured : Boolean;
   begin
      CuBit.Launch_Configurations.Get
        (Launch_Configuration.XHCI_Controller, Configuration, Configured);
      if Configured then
         Location := Launch_Configuration.Location_Image (Configuration.Location);
      end if;
   end;
   Boot_Log.Line ("xhci: " & Location & " stage start ok awaiting devmgr configuration");
   receive (sender, msg);

   if sender = No_Process or else
      sender /= Registered_Driver (DRIVER_DEVMGR) or else
      msg.tag.label /= OP_XHCI_CONFIGURE or else msg.tag.length /= CONFIGURE_WORDS or else
      Shift_Right (msg.words (1), SCRATCHPAD_SHIFT) >
        Unsigned_64 (XHCI_Capabilities.Scratchpad_Buffer_Count'Last) or else
      not CI.Well_Formed (msg.words (3))
   then
      Boot_Log.Line ("xhci: " & Location & " stage configure fail invalid message", True);
      Reply_With (REPLY_ERR);
      ignore := syscall (SYSCALL_EXIT);
      return;
   end if;

   --  Word 3: the interrupt mode devmgr programmed (MSI or MSI-X entry 0
   --  for the manifest's events slot) and the controller's PCI identity.
   case CI.Mode_Of (msg.words (3)) is
      when XHCI.Runtime_Interrupt_Mode'Enum_Rep (XHCI.INTERRUPT_POLLING) =>
         interruptMode := XHCI.INTERRUPT_POLLING;
      when XHCI.Runtime_Interrupt_Mode'Enum_Rep (XHCI.INTERRUPT_MSI) =>
         interruptMode := XHCI.INTERRUPT_MSI;
      when XHCI.Runtime_Interrupt_Mode'Enum_Rep (XHCI.INTERRUPT_MSIX) =>
         interruptMode := XHCI.INTERRUPT_MSIX;
      when others =>
         Boot_Log.Line ("xhci: " & Location & " stage configure fail interrupt mode", True);
         Reply_With (REPLY_ERR);
         ignore := syscall (SYSCALL_EXIT);
         return;
   end case;
   if interruptMode in XHCI.INTERRUPT_MSI | XHCI.INTERRUPT_MSIX and then
     not CuBit.Device_Interrupts.Block_Of (Startup.Events).Valid
   then
      Boot_Log.Line ("xhci: " & Location & " stage configure fail no interrupt capability; polling", True);
      interruptMode := XHCI.INTERRUPT_POLLING;
   end if;
   XHCI.Set_Identity (Location, CI.Identity_Of (msg.words (3)), interruptMode);

   XHCI.Initialize
     (barPhys  => msg.words (0),
      barPages => msg.words (1) and PAGES_MASK,
      dmaPhys  => msg.words (2),
      expectedScratchpads => Natural (Shift_Right (msg.words (1), SCRATCHPAD_SHIFT)),
      result   => initResult);

   if initResult /= XHCI.INIT_OK then
      Boot_Log.Line ("xhci: " & Location & " bring-up failed " &
                     "result=" & Image (Unsigned_64 (XHCI.Init_Result'Pos (initResult))) &
                     " (XHCI.Init_Result position)", True);
      Reply_With (REPLY_ERR, Unsigned_64 (XHCI.Init_Result'Pos (initResult)));
      --  Give the log grant a chance to carry the failure out.
      for Attempt in 1 .. 50 loop
         Boot_Log.Poll;
         ignore := syscall (SYSCALL_SLEEP, 20);
      end loop;
      ignore := syscall (SYSCALL_EXIT);
      return;
   end if;

   XHCI.Probe_Optical;
   XHCI.Start_Boot_Mouse_Transfers;
   XHCI.Start_Boot_Keyboard_Transfers;
   XHCI.Enable_Runtime_Interrupts (interruptMode, interruptEnabled);

   --  This controller's devices, one line naming it.
   Boot_Log.Line ("xhci: " & Location & " devices keyboard=" &
                  (if XHCI.Has_Keyboard then "1" else "0") & " mouse=" &
                  (if XHCI.Has_Mouse then "1" else "0") & " storage=" &
                  (if XHCI.Has_Storage then "1" else "0") & " ports=" &
                  Image (Unsigned_64 (XHCI.Port_Count)) & " connected=" &
                  Image (Unsigned_64 (XHCI.Connected_Port_Count)));
   Boot_Log.Line ("xhci: " & Location & " bring-up complete (" &
                  (if interruptEnabled then "interrupt-driven" else "polled") & ", hotplug armed)");
   --  For MSI-X this reply tells devmgr the interrupter is ready, so it
   --  may unmask the table entry and the function.
   Reply_With
     (REPLY_OK,
      Unsigned_64 (XHCI.Port_Count),
      Unsigned_64 (XHCI.Connected_Port_Count),
      XHCI_Configure_Reply.Device_Bits
        (Keyboard => XHCI.Has_Keyboard, Mouse => XHCI.Has_Mouse, Storage => XHCI.Has_Storage));
   keyboardPresent := XHCI.Has_Keyboard;

   loop
      --  The interrupt doorbell first: acknowledge, then drain (Service).
      irqAvailable := Poll_Event (msg);
      if irqAvailable and then CuBit.Device_Interrupts.Is_Doorbell (msg) then
         XHCI.Acknowledge_Runtime_Interrupt;
      end if;
      Boot_Log.Poll;
      XHCI.Service (serviceProgress);
      Optical_Service.Poll (storageProgress);

      --  A keyboard that went away releases every key it held.
      if keyboardPresent and then not XHCI.Has_Keyboard then
         USB_Keyboards.Release_All (keyboardState, keyboardChanges);
         Refresh_Keyboard_Consumer;
         Send_Changes;
      end if;
      keyboardPresent := XHCI.Has_Keyboard;

      XHCI.Poll_Boot_Keyboard (keyboardData, keyboardReady, keyboardProgress);
      if keyboardReady or else Keyboard_Pending.Count (keyboardPending) > 0 then
         Refresh_Keyboard_Consumer;
         Flush_Keyboard;
      end if;
      if keyboardReady then
         USB_Keyboards.Update (keyboardState, keyboardData, keyboardChanges, keyboardResult);
         if keyboardResult = USB_Keyboards.Decoded then
            Send_Changes;
         end if;
      end if;

      XHCI.Poll_Boot_Mouse (buttons, deltaX, deltaY, deltaZ, reportReady, eventAvailable);
      if reportReady or else Pointer_Pending.Count (pointerPending) > 0 then
         Refresh_Pointer_Consumer;
         Flush_Pointer;
      end if;
      if reportReady and then mouseConsumer /= No_Process and then
        (deltaX /= 0 or else deltaY /= 0 or else deltaZ /= 0 or else
         buttons /= lastButtons or else pointerNeedsSnapshot)
      then
         --  The desktop's event ABI uses PS/2 Y orientation (positive
         --  upward); USB HID uses positive downward.
         Pointer_Pending.Append
           (pointerPending,
            (Buttons => buttons, X => deltaX, Y => -deltaY, Wheel => deltaZ, Flags => 0),
            CuBit.Monotonic.Milliseconds, pointerOutcome);
         case pointerOutcome is
            when Pointer_Pending.Appended => null;
            when Pointer_Pending.Coalesced =>
               pointerCoalesced := pointerCoalesced + 1;
            when Pointer_Pending.Overflowed =>
               pointerOverflows := pointerOverflows + 1;
               if not pointerOverflowReported then
                  Boot_Log.Line ("xhci: " & Location & " pointer retention overflow; resynchronizing", True);
                  pointerOverflowReported := True;
               end if;
         end case;
         Flush_Pointer;
         pointerNeedsSnapshot := False;
         lastButtons := buttons;
      end if;

      if eventAvailable or else keyboardProgress then
         if diagnosticsCountdown = 0 then
            Maybe_Print_Diagnostics;
            diagnosticsCountdown := STATISTICS_EVERY;
         else
            diagnosticsCountdown := diagnosticsCountdown - 1;
         end if;
      end if;

      if not eventAvailable and then not keyboardProgress and then not storageProgress and then
         not irqAvailable and then not serviceProgress
      then
         --  Flush the last statistics of a burst before sleeping.
         Maybe_Print_Diagnostics;
         declare
            Now : constant Unsigned_64 := CuBit.Monotonic.Milliseconds;
            D : Unsigned_64 := Earliest (XHCI.Next_Deadline, XHCI.Optical_Deadline);
         begin
            D := Earliest (D, Boot_Log.Deadline);
            D := Earliest (D, diagnosticsDue);
            if Pointer_Pending.Count (pointerPending) > 0 then
               D := Pointer_Pending.Wake_Deadline (pointerPending, Now, D);
            end if;
            if Keyboard_Pending.Count (keyboardPending) > 0 then
               D := Keyboard_Pending.Wake_Deadline (keyboardPending, Now, D);
            end if;
            if interruptEnabled and then XHCI.Interrupts_Trusted then
               if D > Now then
                  activity := Wait_For_Activity_Until (D);
                  if activity = Unavailable then
                     ignore := syscall (SYSCALL_SLEEP, 1);
                  end if;
               end if;
            else
               --  Polled controller: the event ring every millisecond.
               ignore := syscall (SYSCALL_SLEEP, 1);
            end if;
         end;
      end if;
   end loop;
end main;
