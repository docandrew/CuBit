------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  xHCI userspace driver entry point. devmgr starts one instance per
--  controller; each serves only its own controller's devices and publishes
--  their input under its own process identity (Desktop tells sources apart
--  by publisher, CuBit.Input.Decode).
------------------------------------------------------------------------------
with CuBit.Waits;
with Interfaces; use Interfaces;

with CuBit.Messages; use CuBit.Messages;
with CuBit.Input; use CuBit.Input;
with CuBit.Devices;
with CuBit.Device_Interrupts;
with CuBit.Monotonic;
with Startup;
with Input_Urgency;
with CuBit.Launch_Configurations;
with Launch_Configuration;
with Boot_Log;
with CuBit.Console_Route;
with XHCI;
with XHCI_Capabilities;
with XHCI_DMA_Layout;
with XHCI_Bring_Up_Report;
with XHCI_Controller_Identity;
with Storage_Requests;
with USB_Keyboards;
with Pointer_Pending;
with Input_Sources;
with CuBit.Control_Events;
with CuBit.Disk_Presence;

procedure main is
   use type XHCI.Init_Result;
   package CI renames XHCI_Controller_Identity;

   OP_XHCI_CONFIGURE : constant Unsigned_32 := 16#0220#;
   CONFIGURE_WORDS   : constant := 4;
   REPLY_OK          : constant Unsigned_32 := 16#F000#;
   REPLY_ERR         : constant Unsigned_32 := 16#F001#;
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
   buttons : Unsigned_8;
   deltaX, deltaY, deltaZ : Integer;
   reportReady, eventAvailable : Boolean;
   interruptMode : XHCI.Runtime_Interrupt_Mode := XHCI.INTERRUPT_POLLING;
   interruptEnabled : Boolean := False;
   lastButtons : Unsigned_8 := 0;
   pointerNeedsSnapshot : Boolean := True;
   --  The boot keyboard and mouse as input sources (DEV-002): each its own
   --  ring to Desktop, open while the device is attached.
   package Keyboards renames Input_Sources.Keyboards;
   package Pointers renames Input_Sources.Relative_Pointers;
   Keyboard_Source : Keyboards.Source;
   Mouse_Source : Pointers.Source;
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
   keyboardPresent : Boolean := False;
   mousePresent : Boolean := False;
   use type USB_Keyboards.Decode_Result;

   function Image (Value : Unsigned_64) return String is
      Text : constant String := Unsigned_64'Image (Value);
   begin
      return Text (Text'First + 1 .. Text'Last);
   end Image;

   --  Each attached device's source looks for Desktop (through devmgr)
   --  and pushes what it retained; a detached one has no ring.
   procedure Service_Sources is
   begin
      if keyboardPresent then
         Keyboards.Service (Keyboard_Source, Startup.Ready);
      end if;
      if mousePresent then
         Pointers.Service (Mouse_Source, Startup.Ready);
      end if;
   end Service_Sources;

   procedure Note_Source_Event (Event : Message) is
      Item : constant CuBit.Control_Events.Event :=
        CuBit.Control_Events.Decode
          (Event.tag.label, Event.tag.length, Event.words (0), Event.words (1), Event.words (2));
      Ended : Boolean;
   begin
      Keyboards.Note_Event (Keyboard_Source, Item, Ended);
      Pointers.Note_Event (Mouse_Source, Item, Ended);
   end Note_Source_Event;

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
      --  The acquisition stamp (LAT-002): the report was decoded just now.
      Stamp : constant Unsigned_64 := CuBit.Monotonic.Microseconds;
      procedure Send_Byte (Byte : Unsigned_8) is
      begin
         Keyboards.Publish (Keyboard_Source, Byte, Stamp);
      end Send_Byte;
   begin
      if Code = 0 then return; end if;
      if Code >= PS2_EXTENDED_FLAG then Send_Byte (PS2_EXTENDED); end if;
      Send_Byte (Unsigned_8 (Code and 16#FF#) or (if Released then PS2_RELEASED else 0));
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
      if Pointers.Published (Mouse_Source) + Keyboards.Published (Keyboard_Source) = lastPublished then
         return;
      end if;
      lastPublished := Pointers.Published (Mouse_Source) + Keyboards.Published (Keyboard_Source);
      Boot_Log.Debug
        ("xhci: " & Location & " stats events=" & Image (diagnostics.transferEvents) &
         " reports=" & Image (diagnostics.decodedReports) &
         " motion=" & Image (diagnostics.motionReports) &
         " buttons=" & Image (diagnostics.buttonTransitions) &
         " errors=" & Image (diagnostics.completionErrors) &
         " short=" & Image (diagnostics.shortReports) &
         " other=" & Image (diagnostics.unexpectedEvents) &
         " cc=" & Image (Unsigned_64 (diagnostics.lastCompletion)) &
         " published=" & Image (Pointers.Published (Mouse_Source)) &
         " keys-published=" & Image (Keyboards.Published (Keyboard_Source)) &
         " overflow=" & Image (Pointers.Overflows (Mouse_Source)) &
         " keys-overflow=" & Image (Keyboards.Overflows (Keyboard_Source)) &
         " ring=" & (if Pointers.Established (Mouse_Source) then "open" else "none") &
         " keys-ring=" & (if Keyboards.Established (Keyboard_Source) then "open" else "none"));
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

   --  devmgr's configure call answered: REPLY_OK accepted, REPLY_ERR
   --  refused (XHCI_Bring_Up_Report).
   procedure Reply_With (label : Unsigned_32) is
   begin
      ignore := reply
        (sender,
         (tag => (label => label, length => 0, protocol => 0, ipc => <>, reserved => 0),
          authorityTag => 0,
          words => [others => 0]));
   end Reply_With;

   --  The bring-up's outcome to devmgr (XHCI_Bring_Up_Report), once. A
   --  synchronous send: this call goes down the IPC DAG; devmgr answers it
   --  before acting on it and calls nothing here meanwhile.
   procedure Report_Bring_Up
     (Label : Unsigned_32;
      Word0 : Unsigned_64 := 0;
      Word1 : Unsigned_64 := 0;
      Word2 : Unsigned_64 := 0)
   is
      Tag : MessageTag;
   begin
      Tag := capSend
        (Startup.Ready,
         (tag => (label => Label, length => XHCI_Bring_Up_Report.Report_Words,
                  protocol => 0, ipc => <>, reserved => 0),
          authorityTag => 0,
          words => [0 => Word0, 1 => Word1, 2 => Word2, others => 0]),
         Wait_Forever);
      if Tag /= NULL_TAG then
         Boot_Log.Line ("xhci: " & Location & " stage report fail devmgr did not take it", True);
      end if;
   end Report_Bring_Up;

   function Earliest (A, B : Unsigned_64) return Unsigned_64 is (Unsigned_64'Min (A, B));

   --  FS-034: devmgr learns of each disk that came or went by a report
   --  submitted to its endpoint, never by a call (devmgr calls us; the
   --  synchronous graph stays a DAG). One report is in flight at a time,
   --  and it counts as delivered only when devmgr answers it REPLY_OK (its
   --  completion): one the kernel did not take (devmgr's queue full), or
   --  that devmgr's startup received without answering (REPLY_ENDED), stays
   --  owed and is offered again after Report_Retry_Ms. Nothing is dropped
   --  (XHCI.Next_Report).
   Report_Retry_Ms : constant Unsigned_64 := 10;
   --  devmgr refused it (it does not take reports from this process yet).
   Report_Refused_Retry_Ms : constant Unsigned_64 := 1_000;
   Report_Token : constant Unsigned_64 := 16#D15C_0000_0001#;
   reportRetryAt : Unsigned_64 := 0;
   reportInFlight : Boolean := False;
   reportItem : CuBit.Disk_Presence.Report;

   procedure Send_Disk_Reports (Progressed : out Boolean) is
      package DP renames CuBit.Disk_Presence;
      Due : Boolean;
      Words : DP.Words;
      Done : CompletionEntry;
   begin
      Progressed := False;
      if reportInFlight then
         if Poll_Completion (Done'Address) = 0 then
            return;
         end if;
         Progressed := True;
         if Done.token /= Report_Token then
            return;   --  no other completions here
         end if;
         reportInFlight := False;
         if Done.status = COMPLETION_OK and then Done.msg.tag.label = REPLY_OK then
            XHCI.Report_Sent;
            Boot_Log.Line ("xhci: " & Location & " disk unit" & reportItem.Unit'Image & " reported " &
                           (case reportItem.State is when DP.Present => "present", when DP.Gone => "gone"));
         else
            reportRetryAt := CuBit.Monotonic.Milliseconds +
              (if Done.status = COMPLETION_OK and then Done.msg.tag.label = REPLY_ERR
               then Report_Refused_Retry_Ms else Report_Retry_Ms);
         end if;
         return;
      end if;
      if reportRetryAt /= 0 and then CuBit.Monotonic.Milliseconds < reportRetryAt then
         return;
      end if;
      reportRetryAt := 0;
      XHCI.Next_Report (Due, reportItem);
      if not Due or else not DP.Well_Formed (reportItem) then
         return;
      end if;
      Words := DP.Encode (reportItem);
      if capSubmit
        (Startup.Ready,
         (tag => (label => DP.Report_Label, length => DP.Report_Words,
                  protocol => 0, ipc => <>, reserved => 0),
          authorityTag => 0,
          words => [0 => Words (DP.Unit_Word), 1 => Words (DP.State_Word), others => 0]),
         Report_Token)
      then
         reportInFlight := True;
         Progressed := True;
      else
         reportRetryAt := CuBit.Monotonic.Milliseconds + Report_Retry_Ms;
      end if;
   end Send_Disk_Reports;

begin
   --  ABI-002: its lines into its logstore ring, never the kernel console;
   --  queued until devmgr introduces logstore (CuBit.Late_Introduction).
   CuBit.Console_Route.Route_Named ("logstore");
   --  Stop (devmgr's, at shutdown): nothing to finish; the runtime ends
   --  the process when a receive returns it (CuBit.Messages).
   CuBit.Messages.Exit_On_Stop;
   --  Its keyboards' and mice's urgency (SCHED-001, Input_Urgency): their
   --  report signals lend Desktop urgent turns, nothing else it does.
   if Startup.Urgency.Present and then Input_Urgency.Reserve (Startup.Urgency.Value) then
      debugPrint ("xhci: urgency reserved for the input path" & ASCII.LF);
   end if;
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
      XHCI_DMA_Layout.Scratchpads_Of (msg.words (1)) >
        Unsigned_64 (XHCI_Capabilities.Scratchpad_Buffer_Count'Last) or else
      XHCI_DMA_Layout.Slots_Of (msg.words (1)) not in
        Unsigned_64 (XHCI_DMA_Layout.Slot_Count'First) .. Unsigned_64 (XHCI_DMA_Layout.Slot_Count'Last) or else
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
   --  The message is well formed: accepted now. devmgr waits on nothing
   --  but this check; the bring-up below (handoff, reset, settle,
   --  enumeration, storage probe) follows the answer, and its outcome is
   --  this driver's report (XHCI_Bring_Up_Report).
   Reply_With (REPLY_OK);

   if interruptMode in XHCI.INTERRUPT_MSI | XHCI.INTERRUPT_MSIX and then
     not CuBit.Device_Interrupts.Block_Of (Startup.Events).Valid
   then
      Boot_Log.Line ("xhci: " & Location & " stage configure fail no interrupt capability; polling", True);
      interruptMode := XHCI.INTERRUPT_POLLING;
   end if;
   XHCI.Set_Identity (Location, CI.Identity_Of (msg.words (3)), interruptMode);

   XHCI.Initialize
     (barPhys  => msg.words (0),
      barPages => msg.words (1) and XHCI_DMA_Layout.CONFIG_PAGES_MASK,
      dmaPhys  => msg.words (2),
      expectedScratchpads => Natural (XHCI_DMA_Layout.Scratchpads_Of (msg.words (1))),
      expectedSlots => Natural (XHCI_DMA_Layout.Slots_Of (msg.words (1))),
      result   => initResult);

   if initResult /= XHCI.INIT_OK then
      Boot_Log.Line ("xhci: " & Location & " bring-up failed " &
                     "result=" & Image (Unsigned_64 (XHCI.Init_Result'Pos (initResult))) &
                     " (XHCI.Init_Result position)", True);
      Report_Bring_Up (XHCI_Bring_Up_Report.Failed_Label,
                       Unsigned_64 (XHCI.Init_Result'Pos (initResult)));
      --  Let logstore take the failure before the process ends.
      Boot_Log.Flush;
      ignore := syscall (SYSCALL_EXIT);
      return;
   end if;

   XHCI.Probe_Optical;
   XHCI.Probe_Disks;
   XHCI.Start_Boot_Mouse_Transfers;
   XHCI.Start_Boot_Keyboard_Transfers;
   XHCI.Enable_Runtime_Interrupts (interruptMode, interruptEnabled);

   --  This controller's devices, one line naming it.
   Boot_Log.Line ("xhci: " & Location & " devices keyboard=" &
                  (if XHCI.Has_Keyboard then "1" else "0") & " mouse=" &
                  (if XHCI.Has_Mouse then "1" else "0") & " storage=" &
                  (if XHCI.Has_Storage then "1" else "0") & " disk=" &
                  (if XHCI.Has_Disk then "1" else "0") & " ports=" &
                  Image (Unsigned_64 (XHCI.Port_Count)) & " connected=" &
                  Image (Unsigned_64 (XHCI.Connected_Port_Count)));
   Boot_Log.Line ("xhci: " & Location & " bring-up complete (" &
                  (if interruptEnabled then "interrupt-driven" else "polled") & ", hotplug armed)");
   --  For MSI-X this report tells devmgr the interrupter is ready, so it
   --  may unmask the table entry and the function.
   Report_Bring_Up
     (XHCI_Bring_Up_Report.Ready_Label,
      Unsigned_64 (XHCI.Port_Count),
      Unsigned_64 (XHCI.Connected_Port_Count),
      XHCI_Bring_Up_Report.Device_Bits
        (Keyboard => XHCI.Has_Keyboard, Mouse => XHCI.Has_Mouse, Storage => XHCI.Has_Storage));
   keyboardPresent := XHCI.Has_Keyboard;

   loop
      --  The interrupt doorbell first: acknowledge, then drain (Service).
      irqAvailable := Poll_Event (msg);
      if irqAvailable and then CuBit.Device_Interrupts.Is_Doorbell (msg) then
         XHCI.Acknowledge_Runtime_Interrupt;
      elsif irqAvailable then
         --  A source ring's end, or Desktop waking a source for room.
         Note_Source_Event (msg);
      end if;
      XHCI.Service (serviceProgress);
      Storage_Requests.Poll (storageProgress);
      declare
         Reported : Boolean;
      begin
         Send_Disk_Reports (Reported);
         storageProgress := storageProgress or else Reported;
      end;

      --  A keyboard that went away releases every key it held, then its
      --  source ends (hot-plug: one that comes back is a new source).
      if keyboardPresent and then not XHCI.Has_Keyboard then
         USB_Keyboards.Release_All (keyboardState, keyboardChanges);
         Send_Changes;
         Keyboards.Withdraw (Keyboard_Source);
      end if;
      keyboardPresent := XHCI.Has_Keyboard;
      if mousePresent and then not XHCI.Has_Mouse then
         Pointers.Withdraw (Mouse_Source);
         pointerNeedsSnapshot := True;
      end if;
      mousePresent := XHCI.Has_Mouse;
      Service_Sources;

      XHCI.Poll_Boot_Keyboard (keyboardData, keyboardReady, keyboardProgress);
      if keyboardReady then
         USB_Keyboards.Update (keyboardState, keyboardData, keyboardChanges, keyboardResult);
         if keyboardResult = USB_Keyboards.Decoded then
            Send_Changes;
         end if;
      end if;

      XHCI.Poll_Boot_Mouse (buttons, deltaX, deltaY, deltaZ, reportReady, eventAvailable);
      if reportReady and then Pointers.Established (Mouse_Source) and then
        (deltaX /= 0 or else deltaY /= 0 or else deltaZ /= 0 or else
         buttons /= lastButtons or else pointerNeedsSnapshot)
      then
         --  The desktop's event ABI uses PS/2 Y orientation (positive
         --  upward); USB HID uses positive downward.
         Pointers.Publish
           (Mouse_Source,
            Pointer_Pending.Report'(Buttons => buttons, X => deltaX, Y => -deltaY, Wheel => deltaZ, Flags => 0),
            CuBit.Monotonic.Microseconds);
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
            D := Earliest (D, diagnosticsDue);
            if reportRetryAt /= 0 then
               D := Earliest (D, reportRetryAt);
            end if;
            --  A source still looking for Desktop looks again by then.
            if keyboardPresent then
               D := Earliest (D, Keyboards.Next_Deadline (Keyboard_Source));
            end if;
            if mousePresent then
               D := Earliest (D, Pointers.Next_Deadline (Mouse_Source));
            end if;
            if interruptEnabled and then XHCI.Interrupts_Trusted then
               if D > Now then
                  activity := Wait_For_Activity_Until (D);
                  if activity = Unavailable then
                     ignore := CuBit.Waits.Sleep_For (1);
                  end if;
               end if;
            else
               --  Polled controller: the event ring every millisecond.
               ignore := CuBit.Waits.Sleep_For (1);
            end if;
         end;
      end if;
   end loop;
end main;
