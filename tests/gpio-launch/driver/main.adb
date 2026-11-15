------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The ACPI launch's fixture driver (docs/device-matching.md "ACPI
--  launch"): what a driver devmgr launched for an admitted ACPI device
--  receives, and nothing else, each a bring-up line through logstore.
--
--  @description
--  Stages: identify (its Generic_ACPI configuration: _HID, path, UID,
--  epoch, two windows), map (each window through its own device-memory
--  capability, read-only for the read-only request), bounded (the page
--  just past each window is refused by the kernel: the capability is
--  narrowed to exactly the admitted span, or the request's smaller
--  max_bytes). Then OP_READY. On q35 nothing decodes the spans, so a load
--  reads whatever the bus returns; the value is logged, not checked.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with System;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Launch_Configurations;
with Launch_Configuration;
with CuBit.Handles.Devices;
with CuBit.Driver_Readiness;
with CuBit.Logging;
with CuBit.Log_Records;
with Startup;
with CuBit.Handles;
with CuBit.Handles.IPC;
with CuBit.Process_IDs;
with CuBit.Handles.Lines;
with CuBit.Handles.Startup;
with Interrupt_Line_ABI;
with GPIO_Admin_Wire;
with GPIO_Control_Wire;

procedure Main is
   package LC renames Launch_Configuration;
   package GA renames GPIO_Admin_Wire;
   package GW renames GPIO_Control_Wire;
   use type LC.ACPI_UID_Kind;

   --  The gpio-launch profile's controller (devmgr's
   --  Intel_GPIO_Communities.Test_GPIO_Controller_HID).
   EXPECTED_HID : constant LC.ACPI_HID_Text := "CBIT1057";
   EXPECTED_WINDOWS : constant := 2;
   --  The requester's observation: GSI 10, level, active low, shared.
   EXPECTED_PARENT_GSI : constant := 10;
   PAGE_BYTES : constant := 4096;
   --  Where it maps each window (its own choice).
   SECOND_SPAN_VIRTUAL : constant := 16#0000_6800_0000_0000#;
   FIRST_SPAN_VIRTUAL  : constant := 16#0000_6800_0100_0000#;
   PAST_VIRTUAL        : constant := 16#0000_6800_0200_0000#;
   TOTAL_STAGES : constant := 4;

   Log : CuBit.Logging.Publisher := CuBit.Logging.Bound_To (Startup.Logstore);
   Config : LC.Generic_ACPI_Configuration;
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

   function Stage (Name : String; OK : Boolean; Details : String) return Boolean is
   begin
      Line ("gpio-launch-check: stage " & Name & (if OK then " ok " else " fail ") & Details,
            (if OK then CuBit.Log_Records.Information else CuBit.Log_Records.Warning));
      if OK then
         Reached := Reached + 1;
      else
         Line ("gpio-launch-check: bring-up reached" & Natural'Image (Reached) & "/" &
               Natural'Image (TOTAL_STAGES), CuBit.Log_Records.Warning);
      end if;
      return OK;
   end Stage;

   function Path_Text return String is
      Result : String (1 .. 1 + LC.Maximum_ACPI_Segments * (LC.ACPI_Segment_Bytes + 1));
      Length : Natural := 1;
   begin
      Result (1) := '\';
      for K in 1 .. Config.Segments loop
         if K > 1 then
            Length := Length + 1;
            Result (Length) := '.';
         end if;
         Result (Length + 1 .. Length + LC.ACPI_Segment_Bytes) := Config.Path (K);
         Length := Length + LC.ACPI_Segment_Bytes;
      end loop;
      return Result (1 .. Length);
   end Path_Text;

   function Window_Text (K : LC.ACPI_Window_Index) return String is
     (" window" & Natural'Image (K) & "=" & Hex (Config.Windows (K).First) & "+" &
      Hex (Config.Windows (K).Bytes));

   function Load (Virtual : Unsigned_64) return Unsigned_32 is
      Value : Unsigned_32 with Import, Volatile, Address => System'To_Address (Virtual);
   begin
      return Value;
   end Load;

   procedure Report (Label : Unsigned_32) is
      Ignore : MessageTag;
   begin
      Ignore := capSend (Startup.Ready,
        (tag => (label => Label, length => 0, protocol => 0, ipc => <>, reserved => 0),
         authorityTag => 0, words => [others => 0]), CuBit.Messages.Wait_Forever);
   end Report;
   --  The test's touchpad offer's GpioInt (gpio-bootstrap-check): pin 76,
   --  level, active low.
   EXPECTED_SINK_BADGE : constant Unsigned_64 := 16#4750_1000_0001_004C#;
   Test_Lease : constant GW.Identity := 1;
   Test_Generation : constant GW.Identity := 1;

   procedure Admit (From : Process_ID; Request : Message) is
      use type CuBit.Handles.ABI.Kind, CuBit.Handles.Rights, GA.Decode_Status, GW.Completion_Status;
      Launcher : constant CuBit.Handles.IPC.Description :=
        CuBit.Handles.IPC.Describe (Startup.Ready);
      Words : GW.Message_Words (0 .. Natural (Request.tag.length) - 1);
      Decoded : GA.Decoding;
      Sink : CuBit.Handles.Endpoint := CuBit.Handles.No_Endpoint;
      Owner : Process_ID := No_Process;
      Status : GW.Completion_Status := GW.Refused;
      Detail : Unsigned_64 := 0;
      Ignore : Unsigned_64;
   begin
      for K in Words'Range loop
         Words (K) := Request.words (K);
      end loop;
      Decoded := GA.Decode (Request.tag.label, Unsigned_64 (Request.tag.protocol), Words,
                            CuBit.Handles.IPC.Offers (Request));
      if not (Launcher.Current and then Launcher.Kind = CuBit.Handles.ABI.Endpoint
              and then From = CuBit.Process_IDs.From_Word (Launcher.Object))
      then
         Status := GW.Unauthorized;
      elsif Decoded.Status /= GA.Decoded then
         Status := GW.Invalid_Frame;
      else
         declare
            Taken : constant CuBit.Handles.IPC.Accepted_Endpoint :=
              CuBit.Handles.IPC.Accept_Endpoint (CuBit.Handles.IPC.Accept_Offered (From));
         begin
            if Taken.OK then
               Sink := Taken.Value;
               declare
                  D : constant CuBit.Handles.IPC.Description := CuBit.Handles.IPC.Describe (Sink);
               begin
                  if D.Current and then D.Kind = CuBit.Handles.ABI.Endpoint
                    and then D.Held = CuBit.Handles.Write_Only
                    and then D.Authority_Tag = EXPECTED_SINK_BADGE
                    and then CuBit.Process_IDs.Is_Process (CuBit.Process_IDs.From_Word (D.Object))
                  then
                     Owner := CuBit.Process_IDs.From_Word (D.Object);
                     Detail := D.Authority_Tag;
                     Status := GW.Completed;
                  else
                     Detail := (if D.Current then D.Authority_Tag else 0);
                     Status := GW.Invalid_Channel;
                  end if;
               end;
            end if;
         end;
      end if;
      Line ("gpio-launch-check: sink admission " & (if Status = GW.Completed then "ok" else "fail") &
            " status=" & GW.Completion_Status'Image (Status) & " badge=" & Hex (Detail) &
            " owner=" & Hex (CuBit.Process_IDs.To_Word (Owner)),
            (if Status = GW.Completed then CuBit.Log_Records.Information else CuBit.Log_Records.Warning));
      declare
         Answer_Words : constant GW.Message_Words := GA.Encode_Reply (Status);
         Answer : Message := NULL_MESSAGE;
      begin
         Answer.tag := (label => GA.OP_GPIO_ADMIT_SINK, length => Answer_Words'Length,
                        protocol => Protocol_Flags (GA.Protocol_Version), ipc => <>, reserved => 0);
         for K in Answer_Words'Range loop
            Answer.words (K) := Answer_Words (K);
         end loop;
         Ignore := reply (From, Answer);
      end;
      if Status = GW.Completed then
         declare
            Event_Words : constant GW.Message_Words :=
              GW.Encode_Event ((Lease => Test_Lease, Generation => Test_Generation));
            Event : Message := NULL_MESSAGE;
         begin
            Event.tag := (label => GW.OP_GPIO_EVENT, length => Event_Words'Length,
                          protocol => Protocol_Flags (GW.Protocol_Version), ipc => <>, reserved => 0);
            for K in Event_Words'Range loop
               Event.words (K) := Event_Words (K);
            end loop;
            Line ("gpio-launch-check: event through the sink " &
                  (if trySendEvent (Owner, Event, Sink) then "sent" else "refused"),
                  CuBit.Log_Records.Information);
         end;
      end if;
   end Admit;
begin
   CuBit.Launch_Configurations.Get (Config, Valid);
   if not Stage ("identify",
                 Valid and then Config.HID = EXPECTED_HID
                 and then Config.Window_Total = EXPECTED_WINDOWS,
                 "hid=" & Config.HID & " path=" & Path_Text &
                 " uid-type=" & Natural'Image (LC.ACPI_UID_Kind'Enum_Rep (Config.UID_Type)) &
                 " uid=" & Hex (Config.UID) & " epoch=" & Hex (Config.Epoch) &
                 " windows=" & Natural'Image (Config.Window_Total) &
                 (if Valid and then Config.Window_Total >= EXPECTED_WINDOWS
                  then Window_Text (1) & Window_Text (2) else ""))
   then
      Report (CuBit.Driver_Readiness.Not_Present_Label);
      return;
   end if;
   --  The parent line: routed with the observation's facts, and its
   --  owner's acknowledgement is accepted (nothing asserts it on q35).
   declare
      package LA renames Interrupt_Line_ABI;
      use type LA.Trigger_Mode, LA.Pin_Polarity;
      Parent : constant CuBit.Handles.Startup.Optional_Interrupt := Startup.Parent;
      Ack : CuBit.Handles.Lines.Acknowledgement;
   begin
      if Parent.Present and then Config.Line.Present
        and then Config.Line.Vector in LA.Line_Vector
      then
         Ack := CuBit.Handles.Lines.Acknowledge (Parent.Value, Config.Line.Vector);
      end if;
      if not Stage ("parent-line",
                    Parent.Present and then Config.Line.Present
                    and then Config.Line.GSI = EXPECTED_PARENT_GSI
                    and then Config.Line.Vector = LA.Vector_Of (EXPECTED_PARENT_GSI)
                    and then Config.Line.Trigger = LA.Level
                    and then Config.Line.Polarity = LA.Active_Low
                    and then Config.Line.Shared
                    and then Ack.Accepted,
                    "binding=" & Boolean'Image (Parent.Present) &
                    " present=" & Boolean'Image (Config.Line.Present) &
                    " gsi=" & Unsigned_32'Image (Config.Line.GSI) &
                    " vector=" & Natural'Image (Config.Line.Vector) &
                    " ack=" & Boolean'Image (Ack.Accepted))
      then
         Report (CuBit.Driver_Readiness.Not_Present_Label);
         return;
      end if;
   end;
   declare
      Second : constant LC.Physical_Span := Config.Windows (1);
      First  : constant LC.Physical_Span := Config.Windows (2);
      Second_Mapped : constant Boolean := CuBit.Handles.Devices.Map_Device
        (Startup.Second_Span, Second.First, SECOND_SPAN_VIRTUAL, Second.Bytes / PAGE_BYTES,
         Read_Only => False);
      First_Mapped : constant Boolean := CuBit.Handles.Devices.Map_Device
        (Startup.First_Span, First.First, FIRST_SPAN_VIRTUAL, First.Bytes / PAGE_BYTES,
         Read_Only => True);
      --  The read-only request's capability cannot map read-write.
      First_Writable : constant Boolean := CuBit.Handles.Devices.Map_Device
        (Startup.First_Span, First.First, PAST_VIRTUAL, 1, Read_Only => False);
   begin
      if not Stage ("map", Second_Mapped and then First_Mapped and then not First_Writable,
                    "second-span pages=" & Hex (Second.Bytes / PAGE_BYTES) &
                    " first-span pages=" & Hex (First.Bytes / PAGE_BYTES) &
                    " first-span read-write refused=" & Boolean'Image (not First_Writable) &
                    (if Second_Mapped and then First_Mapped
                     then " read=" & Hex (Unsigned_64 (Load (SECOND_SPAN_VIRTUAL))) & "," &
                          Hex (Unsigned_64 (Load (FIRST_SPAN_VIRTUAL)))
                     else ""))
      then
         Report (CuBit.Driver_Readiness.Not_Present_Label);
         return;
      end if;
      --  One page past each window: outside its capability.
      declare
         Past_Second : constant Boolean := CuBit.Handles.Devices.Map_Device
           (Startup.Second_Span, Second.First + Second.Bytes, PAST_VIRTUAL, 1, Read_Only => True);
         Past_First : constant Boolean := CuBit.Handles.Devices.Map_Device
           (Startup.First_Span, First.First + First.Bytes, PAST_VIRTUAL, 1, Read_Only => True);
         Crossed : constant Boolean := CuBit.Handles.Devices.Map_Device
           (Startup.First_Span, Second.First, PAST_VIRTUAL, 1, Read_Only => True);
      begin
         if not Stage ("bounded", not Past_Second and then not Past_First and then not Crossed,
                       "past-second refused=" & Boolean'Image (not Past_Second) &
                       " past-first refused=" & Boolean'Image (not Past_First) &
                       " other-span refused=" & Boolean'Image (not Crossed))
         then
            Report (CuBit.Driver_Readiness.Not_Present_Label);
            return;
         end if;
      end;
   end;
   Line ("gpio-launch-check: bring-up complete", CuBit.Log_Records.Information);
   Report (CuBit.Driver_Readiness.Ready_Label);
   --  The GPIO sink admission (SUP-001), as gpio.drv takes it: only from
   --  its launcher (kernel sender = Describe of its "ready" endpoint's
   --  process), zero words, exactly one offered endpoint, Write-only, whose
   --  owner and badge are what Describe says. Then one event through the
   --  sink, as an interrupt input would deliver it.
   loop
      declare
         From : Process_ID;
         Request : Message;
         Found : Boolean;
         Ignore : Unsigned_64;
         Waited : Activity_Result;
      begin
         Poll_Service_Request (From, Request, Found);
         if Found and then Request.tag.label = GA.OP_GPIO_ADMIT_SINK then
            Admit (From, Request);
         elsif Found then
            Ignore := reply (From, NULL_MESSAGE);
         else
            Waited := Wait_For_Activity_Until (Unsigned_64'Last);
         end if;
      end;
   end loop;
end Main;
