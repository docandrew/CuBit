------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  TEST STAND-IN for acpi.svc's side of CuBit.Power_Event_Exchange (PWR-003
--  slice 1): the power-button watch, Prepare_To_Sleep and Enter_Sleep_State,
--  on QEMU q35, until the ACPI agent implements them in acpi.svc. Not
--  acpi.svc: no AML (\_TTS, \_PTS, \_S5 are not evaluated; the S5 sleep
--  type is a startup parameter), no SCI (PM1_STS is polled), only the
--  fixed power button, only S5. It enters S5 the way acpi.svc is to: through
--  the kernel's platform power object (KERN-004, CuBit.Handles.Power), which
--  drains the console and parks the other CPUs before it writes PM1; the
--  stand-in writes no PM1 control register itself.
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Console_Route;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Handles;
with CuBit.Handles.IPC;
with CuBit.Handles.Devices;
with CuBit.Handles.Power;
with Platform_Power_ABI;
with CuBit.ACPI_Windows;
with CuBit.ACPI_Fixed_Windows;
with CuBit.Firmware_Snapshot;
with CuBit.Power_Event_Exchange;
with Parameters;
with Startup;

procedure Main is
   use ASCII;
   package PE renames CuBit.Power_Event_Exchange;
   package FS renames CuBit.Firmware_Snapshot;
   package AF renames CuBit.ACPI_Fixed_Windows;
   package HIPC renames CuBit.Handles.IPC;
   use type FS.Outcome, FS.Byte_Count;

   P : constant Parameters.Standin_Parameters_Value := Parameters.Value;
   Tables : constant FS.Authority := Startup.Tables;
   G : constant CuBit.ACPI_Windows.Grant := Startup.Hardware;

   --  PM1 status/enable bits (ACPI 6.5, 4.8.3.1): the power button.
   PWRBTN : constant Unsigned_16 := 16#0100#;
   Poll_Ms : constant := 20;
   Table_Bytes : constant := 256;

   FADT : AF.Byte_Array (1 .. Table_Bytes) := [others => 0];
   FADT_Length : Natural := 0;

   function U32 (Offset : Natural) return Unsigned_32 is
     (if Offset + 4 > FADT_Length then 0
      else Unsigned_32 (FADT (Offset + 1)) or Shift_Left (Unsigned_32 (FADT (Offset + 2)), 8)
           or Shift_Left (Unsigned_32 (FADT (Offset + 3)), 16) or Shift_Left (Unsigned_32 (FADT (Offset + 4)), 24));

   procedure Read_FADT is
      Catalog : FS.Catalog_Info;
      D : FS.Table_Description;
      Status : FS.Outcome;
   begin
      FS.Info (Tables, Catalog, Status);
      if Status /= FS.Ok then
         return;
      end if;
      for T in 1 .. FS.Table_ID (Catalog.Table_Count) loop
         FS.Describe (Tables, T, D, Status);
         if Status = FS.Ok and then
           (for all I in FS.Signature_Index =>
              D.Signature (I) = Character'Pos (AF.FADT_Signature (AF.FADT_Signature'First + I - 1)))
         then
            declare
               Wanted : constant Natural := Natural (FS.Byte_Count'Min (D.Extent, Table_Bytes));
               Chunk : FS.Byte_Array (1 .. Wanted);
               Copied : FS.Byte_Count;
            begin
               FS.Copy_Part (Tables, T, 0, Chunk, Copied, Status);
               if Status = FS.Ok and then Copied = FS.Byte_Count (Wanted) then
                  for I in 1 .. Wanted loop
                     FADT (I) := Chunk (I);
                  end loop;
                  FADT_Length := Wanted;
               end if;
            end;
            return;
         end if;
      end loop;
   end Read_FADT;

   --  The granted window holding Port (its handle), if any.
   function Window_Of (Port : Unsigned_32) return CuBit.Handles.IO_Ports is
   begin
      for K in 1 .. G.IO_Used loop
         if Port >= Unsigned_32 (G.IO (K).First)
           and then Port < Unsigned_32 (G.IO (K).First) + G.IO (K).Count
         then
            return G.IO (K).Ports;
         end if;
      end loop;
      return CuBit.Handles.No_IO_Ports;
   end Window_Of;

   Event_Port, Control_Port : Unsigned_32 := 0;
   Enable_Port : Unsigned_32 := 0;
   Events, Enables : CuBit.Handles.IO_Ports := CuBit.Handles.No_IO_Ports;

   Count : Unsigned_64 := 0;
   Kept : CuBit.Handles.Reply := CuBit.Handles.No_Reply;
   Kept_Id : Unsigned_64 := 0;
   Ignore64 : Unsigned_64;
   Ignore : Boolean;

   function Image (Value : Unsigned_64) return String is
      Text : constant String := Unsigned_64'Image (Value);
   begin
      return Text (Text'First + 1 .. Text'Last);
   end Image;

   function Watch_Reply (Id : Unsigned_64; Status : PE.Watch_Status) return Message is
      Answer : Message := NULL_MESSAGE;
   begin
      Answer.tag := (label => PE.Button_Watch, length => PE.Watch_Reply_Words, protocol => 0, ipc => <>, reserved => 0);
      Answer.words (PE.Reply_Request_Word) := Id;
      Answer.words (PE.Watch_Status_Word) := PE.Watch_Status'Enum_Rep (Status);
      Answer.words (PE.Count_Word) := Count;
      Answer.words (PE.Source_Word) := PE.Press_Source'Enum_Rep
        (if Count = 0 then PE.No_Press else PE.Fixed_Event);
      return Answer;
   end Watch_Reply;

   procedure Serve_Watch (From : Process_ID; Request : Message) is
   begin
      if Request.authorityTag /= PE.Authority_Tag then
         Ignore64 := reply (From, Watch_Reply (Request.words (PE.Request_Word), PE.Not_Approved));
      elsif Request.tag.length /= PE.Watch_Request_Words or else Request.words (PE.Request_Word) = 0 then
         Ignore64 := reply (From, Watch_Reply (Request.words (PE.Request_Word), PE.Malformed));
      elsif Request.words (PE.Seen_Word) > Count then
         Ignore64 := reply (From, Watch_Reply (Request.words (PE.Request_Word), PE.Counter_Reset));
      elsif Count > Request.words (PE.Seen_Word) then
         Ignore64 := reply (From, Watch_Reply (Request.words (PE.Request_Word), PE.Pressed));
      else
         declare
            Saved : constant HIPC.Saved_Reply := HIPC.Save_Reply;
         begin
            if Saved.Saved then
               if CuBit.Handles.Valid (Kept) then
                  Ignore := HIPC.Answer (Kept, Watch_Reply (Kept_Id, PE.Superseded));
               end if;
               Kept := Saved.Handle;
               Kept_Id := Request.words (PE.Request_Word);
               debugPrint ("shutdown-standin: watch kept (seen " & Image (Request.words (PE.Seen_Word)) & ")" & LF);
            end if;
         end;
      end if;
   end Serve_Watch;

   Prepared : Boolean := False;

   function Request_Valid (Request : Message) return Boolean is
     (PE.Sleep_Request_Valid (Unsigned_64 (Request.tag.length), Request.words (PE.Request_Word),
                              Request.words (PE.State_Word), Request.words (PE.Confirm_Word)));

   procedure Serve_Prepare (From : Process_ID; Request : Message) is
      Answer : Message := NULL_MESSAGE;
      Status : PE.Prepare_Status;
   begin
      Answer.tag := (label => PE.Prepare_To_Sleep, length => PE.Prepare_Reply_Words,
                     protocol => 0, ipc => <>, reserved => 0);
      Answer.words (PE.Reply_Request_Word) := Request.words (PE.Request_Word);
      if Request.authorityTag /= PE.Authority_Tag then
         Status := PE.Not_Approved;
      elsif not Request_Valid (Request) then
         Status := PE.Malformed;
      elsif Request.words (PE.State_Word) /= PE.Sleep_State'Enum_Rep (PE.S5) then
         Status := PE.Unsupported_State;
      else
         --  acpi.svc evaluates \_TTS (5), \_PTS (5) and copies \_S5's types
         --  here; the stand-in has no AML: its copy is its startup parameter.
         Prepared := True;
         Status := PE.Prepared;
         Answer.words (PE.Sleep_Types_Word) := PE.Sleep_Types_Of
           (Unsigned_16 (P.s5_sleep_type), Unsigned_16 (P.s5_sleep_type));
      end if;
      debugPrint ("shutdown-standin: Prepare_To_Sleep S5 answered " & PE.Name (Status) &
                  " (STAND-IN: no \_TTS/\_PTS evaluated; SLP_TYPa " &
                  Image (Unsigned_64 (P.s5_sleep_type)) & " from its plan)" & LF);
      Answer.words (PE.Prepare_Status_Word) := PE.Prepare_Status'Enum_Rep (Status);
      --  Its line reaches serial through logstore (ABI-002): hand it over
      --  before the answer lets power.svc go on to procmgr's stops.
      CuBit.Console_Route.Flush;
      Ignore64 := reply (From, Answer);
   end Serve_Prepare;

   procedure Serve_Enter (From : Process_ID; Request : Message) is
      Answer : Message := NULL_MESSAGE;
      Status : PE.Enter_Status;
      Power : constant CuBit.Handles.Platform_Power := Startup.Power;
      Entered : CuBit.Handles.Power.Outcome;
      use type Platform_Power_ABI.Refusal;
   begin
      Answer.tag := (label => PE.Enter_Sleep_State, length => PE.Enter_Reply_Words,
                     protocol => 0, ipc => <>, reserved => 0);
      Answer.words (PE.Reply_Request_Word) := Request.words (PE.Request_Word);
      if Request.authorityTag /= PE.Authority_Tag then
         Status := PE.Enter_Not_Approved;
      elsif not Request_Valid (Request) or else Request.words (PE.State_Word) /= PE.Sleep_State'Enum_Rep (PE.S5) then
         Status := PE.Enter_Malformed;
      elsif not Prepared then
         Status := PE.Not_Prepared;
      elsif not CuBit.Handles.Valid (Power) then
         Status := PE.No_Sleep_Registers;
      else
         debugPrint ("shutdown-standin: entering S5 (SLP_TYP " & Image (Unsigned_64 (P.s5_sleep_type)) &
                     ") through the platform power object; STAND-IN, no \_PTS" & LF);
         --  The line reaches serial through logstore (ABI-002): hand it over
         --  before S5 (the kernel drains only its own console, LOG-009).
         CuBit.Console_Route.Flush;
         Entered := CuBit.Handles.Power.Enter_State
           (Power, Platform_Power_ABI.S5, Type_A => Unsigned_64 (P.s5_sleep_type));
         --  Only a refusal returns.
         if not Entered.Answered then
            Status := PE.Write_Refused;
         else
            debugPrint ("shutdown-standin: platform power refused: " &
                        Platform_Power_ABI.Refusal'Image (Entered.Why) & LF);
            Status := (case Entered.Why is
                         when Platform_Power_ABI.Did_Not_Take_Effect => PE.Still_Running,
                         when Platform_Power_ABI.No_Sleep_Registers
                            | Platform_Power_ABI.Unsupported_Register => PE.No_Sleep_Registers,
                         when others => PE.Write_Refused);
         end if;
      end if;
      debugPrint ("shutdown-standin: Enter_Sleep_State answered " & PE.Name (Status) & LF);
      Answer.words (PE.Enter_Status_Word) := PE.Enter_Status'Enum_Rep (Status);
      Ignore64 := reply (From, Answer);
   end Serve_Enter;

   From : Process_ID;
   Request : Message;
   Received : Boolean;
   Status_Now : Unsigned_64;
   Event_Length : Unsigned_32;
begin
   --  ABI-002: debug text into its logstore ring, never the kernel console.
   CuBit.Console_Route.Route_Named ("logstore");
   Read_FADT;
   Event_Port := U32 (AF.PM1a_Event_Offset);
   Control_Port := U32 (AF.PM1a_Control_Offset);
   Event_Length := (if FADT_Length > AF.PM1_Event_Length_Offset
                    then Unsigned_32 (FADT (AF.PM1_Event_Length_Offset + 1)) else 0);
   --  PM1 enable is the event block's second half.
   Enable_Port := Event_Port + Event_Length / 2;
   Events := Window_Of (Event_Port);
   Enables := Window_Of (Enable_Port);
   if CuBit.Handles.Valid (Enables) then
      Status_Now := CuBit.Handles.Devices.In_16 (Enables, Unsigned_16 (Enable_Port));
      Ignore := CuBit.Handles.Devices.Out_16
        (Enables, Unsigned_16 (Enable_Port), Unsigned_16 (Status_Now and 16#FFFF#) or PWRBTN);
   end if;
   if registerDriver (DRIVER_ACPI, Startup.Registration) = Unsigned_64'Last then
      debugPrint ("shutdown-standin: registration failed" & LF);
      return;
   end if;
   debugPrint ("shutdown-standin: registered (STAND-IN for acpi.svc; PM1a_EVT " & Image (Unsigned_64 (Event_Port)) &
               ", PM1a_CNT " & Image (Unsigned_64 (Control_Port)) & ", power button " &
               (if CuBit.Handles.Valid (Events) and then CuBit.Handles.Valid (Enables) then "armed" else "NOT armed") &
               ")" & LF);
   loop
      receiveUntil (Deadline_After (Poll_Ms), From, Request, Received);
      if Received then
         if Request.tag.label = PE.Button_Watch then
            Serve_Watch (From, Request);
         elsif Request.tag.label = PE.Prepare_To_Sleep then
            Serve_Prepare (From, Request);
         elsif Request.tag.label = PE.Enter_Sleep_State then
            Serve_Enter (From, Request);
         else
            Request := NULL_MESSAGE;
            Request.tag := (label => 16#F001#, length => 0, protocol => 0, ipc => <>, reserved => 0);
            Ignore64 := reply (From, Request);
         end if;
      end if;
      if CuBit.Handles.Valid (Events) then
         Status_Now := CuBit.Handles.Devices.In_16 (Events, Unsigned_16 (Event_Port));
         if Status_Now /= Unsigned_64'Last and then (Unsigned_16 (Status_Now and 16#FFFF#) and PWRBTN) /= 0 then
            --  Write one to clear (only this bit).
            Ignore := CuBit.Handles.Devices.Out_16 (Events, Unsigned_16 (Event_Port), PWRBTN);
            Count := Count + 1;
            debugPrint ("shutdown-standin: power button pressed (count " & Image (Count) & ")" & LF);
            if CuBit.Handles.Valid (Kept) then
               Ignore := HIPC.Answer (Kept, Watch_Reply (Kept_Id, PE.Pressed));
               Kept := CuBit.Handles.No_Reply;
            end if;
         end if;
      end if;
   end loop;
end Main;
