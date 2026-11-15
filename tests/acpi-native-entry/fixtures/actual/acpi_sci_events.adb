pragma Ada_2022;
with Interfaces; use Interfaces;
with ACPI_FADT;
with Firmware_Tables;
with Interrupt_Line_ABI;
with CuBit.Handles;
with CuBit.Handles.Devices;
with CuBit.Handles.Lines;
with CuBit.Device_Interrupts;
with CuBit.Firmware_Snapshot.Whole_Tables;
with CuBit.Monotonic;
with ACPI_Native_Events;
with ACPI_Native_Log;
package body ACPI_SCI_Events is
   package D renames CuBit.Handles.Devices;
   package WT renames CuBit.Firmware_Snapshot.Whole_Tables;
   package LA renames Interrupt_Line_ABI;
   use type CuBit.Process_IDs.Process_ID;
   use type WT.Read_Status;
   use type R.Refusal;
   use type R.Mode_Decision;
   use type ACPI_SCI_Quiesce.Doorbell_Class;

   -- The FADT is a few hundred bytes (ACPI 6.5: 276); a larger one than
   -- this ceiling is refused, not cut.
   FADT_Ceiling : constant := 4_096;
   FADT_Signature : constant String := "FACP";
   -- An SCI log line names at most this many GPEs, then a count.
   Logged_GPE_Ceiling : constant := 8;
   -- The ACPI-mode switch: how long SCI_EN may take to set after
   -- ACPI_ENABLE (ACPICA allows 3 s), and how often it is read meanwhile.
   ACPI_Mode_Switch_Deadline_Ms : constant Word := 3_000;
   Mode_Poll_Interval_Ms        : constant Word := 1;
   -- Port reads answer this word when the window refuses them.
   Refused_Read : constant Word := Word'Last;

   type Port_Handle is record
      Present : Boolean := False;
      Ports : CuBit.Handles.IO_Ports := CuBit.Handles.No_IO_Ports;
   end record;
   type PM1_Handles is array (R.PM1_Kind) of Port_Handle;
   type GPE_Handles is array (R.GPE_Kind) of Port_Handle;

   type State is record
      Armed : Boolean := False;
      -- The line is routed here (set by Arm, even when Arm then refuses):
      -- every doorbell is acknowledged, armed or not.
      Line_Held : Boolean := False;
      Disarmed : Boolean := False;
      Layout : R.Layout;
      SCI : CuBit.ACPI_Windows.SCI_Line;
      Event_Status, Event_Enable : PM1_Handles;
      GPE_Status, GPE_Enable : GPE_Handles;
      Control : Port_Handle;
      Unbound_Logged : R.GPE_Set := R.No_GPE;
      Stats : Statistics;
   end record;
   S : State;

   function Observe return Statistics is (S.Stats);
   function Is_Armed return Boolean is (S.Armed);
   function Fixed_Power_Button_Available return Boolean is
     (S.Armed and then not S.Disarmed and then S.Layout.Fixed_Hardware (R.Power_Button));

   function Hex (Value : Word) return String is
      Hex_Digits : constant String := "0123456789ABCDEF";
      Text : String (1 .. 16);
      Left : Word := Value;
      First : Positive := Text'Last;
   begin
      for I in reverse Text'Range loop
         Text (I) := Hex_Digits (Natural (Left and 16#F#) + 1);
         Left := Shift_Right (Left, 4);
         if Text (I) /= '0' then First := I; end if;
      end loop;
      return "0x" & Text (First .. Text'Last);
   end Hex;

   function Image (Value : Natural) return String is
      Text : constant String := Natural'Image (Value);
   begin
      return Text (Text'First + 1 .. Text'Last);
   end Image;

   -- The window of Grant that holds [First, First + Bytes).
   function Window_For
     (Grant : CuBit.ACPI_Windows.Grant; First : R.Port_Number; Bytes : Natural)
      return Port_Handle is
   begin
      for K in 1 .. Grant.IO_Used loop
         if Unsigned_32 (First) >= Unsigned_32 (Grant.IO (K).First)
           and then Unsigned_32 (First) + Unsigned_32 (Bytes)
                    <= Unsigned_32 (Grant.IO (K).First) + Grant.IO (K).Count
         then
            return (Present => True, Ports => Grant.IO (K).Ports);
         end if;
      end loop;
      return (others => <>);
   end Window_For;

   procedure Note_Refusal is
   begin
      S.Stats.Register_Refusals := S.Stats.Register_Refusals + 1;
   end Note_Refusal;

   function Read_16 (Where : Port_Handle; Port : R.Port_Number) return Word is
     (D.In_16 (Where.Ports, Port));
   function Read_8 (Where : Port_Handle; Port : R.Port_Number) return Word is
     (D.In_8 (Where.Ports, Port));

   ---------------------------------------------------------------------------
   -- GPE primitives
   ---------------------------------------------------------------------------
   procedure Locate
     (GPE : R.GPE_Number; Found : out Boolean; Kind : out R.GPE_Kind;
      Register : out R.GPE_Register_Index; Mask : out Unsigned_8) is
      Where : constant R.GPE_Location := R.Locate (S.Layout, GPE);
   begin
      Found := S.Armed and then Where.Present;
      Kind := R.GPE0; Register := 0; Mask := 0;
      if Found then
         Kind := Where.Block; Register := Where.Register; Mask := R.Bit_Mask (Where.Bit);
      end if;
   end Locate;

   procedure Write_Enable (GPE : R.GPE_Number; Set : Boolean; Result : out IO_Outcome) is
      Found : Boolean;
      Kind : R.GPE_Kind;
      Register : R.GPE_Register_Index;
      Mask : Unsigned_8;
      Port : R.Port_Number;
      Current : Word;
   begin
      Result := Refused;
      Locate (GPE, Found, Kind, Register, Mask);
      if not Found then return; end if;
      Port := S.Layout.GPEs (Kind).Enable + R.Port_Number (Register);
      Current := Read_8 (S.GPE_Enable (Kind), Port);
      if Current = Refused_Read then Note_Refusal; return; end if;
      if D.Out_8 (S.GPE_Enable (Kind).Ports, Port,
                  (if Set then Unsigned_8 (Current and 16#FF#) or Mask
                   else Unsigned_8 (Current and 16#FF#) and not Mask))
      then
         Result := Completed;
      else
         Note_Refusal;
      end if;
   end Write_Enable;

   procedure Mask_GPE (GPE : R.GPE_Number; Result : out IO_Outcome) is
   begin
      Write_Enable (GPE, False, Result);
   end Mask_GPE;

   procedure Enable_GPE (GPE : R.GPE_Number; Result : out IO_Outcome) is
   begin
      Write_Enable (GPE, True, Result);
   end Enable_GPE;

   procedure Clear_GPE (GPE : R.GPE_Number; Result : out IO_Outcome) is
      Found : Boolean;
      Kind : R.GPE_Kind;
      Register : R.GPE_Register_Index;
      Mask : Unsigned_8;
   begin
      Result := Refused;
      Locate (GPE, Found, Kind, Register, Mask);
      if not Found then return; end if;
      -- Write-1-to-clear: only this GPE's bit.
      if D.Out_8 (S.GPE_Status (Kind).Ports,
                  S.Layout.GPEs (Kind).Status + R.Port_Number (Register), Mask)
      then
         Result := Completed;
      else
         Note_Refusal;
      end if;
   end Clear_GPE;

   ---------------------------------------------------------------------------
   -- Fixed-event primitives (PM1a and PM1b both)
   ---------------------------------------------------------------------------
   procedure Write_Fixed_Enable (Event : R.Fixed_Event; Set : Boolean; Result : out IO_Outcome) is
      Current : Word;
      Mask : constant Unsigned_16 := R.Fixed_Mask (Event);
   begin
      Result := (if S.Armed then Completed else Refused);
      if not S.Armed then return; end if;
      for Kind in R.PM1_Kind loop
         if S.Layout.Events (Kind).Present then
            Current := Read_16 (S.Event_Enable (Kind), S.Layout.Events (Kind).Enable);
            if Current = Refused_Read
              or else not D.Out_16 (S.Event_Enable (Kind).Ports, S.Layout.Events (Kind).Enable,
                                    (if Set then Unsigned_16 (Current and 16#FFFF#) or Mask
                                     else Unsigned_16 (Current and 16#FFFF#) and not Mask))
            then
               Note_Refusal; Result := Refused;
            end if;
         end if;
      end loop;
   end Write_Fixed_Enable;

   procedure Enable_Fixed (Event : R.Fixed_Event; Result : out IO_Outcome) is
   begin
      Write_Fixed_Enable (Event, True, Result);
   end Enable_Fixed;

   procedure Disable_Fixed (Event : R.Fixed_Event; Result : out IO_Outcome) is
   begin
      Write_Fixed_Enable (Event, False, Result);
   end Disable_Fixed;

   -- Write-1-to-clear one fixed event's status in every PM1 block.
   procedure Clear_Fixed (Event : R.Fixed_Event) is
   begin
      for Kind in R.PM1_Kind loop
         if S.Layout.Events (Kind).Present
           and then not D.Out_16 (S.Event_Status (Kind).Ports, S.Layout.Events (Kind).Status,
                                  R.Fixed_Mask (Event))
         then
            Note_Refusal;
         end if;
      end loop;
   end Clear_Fixed;

   ---------------------------------------------------------------------------
   -- Arm
   ---------------------------------------------------------------------------
   procedure Read_FADT
     (Tables : CuBit.Firmware_Snapshot.Authority; Decoded : out ACPI_FADT.Result;
      Readable : out Boolean) is
      Table : CuBit.Firmware_Snapshot.Table_ID;
      Bytes : WT.Table_Bytes;
      Status : WT.Read_Status;
   begin
      Decoded := (Valid => False); Readable := False;
      WT.Find (Tables, FADT_Signature, Table, Bytes, Status);
      if Status /= WT.Found or else Bytes = 0 or else Bytes > FADT_Ceiling then
         return;
      end if;
      declare
         Raw : WT.Byte_Array (1 .. Bytes);
         Copy : Firmware_Tables.Bytes (1 .. Bytes);
      begin
         WT.Copy (Tables, Table, Raw, Status);
         if Status /= WT.Found then return; end if;
         Readable := True;
         for I in Raw'Range loop
            Copy (I) := Raw (I);
         end loop;
         Decoded := ACPI_FADT.Decode (Copy);
      end;
   end Read_FADT;

   function Enabled_GPE_Text return String is
      Text : String (1 .. 256) := [others => ' '];
      Last : Natural := 0;
      Listed, Total : Natural := 0;
      procedure Add (Item : String) is
      begin
         if Last + Item'Length <= Text'Last then
            Text (Last + 1 .. Last + Item'Length) := Item;
            Last := Last + Item'Length;
         end if;
      end Add;
   begin
      for Kind in R.GPE_Kind loop
         if S.Layout.GPEs (Kind).Present then
            for Register in 0 .. S.Layout.GPEs (Kind).Registers - 1 loop
               declare
                  Enabled : constant Word := Read_8
                    (S.GPE_Enable (Kind), S.Layout.GPEs (Kind).Enable + R.Port_Number (Register));
               begin
                  if Enabled /= Refused_Read then
                     for Bit in R.GPE_Bit loop
                        if (Unsigned_8 (Enabled and 16#FF#) and R.Bit_Mask (Bit)) /= 0 then
                           Total := Total + 1;
                           if Listed < Logged_GPE_Ceiling then
                              Listed := Listed + 1;
                              Add ((if Listed > 1 then "," else "") & Hex (Word
                                (S.Layout.GPEs (Kind).First_GPE + Register * R.GPE_Register_Bits + Bit)));
                           end if;
                        end if;
                     end loop;
                  end if;
               end;
            end loop;
         end if;
      end loop;
      return "[" & Text (1 .. Last) & (if Total > Listed then " +" & Image (Total - Listed) else "") & "]";
   end Enabled_GPE_Text;

   procedure Arm
     (Grant : CuBit.ACPI_Windows.Grant;
      Tables : CuBit.Firmware_Snapshot.Authority;
      Outcome : out Arm_Outcome;
      Layout_Why : out R.Refusal)
   is
      Decoded : ACPI_FADT.Result;
      Readable : Boolean;
      Resolved : R.Layout_Result;
      Acpi_Mode : Word := Refused_Read;
      Enabled : IO_Outcome;
      Fixed_Text : String (1 .. 64) := [others => ' '];
      Fixed_Last : Natural := 0;
      Decision : R.Mode_Decision;
      subtype Switch_String is String (1 .. 32);
      Switch_Text : Switch_String;
      function Switch_Text_Of (Text : String) return Switch_String is
         Result : Switch_String := [others => ' '];
      begin
         Result (1 .. Text'Length) := Text;
         return Result;
      end Switch_Text_Of;
      function Trimmed (Text : Switch_String) return String is
         Last : Natural := 0;
      begin
         for I in Text'Range loop
            if Text (I) /= ' ' then Last := I; end if;
         end loop;
         return Text (1 .. Last);
      end Trimmed;
   begin
      Layout_Why := R.No_Refusal;
      S.Armed := False;
      if not Grant.SCI.Present then
         Outcome := No_SCI_Line; return;
      end if;
      S.SCI := Grant.SCI;
      S.Line_Held := True;
      Read_FADT (Tables, Decoded, Readable);
      if not Readable then
         Outcome := FADT_Unreadable; return;
      elsif not Decoded.Valid then
         Outcome := FADT_Malformed; return;
      end if;
      Resolved := R.Resolve (Decoded.Value);
      if not Resolved.Valid then
         Layout_Why := Resolved.Why; Outcome := Layout_Refused; return;
      end if;
      S.Layout := Resolved.Value;
      Layout_Why := S.Layout.GPE1_Refused;
      for Kind in R.PM1_Kind loop
         if S.Layout.Events (Kind).Present then
            S.Event_Status (Kind) := Window_For (Grant, S.Layout.Events (Kind).Status, R.PM1_Register_Bytes);
            S.Event_Enable (Kind) := Window_For (Grant, S.Layout.Events (Kind).Enable, R.PM1_Register_Bytes);
            if not (S.Event_Status (Kind).Present and then S.Event_Enable (Kind).Present) then
               Outcome := Register_Outside_Grant; return;
            end if;
         end if;
      end loop;
      for Kind in R.GPE_Kind loop
         if S.Layout.GPEs (Kind).Present then
            S.GPE_Status (Kind) := Window_For (Grant, S.Layout.GPEs (Kind).Status, S.Layout.GPEs (Kind).Registers);
            S.GPE_Enable (Kind) := Window_For (Grant, S.Layout.GPEs (Kind).Enable, S.Layout.GPEs (Kind).Registers);
            if not (S.GPE_Status (Kind).Present and then S.GPE_Enable (Kind).Present) then
               Outcome := Register_Outside_Grant; return;
            end if;
         end if;
      end loop;
      if S.Layout.Control_Present then
         S.Control := Window_For (Grant, S.Layout.Control, R.PM1_Register_Bytes);
         if S.Control.Present then
            Acpi_Mode := Read_16 (S.Control, S.Layout.Control);
         end if;
      end if;
      -- ACPI mode (4.8.2.1): SCIs arrive only with SCI_EN set. When the
      -- firmware left legacy mode on, write ACPI_ENABLE to SMI_CMD (both
      -- from the FADT, the port through its own FADT window) and wait for
      -- SCI_EN under an explicit deadline. ACPI_DISABLE is never written.
      Decision := R.Decide_Mode
        (S.Layout, Acpi_Mode /= Refused_Read, Unsigned_16 (Acpi_Mode and 16#FFFF#));
      case Decision is
         when R.Already_ACPI_Mode =>
            Switch_Text := Switch_Text_Of ("NOT_NEEDED");
         when R.No_Mode_Transition =>
            Switch_Text := Switch_Text_Of ("NONE_DEFINED");
         when R.Control_Unreadable =>
            Switch_Text := Switch_Text_Of ("CONTROL_UNREADABLE");
         when R.Switch_To_ACPI_Mode =>
            declare
               SMI : constant Port_Handle :=
                 Window_For (Grant, S.Layout.SMI_Command, R.SMI_Command_Bytes);
               Started : constant Word := CuBit.Monotonic.Milliseconds;
               Deadline : constant Word := Started + ACPI_Mode_Switch_Deadline_Ms;
               Entered : Boolean := False;
               Ignore : CuBit.Messages.Activity_Result;
            begin
               if not SMI.Present then
                  Outcome := ACPI_Mode_Port_Outside_Grant; return;
               elsif not D.Out_8 (SMI.Ports, S.Layout.SMI_Command, S.Layout.ACPI_Enable) then
                  Outcome := Enable_Refused; return;
               end if;
               loop
                  Acpi_Mode := Read_16 (S.Control, S.Layout.Control);
                  Entered := Acpi_Mode /= Refused_Read and then
                    (Unsigned_16 (Acpi_Mode and 16#FFFF#) and R.SCI_Enable_Mask) /= 0;
                  exit when Entered or else CuBit.Monotonic.Milliseconds >= Deadline;
                  Ignore := CuBit.Messages.Wait_For_Activity_Until
                    (CuBit.Monotonic.Milliseconds + Mode_Poll_Interval_Ms);
               end loop;
               if not Entered then
                  ACPI_Native_Log.Warning ("acpi: SCI ACPI-mode switch failed: SCI_EN still 0" &
                    Word'Image (CuBit.Monotonic.Milliseconds - Started) & " ms after ACPI_ENABLE " &
                    Hex (Word (S.Layout.ACPI_Enable)) & " to SMI_CMD " & Hex (Word (S.Layout.SMI_Command)));
                  Outcome := ACPI_Mode_Not_Entered; return;
               end if;
               Switch_Text := Switch_Text_Of
                 ("DONE_IN_" & Image (Natural (Word'Min (CuBit.Monotonic.Milliseconds - Started,
                                                         ACPI_Mode_Switch_Deadline_Ms))) & "_MS");
            end;
      end case;
      S.Armed := True;
      -- The fixed-hardware buttons: enable. A press already latched (one
      -- in a handover gap after the previous owner's Disarm) is kept and
      -- delivered: nothing is lost.
      for Event in R.Power_Button .. R.Sleep_Button loop
         if S.Layout.Fixed_Hardware (Event) then
            Enable_Fixed (Event, Enabled);
            if Enabled /= Completed then
               S.Armed := False; Outcome := Enable_Refused; return;
            end if;
            declare
               Name : constant String := (if Fixed_Last > 0 then "," else "") & R.Fixed_Event'Image (Event);
            begin
               Fixed_Text (Fixed_Last + 1 .. Fixed_Last + Name'Length) := Name;
               Fixed_Last := Fixed_Last + Name'Length;
            end;
         end if;
      end loop;
      ACPI_Native_Log.Info
        ("acpi: SCI armed line=" & Image (S.SCI.Vector) &
         " pm1a=" & Hex (Word (S.Layout.Events (R.PM1a).Status)) &
         (if S.Layout.Events (R.PM1b).Present
          then " pm1b=" & Hex (Word (S.Layout.Events (R.PM1b).Status)) else "") &
         (if S.Layout.GPEs (R.GPE0).Present
          then " gpe0=" & Hex (Word (S.Layout.GPEs (R.GPE0).Status)) & "+" &
               Image (S.Layout.GPEs (R.GPE0).Registers) else " gpe0=none") &
         (if S.Layout.GPEs (R.GPE1).Present
          then " gpe1=" & Hex (Word (S.Layout.GPEs (R.GPE1).Status)) & "+" &
               Image (S.Layout.GPEs (R.GPE1).Registers) & "@" &
               Image (S.Layout.GPEs (R.GPE1).First_GPE)
          elsif S.Layout.GPE1_Refused /= R.No_Refusal
          then " gpe1=REFUSED_" & R.Refusal'Image (S.Layout.GPE1_Refused) else "") &
         " acpi_mode=" &
         (if Acpi_Mode = Refused_Read then "UNKNOWN"
          elsif (Unsigned_16 (Acpi_Mode and 16#FFFF#) and R.SCI_Enable_Mask) /= 0 then "ON"
          else "OFF") &
         " acpi_mode_switch=" & Trimmed (Switch_Text) &
         " fixed=[" & Fixed_Text (1 .. Fixed_Last) & "]" &
         " enabled_gpes=" & Enabled_GPE_Text);
      if Decision = R.No_Mode_Transition then
         -- SCI_EN=0 and the FADT defines no transition (SMI_CMD or
         -- ACPI_ENABLE zero): the platform claims ACPI mode only.
         ACPI_Native_Log.Warning ("acpi: SCI armed with SCI_EN=0 and no SMI_CMD transition defined");
      end if;
      Outcome := Armed;
   end Arm;

   ---------------------------------------------------------------------------
   -- Disarm
   ---------------------------------------------------------------------------
   procedure Disarm (Result : out ACPI_SCI_Quiesce.Result) is
      package Q renames ACPI_SCI_Quiesce;
      Fixed_Done, GPE_Done, Acknowledged : Boolean := True;
      Current : Word;
      Answer : CuBit.Handles.Lines.Acknowledgement;
   begin
      if not S.Line_Held then
         Result := (State => Q.Quiesced); return;
      end if;
      -- Stop dispatching first: a doorbell from here on is only acknowledged.
      S.Armed := False;
      S.Disarmed := True;
      if S.Event_Enable (R.PM1a).Present then
         for Kind in R.PM1_Kind loop
            if S.Layout.Events (Kind).Present and then S.Event_Enable (Kind).Present then
               Current := Read_16 (S.Event_Enable (Kind), S.Layout.Events (Kind).Enable);
               if Current = Refused_Read
                 or else not D.Out_16 (S.Event_Enable (Kind).Ports, S.Layout.Events (Kind).Enable,
                                       Q.Disarmed_PM1_Enable (Unsigned_16 (Current and 16#FFFF#)))
               then
                  Fixed_Done := False;
               end if;
            end if;
         end loop;
      end if;
      for Kind in R.GPE_Kind loop
         if S.Layout.GPEs (Kind).Present and then S.GPE_Enable (Kind).Present then
            for Register in 0 .. S.Layout.GPEs (Kind).Registers - 1 loop
               if not D.Out_8 (S.GPE_Enable (Kind).Ports,
                               S.Layout.GPEs (Kind).Enable + R.Port_Number (Register),
                               Q.Disarmed_GPE_Enable)
               then
                  GPE_Done := False;
               end if;
            end loop;
         end if;
      end loop;
      -- With every source disabled the level line deasserts; acknowledge so
      -- a delivery taken before this is not left holding it masked.
      Answer := CuBit.Handles.Lines.Acknowledge (S.SCI.Line, S.SCI.Vector);
      Acknowledged := Answer.Accepted;
      Result := Q.Report (Fixed_Done, GPE_Done, Acknowledged);
      case Result.State is
         when Q.Quiesced =>
            ACPI_Native_Log.Info ("acpi: SCI disarmed QUIESCED interrupts=" &
              Word'Image (S.Stats.Interrupts) & " (sources disabled, status kept, line acknowledged)");
         when Q.Pending =>
            ACPI_Native_Log.Warning ("acpi: SCI disarm PENDING " & Q.Pending_Reason'Image (Result.Why));
      end case;
   end Disarm;

   procedure Acknowledge_Only
     (Publisher : CuBit.Process_IDs.Process_ID; Item : CuBit.Messages.Message;
      Result : out ACPI_SCI_Quiesce.Acknowledgement)
   is
      package Q renames ACPI_SCI_Quiesce;
   begin
      case Q.Classify (CuBit.Device_Interrupts.Is_Doorbell (Item),
                       Publisher = CuBit.Process_IDs.No_Process, S.Line_Held) is
         when Q.Foreign => Result := Q.Not_SCI_Doorbell;
         when Q.No_Line => Result := Q.No_SCI_Line;
         when Q.SCI_Doorbell =>
            Result := (if CuBit.Handles.Lines.Acknowledge (S.SCI.Line, S.SCI.Vector).Accepted
                       then Q.Acknowledged else Q.Acknowledge_Refused);
      end case;
   end Acknowledge_Only;

   ---------------------------------------------------------------------------
   -- The minimal consumers
   ---------------------------------------------------------------------------
   procedure Log_Fixed_Event (Event : R.Fixed_Event; Result : out Disposition) is
   begin
      case Event is
         when R.Power_Button =>
            ACPI_Native_Log.Info ("acpi: SCI power button pressed"); Result := Handled;
         when R.Sleep_Button =>
            ACPI_Native_Log.Info ("acpi: SCI sleep button pressed"); Result := Handled;
         when R.Timer_Overflow | R.Global_Lock_Release | R.RTC_Alarm =>
            Result := Unbound;
      end case;
   end Log_Fixed_Event;

   procedure Unbound_GPE (GPE : R.GPE_Number; Result : out Disposition) is
      pragma Unreferenced (GPE);
   begin
      Result := Unbound;
   end Unbound_GPE;

   ---------------------------------------------------------------------------
   -- The SCI
   ---------------------------------------------------------------------------
   package body With_Consumer is
      procedure Service is
         Status, Enabled, Active : Word;
         Disposed : Disposition;
         Done : IO_Outcome;
         GPE : R.GPE_Number;
         Fixed_Seen : R.Fixed_Set := R.No_Fixed;
         GPE_Text : String (1 .. 128) := [others => ' '];
         GPE_Last : Natural := 0;
         GPE_Listed, GPE_Total : Natural := 0;
      begin
         -- Fixed events: PM1a and PM1b together (an event is set in either).
         for Kind in R.PM1_Kind loop
            if S.Layout.Events (Kind).Present then
               Status := Read_16 (S.Event_Status (Kind), S.Layout.Events (Kind).Status);
               Enabled := Read_16 (S.Event_Enable (Kind), S.Layout.Events (Kind).Enable);
               if Status = Refused_Read or else Enabled = Refused_Read then
                  Note_Refusal;
               else
                  Active := Status and Enabled;
                  for Event in R.Fixed_Event loop
                     if (Unsigned_16 (Active and 16#FFFF#) and R.Fixed_Mask (Event)) /= 0 then
                        Fixed_Seen (Event) := True;
                     end if;
                  end loop;
               end if;
            end if;
         end loop;
         for Event in R.Fixed_Event loop
            if Fixed_Seen (Event) then
               S.Stats.Fixed_Events := S.Stats.Fixed_Events + 1;
               Clear_Fixed (Event);
               Fixed_Event (Event, Disposed);
               if Disposed = Unbound then
                  S.Stats.Unbound_Events := S.Stats.Unbound_Events + 1;
                  Disable_Fixed (Event, Done);
                  ACPI_Native_Log.Warning ("acpi: SCI fixed event " & R.Fixed_Event'Image (Event) &
                    " has no consumer: disabled" & (if Done = Refused then " (REFUSED)" else ""));
               end if;
            end if;
         end loop;
         -- GPEs: every enabled, set bit of every status byte.
         for Kind in R.GPE_Kind loop
            if S.Layout.GPEs (Kind).Present then
               for Register in 0 .. S.Layout.GPEs (Kind).Registers - 1 loop
                  Status := Read_8 (S.GPE_Status (Kind), S.Layout.GPEs (Kind).Status + R.Port_Number (Register));
                  Enabled := Read_8 (S.GPE_Enable (Kind), S.Layout.GPEs (Kind).Enable + R.Port_Number (Register));
                  if Status = Refused_Read or else Enabled = Refused_Read then
                     Note_Refusal;
                  else
                     Active := Status and Enabled;
                     for Bit in R.GPE_Bit loop
                        if (Unsigned_8 (Active and 16#FF#) and R.Bit_Mask (Bit)) /= 0 then
                           GPE := S.Layout.GPEs (Kind).First_GPE + Register * R.GPE_Register_Bits + Bit;
                           S.Stats.GPE_Events := S.Stats.GPE_Events + 1;
                           GPE_Total := GPE_Total + 1;
                           if GPE_Listed < Logged_GPE_Ceiling then
                              GPE_Listed := GPE_Listed + 1;
                              declare
                                 Item : constant String := (if GPE_Listed > 1 then "," else "") & Hex (Word (GPE));
                              begin
                                 GPE_Text (GPE_Last + 1 .. GPE_Last + Item'Length) := Item;
                                 GPE_Last := GPE_Last + Item'Length;
                              end;
                           end if;
                           GPE_Event (GPE, Disposed);
                           if Disposed = Unbound then
                              S.Stats.Unbound_Events := S.Stats.Unbound_Events + 1;
                              Mask_GPE (GPE, Done);
                              if Done = Completed then Clear_GPE (GPE, Done); end if;
                              if not S.Unbound_Logged (GPE) then
                                 S.Unbound_Logged (GPE) := True;
                                 ACPI_Native_Log.Warning ("acpi: SCI GPE " & Hex (Word (GPE)) &
                                   " has no consumer: masked and cleared" &
                                   (if Done = Refused then " (REFUSED)" else ""));
                              end if;
                           end if;
                        end if;
                     end loop;
                  end if;
               end loop;
            end if;
         end loop;
         ACPI_Native_Log.Info
           ("acpi: SCI" & Word'Image (S.Stats.Interrupts) & " fixed=[" &
            (if Fixed_Seen (R.Power_Button) then "PWRBTN " else "") &
            (if Fixed_Seen (R.Sleep_Button) then "SLPBTN " else "") &
            (if Fixed_Seen (R.Global_Lock_Release) then "GBL " else "") &
            (if Fixed_Seen (R.RTC_Alarm) then "RTC " else "") &
            (if Fixed_Seen (R.Timer_Overflow) then "TMR " else "") &
            "] gpe=[" & GPE_Text (1 .. GPE_Last) &
            (if GPE_Total > GPE_Listed then " +" & Image (GPE_Total - GPE_Listed) else "") & "]");
      end Service;

      procedure Handle_Event
        (Publisher : CuBit.Process_IDs.Process_ID; Item : CuBit.Messages.Message;
         Stop : out Boolean; Handled : out Boolean)
      is
         Answer : CuBit.Handles.Lines.Acknowledgement;
      begin
         if not CuBit.Device_Interrupts.Is_Doorbell (Item) then
            ACPI_Native_Events.Handle (Publisher, Item, Stop, Handled);
            return;
         end if;
         Stop := False; Handled := True;
         -- Only the kernel rings the doorbell; this service owns no other
         -- line. No line held: nothing to acknowledge.
         if ACPI_SCI_Quiesce.Classify (True, Publisher = CuBit.Process_IDs.No_Process,
                                       S.Line_Held) /= ACPI_SCI_Quiesce.SCI_Doorbell
         then
            return;
         end if;
         -- Unarmed (refused, or disarmed): acknowledge only, so this owner
         -- never holds a shared or handed-over line masked.
         if S.Armed then
            S.Stats.Interrupts := S.Stats.Interrupts + 1;
            Service;
         end if;
         Answer := CuBit.Handles.Lines.Acknowledge (S.SCI.Line, S.SCI.Vector);
         if Answer.Accepted then
            S.Stats.Acknowledged := S.Stats.Acknowledged + 1;
            S.Stats.Kernel_Deliveries := Answer.Delivered;
         else
            S.Stats.Acknowledge_Refusals := S.Stats.Acknowledge_Refusals + 1;
            ACPI_Native_Log.Critical ("acpi: SCI acknowledgement refused: " &
              LA.Route_Outcome'Image (Answer.Why) & " (the line stays masked)");
         end if;
      end Handle_Event;
   end With_Consumer;
end ACPI_SCI_Events;
