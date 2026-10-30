pragma Ada_2022;
with Ada.Text_IO;
with DW_I2C_Configuration; use DW_I2C_Configuration;
procedure Configuration_Tests is
   use type Word;
   use type T.Tick;
   use type T.Count;
   type Entry_Record is record
      ID : Register_ID;
      Written : Boolean;
      Value : Word;
   end record;
   Trace : array (1 .. 1024) of Entry_Record;
   Used, Clock_Calls, Fail_At, Clock_Fail_At, Clock_Expire_At, Clock_Reverse_At : Natural := 0;
   Inject_Status : Access_Status := Access_OK;
   Pending_RX : Word := 0;
   Bad_TX, Bad_RX, Stuck_RX, Is_Enabled, Hold_Disabled, Hold_Enabled : Boolean := False;
   Abort_Value : Word := 16#ABCD#;
   Checks : Natural := 0;
   Settings : constant Configuration := (Fast, 16#15#, 100, 200, True, 16#10001#, 16, 4);
   procedure Check (OK : Boolean; Context : String) is
   begin
      if not OK then raise Program_Error with Context; end if;
      Checks := Checks + 1;
   end Check;
   procedure Reset is
   begin
      Used := 0; Clock_Calls := 0; Fail_At := 0;
      Clock_Fail_At := 0; Clock_Expire_At := 0; Clock_Reverse_At := 0;
      Inject_Status := Access_OK;
      Pending_RX := 0; Bad_TX := False; Bad_RX := False; Stuck_RX := False;
      Is_Enabled := False; Hold_Disabled := False; Hold_Enabled := False;
      Abort_Value := 16#ABCD#;
   end Reset;
   procedure Clock (Now : out T.Tick; Available : out Boolean) is
   begin
      Clock_Calls := Clock_Calls + 1;
      Now := (if Clock_Calls = Clock_Expire_At then 100
              elsif Clock_Calls = Clock_Reverse_At then 0 else 1);
      Available := Clock_Calls /= Clock_Fail_At;
   end Clock;
   procedure Record_Access (ID : Register_ID; Written : Boolean; Value : Word;
                            Status : out Access_Status) is
   begin
      Used := Used + 1; Trace (Used) := (ID, Written, Value);
      Status := (if Used = Fail_At then Inject_Status else Access_OK);
   end Record_Access;
   procedure Read (ID : Register_ID; Value : out Word; Status : out Access_Status) is
   begin
      Value := 0;
      case ID is
         when Enable_Status =>
            Value := (if Is_Enabled then (if Hold_Enabled then 0 else Enabled_Bit)
                      else (if Hold_Disabled then Enabled_Bit else 0));
         when Controller_Status => Value := 0;
         when TX_Level => Value := (if Bad_TX then 1 else 0);
         when RX_Level => Value := (if Bad_RX then Word'Last else Pending_RX);
         when Data_Command =>
            Check (Pending_RX > 0, "never drain empty RX");
            if not Stuck_RX and then
              not (Used + 1 = Fail_At and then Inject_Status in Unavailable | Denied)
            then Pending_RX := Pending_RX - 1;
            end if;
            Value := 16#44#;
         when Abort_Source => Value := Abort_Value;
         when Clear_Interrupts =>
            if not (Used + 1 = Fail_At and then Inject_Status in Unavailable | Denied)
            then Abort_Value := 0;
            end if;
         when others => raise Program_Error with "unexpected setup read";
      end case;
      Record_Access (ID, False, Value, Status);
   end Read;
   procedure Write (ID : Register_ID; Value : Word; Status : out Access_Status) is
   begin
      case ID is
         when Interrupt_Mask | Standard_High | Standard_Low | Fast_High | Fast_Low |
              SDA_Hold | Control | Target | TX_Threshold | RX_Threshold => null;
         when Enable =>
            if not (Used + 1 = Fail_At and then Inject_Status in Unavailable | Denied)
            then Is_Enabled := Value = Enabled_Bit;
            end if;
         when others => raise Program_Error with "unexpected setup write";
      end case;
      Record_Access (ID, True, Value, Status);
   end Write;
   procedure Advance is new Step (Clock, Read, Write);
   procedure Run (S : in out State) is
      Calls : Natural := 0;
   begin
      while Observe (S).At_Stage not in Ready | Failed loop
         Calls := Calls + 1;
         Check (Calls < 300, "bounded test progress");
         declare Before : constant Natural := Used;
         begin
            Advance (S);
            Check (Used <= Before + 1, "at most one access per step");
         end;
      end loop;
   end Run;
   procedure Nominal (Mode : Speed_Mode; Hold : Boolean; RX : Word) is
      S : State;
      Config : Configuration := Settings;
      Admitted : Admission;
      Info : Report;
      High_Seen, Low_Seen, Hold_Seen, Cleared : Boolean := False;
   begin
      Reset; Pending_RX := RX; Config.Speed := Mode; Config.Program_Hold := Hold;
      Start (S, Config, True, Minimum_Hold_Version, 1, 100, 100, Admitted);
      Check (Admitted = Accepted, "nominal accepted");
      Run (S); Info := Observe (S);
      Check (Info.At_Stage = Ready and then Info.Reason = None, "configuration ready");
      Check (Info.Abort_Copied and then Info.Abort_Value = 16#ABCD# and then Abort_Value = 0,
             "abort diagnostic copied before destructive clear");
      Check (Info.Drained_Bytes = T.Count (RX) and then Pending_RX = 0, "bounded stale drain");
      Check (Info.May_Be_Enabled and then Info.Writes_Completed = (if Hold then 10 else 9),
             "successful write accounting");
      for I in 1 .. Used loop
         if Trace (I).Written then
            case Trace (I).ID is
               when Standard_High | Fast_High =>
                  Check (not High_Seen and then Trace (I).Value = 100
                     and then Trace (I).ID = (if Mode = DW_I2C_Configuration.Standard then Standard_High else Fast_High), "selected high register");
                  High_Seen := True;
               when Standard_Low | Fast_Low =>
                  Check (not Low_Seen and then Trace (I).Value = 200
                     and then Trace (I).ID = (if Mode = DW_I2C_Configuration.Standard then Standard_Low else Fast_Low), "selected low register");
                  Low_Seen := True;
               when SDA_Hold => Hold_Seen := True;
                  Check (Hold and then Trace (I).Value = 16#10001#, "explicit hold field");
               when Control => Check (Trace (I).Value = (if Mode = DW_I2C_Configuration.Standard then 16#63# else 16#65#), "control word");
               when Target => Check (Trace (I).Value = 16#15#, "explicit target");
               when TX_Threshold => Check (Trace (I).Value = 8, "TX threshold from depth");
               when RX_Threshold => Check (Trace (I).Value = 0, "RX threshold");
               when Interrupt_Mask => Check (I = 1 and then Trace (I).Value = 0, "mask before reconfiguration");
               when Enable =>
                  if Trace (I).Value = 1 then Check (Cleared, "clear stale status before enable"); end if;
               when others => raise Program_Error;
            end case;
         elsif Trace (I).ID = Clear_Interrupts then Cleared := True;
         end if;
      end loop;
      Check (High_Seen and then Low_Seen and then Hold_Seen = Hold, "timing selectors complete");
      declare Before : constant Natural := Used;
      begin
         Advance (S); Check (Used = Before, "ready never repeats IO");
         Start (S, Config, True, Minimum_Hold_Version, 1, 100, 100, Admitted);
         Check (Admitted = Busy, "ready owner cannot restart setup");
         declare Retired : Boolean;
         begin
            Retire (S, Retired);
            Check (Retired and then Observe (S).At_Stage = Idle and then Used = Before,
                   "explicit ready retirement resets state without IO");
            Retire (S, Retired);
            Check (not Retired and then Used = Before, "retirement cannot repeat");
            Start (S, Config, True, Minimum_Hold_Version, 1, 100, 100, Admitted);
            Check (Admitted = Accepted, "retired configuration accepts a new target plan");
         end;
      end;
   end Nominal;
   procedure Faults (Expected : Failure) is
      S : State;
      Admitted : Admission;
      Before : Natural;
   begin
      Start (S, Settings, True, Minimum_Hold_Version, 1, 100, 100, Admitted);
      Run (S);
      Check (Observe (S).At_Stage = Failed, "fault terminal");
      Check (Observe (S).Reason = Expected, "typed fault reason");
      if Clock_Fail_At = 32 or else Clock_Expire_At = 32 or else Clock_Reverse_At = 32 then
         Check (Observe (S).May_Be_Enabled and then Observe (S).Writes_Completed = 10,
                "successful enable is retained after post-access clock failure");
      end if;
      Before := Used; Advance (S);
      Check (Used = Before, "failed owner never replays access");
      Start (S, Settings, True, Minimum_Hold_Version, 1, 100, 100, Admitted);
      Check (Admitted = Busy, "failed owner cannot restart setup");
      declare Retired : Boolean;
      begin
         Retire (S, Retired);
         Check (not Retired and then Observe (S).At_Stage = Failed and then Used = Before,
                "failed configuration never retires or hides evidence");
      end;
   end Faults;
begin
   for Mode in Speed_Mode loop
      for Hold in Boolean loop
         for RX in Word range 0 .. 4 loop Nominal (Mode, Hold, RX); end loop;
      end loop;
   end loop;
   for At_Access in 1 .. 17 loop
      for Status in Access_Status range Unavailable .. Unknown_Effect loop
         Reset; Fail_At := At_Access; Inject_Status := Status;
         Faults ((if Status = Unknown_Effect then Register_Indeterminate else Register_Refused));
         Check (Used = At_Access, "register error stops exactly at failing access");
      end loop;
   end loop;
   for At_Call in 1 .. 34 loop
      Reset; Clock_Fail_At := At_Call; Faults (Clock_Unavailable);
      Reset; Clock_Expire_At := At_Call; Faults (Deadline_Expired);
      Reset; Clock_Reverse_At := At_Call; Faults (Clock_Reversed);
   end loop;
   declare
      S : State; Admitted : Admission;
   begin
      Reset;
      Start (S, Settings, False, Minimum_Hold_Version, 1, 100, 10, Admitted);
      Check (Admitted = Reset_Not_Established, "LPSS reset prerequisite required");
      Start (S, Settings, True, Minimum_Hold_Version, 100, 100, 10, Admitted);
      Check (Admitted = Expired, "expired start refused");
      Start (S, Settings, True, Minimum_Hold_Version - 1, 1, 100, 10, Admitted);
      Check (Admitted = Unsupported_Hold and then Used = 0, "hold version gate before IO");
      Start (S, Settings, True, Minimum_Hold_Version, 1, 100, 10, Admitted);
      Run (S);
      Check (Observe (S).Reason = Budget_Exhausted and then Used = 10,
             "independent step budget bounds constant clock");
   end;
   Reset; Hold_Disabled := True;
   declare S : State; A : Admission;
   begin
      Start (S, Settings, True, Minimum_Hold_Version, 1, 100, 10, A); Run (S);
      Check (Observe (S).Reason = Budget_Exhausted and then Used = 10
         and then Observe (S).May_Be_Enabled, "disable wait cannot program controller");
   end;
   Reset; Bad_TX := True; Faults (TX_Not_Empty);
   Reset; Bad_RX := True; Faults (Invalid_RX_Level);
   Reset; Pending_RX := 4; Stuck_RX := True; Faults (Drain_Limit_Reached);
   Reset; Hold_Enabled := True; Faults (Budget_Exhausted);
   Ada.Text_IO.Put_Line ("DesignWare configuration checks" & Checks'Image);
end Configuration_Tests;
