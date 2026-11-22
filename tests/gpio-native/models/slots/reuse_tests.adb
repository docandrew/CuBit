pragma Ada_2022;
with Ada.Text_IO;
with Intel_ADLN_GPIO;
with Intel_GPIO_IRQ;
with Intel_GPIO_Controller;
procedure Reuse_Tests is
   package G renames Intel_ADLN_GPIO;
   package I renames Intel_GPIO_IRQ;
   use type G.Outcome, G.Location, I.Word, I.Tick, I.Ticket, I.Write_ID, I.Trigger;
   type Configurations is array (G.Pad_Number) of I.Word;
   Config : Configurations := [others => 0];
   Enabled, Pending : I.Word := 0;
   Now : I.Tick := 10;
   Clock_OK : Boolean := True;
   Calls, Checks : Natural := 0;
   procedure Check (OK : Boolean; Why : String) is
   begin Checks := Checks + 1; if not OK then raise Program_Error with Why; end if; end Check;
   procedure Clock (Value : out I.Tick; Available : out Boolean) is
   begin Value := Now; Available := Clock_OK; end Clock;
   procedure Read (Plan : G.Plan_Result; ID : I.Read_ID; Value : out I.Word; Status : out I.Access_Status) is
   begin
      Calls := Calls + 1; Check (Plan.Status = G.Ready, "canonical plan reaches callback"); Status := I.Access_OK;
      Value := (case ID is when I.Pad_Owner | I.Configuration_Lock => 0,
         when I.Host_Owner => I.Word'Last, when I.Pad_Configuration => Config (Plan.Selected.Pad),
         when I.Group_Enable => Enabled, when I.Group_Status => Pending);
   end Read;
   procedure Write (Plan : G.Plan_Result; ID : I.Write_ID; Value : I.Word; Status : out I.Access_Status) is
   begin
      Calls := Calls + 1; Check (Plan.Status = G.Ready, "canonical write plan"); Status := I.Access_OK;
      case ID is
         when I.Write_Configuration => Config (Plan.Selected.Pad) := Value;
         when I.Write_Enable => Enabled := Value;
         when I.Clear_Status => Check (Value = Plan.Selected.Interrupt_Mask, "clear selected status only"); Pending := Pending and not Value;
      end case;
   end Write;
   package C is new Intel_GPIO_Controller (2, Clock, Read, Write);
   use type C.Pin_ID, C.Admission, C.Engine.Admission, C.Engine.Phase;
   Plans : constant array (Positive range 1 .. 3) of G.Plan_Result :=
     [G.Plan (G.Target_HID, 160, 16#0094_0000#, 16#700#, 4096),
      G.Plan (G.Target_HID, 161, 16#0094_0000#, 16#700#, 4096),
      G.Plan (G.Target_HID, 162, 16#0094_0000#, 16#700#, 4096)];
   A, B : C.Pin_ID;
   Result : C.Admission;
   Engine_Result : C.Engine.Admission;
   Before : Natural;
   Ticket : I.Ticket;
   procedure Finish is
      Worked : Boolean;
   begin
      for N in 1 .. 64 loop
         exit when C.Observe.Active = C.No_Pin;
         declare Previous : constant Natural := Calls; begin
            C.Advance (Worked); Check (Worked and then Calls <= Previous + 1, "one bounded register access per step");
         end;
      end loop;
      Check (C.Observe.Active = C.No_Pin and then not C.Observe.Faulted, "controller settled without fault");
   end Finish;
   procedure Fault_Run (Position : Positive; Unknown : Boolean) is
      Access_Count, Failure_At : Natural := 0;
      procedure F_Read (Plan : G.Plan_Result; ID : I.Read_ID; Value : out I.Word; Status : out I.Access_Status) is
      begin
         Access_Count := Access_Count + 1; Read (Plan, ID, Value, Status);
         if Access_Count = Failure_At then Status := (if Unknown then I.Unknown_Effect else I.Denied); end if;
      end F_Read;
      procedure F_Write (Plan : G.Plan_Result; ID : I.Write_ID; Value : I.Word; Status : out I.Access_Status) is
      begin
         Access_Count := Access_Count + 1;
         if Access_Count /= Failure_At or else Unknown then Write (Plan, ID, Value, Status); end if;
         if Access_Count = Failure_At then Status := (if Unknown then I.Unknown_Effect else I.Denied); end if;
      end F_Write;
      package FC is new Intel_GPIO_Controller (1, Clock, F_Read, F_Write);
      use type FC.Pin_ID, FC.Admission, FC.Engine.Failure;
      Pin : FC.Pin_ID;
      R : FC.Admission;
      E : FC.Engine.Admission;
      Worked : Boolean;
      Before_Fault : Natural;
      procedure Drain is
      begin
         for N in 1 .. 64 loop
            exit when FC.Observe.Active = FC.No_Pin;
            declare Previous : constant Natural := Access_Count; begin
               FC.Advance (Worked); Check (Access_Count <= Previous + 1, "faulted reuse still one access per step");
            end;
         end loop;
      end Drain;
   begin
      -- Each local controller fault case starts with its own reset register model.
      Config := [others => 0]; Enabled := 0; Pending := 0;
      FC.Register_Parked (Plans (1), I.Level_Low, 1000, 32, Pin, R, E); Drain;
      Check (not FC.Observe.Faulted, "fault fixture starts confirmed parked");
      FC.Release_Parked (Pin, R); Check (R = FC.Accepted, "fault fixture prior owner detached");
      Failure_At := Access_Count + Position;
      FC.Retarget_Parked (Pin, Plans (3), I.Rising_Edge, 1000, 32, R, E);
      Check (R = FC.Accepted, "fault fixture new target accepted"); Drain;
      Check (FC.Observe.Faulted and then Access_Count = Failure_At and then
         FC.Observe_Pin (Pin).Report.Reason = (if Unknown then FC.Engine.Register_Indeterminate else FC.Engine.Register_Refused),
         "every retarget access fault freezes exact target with typed diagnostic");
      Before_Fault := Access_Count;
      FC.Release_Parked (Pin, R); Check (R = FC.Controller_Faulted, "fault cannot release or recycle slot");
      FC.Retarget_Parked (Pin, Plans (1), I.Level_Low, 1000, 32, R, E);
      Check (R = FC.Controller_Faulted, "fault cannot replay a prior target");
      for N in 1 .. 8 loop FC.Advance (Worked); end loop;
      Check (Access_Count = Before_Fault and then not FC.Released (Pin), "unknown/failed target remains retained without cleanup IO");
   end Fault_Run;
begin
   Check (not C.Released (C.No_Pin) and then not C.Released (C.Pin_ID'Last), "unknown slot is never released");
   C.Release_Parked (C.No_Pin, Result); Check (Result = C.Unknown_Pin, "unknown release rejected");
   C.Register_Parked (Plans (1), I.Level_Low, 1000, 32, A, Result, Engine_Result);
   Check (Result = C.Accepted, "first dynamic slot admitted");
   C.Release_Parked (A, Result); Check (Result = C.Busy, "active startup cannot release slot"); Finish;
   C.Register_Parked (Plans (2), I.Rising_Edge, 1000, 32, B, Result, Engine_Result); Finish;
   Check (A /= B and then Enabled = 0, "two independent slots initialized masked");
   Before := Calls;
   C.Retarget_Parked (A, Plans (3), I.Falling_Edge, 1000, 32, Result, Engine_Result);
   Check (Result = C.Owner_Refused and then Calls = Before and then C.Observe_Pin (A).Plan.Selected = Plans (1).Selected,
      "parked but still reserved slot cannot change pin or mode");
   C.Resume (A, 1000, 32, Result, Engine_Result); Finish;
   C.Release_Parked (A, Result); Check (Result = C.Owner_Refused, "listening slot cannot release");
   Pending := Plans (1).Selected.Interrupt_Mask;
   C.Poll (A, 1000, 32, Result, Engine_Result); Finish;
   Ticket := C.Observe_Pin (A).Report.Event;
   Check (Ticket /= I.No_Ticket and then C.Observe_Pin (A).Report.Event_Pending, "prior lease has real retained event");
   C.Release_Parked (A, Result); Check (Result = C.Owner_Refused, "retained event prevents release");
   C.Retire_Event (A, Ticket, Result, Engine_Result); Check (Result = C.Accepted, "event explicitly retired while masked");
   Before := Calls; C.Release_Parked (A, Result);
   Check (Result = C.Accepted and then C.Released (A) and then Calls = Before, "detached confirmed parked slot released with no IO");
   C.Resume (A, 1000, 32, Result, Engine_Result); Check (Result = C.Unknown_Pin and then Calls = Before, "released slot rejects stale direct control");
   C.Release_Parked (A, Result); Check (Result = C.Unknown_Pin, "release is not replayed");
   C.Retarget_Parked (A, Plans (2), I.Falling_Edge, 1000, 32, Result, Engine_Result);
   Check (Result = C.Duplicate_Pin and then Calls = Before and then C.Released (A), "retarget cannot steal reserved neighbor");
   C.Retarget_Parked (A, (Status => G.No_GPIO_Pin), I.Falling_Edge, 1000, 32, Result, Engine_Result);
   Check (Result = C.Invalid_Plan and then C.Released (A), "invalid target preserves released slot");
   Now := 9;
   C.Retarget_Parked (A, Plans (3), I.Falling_Edge, 1000, 32, Result, Engine_Result);
   Check (Result = C.Owner_Refused and then Engine_Result = C.Engine.Clock_Refused and then C.Released (A)
      and then Calls = Before, "retarget preserves prior monotonic clock continuity");
   Now := 10;
   C.Retarget_Parked (A, Plans (3), I.Falling_Edge, 10, 32, Result, Engine_Result);
   Check (Result = C.Owner_Refused and then Engine_Result = C.Engine.Expired and then C.Released (A), "expired retarget has no allocation or effect");
   Clock_OK := False;
   C.Retarget_Parked (A, Plans (3), I.Falling_Edge, 1000, 32, Result, Engine_Result);
   Check (Result = C.Owner_Refused and then Engine_Result = C.Engine.Clock_Refused and then C.Released (A), "missing clock cannot reset old engine");
   Clock_OK := True;
   C.Retarget_Parked (A, Plans (3), I.Falling_Edge, 1000, 32, Result, Engine_Result);
   Check (Result = C.Accepted and then not C.Released (A) and then C.Observe.Active = A and then
      C.Observe_Pin (A).Plan.Selected = Plans (3).Selected and then C.Observe_Pin (A).Mode = I.Falling_Edge,
      "released slot reused for new badge facts without capacity growth");
   C.Release_Parked (B, Result); Check (Result = C.Busy, "retarget owns the controller RMW interval"); Finish;
   Check (Enabled = 0 and then C.Observe_Pin (A).Report.At_Phase = C.Engine.Held_Masked and then
      not C.Observe_Pin (A).Report.Pin_May_Be_Enabled, "retarget cannot arm without later authenticated lease");
   C.Resume (A, 1000, 32, Result, Engine_Result); Finish;
   Pending := Plans (3).Selected.Interrupt_Mask;
   C.Poll (A, 1000, 32, Result, Engine_Result); Finish;
   Check (C.Observe_Pin (A).Report.Event = Ticket + 1, "event generation never resets across slot reuse");
   C.Retire_Event (A, Ticket, Result, Engine_Result);
   Check (Result = C.Owner_Refused and then Engine_Result = C.Engine.Wrong_Ticket, "prior slot ticket cannot retire new pin event");
   C.Retire_Event (A, Ticket + 1, Result, Engine_Result); C.Release_Parked (A, Result);
   Check (Result = C.Accepted and then C.Released (A), "second lifetime returns to parked release");
   Check (C.Observe_Pin (B).Plan.Selected = Plans (2).Selected and then C.Observe_Pin (B).Mode = I.Rising_Edge,
      "neighbor identity and mode remain unchanged");
   for Pass in 1 .. 16 loop
      C.Retarget_Parked (A, Plans (if Pass mod 2 = 0 then 3 else 1), I.Level_High, 1000, 32, Result, Engine_Result);
      Check (Result = C.Accepted, "slot can repeatedly admit different firmware pins beyond capacity count"); Finish;
      C.Release_Parked (A, Result); Check (Result = C.Accepted and then C.Released (A), "repeated reuse returns to released parked state");
   end loop;
   for Position in 1 .. 10 loop Fault_Run (Position, False); Fault_Run (Position, True); end loop;
   Ada.Text_IO.Put_Line ("GPIO dynamic slot checks" & Checks'Image);
end Reuse_Tests;
