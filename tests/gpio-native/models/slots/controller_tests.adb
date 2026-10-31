pragma Ada_2022;
with Ada.Text_IO;
with Intel_ADLN_GPIO;
with Intel_GPIO_IRQ;
with Intel_GPIO_Controller;
procedure Controller_Tests is
   package G renames Intel_ADLN_GPIO;
   package I renames Intel_GPIO_IRQ;
   use type G.Outcome;
   use type G.Pad_Number;
   use type I.Word;
   use type I.Ticket;
   use type I.Access_Status;
   Checks : Natural := 0;
   procedure Check (OK : Boolean; Why : String) is
   begin
      if not OK then raise Program_Error with Why; end if;
      Checks := Checks + 1;
   end Check;
   procedure Case_Run (Fault_At : Natural; Unknown : Boolean) is
      First_Plan : constant G.Plan_Result := G.Plan (G.Target_HID, 160, 16#0094_0000#, 16#700#, 4096);
      Second_Plan : constant G.Plan_Result := G.Plan (G.Target_HID, 161, 16#0094_0000#, 16#700#, 4096);
      Third_Plan : constant G.Plan_Result := G.Plan (G.Target_HID, 162, 16#0094_0000#, 16#700#, 4096);
      Config : array (G.Pad_Number) of I.Word := [others => 0];
      Enable, Status_Bits : I.Word := 0;
      Calls, Fail_IO : Natural := 0;
      procedure Clock (Now : out I.Tick; Available : out Boolean);
      procedure Read
        (Plan : G.Plan_Result; ID : I.Read_ID; Value : out I.Word; Status : out I.Access_Status);
      procedure Write
        (Plan : G.Plan_Result; ID : I.Write_ID; Value : I.Word; Status : out I.Access_Status);
      package C is new Intel_GPIO_Controller (2, Clock, Read, Write);
      use type C.Pin_ID;
      use type C.Admission;
      use type C.Engine.Admission;
      use type C.Engine.Phase;
      use type C.Engine.Failure;
      procedure No_Reentry is
         Worked : Boolean;
         Before : constant Natural := Calls;
      begin
         C.Advance (Worked);
         Check (not Worked and Calls = Before, "callbacks cannot reenter a controller operation");
      end No_Reentry;
      procedure Clock (Now : out I.Tick; Available : out Boolean) is
      begin
         No_Reentry; Now := 1; Available := True;
      end Clock;
      procedure Context (Plan : G.Plan_Result; Status : out I.Access_Status) is
      begin
         Check (Plan.Status = G.Ready and then C.Observe.Active /= C.No_Pin and then
           C.Observe_Pin (C.Observe.Active).Plan.Selected.Pad = Plan.Selected.Pad,
           "only active pin plan reaches shared backend");
         No_Reentry; Calls := Calls + 1;
         Status := (if Calls = Fail_IO then (if Unknown then I.Unknown_Effect else I.Denied) else I.Access_OK);
      end Context;
      procedure Read
        (Plan : G.Plan_Result; ID : I.Read_ID; Value : out I.Word; Status : out I.Access_Status) is
      begin
         Context (Plan, Status);
         Value := (case ID is when I.Pad_Owner | I.Configuration_Lock => 0,
           when I.Host_Owner => 7, when I.Group_Enable => Enable,
           when I.Group_Status => Status_Bits,
           when I.Pad_Configuration => Config (Plan.Selected.Pad));
      end Read;
      procedure Write
        (Plan : G.Plan_Result; ID : I.Write_ID; Value : I.Word; Status : out I.Access_Status) is
      begin
         Context (Plan, Status);
         if Status in I.Denied | I.Unavailable then return; end if;
         case ID is
            when I.Write_Configuration => Config (Plan.Selected.Pad) := Value;
            when I.Write_Enable =>
               Check (((Value xor Enable) and not Plan.Selected.Interrupt_Mask) = 0,
                 "shared RMW preserves every neighbor pin enable");
               Enable := Value;
            when I.Clear_Status =>
               Check (Value = Plan.Selected.Interrupt_Mask, "one pin W1C only");
               Status_Bits := Status_Bits and not Value;
         end case;
      end Write;
      procedure Step is
         Worked : Boolean;
         Before : constant Natural := Calls;
      begin
         C.Advance (Worked);
         Check (Worked and Calls <= Before + 1, "one bounded child step");
      end Step;
      procedure Finish is
         Steps : Natural := 0;
      begin
         while C.Observe.Active /= C.No_Pin loop
            Steps := Steps + 1; Check (Steps <= 32, "lease completes or faults in budget");
            Step;
         end loop;
      end Finish;
      P1, P2, Refused : C.Pin_ID;
      Admission : C.Admission;
      Owner : C.Engine.Admission;
      Before : Natural;
      Event : I.Ticket;
      Value : I.Word;
      Port_Status : I.Access_Status;
   begin
      Check (First_Plan.Status = G.Ready and Second_Plan.Status = G.Ready, "reference pin plans");
      C.Dispatched_Read (I.Group_Enable, Value, Port_Status);
      Check (Port_Status = I.Unavailable and Value = 0 and Calls = 0, "outside-step read has no backend context");
      C.Dispatched_Write (I.Write_Enable, 7, Port_Status);
      Check (Port_Status = I.Unavailable and Calls = 0, "outside-step write has no backend context");
      C.Poll (C.No_Pin, 1000, 32, Admission, Owner);
      Check (Admission = C.Unknown_Pin and Calls = 0, "unknown pin no IO");
      C.Register_Pin ((Status => G.No_GPIO_Pin), I.Level_Low, 1000, 32, Refused, Admission, Owner);
      Check (Admission = C.Invalid_Plan and Refused = C.No_Pin and Calls = 0, "invalid plan no slot or IO");
      C.Register_Pin (First_Plan, I.Level_Low, 1000, 32, P1, Admission, Owner);
      Check (Admission = C.Accepted and P1 /= C.No_Pin and C.Observe.Active = P1 and Calls = 0,
         "registration holds setup lease before MMIO");
      while C.Observe_Pin (P1).Report.At_Phase /= C.Engine.Read_Mask loop Step; end loop;
      Step; -- RMW read completed; write is pending across this public call boundary.
      Check (C.Observe_Pin (P1).Report.At_Phase = C.Engine.Mask_Write, "RMW split-step fixture");
      Before := Calls;
      C.Register_Pin (Second_Plan, I.Rising_Edge, 1000, 32, Refused, Admission, Owner);
      Check (Admission = C.Busy and Refused = C.No_Pin and Calls = Before,
         "second setup cannot enter between shared RMW read/write");
      Finish;
      Check (Enable = 1 and C.Observe_Pin (P1).Report.At_Phase = C.Engine.Listening,
         "first pin configured and lease released only at stable state");
      C.Register_Pin (Second_Plan, I.Rising_Edge, 1000, 32, P2, Admission, Owner);
      Check (Admission = C.Accepted and P2 /= P1, "second pin has distinct process-lifetime ID");
      Before := Calls;
      if Fault_At > 0 then Fail_IO := Calls + Fault_At; end if;
      Finish;
      if Fault_At > 0 then
         Check (C.Observe.Faulted and C.Observe.Last_Fault = P2 and Calls = Before + Fault_At,
           "child fault freezes entire controller at exact access");
         Check (C.Observe_Pin (P1).Report.At_Phase = C.Engine.Listening and
           C.Observe_Pin (P2).Report.Reason =
             (if Unknown then C.Engine.Register_Indeterminate else C.Engine.Register_Refused),
           "typed fault retained beside untouched live neighbor state");
         Before := Calls;
         C.Poll (P1, 1000, 32, Admission, Owner);
         Check (Admission = C.Controller_Faulted and Calls = Before,
           "neighbor cannot run after shared controller fault");
         C.Register_Pin (Third_Plan, I.Level_Low, 1000, 32, Refused, Admission, Owner);
         Check (Admission = C.Controller_Faulted and Calls = Before, "fault cannot allocate/retry");
         declare Worked : Boolean; begin C.Advance (Worked);
            Check (not Worked and Calls = Before, "faulted controller never resumes IO"); end;
         return;
      end if;
      Check (Enable = 3, "second setup preserves first pin enable");
      Before := Calls;
      C.Register_Pin (First_Plan, I.Level_Low, 1000, 32, Refused, Admission, Owner);
      Check (Admission = C.Duplicate_Pin and Calls = Before, "duplicate physical pin rejected");
      C.Register_Pin (Third_Plan, I.Level_Low, 1000, 32, Refused, Admission, Owner);
      Check (Admission = C.Capacity_Refused and Calls = Before, "explicit registry capacity before slot/IO");
      Status_Bits := 3;
      C.Poll (P1, 1000, 32, Admission, Owner); Step;
      Before := Calls;
      C.Poll (P2, 1000, 32, Admission, Owner);
      Check (Admission = C.Busy and Calls = Before, "event RMW lease excludes other pin polling");
      Finish; Event := C.Observe_Pin (P1).Report.Event;
      Check (Enable = 2 and Status_Bits = 2 and C.Observe_Pin (P1).Report.Event_Pending,
         "first event masks/acks selected pin and retains neighbor IRQ");
      C.Poll (P2, 1000, 32, Admission, Owner); Finish;
      Check (Enable = 0 and Status_Bits = 0 and C.Observe_Pin (P2).Report.Event_Pending,
         "both pin events can be retained independently without active lease");
      Before := Calls;
      C.Rearm (P1, Event + 1, 1000, 32, Admission, Owner);
      Check (Admission = C.Owner_Refused and Owner = C.Engine.Wrong_Ticket and Calls = Before,
         "incorrect event generation cannot acquire rearm lease");
      Status_Bits := 1;
      C.Rearm (P1, Event, 1000, 32, Admission, Owner);
      Check (Admission = C.Accepted, "exact pin/event pair rearms");
      C.Rearm (P2, C.Observe_Pin (P2).Report.Event, 1000, 32, Admission, Owner);
      Check (Admission = C.Busy, "neighbor rearm cannot interrupt active rearm");
      Finish;
      Check (Enable = 1 and Status_Bits = 1, "masked-time edge survives serialized rearm");
      C.Poll (P1, 1000, 32, Admission, Owner); Finish;
      Check (C.Observe_Pin (P1).Report.Event = Event + 1 and
         C.Observe_Pin (P2).Report.Event = 1, "generations remain scoped to each pin");
      declare Worked : Boolean; begin Before := Calls; C.Advance (Worked);
         Check (not Worked and Calls = Before, "idle controller performs no implicit work"); end;
   end Case_Run;
begin
   Case_Run (0, False);
   for Fault_At in 1 .. 13 loop
      Case_Run (Fault_At, False); Case_Run (Fault_At, True);
   end loop;
   Ada.Text_IO.Put_Line ("GPIO controller arbiter checks" & Checks'Image);
end Controller_Tests;
