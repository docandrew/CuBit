pragma Ada_2022;
with Main_Model;
with GPIO_Admin_Wire;
with GPIO_Loop_Time;
package body GPIO_Native_Pump is
   package B renames Main_Model;
   package A renames GPIO_Admin_Wire;
   package W renames B.W;
   use type B.Scenario, W.Completion_Status, B.I.Word;
   procedure Poll_Event (S : in out Driver.Admin.State; Found : out Boolean) is
   begin
      Found := B.Stop_Queued; if not Found then return; end if;
      B.Stop_Queued := False;
      Driver.Registry.Request_Stop (GPIO_Loop_Time.After (Driver.Controller.I.Tick (B.Clock), Cleanup_Window));
      Driver.Admin.Request_Stop (S);
      Disable_Logging ("modeled-stop");
   end Poll_Event;
   procedure Poll_Request (S : in out Driver.Admin.State; Found : out Boolean) is
      Taken : Boolean;
      Setup : constant Driver.Controller.I.Tick := GPIO_Loop_Time.After (Driver.Controller.I.Tick (B.Clock), Setup_Window);
      Cleanup : constant Driver.Controller.I.Tick := GPIO_Loop_Time.After (Driver.Controller.I.Tick (B.Clock), Cleanup_Window);
   begin
      Found := False;
      if not B.Ready_ACK or else Driver.Admin.Observe (S).Stopping then return; end if;
      case B.Request_Stage is
         when 0 =>
            B.Request_Stage := 1; Found := True;
            Driver.Admin.Handle (S, B.Launcher_ID, A.OP_GPIO_ADMIT_SINK, 1, [], True, True, Setup, Cleanup);
         when 1 =>
            if B.Current = B.Stop_During_Admission then
               if B.Saved_Answers = 0 then B.Stop_Queued := True; end if;
            elsif Driver.Admin.Quiescent (S) and then B.Last_Admin = W.Completed then
               Driver.Registry.Offer (B.Client_ID, B.Badge, (W.Arm, (W.Within, W.Millisecond (Setup))), 2, Taken);
               B.Check (Taken, "real registry accepts modeled arm"); B.Request_Stage := 2; Found := True;
            end if;
         when 2 =>
            if B.Control_Answers = 1 then
               B.Check (B.Last_Control.Status = W.Completed, "real GPIO owner completes arm");
               declare L : constant B.G.Location := B.G.Locate (160); begin
                  B.Memory (L.Resource) (Natural (L.Status_Register) / 4) :=
                     B.Memory (L.Resource) (Natural (L.Status_Register) / 4) or L.Interrupt_Mask;
               end;
               B.Request_Stage := 3;
            end if;
         when 3 =>
            if B.Event_Deliveries = 1 then
               Driver.Registry.Offer (B.Client_ID, B.Badge,
                  (W.Acknowledge, (W.Within, W.Millisecond (Setup)), B.Last_Event.Lease, B.Last_Event.Generation), 3, Taken);
               B.Check (Taken, "real registry accepts exact event acknowledgement"); B.Request_Stage := 4; Found := True;
            end if;
         when 4 =>
            if B.Control_Answers = 2 then
               B.Check (B.Last_Control.Status = W.Completed, "real owner completes event ACK");
               Driver.Admin.Handle (S, B.Launcher_ID, A.OP_GPIO_RETIRE_SINK, 1,
                  A.Encode ((A.Retire_Sink, B.Badge, B.Client_ID)), True, False, Setup, Cleanup);
               B.Request_Stage := 5; Found := True;
            end if;
         when 5 =>
            if Driver.Admin.Quiescent (S) and then B.Saved_Answers = 2 then
               B.Check (B.Last_Admin = W.Completed and then Driver.Registry.Quiescent, "real retirement consumed reply and released lease");
               B.Stop_Queued := True; B.Request_Stage := 6;
            end if;
         when others => null;
      end case;
   end Poll_Request;
end GPIO_Native_Pump;
