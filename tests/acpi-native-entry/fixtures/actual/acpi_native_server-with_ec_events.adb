with Ada.Unchecked_Deallocation;
with ACPI_System_Events;
package body ACPI_Native_Server.With_EC_Events is
   package Control renames ACPI_EC_Supervisor_Control;
   use type Pump.Outcome;
   use type Receiver.Outcome;
   use type Events.Controller.Admission;
   procedure Free is new Ada.Unchecked_Deallocation (Receiver.Delivery, Delivery_Access);
   procedure Advance (Server : in out ACPI_Requests.State; Result : out Owner_Outcome) is
      Sender : CuBit.Messages.Process_ID;
      Item : ACPI_EC_Event_Types.Notice;
      Found, Healthy, Accepted : Boolean;
      State_View : Control.Snapshot;
      Delivery_Result : Receiver.Outcome;
      procedure Finish (Status : Outcome; Owner : Owner_Outcome) is
      begin
         Last.Status := Status;
         Control.Finish_Advance (Lifecycle, Terminal => Owner = Owner_Refused);
         Result := Owner;
      end Finish;
   begin
      Result := Owner_Refused;
      Control.Enter_Advance (Lifecycle, Accepted);
      if not Accepted then return; end if;
      State_View := Control.View (Lifecycle);
      Last := (others => <>);
      Last.Completion := State_View.Pending;
      Last.Completion_Charge := State_View.Charge;
      -- An exception or reentrant call must see a terminal owner and retained
      -- state. Only a normal bounded result may reopen the next local turn.
      if Receiver.Recovery_Needed then
         Last.Fatal := Receiver.Fatal_Diagnostic;
         Finish (Recovery_Required, Owner_Refused); return;
      end if;
      Pump.Run (Server, Claims_Per_Turn, Last.Notifications);
      if Last.Notifications.Status = Pump.Limit_Reached then
         Finish (Notifications_Pending, Owner_Pending); return;
      elsif Last.Notifications.Status /= Pump.Queue_Empty then
         Finish (Notifications_Refused, Owner_Refused); return;
      end if;
      if State_View.Pending.Present then
         Last.Completion_Attempted := True;
         Complete_Event (State_View.Pending.Item, State_View.Charge, Last.Completion_Confirmed);
         if not Last.Completion_Confirmed then
            Finish (Completion_Unconfirmed, Owner_Refused); return;
         end if;
         Control.Confirm (Lifecycle, State_View.Pending.Item, Accepted);
         if not Accepted then Finish (Completion_Unconfirmed, Owner_Refused); return; end if;
      end if;
      Take_Event (Sender, Item, Found, Healthy);
      Last.Source_Found := Found;
      Last.Source_Item := (if Found then (Present => True, Item => Item) else (Present => False));
      if not Healthy then Finish (Source_Unhealthy, Owner_Refused); return; end if;
      if not Found then Finish (Source_Empty, Owner_Idle); return; end if;
      -- At most one allocation is live. Successful, non-pending deliveries
      -- are released before the next turn; failures stay process-owned.
      Active := new Receiver.Delivery;
      Events.Dispatch (Server, Active.all, Sender, Item, Fuel_Per_Event,
        Last.Admission, Delivery_Result);
      Last.Delivery := Receiver.Observe (Active.all);
      Last.Event := Receiver.Receipt (Active.all);
      if Receiver.Recovery_Needed then
         Last.Fatal := Receiver.Fatal_Diagnostic;
         Finish (Recovery_Required, Owner_Refused); return;
      end if;
      if Last.Admission /= Events.Controller.Admitted
        or else Receiver.Pending (Active.all)
        or else Delivery_Result not in Receiver.Completed | Receiver.Handler_Missing
        or else not Last.Event.Present
      then Finish (Event_Refused, Owner_Refused); return; end if;
      Control.Retain (Lifecycle, Last.Event.Item, Last.Delivery.Invocation.Charged, Fuel_Per_Event, Accepted);
      if not Accepted then Finish (Event_Refused, Owner_Refused); return; end if;
      Free (Active);
      Finish (Event_Processed, Owner_Pending);
   end Advance;
   function Observation return Report is (Last);
   function Observation_Complete return Boolean is (Control.Report_Complete (Lifecycle));
   function Is_Stopped return Boolean is (Control.Entry_Closed (Lifecycle));
   procedure Request_Stop (Result : out Stop_Result) is
      Decision : Control.Stop_Result;
   begin
      Control.Request_Stop (Lifecycle, Decision);
      case Decision is
         when Control.Stop_Already_Closed => Result := Already_Stopped;
         when Control.Stop_Incomplete => Result := Operation_Incomplete;
         when Control.Stop_Accepted =>
            Last.Status := Stopped_By_Owner;
            Last.Completion := Control.View (Lifecycle).Pending;
            Last.Completion_Charge := Control.View (Lifecycle).Charge;
            Last.Completion_Attempted := False;
            Last.Completion_Confirmed := False;
            Result := Stopped;
      end case;
   end Request_Stop;
   function Pending_Completion return ACPI_EC_Event_Types.Notice_Option is (Control.View (Lifecycle).Pending);
   procedure Cleanup
     (Server : in out ACPI_Requests.State; Result : out Receiver.Close_Result) is
      Accepted : Boolean;
   begin
      Result := Receiver.Receiver_Busy;
      if not Control.Cleanup_Allowed (Lifecycle) then return; end if;
      Result := Receiver.No_Pending_Result;
      if Active = null then return; end if;
      Control.Enter_Cleanup (Lifecycle, Accepted);
      if not Accepted then Result := Receiver.Receiver_Busy; return; end if;
      Events.Close (Server, Active.all, Result);
      Last.Delivery := Receiver.Observe (Active.all);
      Control.Finish_Cleanup (Lifecycle);
   end Cleanup;
   package body With_Dispatch is
      In_Serve, Stop_Denied : Boolean := False;
      procedure Route_Event
        (Publisher : CuBit.Messages.Process_ID; Item : CuBit.Messages.Message;
         Stop, Handled : out Boolean) is
         Decision : Stop_Result;
      begin
         case ACPI_System_Events.Classify (Publisher, Item) is
            when ACPI_System_Events.Stop_Requested => Stop := True; Handled := True;
            when ACPI_System_Events.Rejected => Stop := False; Handled := True;
            when others => Handle_Event (Publisher, Item, Stop, Handled);
         end case;
         if Stop then
            Request_Stop (Decision);
            Stop_Denied := Decision = Operation_Incomplete;
         end if;
      end Route_Event;
      package Loop_Driver is new ACPI_Native_Server.With_Dispatch
        (Dispatch_Message, Handle_Event => Route_Event, Advance_Owner => Advance);
      procedure Serve (Server : in out ACPI_Requests.State; Reason : out Stop_Reason) is
         Decision : Stop_Result;
      begin
         Reason := Owner_Work_Refused;
         if In_Serve or else Is_Stopped then return; end if;
         In_Serve := True; Stop_Denied := False;
         Loop_Driver.Serve (Server, Reason);
         if Stop_Denied then Reason := Owner_Work_Refused;
         elsif Reason = Unexpected_Completion then Request_Stop (Decision);
         end if;
         In_Serve := False;
      end Serve;
   end With_Dispatch;
end ACPI_Native_Server.With_EC_Events;
