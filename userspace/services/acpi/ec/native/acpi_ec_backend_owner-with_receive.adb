pragma Ada_2022;
with ACPI_System_Events;
package body ACPI_EC_Backend_Owner.With_Receive is
   use type Receiver.Stop_Reason, Receiver.Drain_Status;
   use type Events.Phase, Controller.Admission_Status, Registers.Submission;
   use type Events.Query_Response;
   use type Scans.Phase;
   procedure Settle_Scan is
      Fact : constant Events.Report := Events.Observe;
   begin
      if Query_Count = 0 or else Query_Requests (Query_Head).Kind /= Query_Scan
        or else Scans.View (Query_Requests (Query_Head).Scan).Stage /= Scans.Waiting then return; end if;
      if Fact.Stage = Events.Ready then
         if Fact.Response in Events.No_Pending_Event | Events.Zero_Query_Response then
            Scans.Record_Empty (Query_Requests (Query_Head).Scan,
               (if Fact.Response = Events.No_Pending_Event then Scans.Status_Clear else Scans.Zero_Response));
            Last_Scan_Empty := (if Fact.Response = Events.No_Pending_Event
               then No_Pending_Event else Zero_Query_Response);
            Query_Head := (if Query_Head = Query_Slot'Last then Query_Slot'First else Query_Head + 1);
            Query_Count := Query_Count - 1;
         elsif Fact.Response = Events.Event_Response then
            Scans.Record_Event (Query_Requests (Query_Head).Scan);
         else
            Scans.Fail (Query_Requests (Query_Head).Scan);
         end if;
      elsif Fact.Stage in Events.Query_Failed | Events.Correlation_Failed |
        Events.Configuration_Refused | Events.Unconfigured then
         Scans.Fail (Query_Requests (Query_Head).Scan);
      end if;
   end Settle_Scan;
   function Query_Runnable return Boolean is
     (Query_Count > 0 and then not Registers.Active
      and then not Registers.Retained_For_Recovery and then not Calls.Faulted
      and then Events.Observe.Stage = Events.Ready
      and then (Query_Requests (Query_Head).Kind = Single_Query
        or else Scans.Can_Start (Query_Requests (Query_Head).Scan)));
   procedure Schedule_Queries (Runnable, Stop, Receive_Request : out Boolean) is
      Status : Controller.Admission_Status;
      Submission : Registers.Submission;
   begin
      Settle_Scan;
      Runnable := False; Stop := Query_Overflow; Receive_Request := True;
      if Stop then Receive_Request := False; return; end if;
      if Query_Runnable then
         if Prefer_Field then
            -- Give one service opportunity between completed query episodes.
            Prefer_Field := False;
         else
            Controller.Start_Query (Query_Requests (Query_Head).Attempts, Status, Submission);
            Queued_Query_Status := (case Status is
               when Controller.Clock_Unavailable => Query_Clock_Unavailable,
               when Controller.Deadline_Overflow => Query_Deadline_Overflow,
               when Controller.Invalid_Policy => Query_Invalid_Policy,
               when Controller.Admitted | Controller.Refused =>
                 (case Submission is
                    when Registers.Started => Query_Started, when Registers.Busy => Query_Busy,
                    when Registers.Denied => Query_Denied,
                    when Registers.Engine_Unavailable => Query_Engine_Unavailable,
                    when Registers.Exhausted => Query_Exhausted));
            if Status = Controller.Admitted and Submission = Registers.Started then
               if Query_Requests (Query_Head).Kind = Query_Scan then
                  Scans.Start_Iteration (Query_Requests (Query_Head).Scan);
                  Last_Scan_Empty := No_Query_Response;
               else
                  Query_Head := (if Query_Head = Query_Slot'Last then Query_Slot'First else Query_Head + 1);
                  Query_Count := Query_Count - 1;
               end if;
               Prefer_Field := True;
            else
               -- No request is discarded or automatically retried after an
               -- unexpected admission failure. Retain and stop new work.
               if Query_Requests (Query_Head).Kind = Query_Scan then
                  Scans.Fail (Query_Requests (Query_Head).Scan);
               end if;
               Stop := True; Receive_Request := False; return;
            end if;
         end if;
      end if;
      Runnable := Query_Runnable;
      -- Leave requests in the kernel while bounded hardware work progresses.
      -- Recovery/control handling remains reachable once that work is retained.
      Receive_Request := not Registers.Active or else Registers.Retained_For_Recovery;
   end Schedule_Queries;
   function Claim return Boolean is
   begin
      if Claimed then return True; end if;
      if Receiver_Claimed then return False; end if;
      Receiver_Claimed := True; Claimed := True; return True;
   end Claim;
   procedure Route_Event (Sender : CuBit.Messages.Process_ID;
      Item : CuBit.Messages.Message; Stop, Handled : out Boolean) is
   begin
      -- Reserved system labels never enter the EC source admission window.
      -- Stop cannot be forgotten or vetoed by a custom source hook.
      case ACPI_System_Events.Classify (Sender, Item) is
         when ACPI_System_Events.Stop_Requested =>
            Stop := True; Handled := True; return;
         when ACPI_System_Events.Rejected =>
            Stop := False; Handled := True; return;
         when ACPI_System_Events.Channel_Wake | ACPI_System_Events.Grant_Notice |
              ACPI_System_Events.Ignored_Control =>
            -- Trusted hint/logging handling remains available, without query
            -- authority or any automatic grant/backing release.
            Handle_Event (Sender, Item, Stop, Handled);
            Handled := True; return;
         when ACPI_System_Events.Not_System => null;
      end case;
      Query_Event_Window := True;
      begin Handle_Event (Sender, Item, Stop, Handled);
      exception when others => Query_Event_Window := False; raise;
      end;
      Query_Event_Window := False;
   end Route_Event;
   function Convert (Item : Receiver.Report) return Report is
   begin
      return (Reason => (case Item.Reason is
         when Receiver.Serving => Serving, when Receiver.Event_Stop => Event_Stop,
         when Receiver.Control_Stop => Control_Stop,
         when Receiver.Local_Work_Stop => Local_Work_Stop,
         when Receiver.Unexpected_Reply => Unexpected_Reply,
         when Receiver.Owner_Fault => Owner_Fault,
         when Receiver.Inconsistent_Owners => Inconsistent_Owners,
         when Receiver.Wait_Unavailable => Wait_Unavailable,
         when Receiver.Adapter_Fault => Adapter_Fault,
         when Receiver.Reentered => Reentered),
         Event_Seen => Item.Event_Seen, Event_Handled => Item.Event_Handled,
         Request_Seen => Item.Request_Seen, Control_Handled => Item.Control_Handled,
         Field_Attempted => Item.Field_Attempted, Waited => Item.Waited,
         Clock_Available => Item.Clock_Available, State => Observe);
   end Convert;
   procedure Turn (Result : out Report) is
      Actual : Receiver.Report;
   begin
      Result := (State => Observe, others => <>);
      if Current_Setup /= Ready then return; end if;
      if not Claim then Result.Reason := Receive_Already_Owned; return; end if;
      Receiver.Turn (Actual);
      Note_Completion (Actual.Work.Progress_Result.Field);
      if Actual.Field_Attempted then Note_Admission (Actual.Field); end if;
      if Actual.Field_Attempted and then Actual.Field.Status = Controller.Admitted then
         Prefer_Field := False;
      end if;
      if Actual.Reason /= Receiver.Serving then Receiver_Closed := True; end if;
      Result := Convert (Actual);
   end Turn;
   procedure Serve (Result : out Report) is
   begin
      loop Turn (Result); exit when Result.Reason /= Serving; end loop;
   end Serve;
   procedure Drain (Result : out Drain_Report) is
      Actual : Receiver.Drain_Report;
   begin
      Result := (Status => Drain_Not_Ready, State => (State => Observe, others => <>));
      if Current_Setup /= Ready then return; end if;
      if not Claim then
         Result.Status := Quarantined; Result.State.Reason := Receive_Already_Owned; return;
      end if;
      Receiver.Drain (Actual);
      Note_Completion (Actual.State.Work.Progress_Result.Field);
      if Actual.Status = Receiver.Advanced then Settle_Scan; end if;
      if Actual.State.Reason /= Receiver.Serving then Receiver_Closed := True; end if;
      Result := (Status => (case Actual.Status is
         when Receiver.Still_Serving => Still_Serving,
         when Receiver.Advanced => Advanced, when Receiver.Quarantined => Quarantined),
         State => Convert (Actual.State));
   end Drain;
end ACPI_EC_Backend_Owner.With_Receive;
