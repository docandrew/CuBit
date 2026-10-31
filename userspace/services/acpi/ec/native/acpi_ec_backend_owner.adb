pragma Ada_2022;
package body ACPI_EC_Backend_Owner is
   use type ACPI_EC_Types.Microseconds, ACPI_EC_Types.Work_Budget;
   use type Scans.Phase;
   function Configuration return Setup_Status is (Current_Setup);
   procedure Enqueue_Query (Attempts : ACPI_EC_Event_Types.Delivery_Budget;
      Kind : Query_Request_Kind; Queries : Query_Scan_Budget;
      Result : out Query_Request_Result) is
   begin
      Result := Request_Not_Ready;
      if Current_Setup /= Ready then return; end if;
      Result := Request_Stopped;
      if Receiver_Closed then return; end if;
      Result := Request_Outside_Event;
      if not Receiver_Claimed or else not Query_Event_Window then return; end if;
      if Query_Count = Query_Request_Count'Last then
         Query_Overflow := True; Result := Request_Overflow; return;
      end if;
      Query_Requests (Query_Tail) := (Kind => Kind, Attempts => Attempts, others => <>);
      if Kind = Query_Scan then Query_Requests (Query_Tail).Scan := Scans.Create (Queries); end if;
      Query_Tail := (if Query_Tail = Query_Slot'Last then Query_Slot'First else Query_Tail + 1);
      Query_Count := Query_Count + 1; Result := Request_Queued;
   end Enqueue_Query;
   procedure Request_Query (Attempts : ACPI_EC_Event_Types.Delivery_Budget;
      Result : out Query_Request_Result) is
   begin Enqueue_Query (Attempts, Single_Query, 1, Result); end Request_Query;
   procedure Request_Query_Scan
     (Attempts : ACPI_EC_Event_Types.Delivery_Budget; Queries : Query_Scan_Budget;
      Result : out Query_Request_Result) is
   begin Enqueue_Query (Attempts, Query_Scan, Queries, Result); end Request_Query_Scan;
   procedure Resume_Query_Scan (Queries : Query_Scan_Budget; Accepted : out Boolean) is
      use type Events.Phase;
   begin
      Accepted := False;
      if Current_Setup /= Ready or else not Receiver_Claimed or else Receiver_Closed
        or else Query_Count = 0 or else Query_Requests (Query_Head).Kind /= Query_Scan
        or else Events.Observe.Stage /= Events.Ready
        or else Registers.Retained_For_Recovery or else Calls.Faulted
      then return; end if;
      Scans.Extend (Query_Requests (Query_Head).Scan, Queries, Accepted);
   end Resume_Query_Scan;
   function Scan_Observation return Query_Scan_Report is
   begin
      if Query_Count = 0 or else Query_Requests (Query_Head).Kind /= Query_Scan then
         return (Last_Empty_Response => Last_Scan_Empty, others => <>);
      end if;
      declare Item : constant Scans.Description := Scans.View (Query_Requests (Query_Head).Scan); begin
         return (Phase => (case Item.Stage is
            when Scans.Ready => Scan_Queued, when Scans.Waiting => Scan_Waiting,
            when Scans.Exhausted => Scan_Exhausted,
            when Scans.Dormant | Scans.Failed | Scans.Complete => Scan_Failed),
            Remaining => Item.Remaining, Last_Empty_Response => Last_Scan_Empty);
      end;
   end Scan_Observation;
   procedure Note_Completion (Item : Calls.Report) is
      use type Calls.Outcome;
   begin
      if Item.Status = Calls.Completion_Reply then
         Last_Completion := (Present => True, Token => Operation_ID (Item.Token),
            Hardware => Item.Hardware, Delivered => Item.Delivered,
            Effects_Possible => Item.Effects_Possible,
            Recovery_Retained => Item.Recovery_Retained);
      end if;
   end Note_Completion;
   function Convert (Item : Admitted_Cell) return Registers.Admission is
     ((Register => Registers.Register_ID (Item.Register),
       Epoch => Registers.Epoch_ID (Item.Epoch),
       Owner => Registers.Owner_ID (CuBit.Process_IDs.To_Word (Item.Owner)),
       Cell => Item.Cell, Readable => Item.Readable, Writable => Item.Writable,
       Exact_Write_Safe => Item.Exact_Write_Safe, Write_Mask => Item.Write_Mask));
   procedure Configure (Cells : Cell_List;
      Controller : ACPI_EC_Event_Types.Controller_ID;
      Epoch : ACPI_EC_Event_Types.Epoch_ID; Result : out Setup_Result) is
      Accepted : Boolean;
   begin
      Result := Already_Attempted;
      if Current_Setup /= Not_Configured then return; end if;
      Current_Setup := Setup_Refused;
      Result := Invalid_Policy;
      if Request_Window = 0 or else Request_Budget = 0 or else Pause_Budget = 0 then return; end if;
      Result := Too_Many_Cells;
      if Cells'Length > Maximum_Cells then return; end if;
      Result := Invalid_Route;
      Events.Configure (Controller, Epoch, Accepted);
      if not Accepted then return; end if;
      Result := Invalid_Cell;
      for Item of Cells loop
         Registers.Install (Convert (Item), Accepted);
         if not Accepted then return; end if;
      end loop;
      Current_Setup := Ready; Result := Configured;
   end Configure;
   procedure Install (Item : Admitted_Cell; Accepted : out Boolean) is
   begin
      Accepted := False;
      if Current_Setup = Ready and not Receiver_Closed then Registers.Install (Convert (Item), Accepted); end if;
   end Install;
   procedure Revoke (Register : Register_ID; Epoch : Epoch_ID; Accepted : out Boolean) is
   begin
      Accepted := False;
      if Current_Setup = Ready then
         Registers.Revoke (Registers.Register_ID (Register), Registers.Epoch_ID (Epoch), Accepted);
      end if;
   end Revoke;
   function Call_Of (Item : Controller.Call_Report) return Call_Report is
   begin
      return (Status => (case Item.Status is
         when Controller.Admitted => Admitted, when Controller.Refused => Refused,
         when Controller.Clock_Unavailable => Clock_Unavailable,
         when Controller.Deadline_Overflow => Deadline_Overflow,
         when Controller.Invalid_Policy => Invalid_Request_Policy), Replied => Item.Replied,
         Phase => (case Item.Field.Status is
            when Calls.Idle => No_Field, when Calls.Awaiting_Transaction => Awaiting_Transaction,
            when Calls.Immediate_Reply => Immediate_Reply,
            when Calls.Completion_Reply => Completed_Reply, when Calls.Save_Refused => Save_Refused,
            when Calls.Owner_Fault => Reply_Owner_Fault, when Calls.Correlation_Fault => Correlation_Fault));
   end Call_Of;
   procedure Note_Admission (Item : Controller.Call_Report) is
   begin Last_Admission := Call_Of (Item); end Note_Admission;
   procedure Begin_Call (Sender : CuBit.Messages.Process_ID;
      Request : CuBit.Messages.Message; Result : out Call_Report) is
      Actual : Controller.Call_Report;
   begin
      Result := (others => <>);
      if Current_Setup /= Ready then return; end if;
      if Receiver_Closed then Result.Status := Serving_Stopped; return; end if;
      if Receiver_Claimed then Result.Status := Receive_Owned; return; end if;
      Controller.Begin_Call (Sender, Request, Actual);
      Note_Admission (Actual); Result := Last_Admission;

   end Begin_Call;
   procedure Start_Query (Attempts : ACPI_EC_Event_Types.Delivery_Budget;
      Result : out Query_Status) is
      Status : Controller.Admission_Status; Submission : Registers.Submission;
   begin
      Result := Query_Not_Ready;
      if Current_Setup /= Ready then return; end if;
      if Receiver_Closed then Result := Query_Serving_Stopped; return; end if;
      if Receiver_Claimed and not Query_Event_Window then Result := Query_Receive_Owned; return; end if;
      if Query_Count > 0 and then Query_Requests (Query_Head).Kind = Query_Scan then
         Result := Query_Busy; return;
      end if;
      Controller.Start_Query (Attempts, Status, Submission);
      case Status is
         when Controller.Clock_Unavailable => Result := Query_Clock_Unavailable;
         when Controller.Deadline_Overflow => Result := Query_Deadline_Overflow;
         when Controller.Invalid_Policy => Result := Query_Invalid_Policy;
         when Controller.Admitted | Controller.Refused =>
            Result := (case Submission is
               when Registers.Started => Query_Started, when Registers.Busy => Query_Busy,
               when Registers.Denied => Query_Denied,
               when Registers.Engine_Unavailable => Query_Engine_Unavailable,
               when Registers.Exhausted => Query_Exhausted);
      end case;
   end Start_Query;
   function Observe return Observation is
      Event : constant Events.Report := Events.Observe;
   begin
      return (Setup => Current_Setup, Hardware => Engine.Status,
         Catalog_Active => Registers.Active, Field_Reply_Pending => Calls.Pending,
         Field_Faulted => Calls.Faulted, Hardware_Recovery => Registers.Retained_For_Recovery,
         Receive_Owned => Receiver_Claimed, Serving_Stopped => Receiver_Closed,
         Last_Field_Completion => Last_Completion, Last_Field_Admission => Last_Admission,
         Queued_Queries => Query_Count, Query_Request_Overflow => Query_Overflow,
         Last_Queued_Query_Status => Queued_Query_Status, Last_Query_Outcome => Event.Hardware,
         Last_Query_Response => (case Event.Response is
            when Events.No_Query_Response => No_Query_Response,
            when Events.No_Pending_Event => No_Pending_Event,
            when Events.Zero_Query_Response => Zero_Query_Response,
            when Events.Event_Response => Event_Response),
         Last_Query_Event => Event.Event,
         Scan => Scan_Observation,
         Notice => Events.Retained, Remaining_Pauses => Controller.Remaining_Pauses,
         Event_Phase => (case Event.Stage is
            when Events.Unconfigured => Event_Unconfigured,
            when Events.Configuration_Refused => Event_Configuration_Refused,
            when Events.Ready => Event_Ready, when Events.Querying => Event_Querying,
            when Events.Publication_Pending => Event_Publication_Pending,
            when Events.Delivery_Stalled => Event_Delivery_Stalled,
            when Events.Delivery_Unknown => Event_Delivery_Unknown,
            when Events.Query_Failed => Event_Query_Failed,
            when Events.Correlation_Failed => Event_Correlation_Failed));
   end Observe;
   procedure Advance (Result : out Progress_Report) is
      Actual : Waits.Report;
   begin
      Result := (others => <>);
      if Current_Setup = Ready and not Receiver_Claimed then
         Result.Advanced := True;
         Controller.Advance (Actual, Result.Clock_Available);
         Note_Completion (Actual.Progress_Result.Field);
         Result.Wait_Checked := Actual.Wait_Checked; Result.Stop_Requested := Actual.Stop_Requested;
         Result.Action := (case Actual.Progress_Result.Action is
            when Progress.No_Work => No_Work, when Progress.Advance_Field => Advance_Field,
            when Progress.Advance_Query => Advance_Query,
            when Progress.Publish_Notice => Publish_Notice, when Progress.Wait_Until => Wait_Until,
            when Progress.Owner_Fault => Owner_Fault,
            when Progress.Inconsistent_Owners => Inconsistent_Owners);
      end if;
      Result.State := Observe;
   end Advance;
   procedure Confirm_Claim (Sender : CuBit.Process_IDs.Process_ID;
      Item : ACPI_EC_Event_Types.Notice; Accepted : out Boolean) is
   begin
      Accepted := False;
      if Current_Setup = Ready then Events.Confirm_Claim (Sender, Item, Accepted); end if;
   end Confirm_Claim;
   procedure Resume_Publication (Attempts : ACPI_EC_Event_Types.Delivery_Budget;
      Accepted : out Boolean) is
   begin
      Accepted := False;
      if Current_Setup = Ready then Events.Resume_Publication (Attempts, Accepted); end if;
   end Resume_Publication;
   procedure Resume_Queries (Accepted : out Boolean) is
   begin
      Accepted := False;
      if Current_Setup = Ready and then not Receiver_Closed
        and then (Query_Count = 0 or else Query_Requests (Query_Head).Kind /= Query_Scan)
      then Events.Resume_Queries (Accepted); end if;
   end Resume_Queries;
end ACPI_EC_Backend_Owner;
