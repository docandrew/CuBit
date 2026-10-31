pragma Ada_2022;
with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Command_Line;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with ACPI_Service;
with ACPI_Requests;
with ACPI_Requests.Lids;
with ACPI_Requests.EC_Events;
with ACPI_Requests.With_Notification_Routing;
with ACPI_Requests.Notification_Pump;
with ACPI_Native_Server.With_Notifications;
with ACPI_Native_Server.With_EC_Events;
with ACPI_Native_Endpoint;
with CuBit.Messages;
with ACPI_Protocol;
with ACPI_EC_Methods;
with ACPI_EC_Event_Receiver;
with ACPI_EC_Event_Types;
with ACPI_Lid;
with ACPI_Power_Source;
with ACPI_Requests.Power_Sources;
with Firmware_Tables;
with CuBit.Process_IDs;
with Interfaces;
with CuBit.Control_Events;
procedure EC_Owner_Tests is
   package Q renames ACPI_Requests;
   package E renames ACPI_EC_Event_Types;
   package Methods is new ACPI_EC_Methods (ACPI_Service);
   package Receiver is new ACPI_EC_Event_Receiver (ACPI_Service, Methods);
   package Events is new Q.EC_Events (Receiver);
   package Driver is new ACPI_Lid (ACPI_Service);
   package Lids is new Q.Lids (Driver);
   package C renames Lids.Cached;
   package S renames Lids.Samples;
   package Power_Driver is new ACPI_Power_Source (ACPI_Service);
   package Power is new Q.Power_Sources (Power_Driver);
   package PC renames Power.Cached;
   package PS renames Power.Samples;
   package IO renames Ada.Streams.Stream_IO;
   use type Ada.Streams.Stream_Element_Offset;
   use type Q.Outcome, Q.Phase, Q.Revision_Number;
   use type Events.Controller.Admission;
   use type Receiver.Enrollment, Receiver.Outcome, Receiver.Close_Result;
   use type E.Controller_ID, E.Epoch_ID, E.Token_ID;
   use type E.Notice, E.Notice_Option, E.Event_Number;
   use type C.Refresh_Outcome, S.Availability, Driver.Lid_State;
   use type Lids.Controller.Discovery_Outcome;
   use type PC.Refresh_Outcome, PS.Availability, PS.Publication_Result;
   use type Power_Driver.Source_State, Power.Controller.Discovery_Outcome;
   Checks : Natural := 0;
   Fixture_Limit : constant := 4096;
   Budget : constant := 1000;
   -- Seven predefined root children precede the fixture's first declaration.
   Fixture_EC_Node : constant ACPI_Service.Namespace.Node_ID := 8;
   Fatal_Event : constant E.Event_Number := 16#FE#;
   Batch_Event : constant E.Event_Number := 16#10#;
   Source : constant E.Controller_ID := 17;
   Epoch : constant E.Epoch_ID := 3;
   Sender : constant CuBit.Process_IDs.Process_ID :=
     CuBit.Process_IDs.To_Process_ID (300, 16#123456#);
   Other_Sender : constant CuBit.Process_IDs.Process_ID :=
     CuBit.Process_IDs.To_Process_ID (300, 16#123457#);
   procedure Check (Good : Boolean; Why : String) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Why; end if;
   end Check;
   function Read_Table return Firmware_Tables.Bytes is
      File : IO.File_Type;
   begin
      Check (Ada.Command_Line.Argument_Count = 2, "fixture and lifecycle");
      IO.Open (File, IO.In_File, Ada.Command_Line.Argument (1));
      Check (IO.Size (File) in IO.Count (Firmware_Tables.Table_Header_Size) .. Fixture_Limit,
        "bounded exact table");
      declare
         Raw : Ada.Streams.Stream_Element_Array (1 .. Ada.Streams.Stream_Element_Offset (IO.Size (File)));
         Last : Ada.Streams.Stream_Element_Offset;
         Data : Firmware_Tables.Bytes (1 .. Raw'Length);
      begin
         IO.Read (File, Raw, Last); IO.Close (File);
         Check (Last = Raw'Last, "complete table read");
         for I in Data'Range loop
            Data (I) := Firmware_Tables.Byte (Raw (Ada.Streams.Stream_Element_Offset (I)));
         end loop;
         return Data;
      end;
   end Read_Table;
   Table : constant Firmware_Tables.Bytes := Read_Table;
   Server : Q.State (1, Table'Length, Table'Length, 1, 0);
   Cache, Other_Cache : Lids.Cache;
   Power_Cache : Power.Cache;
   Power_Discovery : Power.Controller.Discovery_Report;
   Power_Refresh : PC.Refresh_Report;
   Power_View : PS.View;
   Power_Run, Power_Duplicate, Batch_Run : Receiver.Delivery;
   Reply : Q.Response;
   Admission : Events.Controller.Admission;
   Enrollment : Receiver.Enrollment;
   Outcome : Receiver.Outcome;
   Closed : Receiver.Close_Result;
   Run, Rejected, Duplicate, Missing, Empty_Budget : Receiver.Delivery;
   Notice : E.Notice := (Source, Epoch, 1, 1);
   Discovery : Lids.Controller.Discovery_Report;
   Refresh : C.Refresh_Report;
   View : S.View;
   Receipt : Q.Revision_Number;
   Fanout : Boolean := False;
   Raise_Route : Boolean := False;
   Partial_Fanout : Boolean := False;
   Route_Calls, Fail_Route_At : Natural := 0;
   procedure Route (Item : Q.State; Routed : out Boolean) is
      Seen : Boolean;
   begin
      if Raise_Route then raise Program_Error with "modeled local fanout exception"; end if;
      Route_Calls := Route_Calls + 1;
      Routed := Fanout and then (Fail_Route_At = 0 or else Route_Calls /= Fail_Route_At);
      if not Routed then return; end if;
      Lids.Observe_Claimed (Cache, Item, Seen);
      if Partial_Fanout then Routed := False; return; end if;
      Lids.Observe_Claimed (Other_Cache, Item, Seen);
      Power.Observe_Claimed (Power_Cache, Item, Seen);
   end Route;
   package Routing is new Q.With_Notification_Routing (Route);
   package Native_Owner is new ACPI_Native_Server.With_Notifications (Route);
   package Native_Loop is new ACPI_Native_Server.With_Dispatch
     (ACPI_Native_Endpoint.Dispatch, Advance_Owner => Native_Owner.Advance);

   type Source_Queue is array (Positive range 1 .. 4) of E.Notice;
   Queued : Source_Queue := [others => (Source, Epoch, 0, 1)];
   Queue_Length, Source_Taken, Take_Calls : Natural := 0;
   Completion_Calls, Completion_Route_Base, Completion_Charge : Natural := 0;
   Completed_Item : E.Notice_Option;
   Refuse_Completion, Raise_Completion, Reenter_Completion : Boolean := False;
   Stop_In_Completion : Boolean := False;
   procedure Try_Reentry;
   procedure Try_Stop_In_Completion;
   procedure Complete_Source (Item : E.Notice; Charged : Natural; Confirmed : out Boolean) is
   begin
      Confirmed := False;
      Completion_Calls := Completion_Calls + 1;
      Completed_Item := (Present => True, Item => Item); Completion_Charge := Charged;
      Check (Completion_Calls <= Source_Taken and then Item = Queued (Completion_Calls)
        and then Charged <= Budget, "completion preserves exact source identity and bounded charge");
      -- Each of Q03's two notifications routes on claim and acknowledgement.
      Check (Route_Calls = Completion_Route_Base + (if Item.Number = 3 then 4 elsif Item.Number = Batch_Event then 12 else 0),
        "completion follows all notifications before the next source is taken");
      if Raise_Completion then raise Program_Error with "modeled completion handoff exception"; end if;
      if Reenter_Completion then Try_Reentry; end if;
      if Stop_In_Completion then Try_Stop_In_Completion; end if;
      Confirmed := not Refuse_Completion;
   end Complete_Source;
   Source_Healthy : Boolean := True;
   Raise_Source : Boolean := False;
   procedure Take_Source (Identity : out CuBit.Messages.Process_ID;
      Item : out E.Notice; Found, Healthy : out Boolean) is
   begin
      Take_Calls := Take_Calls + 1;
      if Raise_Source then raise Program_Error with "modeled source queue exception"; end if;
      Identity := Sender; Item := (others => <>); Healthy := Source_Healthy;
      Found := Source_Taken < Queue_Length;
      if Found then
         Source_Taken := Source_Taken + 1; Item := Queued (Source_Taken);
         Completion_Route_Base := Route_Calls;
      end if;
   end Take_Source;
   package EC_Owner is new ACPI_Native_Server.With_EC_Events
     (Receiver, Route, Take_Source, Complete_Source, Fuel_Per_Event => Budget);
   package Unready_EC_Owner is new ACPI_Native_Server.With_EC_Events
     (Receiver, Route, Take_Source, Complete_Source, Fuel_Per_Event => Budget);
   package Fresh_Stop_Owner is new ACPI_Native_Server.With_EC_Events
     (Receiver, Route, Take_Source, Complete_Source, Fuel_Per_Event => Budget);
   procedure Try_Stop_In_Completion is
      Result : EC_Owner.Stop_Result;
      Saved : constant E.Notice_Option := EC_Owner.Pending_Completion;
      use type EC_Owner.Stop_Result;
   begin
      EC_Owner.Request_Stop (Result);
      Check (Result = EC_Owner.Operation_Incomplete and then EC_Owner.Pending_Completion = Saved,
        "stop during a callback refuses without promising a permanent stop or changing completion");
   end Try_Stop_In_Completion;
   procedure Try_Reentry is
      Before : constant Natural := Take_Calls;
      State : ACPI_Native_Server.Owner_Outcome;
      use type ACPI_Native_Server.Owner_Outcome;
   begin
      EC_Owner.Advance (Server, State);
      Check (State = ACPI_Native_Server.Owner_Refused and then Take_Calls = Before,
        "completion reentry cannot dequeue, recurse or mutate the outer turn");
   end Try_Reentry;
   Loop_Event_Calls : Natural := 0;
   Raise_Loop_Event, Reenter_Loop_Event : Boolean := False;
   procedure Try_Loop_Reentry;
   procedure Loop_Event (Publisher : CuBit.Messages.Process_ID; Item : CuBit.Messages.Message;
      Stop, Handled : out Boolean) is
      pragma Unreferenced (Publisher, Item);
   begin
      Loop_Event_Calls := Loop_Event_Calls + 1;
      if Raise_Loop_Event then raise Program_Error with "modeled loop event exception"; end if;
      if Reenter_Loop_Event then Try_Loop_Reentry; end if;
      Stop := False; Handled := True;
   end Loop_Event;
   package EC_Loop is new EC_Owner.With_Dispatch
     (ACPI_Native_Endpoint.Dispatch, Handle_Event => Loop_Event);
   package M renames CuBit.Messages;
   procedure Try_Loop_Reentry is
      Before_Events : constant Natural := M.Test.Event_Polls;
      Before_Source : constant Natural := Take_Calls;
      Reason : ACPI_Native_Server.Stop_Reason;
      use type ACPI_Native_Server.Stop_Reason;
   begin
      EC_Loop.Serve (Server, Reason);
      Check (Reason = ACPI_Native_Server.Owner_Work_Refused
        and then M.Test.Event_Polls = Before_Events and then Take_Calls = Before_Source,
        "nested loop reentry refuses before polling or advancing the owner");
   end Try_Loop_Reentry;
   package Pump is new Q.Notification_Pump (Route);
   use type Pump.Outcome, Pump.Operation;
   Pump_Result : Pump.Report;
   procedure Send (Label : Interfaces.Unsigned_32; Value : Q.Revision_Number := 0) is
   begin
      Q.Handle (Server, Q.Snapshot_Provider,
        (Label => Label, Data => [Q.Revision (Server), Value, 0, 0], others => <>), Reply);
      Check (Reply.Status = Q.OK and then Q.Valid (Server), "owner protocol succeeds");
   end Send;
   procedure Deliver (Delivery : in out Receiver.Delivery;
      Identity : CuBit.Process_IDs.Process_ID := Sender; Fuel : Natural := Budget) is
      Before : constant Q.Revision_Number := Q.Revision (Server);
   begin
      Events.Dispatch (Server, Delivery, Identity, Notice, Fuel, Admission, Outcome);
      Check (Q.Valid (Server) and then Q.Revision (Server) = Before,
        "event delivery preserves observer request revision");
   end Deliver;
   procedure Sample_Power (Expected : Power_Driver.Source_State) is
      Before : constant Q.Revision_Number := Q.Revision (Server);
   begin
      Power.Prepare (Power_Cache, Server, Budget, Power_Refresh);
      Check (Power_Refresh.Status = PC.Collected, "fixed PSR evaluator collects");
      Power.Publish (Power_Cache, Server, (True, 100), True, Power_Refresh);
      Power_View := Power.Read (Power_Cache, Server, (True, 100), 0);
      Check (Power_Refresh.Status = PC.Updated and then Power_View.Status = PS.Available
        and then Power_View.Value.State = Expected, "same service publishes expected AC source state");
      Check (Q.Revision (Server) = Before, "power sample preserves request revision");
   end Sample_Power;
   procedure Ack is
   begin
      Routing.Handle (Server, Q.Snapshot_Provider,
        (Label => Q.Acknowledge_Notification,
         Data => [Q.Revision (Server), Receipt, 0, 0], others => <>), Reply);
      Check (Reply.Status = Q.OK, "all local consumers routed before ACK");
   end Ack;
   procedure Sample (Data : in out Lids.Cache; Expected : Driver.Lid_State) is
      Before : constant Q.Revision_Number := Q.Revision (Server);
   begin
      Lids.Prepare (Data, Server, Budget, Refresh);
      Check (Refresh.Status = C.Collected, "fixed LID evaluator collects");
      Lids.Publish (Data, Server, (True, 100), True, Refresh);
      View := Lids.Read (Data, Server, (True, 100), 0);
      Check (Refresh.Status = C.Updated and then View.Status = S.Available
        and then View.Value.State = Expected, "same service publishes expected lid state");
      Check (Q.Revision (Server) = Before, "sampling does not mutate protocol revision");
   end Sample;
begin
   declare
      Result : Fresh_Stop_Owner.Stop_Result;
      Work : ACPI_Native_Server.Owner_Outcome;
      use type Fresh_Stop_Owner.Stop_Result, Fresh_Stop_Owner.Outcome, ACPI_Native_Server.Owner_Outcome;
   begin
      Fresh_Stop_Owner.Request_Stop (Result);
      Check (Result = Fresh_Stop_Owner.Stopped and then Fresh_Stop_Owner.Is_Stopped
        and then Fresh_Stop_Owner.Observation.Status = Fresh_Stop_Owner.Stopped_By_Owner,
        "trusted stop before first turn closes the owner without service readiness");
      Fresh_Stop_Owner.Advance (Server, Work);
      Check (Work = ACPI_Native_Server.Owner_Refused and then Take_Calls = 0 and then Route_Calls = 0
        and then Completion_Calls = 0 and then not Fresh_Stop_Owner.Pending_Completion.Present,
        "stopped initial owner performs no source,notification or completion work");
      Fresh_Stop_Owner.Request_Stop (Result);
      Check (Result = Fresh_Stop_Owner.Already_Stopped,
        "initial stop is idempotent without resetting the owner");
   end;
   declare
      State : ACPI_Native_Server.Owner_Outcome;
      use type ACPI_Native_Server.Owner_Outcome, Unready_EC_Owner.Outcome;
   begin
      Unready_EC_Owner.Advance (Server, State);
      Check (State = ACPI_Native_Server.Owner_Refused
        and then Unready_EC_Owner.Observation.Status = Unready_EC_Owner.Notifications_Refused,
        "unready EC owner refuses before source work");
      Check (Take_Calls = 0 and then Unready_EC_Owner.Is_Stopped,
        "unready refusal leaves source untouched and stops its owner");
   end;
   Pump.Run (Server, 4, Pump_Result);
   Check (Pump_Result.Status = Pump.Not_Ready and then Route_Calls = 0,
     "unstarted owner cannot claim or route notifications");
   declare
      Reason : ACPI_Native_Server.Stop_Reason;
      use type ACPI_Native_Server.Stop_Reason;
      use type Native_Owner.Pump.Outcome;
   begin
      M.Reset; Native_Loop.Serve (Server, Reason);
      Check (Reason = ACPI_Native_Server.Owner_Work_Refused
        and then Native_Owner.Observation.Status = Native_Owner.Pump.Not_Ready
        and then Route_Calls = 0 and then M.Test.Wait_Count = 0,
        "native adapter refuses unready owner without routing or waiting");
   end;
   Events.Install (Server, Source, Epoch, Sender, Fixture_EC_Node, Admission, Enrollment);
   Check (Admission = Events.Controller.Not_Ready, "unstarted service refuses enrollment");
   Deliver (Run);
   Check (Admission = Events.Controller.Not_Ready and then Outcome = Receiver.Not_Started
     and then not Receiver.Consumed (Run) and then not Receiver.Receipt (Run).Present,
     "not-ready event retains unused delivery and source token");
   Events.Close (Server, Run, Closed);
   Check (Closed = Receiver.No_Pending_Result, "cleanup callable before readiness");
   Send (Q.Start_Snapshot, 1);
   Q.Import_Block (Server, Q.Snapshot_Provider, Q.Revision (Server), 1, ACPI_Service.DSDT, Table, Reply);
   Check (Reply.Status = Q.OK, "real AML fixture imported");
   Deliver (Run);
   Check (Admission = Events.Controller.Not_Ready and then not Receiver.Consumed (Run),
     "receiving catalog cannot execute events");
   Send (Q.Finish_Snapshot);
   Check (Q.Current (Server) = Q.Complete, "snapshot completion is not device readiness");
   Events.Install (Server, Source, Epoch, Sender, Fixture_EC_Node, Admission, Enrollment);
   if Ada.Command_Line.Argument (2) = "hardware" then
      Check (Admission = Events.Controller.Not_Ready, "unconfigured EC region bars enrollment");
      Deliver (Run);
      Check (Admission = Events.Controller.Not_Ready and then not Receiver.Consumed (Run),
        "unconfigured EC region bars event AML and does not consume token");
   else
      Check (Admission = Events.Controller.Admitted and then Enrollment = Receiver.Installed,
        "trusted enrollment uses completed service Core");
      Send (Q.Configure_Notifications, 1);
      Lids.Bind (Cache, Server, Budget, Discovery);
      Check (Discovery.Outcome = Lids.Controller.Found, "same service lid discovered");
      Lids.Bind (Other_Cache, Server, Budget, Discovery);
      Check (Discovery.Outcome = Lids.Controller.Found, "second same service consumer");
      Sample (Cache, Driver.Closed); Sample (Other_Cache, Driver.Closed);
      Power.Bind (Power_Cache, Server, Budget, Power_Discovery);
      Check (Power_Discovery.Outcome = Power.Controller.Found, "same Core power source discovered");
      Sample_Power (Power_Driver.Offline);
      Deliver (Rejected, Other_Sender);
      Check (Admission = Events.Controller.Admitted and then Outcome = Receiver.Wrong_Sender
        and then not Receiver.Consumed (Rejected), "full process generation checked before AML");
      Notice.Epoch := 2; Deliver (Rejected);
      Check (Outcome = Receiver.Wrong_Epoch and then not Receiver.Consumed (Rejected),
        "wrong epoch does not consume event");
      Notice.Epoch := Epoch; Deliver (Run);
      Check (Outcome = Receiver.Completed and then Receiver.Consumed (Run)
        and then not Receiver.Pending (Run) and then Receiver.Receipt (Run).Present,
        "same unused delivery executes exact local Q01 and releases result");
      View := Lids.Read (Cache, Server, (True, 100), 0);
      Check (View.Status = S.Not_Ready and then View.Value.State = Driver.Unknown,
        "EC Notify in same Core suppresses stale closed sample before claim");
      Deliver (Duplicate);
      Check (Outcome = Receiver.Duplicate and then not Receiver.Consumed (Duplicate),
        "duplicate does not rerun Q01 or invent a receipt");
      Send (Q.Claim_Notification); Receipt := Reply.Data (1);
      Check (Receipt /= 0, "EC AML Notify enters actual service receipt broker");
      View := Lids.Read (Other_Cache, Server, (True, 100), 0);
      Check (View.Status = S.Not_Ready, "staged unseen receipt still suppresses freshness");
      Routing.Handle (Server, Q.Snapshot_Provider,
        (Label => Q.Acknowledge_Notification,
         Data => [Q.Revision (Server), Receipt, 0, 0], others => <>), Reply);
      Check (Reply.Status = Q.Incomplete, "failed fanout preserves EC notification receipt");
      Fanout := True;
      Routing.Handle (Server, Q.Snapshot_Provider,
        (Label => Q.Acknowledge_Notification,
         Data => [Q.Revision (Server), Receipt, 0, 0], others => <>), Reply);
      Check (Reply.Status = Q.OK, "both caches invalidated before actual ACK");
      View := Lids.Read (Cache, Server, (True, 100), 0);
      Check (View.Status /= S.Available, "ACK does not resurrect old sample");
      Sample (Cache, Driver.Open); Sample (Other_Cache, Driver.Open);
      for I in 1 .. 5 loop
         View := Lids.Read (Cache, Server, (True, 100), 0);
         Check (View.Status = S.Available and then View.Value.Raw = 1,
           "observer copies never execute AML");
      end loop;
      Power_View := Power.Read (Power_Cache, Server, (True, 100), 0);
      Check (Power_View.Status = PS.Available and then Power_View.Value.State = Power_Driver.Offline,
        "routed lid event does not invalidate unrelated AC source");
      Notice.Token := 2; Notice.Number := 2; Deliver (Power_Run);
      Check (Outcome = Receiver.Completed, "Q02 toggles source and emits AC notification");
      Power_View := Power.Read (Power_Cache, Server, (True, 100), 0);
      Check (Power_View.Status = PS.Not_Ready and then Power_View.Value.State = Power_Driver.Unknown,
        "queued AC event hides stale offline sample");
      Deliver (Power_Duplicate);
      Check (Outcome = Receiver.Duplicate and then not Receiver.Consumed (Power_Duplicate),
        "duplicate Q02 never toggles AC source twice");
      Send (Q.Claim_Notification); Receipt := Reply.Data (1);
      Check (Receipt /= 0, "power Notify enters same service receipt broker");
      Power_View := Power.Read (Power_Cache, Server, (True, 100), 0);
      Check (Power_View.Status = PS.Not_Ready, "staged unseen power event hides freshness");
      Ack;
      View := Lids.Read (Cache, Server, (True, 100), 0);
      Check (View.Status = S.Available and then View.Value.Raw = 1,
        "routed power event does not invalidate unrelated lid sample");
      Sample_Power (Power_Driver.Online);
      Notice.Token := 3; Notice.Number := 3; Deliver (Batch_Run);
      Check (Outcome = Receiver.Completed, "one EC handler emits lid and AC events");
      Send (Q.Claim_Notification); Receipt := Reply.Data (1);
      Check (Receipt /= 0, "first event of batch retained");
      Ack;
      View := Lids.Read (Cache, Server, (True, 100), 0);
      Power_View := Power.Read (Power_Cache, Server, (True, 100), 0);
      Check (View.Status = S.Not_Ready and then Power_View.Status = PS.Not_Ready,
        "remaining second event prevents all cached observations from looking fresh");
      Power.Prepare (Power_Cache, Server, Budget, Power_Refresh);
      Check (Power_Refresh.Status = PC.Collected, "power collection can be pending during notification batch");
      Send (Q.Claim_Notification); Receipt := Reply.Data (1);
      Check (Receipt /= 0, "second event receives its own broker receipt");
      Ack;
      Power.Publish (Power_Cache, Server, (True, 100), True, Power_Refresh);
      Check (Power_Refresh.Publication = PS.Stale_Revision,
        "second routed AC event invalidates a sample collected before that event");
      Sample_Power (Power_Driver.Offline);
      Sample (Cache, Driver.Open); Sample (Other_Cache, Driver.Open);
      Check (View.Value.Raw = 2, "batch executes exactly one further lid change");
      Notice.Token := 4; Notice.Number := 4; Deliver (Missing);
      Check (Outcome = Receiver.Handler_Missing and then Receiver.Consumed (Missing),
        "missing handler consumes claimed token without ancestor fallback");
      Notice.Token := 5; Notice.Number := 1; Deliver (Empty_Budget, Fuel => 0);
      Check (Outcome = Receiver.Invocation_Failed and then Receiver.Consumed (Empty_Budget),
        "zero fuel consumes event without executing AML");
      View := Lids.Read (Cache, Server, (True, 100), 0);
      Check (View.Status = S.Available and then View.Value.Raw = 2,
        "rejected duplicate missing and zero-fuel events have no Q01 effects");
      declare
         Pump_Run, Replay : Receiver.Delivery;
         Before : Q.Revision_Number;
         Held : Q.Receipt_Number;
      begin
         Notice.Token := 6; Notice.Number := 3; Deliver (Pump_Run);
         Check (Outcome = Receiver.Completed, "pump fixture executes Q03 once");
         Before := Q.Revision (Server); Route_Calls := 0;
         Pump.Run (Server, 0, Pump_Result);
         Check (Pump_Result.Status = Pump.Limit_Reached and then Pump_Result.Claims = 0
           and then Pump_Result.Acknowledged = 0 and then Route_Calls = 0
           and then Q.Revision (Server) = Before, "zero batch does no claim route or ACK");
         Partial_Fanout := True;
         Pump.Run (Server, 4, Pump_Result); Held := Pump_Result.Retained;
         Check (Pump_Result.Status = Pump.Fanout_Blocked and then Held /= 0
           and then Pump_Result.Last_Operation = Pump.Claiming and then Pump_Result.ACK_Attempts = 0,
           "partial local fanout retains exact receipt before ACK");
         View := Lids.Read (Other_Cache, Server, (True, 100), 0);
         Check (View.Status = S.Not_Ready, "unrouted consumer cannot expose stale value");
         Partial_Fanout := False; Route_Calls := 0; Fail_Route_At := 2;
         Pump.Run (Server, 4, Pump_Result);
         Check (Pump_Result.Status = Pump.Fanout_Blocked and then Pump_Result.Retained = Held
           and then Pump_Result.Last_Operation = Pump.Acknowledging
           and then Pump_Result.ACK_Attempts = 1 and then Pump_Result.Acknowledged = 0,
           "ACK-time fanout failure preserves same staged event");
         Fail_Route_At := 0; Route_Calls := 0; Before := Q.Revision (Server);
         Pump.Run (Server, 1, Pump_Result);
         Check (Pump_Result.Status = Pump.Limit_Reached and then Pump_Result.Acknowledged = 1
           and then Pump_Result.Retained = 0 and then Route_Calls = 2
           and then Q.Revision (Server) = Before + 1,
           "resumed staged receipt ACKs without new claim effect or AML replay");
         Pump.Run (Server, 1, Pump_Result);
         Check (Pump_Result.Status = Pump.Limit_Reached and then Pump_Result.Claims = 1
           and then Pump_Result.Acknowledged = 1, "second event respects one-item turn budget");
         Route_Calls := 0;
         Pump.Run (Server, 4, Pump_Result);
         Check (Pump_Result.Status = Pump.Queue_Empty and then Pump_Result.Claims = 1
           and then Pump_Result.ACK_Attempts = 0 and then Pump_Result.Retained = 0
           and then Route_Calls = 0, "explicit empty observation requires no local fanout");
         View := Lids.Read (Cache, Server, (True, 100), 0);
         Check (View.Status /= S.Available, "local empty does not resurrect invalidated cache");
         Sample (Cache, Driver.Open); Check (View.Value.Raw = 3, "pump never reexecutes Q03");
         Sample_Power (Power_Driver.Online);
         Deliver (Replay);
         Check (Outcome = Receiver.Duplicate and then not Receiver.Consumed (Replay),
           "retired source token still cannot replay event AML");
      end;
      declare
         Delivery, Batch_One, Batch_Two, Replay : Receiver.Delivery;
         Reason : ACPI_Native_Server.Stop_Reason;
         Request : M.Message := M.NULL_MESSAGE;
         Observation : Native_Owner.Pump.Report;
         Raised : Boolean := False;
         use type ACPI_Native_Server.Stop_Reason;
         use type Native_Owner.Pump.Outcome;
         Held : Q.Receipt_Number;
      begin
         Notice.Token := 7; Notice.Number := 3; Deliver (Delivery);
         Check (Outcome = Receiver.Completed, "native composition produces actual EC notifications");
         Request.tag.label := Q.Read_Metrics; Request.tag.length := 4;
         Request.authorityTag := Interfaces.Unsigned_64 (ACPI_Protocol.Observer_Tag);
         Fanout := False; M.Reset; M.Enqueue (Sender, Request);
         Native_Loop.Serve (Server, Reason); Observation := Native_Owner.Observation;
         Held := Observation.Retained;
         Check (Reason = ACPI_Native_Server.Owner_Work_Refused and then Held /= 0
           and then Observation.Status = Native_Owner.Pump.Fanout_Blocked,
           "actual native loop refuses failed fanout with retained receipt");
         Check (M.Test.Reply_Count = 0 and then M.Test.Wait_Count = 0,
           "refused native fanout does not dispatch or idle wait");
         View := Lids.Read (Other_Cache, Server, (True, 100), 0);
         Check (View.Status /= S.Available, "native retained receipt blocks stale cache");
         Notice.Token := 8; Deliver (Batch_One);
         Check (Outcome = Receiver.Completed, "second native batch appends two actual notifications");
         Notice.Token := 9; Deliver (Batch_Two);
         Check (Outcome = Receiver.Completed, "third native batch exceeds one owner turn");
         -- A normal typed fanout refusal can be repaired by the local owner.
         -- This explicit new serving attempt
         -- resumes the retained receipt; neither loop nor adapter retries AML.
         Fanout := True; Route_Calls := 0; M.Reset; M.Enqueue (Sender, Request);
         Native_Loop.Serve (Server, Reason); Observation := Native_Owner.Observation;
         Check (Reason = ACPI_Native_Server.Wait_Unavailable
           and then Observation.Status = Native_Owner.Pump.Queue_Empty
           and then Observation.Acknowledged = 2 and then Observation.Retained = 0,
           "native owner resumes staged and queued notifications before idle");
         Check (Route_Calls = 12 and then M.Test.Reply_Count = 1 and then M.Test.Wait_Count = 1
           and then M.Test.Poll_Count = 2,
           "six notifications drain in two fair turns without replay or busy wait");
         View := Lids.Read (Cache, Server, (True, 100), 0);
         Check (View.Status /= S.Available, "native empty observation never republishes old sample");
         Sample (Cache, Driver.Open); Check (View.Value.Raw = 6, "native drain never reruns Q03");
         Sample_Power (Power_Driver.Offline);
         Deliver (Replay);
         Check (Outcome = Receiver.Duplicate, "native ACK does not authorize source replay");
         Notice.Token := 10;
         declare
            Fresh : Receiver.Delivery;
         begin
            Deliver (Fresh);
            Check (Outcome = Receiver.Completed, "exception fixture creates fresh actual notification");
         end;
         Raise_Route := True; M.Reset; M.Enqueue (Sender, Request);
         begin Native_Loop.Serve (Server, Reason);
         exception when Error : Program_Error =>
            Raised := True;
            Check (Ada.Exceptions.Exception_Message (Error) = "modeled local fanout exception",
              "exact host-only fanout contract violation propagates");
         end;
         Check (Raised and then not Native_Owner.Observation_Complete
           and then M.Test.Reply_Count = 0 and then M.Test.Wait_Count = 0,
           "callback exception propagates with incomplete report and no replay or dispatch");
         -- Hosted fault observation only: the actual native runtime terminates
         -- rather than returning from this unexpected callback failure.
         Raise_Route := False; M.Reset;
         Native_Loop.Serve (Server, Reason);
         Check (Native_Owner.Observation_Complete
           and then Native_Owner.Observation.Status = Native_Owner.Pump.Queue_Empty
           and then Native_Owner.Observation.Acknowledged = 2,
           "explicit repaired-owner attempt resumes exception-retained receipt");
         Sample (Cache, Driver.Open); Check (View.Value.Raw = 7,
           "exception recovery does not rerun EC AML");
      end;
      Events.Close (Server, Run, Closed);
      Check (Closed = Receiver.No_Pending_Result, "completed event has no cleanup replay");
      if Ada.Command_Line.Argument (2) = "normal" then
      declare
         Fatal_Run, Later, Other_Route : Receiver.Delivery;
         Revoked : Boolean;
      begin
         Check (not Receiver.Recovery_Needed and then not Receiver.Fatal_Diagnostic.Present,
           "ordinary EC events leave recovery clear");
         Notice.Token := 11; Notice.Number := Fatal_Event; Deliver (Fatal_Run);
         Check (Outcome = Receiver.Invocation_Failed and then Receiver.Consumed (Fatal_Run)
           and then Receiver.Observe (Fatal_Run).Invocation.Fatal.Present,
           "fatal EC handler is consumed once and preserves its observation");
         Notice.Token := 12; Notice.Number := 1; Deliver (Later);
         Check (Outcome = Receiver.Recovery_Required and then Receiver.Recovery_Needed
           and then Receiver.Fatal_Diagnostic.Present
           and then not Receiver.Consumed (Later) and then not Receiver.Receipt (Later).Present,
           "fresh EC event must not execute after a fatal firmware result");
         Events.Install (Server, Source + 1, Epoch, Sender, Fixture_EC_Node, Admission, Enrollment);
         Check (Enrollment = Receiver.Receiver_Recovery_Required,
           "new source cannot bypass fatal domain recovery");
         Receiver.Revoke (Source, Epoch, Revoked);
         Check (Revoked and then Receiver.Recovery_Needed,
           "revocation stays available and cannot clear fatal recovery");
         Events.Install (Server, Source, Epoch + 1, Sender, Fixture_EC_Node, Admission, Enrollment);
         Check (Enrollment = Receiver.Receiver_Recovery_Required,
           "new epoch cannot clear fatal recovery");
         Notice.Controller := Source + 1; Deliver (Other_Route);
         Check (Outcome = Receiver.Recovery_Required and then not Receiver.Consumed (Other_Route),
           "unknown source cannot bypass fatal recovery");
         Events.Close (Server, Fatal_Run, Closed);
         Check (Closed = Receiver.No_Pending_Result and then Receiver.Recovery_Needed
           and then Receiver.Fatal_Diagnostic.Present,
           "cleanup query preserves fatal recovery and copied diagnostic");
         Deliver (Fatal_Run);
         Check (Outcome = Receiver.Already_Consumed and then Receiver.Recovery_Needed,
           "original fatal delivery never replays");
      end;

      else
         declare
            Reason : ACPI_Native_Server.Stop_Reason;
            Request : M.Message := M.NULL_MESSAGE;
            Info : EC_Owner.Report;
            Taken_Before : Natural;
            Completion_Before : E.Notice_Option;
            Calls_Before : Natural;
            Work : ACPI_Native_Server.Owner_Outcome;
            Stop_Result : EC_Owner.Stop_Result;
            Prior_Status : EC_Owner.Outcome;
            Raised : Boolean := False;
            use type EC_Owner.Outcome, EC_Owner.Pump.Outcome, ACPI_Native_Server.Stop_Reason;
            use type ACPI_Native_Server.Owner_Outcome;
            use type EC_Owner.Stop_Result;
         begin
            Queued := [(Source, Epoch, 11, 3), (Source, Epoch, 12, 3),
              (Source, Epoch, 13, Fatal_Event), (Source, Epoch, 14, 1)];
            if Ada.Command_Line.Argument (2) = "owner-fatal" then Queue_Length := 4;
            elsif Ada.Command_Line.Argument (2) = "owner-drain" then Queue_Length := 2;
            elsif Ada.Command_Line.Argument (2) = "source-fail" then
               Queue_Length := 1; Source_Healthy := False;
            elsif Ada.Command_Line.Argument (2) = "owner-fanout" then
               Queue_Length := 2; Fanout := False;
            elsif Ada.Command_Line.Argument (2) = "source-exception" then
               Queue_Length := 1; Raise_Source := True;
            elsif Ada.Command_Line.Argument (2) = "route-exception" then
               Queue_Length := 2; Raise_Route := True;
            elsif Ada.Command_Line.Argument (2) = "completion-refused" then
               Queue_Length := 2; Refuse_Completion := True;
            elsif Ada.Command_Line.Argument (2) = "completion-exception" then
               Queue_Length := 2; Raise_Completion := True;
            elsif Ada.Command_Line.Argument (2) = "completion-reentry" then
               Queue_Length := 2; Reenter_Completion := True;
            elsif Ada.Command_Line.Argument (2) = "completion-missing" then
               Queue_Length := 1; Queued (1).Number := 16#55#;
            elsif Ada.Command_Line.Argument (2) in "completion-batch" | "completion-batch-blocked" then
               Queue_Length := 1; Queued (1).Number := Batch_Event; Route_Calls := 0;
            elsif Ada.Command_Line.Argument (2) = "stop-pending" then
               Queue_Length := 2; Route_Calls := 0;
            elsif Ada.Command_Line.Argument (2) = "stop-reentry" then
               Queue_Length := 2; Stop_In_Completion := True;
            elsif Ada.Command_Line.Argument (2) in "kernel-stop-pending" | "kernel-stop-before-work" |
              "foreign-stop" | "malformed-stop" | "loop-event-exception" |
              "unexpected-loop-completion" | "loop-reentry" then
               Queue_Length := 2; Route_Calls := 0;
            else Queue_Length := 0;
            end if;
            M.Reset;
            if Queue_Length /= 0 then
               Request.tag.label := Q.Read_Metrics; Request.tag.length := 4;
               Request.authorityTag := Interfaces.Unsigned_64 (ACPI_Protocol.Observer_Tag);
               M.Enqueue (Sender, Request);
               if Queue_Length > 1 then M.Enqueue (Sender, Request); end if;
            end if;
            if Ada.Command_Line.Argument (2) in "completion-batch" | "completion-batch-blocked" then
               EC_Owner.Advance (Server, Work); Info := EC_Owner.Observation;
               Check (Work = ACPI_Native_Server.Owner_Pending and then Info.Status = EC_Owner.Event_Processed
                 and then EC_Owner.Pending_Completion.Present and then Source_Taken = 1
                 and then Completion_Calls = 0 and then Route_Calls = 0,
                 "six notifications remain local work after one successful handler");
               Completion_Before := EC_Owner.Pending_Completion;
               EC_Owner.Advance (Server, Work); Info := EC_Owner.Observation;
               Check (Work = ACPI_Native_Server.Owner_Pending and then Info.Status = EC_Owner.Notifications_Pending
                 and then Info.Notifications.Status = EC_Owner.Pump.Limit_Reached
                 and then Info.Notifications.Acknowledged = 4 and then Route_Calls = 8,
                 "one bounded turn acknowledges at most four receipts");
               Check (not Info.Completion_Attempted and then Completion_Calls = 0
                 and then EC_Owner.Pending_Completion = Completion_Before and then Source_Taken = 1,
                 "batch limit cannot settle completion or consume another source");
               if Ada.Command_Line.Argument (2) = "completion-batch-blocked" then Fail_Route_At := 9; end if;
            end if;
            if Ada.Command_Line.Argument (2) in "stop-pending" | "stop-idle" then
               EC_Owner.Advance (Server, Work);
               Check (Work = (if Queue_Length = 0 then ACPI_Native_Server.Owner_Idle else ACPI_Native_Server.Owner_Pending),
                 "shutdown starts at a completed bounded owner turn");
               Completion_Before := EC_Owner.Pending_Completion;
               EC_Owner.Request_Stop (Stop_Result);
               Check (Stop_Result = EC_Owner.Stopped and then EC_Owner.Is_Stopped
                 and then EC_Owner.Pending_Completion = Completion_Before,
                 "trusted shutdown preserves the exact current completion");
            end if;
            if Ada.Command_Line.Argument (2) in "kernel-stop-pending" | "loop-event-exception" | "unexpected-loop-completion" then
               EC_Owner.Advance (Server, Work);
               Check (Work = ACPI_Native_Server.Owner_Pending and then EC_Owner.Pending_Completion.Present,
                 "control-loop test starts with one retained successful event");
               Completion_Before := EC_Owner.Pending_Completion;
            end if;
            if Ada.Command_Line.Argument (2) in "kernel-stop-pending" | "kernel-stop-before-work" | "foreign-stop" | "malformed-stop" then
               declare
                  package CV renames CuBit.Control_Events;
                  Item : M.Message := M.NULL_MESSAGE;
               begin
                  Item.tag.label := CV.Control_Label; Item.tag.length := CV.Control_Words;
                  Item.words (0) := CV.Control_Kind'Enum_Rep (CV.Stop);
                  if Ada.Command_Line.Argument (2) = "malformed-stop" then Item.tag.length := 0; end if;
                  M.Enqueue_Event ((if Ada.Command_Line.Argument (2) = "foreign-stop" then Other_Sender else M.No_Process), Item);
               end;
            elsif Ada.Command_Line.Argument (2) in "loop-event-exception" | "loop-reentry" then
               declare Item : M.Message := M.NULL_MESSAGE; begin
                  Item.tag.label := 16#333#; M.Enqueue_Event (Sender, Item);
               end;
               Raise_Loop_Event := Ada.Command_Line.Argument (2) = "loop-event-exception";
               Reenter_Loop_Event := not Raise_Loop_Event;
            elsif Ada.Command_Line.Argument (2) = "unexpected-loop-completion" then M.Test.Completion := 1;
            end if;
            begin EC_Loop.Serve (Server, Reason);
            exception when Error : Program_Error =>
               if Ada.Command_Line.Argument (2) not in "source-exception" | "route-exception" | "completion-exception" | "loop-event-exception" then
                  raise;
               end if;
               Raised := True;
               Check (Ada.Exceptions.Exception_Message (Error) =
                 (if Raise_Source then "modeled source queue exception"
                  elsif Raise_Route then "modeled local fanout exception"
                  elsif Raise_Completion then "modeled completion handoff exception"
                  else "modeled loop event exception"), "exact modeled EC contract violation propagates");
            end;
            Info := EC_Owner.Observation;
            if Ada.Command_Line.Argument (2) = "loop-event-exception" then
               Check (Raised and then not EC_Owner.Is_Stopped and then EC_Owner.Observation_Complete,
                 "host-only outer event failure preserves the prior report without inventing a completed stop");
            elsif Ada.Command_Line.Argument (2) in "source-exception" | "route-exception" | "completion-exception" then
               Check (Raised and then EC_Owner.Is_Stopped and then not EC_Owner.Observation_Complete,
                 "callback exception stops EC owner without a completed stale observation");
            else
               Check (not Raised and then EC_Owner.Observation_Complete,
                 "EC loop reports a complete bounded turn");
            end if;
            if Ada.Command_Line.Argument (2) in "kernel-stop-pending" | "kernel-stop-before-work" then
               Check (Reason = ACPI_Native_Server.Stop_Requested and then EC_Owner.Is_Stopped
                 and then Info.Status = EC_Owner.Stopped_By_Owner and then Loop_Event_Calls = 0,
                 "authenticated stop closes the owner and cannot be vetoed by a custom event hook");
               Check (M.Test.Events_Taken = 1 and then M.Test.Poll_Count = 0 and then M.Test.Wait_Count = 0
                 and then M.Test.Reply_Count = 0 and then Completion_Calls = 0 and then Route_Calls = 0,
                 "kernel stop precedes source work,fanout,handoff,request dispatch and wait");
               Check (Source_Taken = (if Ada.Command_Line.Argument (2) = "kernel-stop-pending" then 1 else 0)
                 and then EC_Owner.Pending_Completion = Completion_Before,
                 "kernel stop preserves the exact retained completion and queued follower");
            elsif Ada.Command_Line.Argument (2) = "loop-event-exception" then
               Check (Source_Taken = 1 and then Route_Calls = 0 and then Completion_Calls = 0
                 and then EC_Owner.Pending_Completion = Completion_Before,
                 "outer contract failure preserves retained completion before local work");
               Check (M.Test.Reply_Count = 0 and then M.Test.Wait_Count = 0 and then Loop_Event_Calls = 1,
                 "outer contract failure never fabricates a reply or idle observation");
               declare
                  Prior_Events : constant Natural := M.Test.Event_Polls;
                  Prior_Takes : constant Natural := Take_Calls;
               begin
                  EC_Loop.Serve (Server, Reason);
                  Check (Reason = ACPI_Native_Server.Owner_Work_Refused and then
                    M.Test.Event_Polls = Prior_Events and then Take_Calls = Prior_Takes,
                    "host-only second call cannot reopen the fatal serving attempt");
               end;
            elsif Ada.Command_Line.Argument (2) = "unexpected-loop-completion" then
               Check (EC_Owner.Is_Stopped and then Info.Status = EC_Owner.Stopped_By_Owner
                 and then Source_Taken = 1 and then Route_Calls = 0 and then Completion_Calls = 0
                 and then EC_Owner.Pending_Completion = Completion_Before,
                 "outer loop failure retains completion and stops before later local work");
               Check (M.Test.Reply_Count = 0 and then M.Test.Wait_Count = 0 and then
                 (if Raised then Loop_Event_Calls = 1 else Reason = ACPI_Native_Server.Unexpected_Completion),
                 "loop failure cannot become a successful stop acknowledgement or dispatch");
            elsif Ada.Command_Line.Argument (2) in "stop-pending" | "stop-idle" then
               Check (Reason = ACPI_Native_Server.Owner_Work_Refused
                 and then Info.Status = EC_Owner.Stopped_By_Owner and then M.Test.Reply_Count = 0
                 and then M.Test.Wait_Count = 0 and then Completion_Calls = 0,
                 "explicit stop prevents later native dispatch,wait or completion handoff");
               Check (Source_Taken = (if Queue_Length = 0 then 0 else 1)
                 and then (if Queue_Length /= 0 then Route_Calls = 0 and then EC_Owner.Pending_Completion.Present),
                 "stopped pending event does not drain notifications or take its queued follower");
            elsif Ada.Command_Line.Argument (2) = "owner-fatal" then
               Check (Reason = ACPI_Native_Server.Owner_Work_Refused
                 and then Info.Status = EC_Owner.Recovery_Required and then Info.Fatal.Present
                 and then Info.Event.Present and then Info.Event.Item.Token = 13,
                 "queued Fatal stops native owner with exact claimed event and diagnostic");
               Check (Source_Taken = 3 and then Take_Calls = 3 and then M.Test.Reply_Count = 2
                 and then M.Test.Poll_Count = 2 and then M.Test.Wait_Count = 0,
                 "two successful EC turns give IPC a turn; Fatal leaves later event queued");
            elsif Ada.Command_Line.Argument (2) = "source-fail" then
               Check (Reason = ACPI_Native_Server.Owner_Work_Refused
                 and then Info.Status = EC_Owner.Source_Unhealthy
                 and then Info.Source_Item.Present and then not Info.Event.Present,
                 "unhealthy source retains description without claiming AML delivery");
               Check (Source_Taken = 1 and then M.Test.Reply_Count = 0
                 and then M.Test.Wait_Count = 0 and then not Receiver.Recovery_Needed,
                 "unhealthy source stops before dispatch and AML");
            elsif Ada.Command_Line.Argument (2) in "owner-fanout" | "completion-batch-blocked" then
               Check (Reason = ACPI_Native_Server.Owner_Work_Refused
                 and then Info.Status = EC_Owner.Notifications_Refused
                 and then Info.Notifications.Retained /= 0,
                 "blocked fanout retains broker receipt and refuses another source event");
               Check (Source_Taken = 1 and then
                 M.Test.Reply_Count = (if Ada.Command_Line.Argument (2) = "completion-batch-blocked" then 0 else 1)
                 and then M.Test.Wait_Count = 0,
                 "fanout refusal preserves IPC fairness before stopping");
            elsif Ada.Command_Line.Argument (2) = "completion-refused" then
               Check (Reason = ACPI_Native_Server.Owner_Work_Refused
                 and then Info.Status = EC_Owner.Completion_Unconfirmed
                 and then Info.Completion_Attempted and then not Info.Completion_Confirmed
                 and then Info.Completion.Present and then Info.Completion.Item = Queued (1)
                 and then EC_Owner.Pending_Completion = Info.Completion,
                 "unconfirmed completion retains exact event and stops before later source work");
               Check (Source_Taken = 1 and then Completion_Calls = 1 and then M.Test.Wait_Count = 0,
                 "unconfirmed handoff is never retried and never permits idle");
            elsif Ada.Command_Line.Argument (2) in "source-exception" | "route-exception" | "completion-exception" then
               Check (Info.Status = EC_Owner.Not_Started and then M.Test.Wait_Count = 0,
                 "exception cannot report an old successful EC turn");
               Check ((if Raise_Source then Source_Taken = 0 and then M.Test.Reply_Count = 0
                 else Source_Taken = 1 and then M.Test.Reply_Count = 1),
                 "source and routing exceptions do not replay or consume a later event");
            else
               Check (Reason = ACPI_Native_Server.Wait_Unavailable
                 and then Info.Status = EC_Owner.Source_Empty and then not EC_Owner.Is_Stopped,
                 "explicit source and notification empty observation permits idle wait");
               Check (M.Test.Wait_Count = 1 and then Source_Taken = Queue_Length
                 and then Take_Calls = Queue_Length + (if Ada.Command_Line.Argument (2) = "completion-batch" then 2 else 1)
                 and then M.Test.Reply_Count = Queue_Length,
                 "bounded EC work drains with IPC fairness and exactly one final idle wait");
            end if;
            if Raise_Completion then
               Check (Completion_Calls = 1 and then Info.Completion_Attempted
                 and then EC_Owner.Pending_Completion.Present
                 and then EC_Owner.Pending_Completion.Item = Queued (1),
                 "exception retains incomplete completion without authorizing a retry");
            elsif Ada.Command_Line.Argument (2) in "owner-drain" | "owner-fatal" | "completion-reentry" | "stop-reentry" |
              "foreign-stop" | "malformed-stop" | "loop-reentry" then
               Check (Completion_Calls = 2 and then Completed_Item.Present
                 and then Completed_Item.Item.Token = 12 and then Completion_Charge > 0
                 and then not EC_Owner.Pending_Completion.Present,
                 "both ordinary events settle exactly once before the next source or idle");
               if Ada.Command_Line.Argument (2) in "foreign-stop" | "malformed-stop" then
                  Check (Loop_Event_Calls = 0 and then M.Test.Events_Taken = 1,
                    "rejected reserved system messages never reach the custom hook or stop the owner");
               elsif Reenter_Loop_Event then
                  Check (Loop_Event_Calls = 1 and then not EC_Owner.Is_Stopped,
                    "refused nested serve does not poison the valid outer serving loop");
               end if;
            elsif Ada.Command_Line.Argument (2) = "completion-missing" then
               Check (Completion_Calls = 1 and then Completed_Item.Item = Queued (1)
                 and then Completion_Charge = 0 and then not EC_Owner.Pending_Completion.Present,
                 "missing handler settles without invented execution charge");
            elsif Ada.Command_Line.Argument (2) = "completion-batch" then
               Check (Completion_Calls = 1 and then Completed_Item.Item = Queued (1)
                 and then Completion_Charge > 0 and then Route_Calls = 12
                 and then not EC_Owner.Pending_Completion.Present,
                 "completion follows all six acknowledged notifications over multiple turns");
               Sample (Cache, Driver.Open); Check (View.Value.Raw = 8,
                 "multi-turn notification drain does not rerun Q10");
            elsif Refuse_Completion then null;
            else
               Check (Completion_Calls = 0,
                 "unready,source or routing failures never publish an event completion");
            end if;
            if EC_Owner.Is_Stopped then
               Taken_Before := Take_Calls;
               Completion_Before := EC_Owner.Pending_Completion;
               Calls_Before := Completion_Calls;
               Prior_Status := EC_Owner.Observation.Status;
               EC_Owner.Request_Stop (Stop_Result);
               Check (Stop_Result = (if Raised and then not EC_Owner.Observation_Complete then EC_Owner.Operation_Incomplete else EC_Owner.Already_Stopped)
                 and then EC_Owner.Observation.Status = Prior_Status,
                 "duplicate stop preserves terminal or incomplete diagnostics");
               EC_Loop.Serve (Server, Reason);
               Check (Reason = ACPI_Native_Server.Owner_Work_Refused and then Take_Calls = Taken_Before,
                 "stopped EC owner never dequeues or retries source work");
               EC_Owner.Cleanup (Server, Closed);
               Check (EC_Owner.Is_Stopped and then
                 (if Raised and then not EC_Owner.Observation_Complete then Closed = Receiver.Receiver_Busy
                  else Closed = Receiver.No_Pending_Result),
                 "explicit cleanup preserves stop; exceptional incomplete owner cannot retry cleanup");
               Check (EC_Owner.Pending_Completion = Completion_Before and then Completion_Calls = Calls_Before,
                 "stopped turns and cleanup preserve retained completion without replay");
            else
               EC_Owner.Cleanup (Server, Closed);
               Check (Closed = Receiver.Receiver_Busy,
                 "live EC owner rejects terminal cleanup operation");
            end if;
         end;
      end if;
   end if;
   Ada.Text_IO.Put_Line ("ACPI EC owner checks" & Natural'Image (Checks));
end EC_Owner_Tests;
