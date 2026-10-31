pragma Ada_2022;
with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with ACPI_Service;
with ACPI_Requests;
with ACPI_Requests.Lids;
with ACPI_Requests.EC_Events;
with ACPI_Requests.With_Notification_Routing;
with ACPI_Requests.Notification_Pump;
with ACPI_EC_Methods;
with ACPI_EC_Event_Receiver;
with ACPI_EC_Event_Types;
with ACPI_Lid;
with ACPI_Power_Source;
with ACPI_Requests.Power_Sources;
with Firmware_Tables;
with CuBit.Process_IDs;
with Interfaces;
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
   use type C.Refresh_Outcome, S.Availability, Driver.Lid_State;
   use type Lids.Controller.Discovery_Outcome;
   use type PC.Refresh_Outcome, PS.Availability, PS.Publication_Result;
   use type Power_Driver.Source_State, Power.Controller.Discovery_Outcome;
   Checks : Natural := 0;
   Fixture_Limit : constant := 4096;
   Budget : constant := 1000;
   -- Seven predefined root children precede the fixture's first declaration.
   Fixture_EC_Node : constant ACPI_Service.Namespace.Node_ID := 8;
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
   Partial_Fanout : Boolean := False;
   Route_Calls, Fail_Route_At : Natural := 0;
   procedure Route (Item : Q.State; Routed : out Boolean) is
      Seen : Boolean;
   begin
      Route_Calls := Route_Calls + 1;
      Routed := Fanout and then (Fail_Route_At = 0 or else Route_Calls /= Fail_Route_At);
      if not Routed then return; end if;
      Lids.Observe_Claimed (Cache, Item, Seen);
      if Partial_Fanout then Routed := False; return; end if;
      Lids.Observe_Claimed (Other_Cache, Item, Seen);
      Power.Observe_Claimed (Power_Cache, Item, Seen);
   end Route;
   package Routing is new Q.With_Notification_Routing (Route);
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
   Pump.Run (Server, 4, Pump_Result);
   Check (Pump_Result.Status = Pump.Not_Ready and then Route_Calls = 0,
     "unstarted owner cannot claim or route notifications");
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
      Events.Close (Server, Run, Closed);
      Check (Closed = Receiver.No_Pending_Result, "completed event has no cleanup replay");
   end if;
   Ada.Text_IO.Put_Line ("ACPI EC owner checks" & Natural'Image (Checks));
end EC_Owner_Tests;
