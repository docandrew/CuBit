pragma Ada_2022;
with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with ACPI_Service;
with ACPI_Requests;
with ACPI_Requests.Batteries;
with ACPI_Requests.EC_Events;
with ACPI_Requests.With_Notification_Routing;
with ACPI_Requests.Notification_Pump;
with ACPI_EC_Methods;
with ACPI_EC_Event_Receiver;
with ACPI_EC_Event_Types;
with ACPI_Battery;
with ACPI_Battery_Information;
with ACPI_Battery_Information_Reader;
with ACPI_Battery_Observation;
with ACPI_Battery_Cache;
with Firmware_Tables;
with CuBit.Process_IDs;
with Interfaces;
procedure Battery_Owner_Tests is
   package Q renames ACPI_Requests;
   package Core renames ACPI_Service;
   package IO renames Ada.Streams.Stream_IO;
   Calls : Natural := 0;
   procedure Evaluate (Service : aliased in out Core.State; Item : Core.Node_Descriptor;
      Budget : Natural; Result : out Core.Values.Result; Status : out Core.Values.Access_Status) is
   begin
      Calls := Calls + 1;
      Core.Evaluate_Named (Service, Item, Budget, Result, Status);
   end Evaluate;
   package Format is new ACPI_Battery_Information;
   package Dynamic is new ACPI_Battery (Core, Evaluate);
   package Static_Info is new ACPI_Battery_Information_Reader (Core, Format, Evaluate);
   package Collector is new ACPI_Battery_Observation (Core, Format, Dynamic, Static_Info);
   package Store is new ACPI_Battery_Cache (Core, Format, Dynamic, Static_Info, Collector);
   package Owner is new Q.Batteries (Dynamic, Store);
   package S renames Owner.Samples;
   package Methods is new ACPI_EC_Methods (Core);
   package Receiver is new ACPI_EC_Event_Receiver (Core, Methods);
   package Events is new Q.EC_Events (Receiver);
   package E renames ACPI_EC_Event_Types;
   use type Ada.Streams.Stream_Element_Offset;
   use type Q.Outcome, Q.Phase, Q.Revision_Number;
   use type Owner.Controller.Admission, Store.Binding_Result, Store.Refresh_Result;
   use type Collector.Outcome, S.Availability, S.Publication_Result;
   use type Interfaces.Unsigned_32;
   use type Receiver.Enrollment, Receiver.Outcome, Events.Controller.Admission;
   use type Owner.Controller.Discovery_Outcome;
   Checks : Natural := 0;
   Budget : constant := 4000;
   Fixture_Limit : constant := 4096;
   -- Fixture device indices follow seven predefined root children and the
   -- battery's six named members. They are test layout, not discovery policy.
   Battery_Node : constant Core.Namespace.Node_ID := 8;
   EC_Node : constant Core.Namespace.Node_ID := 15;
   Sender : constant CuBit.Process_IDs.Process_ID := CuBit.Process_IDs.To_Process_ID (301, 987);
   procedure Check (Good : Boolean; Why : String) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Why; end if; end Check;
   function Read_Table return Firmware_Tables.Bytes is
      File : IO.File_Type;
   begin
      Check (Ada.Command_Line.Argument_Count = 2, "fixture and lifecycle");
      IO.Open (File, IO.In_File, Ada.Command_Line.Argument (1));
      Check (IO.Size (File) in IO.Count (Firmware_Tables.Table_Header_Size) .. Fixture_Limit, "bounded table");
      declare
         Raw : Ada.Streams.Stream_Element_Array (1 .. Ada.Streams.Stream_Element_Offset (IO.Size (File)));
         Last : Ada.Streams.Stream_Element_Offset;
         Data : Firmware_Tables.Bytes (1 .. Raw'Length);
      begin
         IO.Read (File, Raw, Last); IO.Close (File); Check (Last = Raw'Last, "exact table");
         for I in Data'Range loop Data (I) := Firmware_Tables.Byte (Raw (Ada.Streams.Stream_Element_Offset (I))); end loop;
         return Data;
      end;
   end Read_Table;
   Table : constant Firmware_Tables.Bytes := Read_Table;
   Server : Q.State (1, Table'Length, Table'Length, 1, 0);
   Cache, Other_Cache : Owner.Cache;
   Allowed : Owner.Controller.Admission;
   Binding : Store.Binding_Result;
   Report : Store.Refresh_Report;
   View : S.View;
   Reply : Q.Response;
   Enrollment : Receiver.Enrollment;
   Event_Admission : Events.Controller.Admission;
   Outcome : Receiver.Outcome;
   Run, Absence, Return_Event : Receiver.Delivery;
   Notice : E.Notice := (1, 1, 1, 1);
   Receipt : Q.Revision_Number;
   Found_Node, Second_Node : Core.Namespace.Node_ID;
   Discovery : Owner.Controller.Discovery_Report;
   Fanout : Boolean := False;
   Seen : Boolean;
   procedure Route (Item : Q.State; Routed : out Boolean) is
      Ignored : Boolean;
   begin
      Routed := Fanout; if not Routed then return; end if;
      Owner.Observe_Claimed (Cache, Item, Ignored);
      Owner.Observe_Claimed (Other_Cache, Item, Ignored);
   end Route;
   package Routing is new Q.With_Notification_Routing (Route);
   package Pump is new Q.Notification_Pump (Route);
   use type Pump.Outcome;
   procedure Send (Label : Interfaces.Unsigned_32; Value : Q.Revision_Number := 0) is
   begin
      Q.Handle (Server, Q.Snapshot_Provider,
        (Label => Label, Data => [Q.Revision (Server), Value, 0, 0], others => <>), Reply);
      Check (Reply.Status = Q.OK and then Q.Valid (Server), "owner protocol");
   end Send;
   procedure Prepare (Data : in out Owner.Cache; Fuel : Natural := Budget) is
      Before : constant Q.Revision_Number := Q.Revision (Server);
   begin
      Owner.Prepare (Data, Server, Fuel, Allowed, Report);
      Check (Q.Valid (Server) and then Q.Revision (Server) = Before, "collection preserves request revision");
   end Prepare;
   procedure Publish (Data : in out Owner.Cache) is
   begin Owner.Publish (Data, Server, (True, 100), True, Report); end Publish;
   procedure Deliver (Item : in out Receiver.Delivery) is
      Before : constant Q.Revision_Number := Q.Revision (Server);
   begin
      Events.Dispatch (Server, Item, Sender, Notice, Budget, Event_Admission, Outcome);
      Check (Event_Admission = Events.Controller.Admitted and then Outcome = Receiver.Completed
        and then Q.Revision (Server) = Before, "trusted same-Core EC handler");
   end Deliver;
   procedure Claim_And_Ack is
   begin
      Send (Q.Claim_Notification); Receipt := Reply.Data (1);
      Check (Receipt /= 0, "notification broker retains event");
      Routing.Handle (Server, Q.Snapshot_Provider,
        (Label => Q.Acknowledge_Notification,
         Data => [Q.Revision (Server), Receipt, 0, 0], others => <>), Reply);
      Check (Reply.Status = Q.OK, "fanout precedes ACK");
   end Claim_And_Ack;
begin
   Owner.Find (Server, Core.Namespace.Root, Budget, Found_Node, Discovery);
   Check (Discovery.Outcome = Owner.Controller.Not_Ready and then Calls = 0,
     "discovery cannot run before service readiness");
   Owner.Bind (Cache, Server, Battery_Node, Allowed, Binding);
   Check (Allowed = Owner.Controller.Not_Ready and then Calls = 0, "no early battery enrollment/evaluation");
   Prepare (Cache);
   Check (Allowed = Owner.Controller.Not_Ready and then Calls = 0, "no early battery sampling");
   View := Owner.Read (Cache, Server, (True, 100), 0);
   Check (View.Status = S.Not_Ready, "unstarted battery observation unavailable");
   Send (Q.Start_Snapshot, 1);
   Q.Import_Block (Server, Q.Snapshot_Provider, Q.Revision (Server), 1, Core.DSDT, Table, Reply);
   Check (Reply.Status = Q.OK, "real battery AML imported");
   Send (Q.Finish_Snapshot);
   Check (Q.Current (Server) = Q.Complete, "catalog complete");
   Owner.Bind (Cache, Server, Battery_Node, Allowed, Binding);
   Check (Allowed = Owner.Controller.Not_Ready, "notification readiness required before battery binding");
   Send (Q.Configure_Notifications, 1);
   Owner.Bind (Cache, Server, Battery_Node, Allowed, Binding);
   if Ada.Command_Line.Argument (2) = "hardware" then
      Check (Allowed = Owner.Controller.Not_Ready, "unconfigured region bars battery binding");
      Prepare (Cache);
      Check (Allowed = Owner.Controller.Not_Ready and then Calls = 0, "unconfigured region bars AML sampling");
   else
      Owner.Find (Server, Core.Namespace.Root, 0, Found_Node, Discovery);
      Check (Discovery.Outcome = Owner.Controller.Incomplete and then Calls = 0,
        "zero-budget discovery has no AML effects");
      Owner.Find (Server, Core.Namespace.Node_ID'Last, Budget, Found_Node, Discovery);
      Check (Discovery.Outcome = Owner.Controller.Invalid_Cursor and then Calls = 0,
        "cursor beyond current namespace rejected without AML");
      Owner.Find (Server, Core.Namespace.Root, Budget, Found_Node, Discovery);
      Check (Discovery.Outcome = Owner.Controller.Found and then Found_Node = Battery_Node
        and then Discovery.Charged <= Budget, "first actual battery discovered without guessed binding");
      Owner.Find (Server, Found_Node, Budget, Second_Node, Discovery);
      Check (Discovery.Outcome = Owner.Controller.Found and then Second_Node > Found_Node,
        "next matching battery skips unrelated EC");
      Owner.Find (Server, Second_Node, Budget, Found_Node, Discovery);
      Check (Discovery.Outcome = Owner.Controller.No_Match and then Found_Node = Core.Namespace.Root,
        "enumeration reaches end without inventing a device");
      Owner.Find (Server, Core.Namespace.Root, Budget, Found_Node, Discovery);
      Owner.Bind (Cache, Server, Found_Node, Allowed, Binding);
      Check (Binding = Store.Bound, "discovered node binds the actual battery cache");
      Calls := 0;
      Check (Allowed = Owner.Controller.Admitted and then Binding = Store.Bound, "battery bound in actual service Core");
      Owner.Bind (Other_Cache, Server, Battery_Node, Allowed, Binding);
      Check (Binding = Store.Bound, "second consumer bound");
      Prepare (Cache);
      Check (Report.Status = Store.Collected and then Report.Collection.Status = Collector.Sampled
        and then Calls = 5, "identity presence BIX BST presence share bounded collector");
      Prepare (Cache);
      Check (Report.Status = Store.Already_Pending and then Calls = 5, "pending collection never reruns AML");
      Publish (Cache);
      View := Owner.Read (Cache, Server, (True, 100), 0);
      Check (Report.Status = Store.Updated and then View.Status = S.Available
        and then View.Value.Status.Remaining_Capacity.Value = 20_000, "owned battery bundle published");
      for I in 1 .. 5 loop
         View := Owner.Read (Cache, Server, (True, 100), 0);
         Check (View.Status = S.Available and then Calls = 5, "observer copies never run AML");
      end loop;
      Prepare (Other_Cache); Publish (Other_Cache);
      Check (Report.Status = Store.Updated and then Calls = 10, "second independent cache sample");
      Events.Install (Server, 1, 1, Sender, EC_Node, Event_Admission, Enrollment);
      Check (Event_Admission = Events.Controller.Admitted and then Enrollment = Receiver.Installed, "EC enrolled after readiness");
      Deliver (Run);
      View := Owner.Read (Other_Cache, Server, (True, 100), 0);
      Check (View.Status = S.Not_Ready, "queued battery Notify suppresses stale bundle");
      Send (Q.Claim_Notification); Receipt := Reply.Data (1);
      View := Owner.Read (Other_Cache, Server, (True, 100), 0);
      Check (View.Status = S.Not_Ready, "staged unseen battery Notify suppresses stale bundle");
      Routing.Handle (Server, Q.Snapshot_Provider,
        (Label => Q.Acknowledge_Notification, Data => [Q.Revision (Server), Receipt, 0, 0], others => <>), Reply);
      Check (Reply.Status = Q.Incomplete, "failed battery fanout holds broker receipt");
      Prepare (Cache);
      Check (Report.Collection.Status = Collector.Sampled, "new battery collection before local routing");
      Fanout := True;
      Owner.Observe_Claimed (Cache, Server, Seen); Check (Seen, "actual retained battery event routed");
      Owner.Observe_Claimed (Cache, Server, Seen); Check (not Seen, "receipt invalidates each cache at most once");
      Claim_And_Ack;
      Publish (Cache);
      Check (Report.Publication = S.Stale_Revision, "event invalidates pre-routing battery collection");
      Prepare (Cache); Publish (Cache);
      View := Owner.Read (Cache, Server, (True, 100), 0);
      Check (View.Status = S.Available and then View.Value.Status.Remaining_Capacity.Value = 19_900,
        "post-event owned bundle reports new capacity");
      Notice.Token := 2; Notice.Number := 2; Deliver (Absence); Claim_And_Ack;
      Prepare (Cache); Check (Report.Collection.Status = Collector.Absent, "absence short circuits BIX/BST");
      Publish (Cache); View := Owner.Read (Cache, Server, (True, 100), 0);
      Check (View.Status = S.Missing, "known absence distinct from zero capacity and failure");
      Notice.Token := 3; Deliver (Return_Event); Claim_And_Ack;
      Prepare (Cache); Publish (Cache); View := Owner.Read (Cache, Server, (True, 100), 0);
      Check (View.Status = S.Available and then View.Value.Status.Remaining_Capacity.Value = 19_900,
        "reinsertion requires full fresh battery collection");
      Prepare (Cache, 0); Check (Report.Collection.Status = Collector.Budget_Exhausted, "zero fuel fails bounded collection");
      Publish (Cache); View := Owner.Read (Cache, Server, (True, 100), 0);
      Check (View.Status = S.Failure, "failed collection never retains previous payload");
      declare
         Pump_Run : Receiver.Delivery;
         Pump_Result : Pump.Report;
      begin
         Notice.Token := 4; Notice.Number := 1; Deliver (Pump_Run);
         Fanout := False; Pump.Run (Server, 4, Pump_Result);
         Check (Pump_Result.Status = Pump.Fanout_Blocked and then Pump_Result.Retained /= 0
           and then Pump_Result.Acknowledged = 0, "battery fanout failure retains pump receipt");
         Fanout := True; Pump.Run (Server, 4, Pump_Result);
         Check (Pump_Result.Status = Pump.Queue_Empty and then Pump_Result.Acknowledged = 1
           and then Pump_Result.Retained = 0, "battery event ACK follows complete local fanout");
         Prepare (Cache); Publish (Cache); View := Owner.Read (Cache, Server, (True, 100), 0);
         Check (View.Status = S.Available and then View.Value.Status.Remaining_Capacity.Value = 19_800,
           "pump leaves only explicit post-event battery refresh publishable");
      end;
      Owner.Forget (Cache); Prepare (Cache);
      Check (Report.Status = Store.Not_Bound, "forgotten battery cache cannot evaluate AML");
   end if;
   Ada.Text_IO.Put_Line ("ACPI battery owner checks" & Natural'Image (Checks));
end Battery_Owner_Tests;
