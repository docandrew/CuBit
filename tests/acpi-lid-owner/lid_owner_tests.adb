pragma Ada_2022;
with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with ACPI_Requests;
with ACPI_Requests.Lids;
with ACPI_Requests.With_Notification_Routing;
with ACPI_Lid;
with ACPI_Service;
with Firmware_Tables;
with Interfaces;
procedure Lid_Owner_Tests is
   package Q renames ACPI_Requests;
   package Driver is new ACPI_Lid (ACPI_Service);
   package Owner is new Q.Lids (Driver);
   package C renames Owner.Cached;
   package S renames Owner.Samples;
   package IO renames Ada.Streams.Stream_IO;
   use type Ada.Streams.Stream_Element_Offset;
   use type Q.Outcome, Q.Phase, Q.Revision_Number, Q.Response_Words;
   use type C.Refresh_Outcome;
   use type S.Availability, S.Publication_Result;
   use type Driver.Lid_State;
   use type Owner.Controller.Discovery_Outcome;
   use type Driver.Query_Outcome;
   Checks : Natural := 0;
   Fixture_Limit : constant := 4096;
   Query_Budget : constant := 1000;
   Startup_And_Receipt_Mutations : constant Q.Revision_Number := 6;
   procedure Check (Good : Boolean; Why : String) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Why; end if; end Check;
   function Read_Table return Firmware_Tables.Bytes is
      File : IO.File_Type;
   begin
      Check (Ada.Command_Line.Argument_Count = 2, "pinned table and isolated lifecycle");
      IO.Open (File, IO.In_File, Ada.Command_Line.Argument (1));
      Check (IO.Size (File) in IO.Count (Firmware_Tables.Table_Header_Size) .. Fixture_Limit, "bounded table");
      declare
         Raw : Ada.Streams.Stream_Element_Array (1 .. Ada.Streams.Stream_Element_Offset (IO.Size (File)));
         Last : Ada.Streams.Stream_Element_Offset;
         Data : Firmware_Tables.Bytes (1 .. Raw'Length);
      begin
         IO.Read (File, Raw, Last); IO.Close (File); Check (Last = Raw'Last, "exact table read");
         for I in Data'Range loop Data (I) := Firmware_Tables.Byte (Raw (Ada.Streams.Stream_Element_Offset (I))); end loop;
         return Data;
      end;
   end Read_Table;
   Table : constant Firmware_Tables.Bytes := Read_Table;
   Initial : constant Q.Revision_Number :=
     (if Ada.Command_Line.Argument (2) = "ceiling" then Q.Max_Revision - Startup_And_Receipt_Mutations else 0);
   Server : Q.State (1, Table'Length, Table'Length, 1, Initial);
   Cache, Other_Cache : Owner.Cache;
   Reply : Q.Response;
   Discovery : Owner.Controller.Discovery_Report;
   Report : C.Refresh_Report;
   View : S.View;
   Seen : Boolean;
   Receipt : Q.Revision_Number;
   Route_Fails : Boolean := True;
   Route_Calls : Natural := 0;
   procedure Route_Local (Item : Q.State; Routed : out Boolean) is
      Ignored : Boolean;
   begin
      Route_Calls := Route_Calls + 1; Routed := not Route_Fails;
      if not Routed then return; end if;
      Owner.Observe_Claimed (Cache, Item, Ignored);
      Owner.Observe_Claimed (Other_Cache, Item, Ignored);
   end Route_Local;
   package Routing is new Q.With_Notification_Routing (Route_Local);
   procedure Route_Failure_Checks is
      Ack : Q.Packet := (Label => Q.Acknowledge_Notification,
        Data => [Q.Revision (Server), Receipt, 0, 0], others => <>);
      Base : constant Q.Packet := Ack;
      Before : constant Q.Revision_Number := Q.Revision (Server);
      procedure Reject (Expected : Q.Outcome; Origin : Q.Authority := Q.Snapshot_Provider) is
         Calls : constant Natural := Route_Calls;
      begin
         Routing.Handle (Server, Origin, Ack, Reply);
         Check (Reply.Status = Expected and then Q.Revision (Server) = Before
           and then Route_Calls = Calls, "invalid ACK retains exact protocol outcome without local callback");
      end Reject;
   begin
      Reject (Q.Denied, Q.No_Authority);
      Check (Reply.Data = [0,0,0,0], "unclassified routing request reply is zeroed");
      Reject (Q.Denied, Q.Observer);
      Ack.Length := 3; Reject (Q.Malformed); Ack := Base;
      Ack.Flags := 1; Reject (Q.Malformed); Ack := Base;
      Ack.Reserved := 1; Reject (Q.Malformed); Ack := Base;
      Ack.Data (2) := 1; Reject (Q.Malformed); Ack := Base;
      Ack.Data (0) := Before - 1; Reject (Q.Stale); Ack := Base;
      Ack.Data (1) := Receipt + 1; Reject (Q.Stale); Ack := Base;
      Ack.Data (1) := 0; Reject (Q.Malformed); Ack := Base;
      Ack.Data (1) := Q.Max_Revision + 1; Reject (Q.Malformed); Ack := Base;
      Routing.Handle (Server, Q.Snapshot_Provider, Ack, Reply);
      Check (Reply.Status = Q.Incomplete and then Reply.Data (1) = Receipt
        and then Q.Revision (Server) = Before and then Route_Calls = 1,
        "local routing failure prevents ACK from discarding retained event");
      Routing.Handle (Server, Q.Snapshot_Provider,
        (Label => Q.Claim_Notification, Data => [Before,0,0,0], others => <>), Reply);
      Check (Reply.Status = Q.Incomplete and then Reply.Data (1) = Receipt
        and then Q.Revision (Server) = Before and then Route_Calls = 2,
        "failed immediate Claim routing preserves same retained receipt and revision");
   end Route_Failure_Checks;
   procedure Send (Label : Interfaces.Unsigned_32; Value : Q.Revision_Number := 0) is
   begin
      Q.Handle (Server, Q.Snapshot_Provider,
        (Label => Label, Data => [Q.Revision (Server), Value, 0, 0], others => <>), Reply);
      Check (Reply.Status = Q.OK and then Q.Valid (Server), "owner protocol operation succeeds");
   end Send;
   procedure Prepare (Budget : Natural := Query_Budget) is
      Before : constant Q.Revision_Number := Q.Revision (Server);
   begin
      Owner.Prepare (Cache, Server, Budget, Report);
      Check (Q.Revision (Server) = Before and then Q.Valid (Server), "cached AML observation preserves request revision");
   end Prepare;
   procedure Publish (Healthy : Boolean := True; Clock : S.Clock_Reading := (True, 100)) is
   begin Owner.Publish (Cache, Server, Clock, Healthy, Report); end Publish;
   procedure Read is
   begin View := Owner.Read (Cache, Server, (True, 100), 0); end Read;
begin
   Owner.Bind (Cache, Server, Query_Budget, Discovery);
   Check (Discovery.Outcome = Owner.Controller.Not_Ready, "fresh service has no lid binding");
   Prepare; Check (Report.Status = C.Not_Bound, "unbound cache cannot execute AML");
   Read; Check (View.Status = S.Not_Ready and then View.Value.State = Driver.Unknown, "unready cache is unknown");
   Send (Q.Start_Snapshot, 1);
   Q.Import_Block (Server, Q.Snapshot_Provider, Q.Revision (Server), 1, ACPI_Service.DSDT, Table, Reply);
   Check (Reply.Status = Q.OK, "actual lid AML table imported");
   Send (Q.Finish_Snapshot);
   Check (Q.Current (Server) = Q.Complete, "catalog completes independently of active readiness");
   Owner.Bind (Cache, Server, Query_Budget, Discovery);
   if Ada.Command_Line.Argument (2) = "hardware" then
      Check (Discovery.Outcome = Owner.Controller.Not_Ready, "unconfigured region blocks active lid discovery");
      Prepare; Check (Report.Status = C.Not_Bound, "region barrier prevents active query");
      Read; Check (View.Status = S.Not_Ready and then View.Value.State = Driver.Unknown, "blocked hardware never looks closed");
   else
      Check (Discovery.Outcome = Owner.Controller.Found, "trusted owner discovers fixed lid identity");
      Prepare; Check (Report.Status = C.Not_Ready, "notification admission required before samples");
      Send (Q.Configure_Notifications, 1);
      Prepare; Check (Report.Status = C.Collected and then Report.Query.Raw = 0, "first actual LID result is closed");
      Prepare; Check (Report.Status = C.Busy, "pending collection never invokes LID twice");
      Publish; Read;
      Check (Report.Status = C.Updated and then View.Status = S.Available
        and then View.Value.State = Driver.Closed and then View.Value.Raw = 0, "closed sample published once");
      for I in 1 .. 4 loop Read; Check (View.Status = S.Available and then View.Value.Raw = 0, "observer copies have no AML effects"); end loop;
      Publish; Check (Report.Status = C.Publication_Refused, "publication cannot repeat");
      Owner.Bind (Other_Cache, Server, Query_Budget, Discovery);
      Check (Discovery.Outcome = Owner.Controller.Found, "second local consumer binds same owner");
      Owner.Prepare (Other_Cache, Server, Query_Budget, Report);
      Owner.Publish (Other_Cache, Server, (True, 100), True, Report);
      View := Owner.Read (Other_Cache, Server, (True, 100), 0);
      Check (Report.Status = C.Updated and then View.Status = S.Available
        and then View.Value.State = Driver.Open and then View.Value.Raw = 1, "second actual query proves no read or busy replay");
      Prepare; Check (Report.Query.Outcome = Driver.Sampled and then Report.Query.Raw = 2, "third query emits actual notification");
      View := Owner.Read (Other_Cache, Server, (True, 100), 0);
      Check (View.Status = S.Not_Ready and then View.Value.State = Driver.Unknown, "queued notification hides another consumer fresh sample");
      Send (Q.Claim_Notification); Receipt := Reply.Data (1);
      Check (Receipt /= 0 and then Reply.Data (2) = 0, "event retained in receipt after Core FIFO empties");
      View := Owner.Read (Other_Cache, Server, (True, 100), 0);
      Check (View.Status = S.Not_Ready and then View.Value.State = Driver.Unknown, "unseen retained receipt hides another consumer freshness");
      declare Pending_Cache : Owner.Cache; Other_Report : C.Refresh_Report; begin
         Owner.Bind (Pending_Cache, Server, Query_Budget, Discovery);
         Owner.Prepare (Pending_Cache, Server, 0, Other_Report);
         Owner.Publish (Pending_Cache, Server, (True, 100), True, Other_Report);
         Check (Other_Report.Status = C.Routing_Failed, "unseen staged event prevents publication with empty Core FIFO");
      end;
      Route_Failure_Checks;
      Owner.Observe_Claimed (Cache, Server, Seen); Check (Seen, "same owner's retained event routed locally");
      declare Before : constant S.Revision_Number := Owner.Read (Cache, Server, (True, 100), 0).Revision; begin
         Owner.Observe_Claimed (Cache, Server, Seen);
         Check (not Seen and then Owner.Read (Cache, Server, (True, 100), 0).Revision = Before,
           "repeated receipt does not invalidate or consume sample counter twice");
      end;
      Publish; Read;
      Check (Report.Publication = S.Stale_Revision and then View.Status = S.Not_Ready,
        "notification arriving during collection prevents stale publication");
      Send (Q.Claim_Notification); Check (Reply.Data (1) = Receipt, "local routing preserves broker receipt");
      Route_Fails := False;
      Routing.Handle (Server, Q.Snapshot_Provider,
        (Label => Q.Claim_Notification, Data => [Q.Revision (Server),0,0,0], others => <>), Reply);
      Check (Reply.Status = Q.OK and then Reply.Data (1) = Receipt and then Route_Calls = 3,
        "successful Claim routes actual retained event to every local cache");
      declare
         Before : constant Q.Revision_Number := Q.Revision (Server);
         Ack : constant Q.Packet := (Label => Q.Acknowledge_Notification, Data => [Before,Receipt,0,0], others => <>);
      begin
         Routing.Handle (Server, Q.Snapshot_Provider, Ack, Reply);
         Check (Reply.Status = Q.OK and then Q.Revision (Server) = Before + 1 and then Route_Calls = 4,
           "successful fanout precedes canonical ACK and consumes one revision");
         Routing.Handle (Server, Q.Snapshot_Provider, Ack, Reply);
         Check (Reply.Status = Q.OK and then Q.Revision (Server) = Before + 1 and then Route_Calls = 4,
           "repeated acknowledged receipt stays idempotent without local replay");
      end;
      View := Owner.Read (Other_Cache, Server, (True, 100), 0);
      Check (View.Status = S.Not_Ready and then View.Value.State = Driver.Unknown,
        "second consumer invalidated before broker receipt disappears");
      Prepare; Publish; Read;
      Check (Report.Status = C.Updated and then View.Status = S.Available and then View.Value.Raw = 3,
        "next explicit refresh publishes after routing without replay");
      Prepare (0); Publish; Read;
      Check (View.Status = S.Failure and then View.Value.State = Driver.Unknown, "budget refusal is unknown not closed");
      Prepare; Publish (Healthy => False); Read;
      Check (Report.Status = C.Routing_Failed and then View.Status = S.Not_Ready, "uncertain event routing blocks publication");
      Prepare; Publish (Clock => (False, 0)); Read;
      Check (View.Status = S.Clock_Unknown and then View.Value.State = Driver.Unknown, "missing clock cannot certify fresh sample");
      Prepare; Publish;
      View := Owner.Read (Cache, Server, (True, 101), 0);
      Check (View.Status = S.Too_Old and then View.Value.State = Driver.Unknown, "expired sample is unknown");
      View := Owner.Read (Cache, Server, (True, 99), 0);
      Check (View.Status = S.Clock_Regressed and then View.Value.State = Driver.Unknown, "regressing clock is unknown");
      Owner.Invalidate (Cache); Read;
      Check (View.Status = S.Not_Ready, "owner invalidation hides sample");
      Owner.Forget (Cache); Prepare;
      Check (Report.Status = C.Not_Bound, "forgotten binding cannot query");
      Check (Q.Revision (Server) = Initial + Startup_And_Receipt_Mutations, "owner observations do not consume request revision even at ceiling");
   end if;
   Ada.Text_IO.Put_Line ("ACPI owned lid cache checks" & Checks'Image);
end Lid_Owner_Tests;
