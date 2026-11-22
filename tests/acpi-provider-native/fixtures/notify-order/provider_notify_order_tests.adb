pragma Ada_2022;
with Ada.Text_IO;
with Ada.Command_Line;
with Interfaces; use Interfaces;
with ACPI_Requests; use ACPI_Requests;
with ACPI_Endpoint;
with ACPI_Service;
with ACPI_Provider_Transport;
with ACPI_Snapshot_Provider;
with ACPI_Notification_Consumer;
with CuBit.Handles;
with Firmware_Tables;
with Migration_Tables;
with Bridge_Trace; use Bridge_Trace;
with Server_Model; use Server_Model;
procedure Provider_Notify_Order_Tests is
   package Transport renames ACPI_Provider_Transport;
   Checks, Interest_Calls, Driver_Queries : Natural := 0;
   procedure Check (Good : Boolean) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Checks'Image; end if;
   end Check;
   procedure Query (Request : Packet; Reply : out Packet; Known : out Boolean) is
   begin
      Transport.Exchange (Request, Reply, Known);
      if Request.Label = Read_Metrics and then Start_Calls = 0 then
         Driver_Queries := Driver_Queries + 1;
         Check (Known and then Reply.Label = ACPI_Endpoint.Reply_OK and then Reply.Data(0) = 1);
      elsif Request.Label = Start_Snapshot then
         Check (Request.Data(0) = 1 and then Driver_Queries = 2);
      end if;
   end Query;
   package Driver is new ACPI_Snapshot_Provider.Driver
     (1, Query, Transport.Import_Table, Transport.Release_Provider_Grant);
   use type ACPI_Snapshot_Provider.Progress;
   package Client is new ACPI_Notification_Consumer (Maximum_Path => 8);
   use type Client.Result;
   -- Exact constructed payload from landed notification_consumer_tests.adb.
   -- No normal-ASL compiler provenance is claimed for these bytes.
   Payload : constant Firmware_Tables.Bytes :=
     [16#5B#,16#82#,5,68,69,86,48,
      16#14#,25,95,73,78,73,0,
      16#86#,68,69,86,48,16#0A#,16#80#,
      16#86#,68,69,86,48,16#0C#,0,0,1,0,16#A4#,0];
   Raw : constant Firmware_Tables.Bytes := Migration_Tables.Make(Payload);
   Items : constant ACPI_Snapshot_Provider.Catalog(5..5) :=
     [5 => (41,ACPI_Service.DSDT,Raw'Length)];
   Slices : constant Transport.Table_Slices(5..5) := [5 => (Items(5),0)];
   S : Driver.State;
   C : Client.State;
   Outcome : Client.Result;
   Token : Client.Token;
   Item : Client.Observation;
   Request, Reply : Packet;
   Accepted, Known : Boolean;
   type Gate_State is (Fresh, Armed, Stopped);
   Gate : Gate_State := Fresh;
   Lost : Boolean;
   procedure Configure_Once is
   begin
      if Gate /= Fresh then return; end if;
      Gate := Stopped; -- consumed before sending; unknown never permits replay
      Interest_Calls := Interest_Calls + 1;
      Route ((Label => Configure_Notifications, Data => [0,1,0,0], others => <>), Reply);
      Known := not Lost; -- loss AFTER real dispatch; unchanged reply is ignored
      if Known and then Reply.Label = ACPI_Endpoint.Reply_OK and then
        Reply.Length = 4 and then Reply.Flags = 0 and then Reply.Reserved = 0 and then
        Reply.Data = [1,1,0,Unsigned_64(Access_Outcome_Code'Enum_Rep(Access_Available_Code))]
      then Gate := Armed; end if;
   end Configure_Once;
   procedure Begin_If_Armed is
   begin
      if Gate /= Armed then return; end if;
      Gate := Stopped;
      Driver.Begin_Catalog(S,Items);
   end Begin_If_Armed;
   procedure Exchange is
   begin
      Client.Next_Request(C,Request,Token,Outcome); Check(Outcome=Client.Ready);
      Route(Request,Reply);
      Client.Accept_Reply(C,Token,Reply,Outcome); Check(Outcome=Client.Applied);
   end Exchange;
   procedure Pending(ID,Count : Unsigned_64) is
   begin
      Route((Label=>Read_Notification_State,others=><>),Reply);
      Check(Reply.Label=ACPI_Endpoint.Reply_OK and then Reply.Data(2)=Count and then Reply.Data(3)=ID);
   end Pending;
begin
   Check(Ada.Command_Line.Argument_Count=1);
   Check(Ada.Command_Line.Argument(1) in "Success" | "Lost_Interest");
   Lost := Ada.Command_Line.Argument(1) = "Lost_Interest";
   Allow_Return := True;
   Transport.Initialize(CuBit.Handles.Test_Endpoint,Slices,Raw,(others=><>),Accepted);
   Check(Accepted);
   Configure_Once;
   Check(Current(Server)=Idle and then Revision(Server)=1 and then Start_Calls=0 and then Finish_Calls=0);
   Configure_Once; Check(Interest_Calls=1);
   Begin_If_Armed;
   if Lost then
      Begin_If_Armed; Configure_Once; Driver.Advance(S);
      Check(Gate=Stopped and then Driver.Status(S)=ACPI_Snapshot_Provider.Not_Started);
      Check(Interest_Calls=1 and then Driver_Queries=0 and then Start_Calls=0 and then
        Import_Calls=0 and then Finish_Calls=0 and then Creates=0);
      Pending(0,0);
   else
      Check(Driver.Status(S)=ACPI_Snapshot_Provider.Running);
      for Step in 1..5 loop Driver.Advance(S); end loop;
      Check(Driver.Status(S)=ACPI_Snapshot_Provider.Completed and then Current(Server)=Complete);
      Check(Driver_Queries=2 and then Start_Calls=1 and then Import_Calls=1 and then Finish_Calls=1);
      Check(Creates=1 and then Acquires=1 and then not Active and then not Transport.Grant_Pending);
      Pending(0,2);
      Client.Start_Session(C,Outcome); Check(Outcome=Client.Applied);
      for Receipt in 1..2 loop
         Exchange; Exchange; -- synchronize and claim
         for Page in 1..3 loop Exchange; end loop; -- header, identity, path
         Check(Client.Has_Observation(C)); Item:=Client.Copy_Observation(C);
         Check(Item.ID=Unsigned_64(Receipt) and then Item.Length=1 and then Item.Path(1)=16#30564544#
           and then Item.Kind=Notify_Device and then Item.Class=Notify_Device_Class
           and then Item.Number=(if Receipt=1 then 128 else 0)
           and then Item.Node>0 and then Item.Incarnation>0 and then Item.Current_At_Claim);
         Client.Next_Request(C,Request,Token,Outcome); Check(Outcome=Client.Idle);
         Pending(Unsigned_64(Receipt),Unsigned_64(2-Receipt));
         Client.Confirm_Delivery(C,Client.Session(C),Item.ID,Outcome); Check(Outcome=Client.Applied);
         Client.Next_Request(C,Request,Token,Outcome);
         Check(Outcome=Client.Ready and then Request.Label=Acknowledge_Notification and then Request.Data(1)=Item.ID);
         Route(Request,Reply); Client.Accept_Reply(C,Token,Reply,Outcome); Check(Outcome=Client.Applied);
         Pending(0,Unsigned_64(2-Receipt));
      end loop;
   end if;
   Ada.Text_IO.Put_Line(Ada.Command_Line.Argument(1) & " notify ordering checks" & Checks'Image);
end Provider_Notify_Order_Tests;
