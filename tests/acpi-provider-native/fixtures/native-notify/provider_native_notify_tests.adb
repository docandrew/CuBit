pragma Ada_2022;
with Ada.Text_IO;
with Ada.Command_Line;
with Interfaces; use Interfaces;
with ACPI_Requests; use ACPI_Requests;
with ACPI_Endpoint;
with ACPI_Service;
with ACPI_Provider_Transport;
with ACPI_Snapshot_Provider;
with CuBit.Handles;
with CuBit.Messages;
with Firmware_Tables;
with Migration_Tables;
with Bridge_Trace; use Bridge_Trace;
with Server_Model; use Server_Model;
procedure Provider_Native_Notify_Tests is
   package T renames ACPI_Provider_Transport;
   package N renames T.Notification_Policy;
   use type T.Interest_Result;
   use type N.Result;
   use type ACPI_Snapshot_Provider.Progress;
   use type CuBit.Messages.MessageWords;
   package Driver is new ACPI_Snapshot_Provider.Driver
     (1,T.Exchange,T.Import_Table,T.Release_Provider_Grant);
   type Test_Case is (Success, Lost_Interest, Bad_Interest, Denied_Interest,
      Lost_Query, Deadline_Interest, Lost_Finish, Bad_Finish, Lost_Claim, Lost_Ack,
      Bad_Receipt, Deadline_Receipt, Reconnect);
   Case_ID : Test_Case;
   Checks : Natural := 0;
   procedure Check(Good : Boolean) is
   begin Checks:=Checks+1; if not Good then raise Program_Error with Checks'Image; end if; end Check;
   -- Exact constructed payload from landed consumer/order fixture.
   Payload : constant Firmware_Tables.Bytes :=
     [16#5B#,16#82#,5,68,69,86,48,
      16#14#,25,95,73,78,73,0,
      16#86#,68,69,86,48,16#0A#,16#80#,
      16#86#,68,69,86,48,16#0C#,0,0,1,0,16#A4#,0];
   Raw : constant Firmware_Tables.Bytes := Migration_Tables.Make(Payload);
   Items : constant ACPI_Snapshot_Provider.Catalog(5..5) := [5=>(41,ACPI_Service.DSDT,Raw'Length)];
   Slices : constant T.Table_Slices(5..5) := [5=>(Items(5),0)];
   S : Driver.State;
   R : T.Interest_Result;
   Result : N.Result;
   Item : N.Observation;
   Reply : Packet;
   Accepted,Known : Boolean;
   Before,Before_Ack : Natural;
   Old_Session : N.Word;
   Saved_Ack : CuBit.Messages.MessageWords;
   procedure Pending(ID,Count : Unsigned_64) is
   begin
      Route((Label=>Read_Notification_State,others=><>),Reply);
      Check(Reply.Label=ACPI_Endpoint.Reply_OK and then Reply.Data(2)=Count and then Reply.Data(3)=ID);
   end Pending;
   procedure No_Upload is
      Loan : ACPI_Snapshot_Provider.Cleanup_State;
   begin
      Before:=Calls;
      T.Exchange((Label=>Start_Snapshot,Data=>[Revision(Server),1,0,0],others=><>),Reply,Known); Check(not Known);
      T.Import_Table(1,Revision(Server),Items(5),Reply,Known,Loan); Check(not Known);
      T.Exchange((Label=>Finish_Snapshot,Data=>[Revision(Server),0,0,0],others=><>),Reply,Known); Check(not Known);
      Check(Calls=Before);
   end No_Upload;
   procedure Read_Item is
   begin
      for Step in 1..9 loop
         exit when T.Has_Notification;
         T.Advance_Notifications(Result);
         Check(Result=N.Applied);
      end loop;
      Check(T.Has_Notification);
      Item:=T.Copy_Notification;
   end Read_Item;
begin
   Check(Ada.Command_Line.Argument_Count=1);
   Case_ID:=Test_Case'Value(Ada.Command_Line.Argument(1));
   Allow_Return:=True;
   T.Initialize(CuBit.Handles.Test_Endpoint,Slices,Raw,(others=><>),Accepted,T.Required); Check(Accepted);
   No_Upload; -- Required gate refuses all three mutations BEFORE arming.
   T.Start_Notifications(Accepted); Check(not Accepted);
   case Case_ID is
      when Lost_Interest => Fault:=Lose_Interest;
      when Bad_Interest => Fault:=Malform_Interest;
      when Denied_Interest => Fault:=Deny_Interest;
      when Lost_Query => Fault:=Lose_Query;
      when Deadline_Interest => Now:=Unsigned_64'Last-1;
      when others => null;
   end case;
   T.Configure_Interest(R);
   if Case_ID in Lost_Interest | Bad_Interest | Denied_Interest | Lost_Query | Deadline_Interest then
      Check(R=(case Case_ID is
        when Lost_Interest | Lost_Query => T.Interest_Unknown,
        when Bad_Interest => T.Interest_Invalid_Response,
        when Denied_Interest => T.Interest_Remote_Rejected,
        when others => T.Interest_Deadline_Unavailable));
      if Case_ID=Denied_Interest then Check(T.Last_Interest_Error=Denied); end if;
      Now:=100; Fault:=No_Notify_Fault;
      Before:=Calls; T.Configure_Interest(R); Check(R=T.Interest_Not_Eligible and then Calls=Before);
      No_Upload;
      T.Start_Notifications(Accepted); Check(not Accepted);
      Check(Driver.Status(S)=ACPI_Snapshot_Provider.Not_Started and then Start_Calls=0 and then Creates=0);
      Check(Interest_Calls=(if Case_ID in Lost_Query | Deadline_Interest then 0 else 1));
   else
      Check(R=T.Interest_Armed and then Revision(Server)=1 and then Interest_Calls=1);
      Before:=Calls; T.Configure_Interest(R); Check(R=T.Interest_Not_Eligible and then Calls=Before);
      if Case_ID=Lost_Finish then Mode:=Bridge_Trace.Lost_Finish; end if;
      if Case_ID=Bad_Finish then Fault:=Malform_Finish; end if;
      Driver.Begin_Catalog(S,Items);
      for Step in 1..5 loop Driver.Advance(S); end loop;
      Check(Start_Calls=1 and then Import_Calls=1 and then Finish_Calls=1);
      Check(not Active and then not T.Grant_Pending);
      if Case_ID in Lost_Finish | Bad_Finish then
         Check(Driver.Status(S)=(if Case_ID=Lost_Finish then ACPI_Snapshot_Provider.Unknown_Outcome
           else ACPI_Snapshot_Provider.Invalid_Response) and then Current(Server)=Complete);
         T.Start_Notifications(Accepted); Check(not Accepted);
         Before:=Calls; T.Advance_Notifications(Result); Check(Result=N.Idle and then Calls=Before);
         No_Upload;
      else
         Check(Driver.Status(S)=ACPI_Snapshot_Provider.Completed);
         Pending(0,2);
         T.Start_Notifications(Accepted); Check(Accepted);
         if Case_ID=Lost_Claim then Fault:=Lose_Claim; end if;
         if Case_ID=Bad_Receipt then
            T.Advance_Notifications(Result); Check(Result=N.Applied);
            T.Advance_Notifications(Result); Check(Result=N.Applied);
            Fault:=Malform_Receipt; T.Advance_Notifications(Result); Check(Result=N.Invalid_Reply);
            Before:=Calls; T.Advance_Notifications(Result); Check(Result=N.Idle and then Calls=Before);
            Pending(1,1);
         else
            if Case_ID=Deadline_Receipt then
               Now:=Unsigned_64'Last-1; Before:=Calls;
               T.Advance_Notifications(Result); Check(Result=N.Applied and then Calls=Before);
               Now:=100;
            end if;
            Read_Item;
            Check(Item.ID=1 and then Item.Length=1 and then Item.Path(1)=16#30564544# and then
              Item.Number=128 and then Item.Class=Notify_Device_Class and then Item.Kind=Notify_Device
              and then Item.Node>0 and then Item.Incarnation>0 and then Item.Current_At_Claim);
            if Case_ID=Lost_Claim then Check(Claim_Calls=2); end if;
            Before:=Calls; T.Advance_Notifications(Result); Check(Result=N.Idle and then Calls=Before);
            T.Confirm_Notification_Delivery(T.Notification_Session,0,Result); Check(Result=N.Invalid_Delivery);
            if Case_ID=Reconnect then
               Old_Session:=T.Notification_Session;
               T.Start_Notifications(Accepted); Check(Accepted and then T.Notification_Session>Old_Session);
               T.Confirm_Notification_Delivery(Old_Session,1,Result); Check(Result=N.Invalid_Delivery);
               Read_Item; Check(Item.ID=1);
            end if;
            T.Confirm_Notification_Delivery(T.Notification_Session,1,Result); Check(Result=N.Applied);
            if Case_ID=Lost_Ack then Fault:=Lose_Ack; end if;
            Before_Ack:=Ack_Calls; T.Advance_Notifications(Result); Check(Result=N.Applied);
            Check(Ack_Calls=Before_Ack+1 and then Last_Ack(1)=1);
            if Case_ID=Lost_Ack then
               Saved_Ack:=Last_Ack;
               -- Independent owner action stages next receipt; old ACK cannot clear it.
               Route((Label=>Claim_Notification,Data=>[Revision(Server),0,0,0],others=><>),Reply);
               Pending(2,0);
               T.Advance_Notifications(Result); Check(Result=N.Applied and then Last_Ack=Saved_Ack);
               Pending(2,0);
            else Pending(0,1);
            end if;
            Read_Item; Check(Item.ID=2 and then Item.Number=0 and then Item.Class=Notify_Device_Class);
            T.Confirm_Notification_Delivery(T.Notification_Session,2,Result); Check(Result=N.Applied);
            T.Advance_Notifications(Result); Check(Result=N.Applied); Pending(0,0);
         end if;
         No_Upload; -- Receipt service never reopens completed upload.
      end if;
   end if;
   Ada.Text_IO.Put_Line(Case_ID'Image & " native notify checks" & Checks'Image);
end Provider_Native_Notify_Tests;
