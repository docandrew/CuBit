pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with ACPI_Requests; use ACPI_Requests;
with ACPI_Endpoint;
with ACPI_Notification_Consumer;
with ACPI_Service;
with Firmware_Tables;
with Migration_Tables;
procedure Notification_Consumer_Tests is
   package Client is new ACPI_Notification_Consumer (Maximum_Path => 8);
   package Tiny is new ACPI_Notification_Consumer (Maximum_Path => 1, Final_Session => 2, Final_Request => 2);
   package Sessions is new ACPI_Notification_Consumer (Maximum_Path => 1, Final_Session => 2, Final_Request => 10);
   package No_Requests is new ACPI_Notification_Consumer (Maximum_Path => 1, Final_Session => 2, Final_Request => 0);
   package Zero is new ACPI_Notification_Consumer (Maximum_Path => 1, Final_Session => 0, Final_Request => 0);
   use type Client.Result;
   use type Tiny.Result;
   use type Zero.Result;
   use type Sessions.Result;
   use type No_Requests.Result;
   Checks : Natural := 0;
   Config : constant ACPI_Endpoint.Configuration := (10,11);
   procedure Check (Good : Boolean) is
   begin Checks:=Checks+1; if not Good then raise Program_Error with Checks'Image; end if; end Check;
   -- Device(DEV0) and _INI Notify(DEV0,0x80), Notify(DEV0,0x10000).
   -- The second value has device class but truncated payload0: class and payload
   -- cannot be validated by reconstructing class from the truncated number.
   Payload : constant Firmware_Tables.Bytes :=
     [16#5B#,16#82#,5,68,69,86,48,
      16#14#,25,95,73,78,73,0,
      16#86#,68,69,86,48,16#0A#,16#80#,
      16#86#,68,69,86,48,16#0C#,0,0,1,0,16#A4#,0];
   Raw : constant Firmware_Tables.Bytes := Migration_Tables.Make(Payload);
   procedure Load (Server : in out State) is
      Reply : Response;
   begin
      Handle(Server,Snapshot_Provider,(Label=>Configure_Notifications,Data=>[0,1,0,0],others=><>),Reply); Check(Reply.Status=OK);
      Handle(Server,Snapshot_Provider,(Label=>Start_Snapshot,Data=>[Revision(Server),1,0,0],others=><>),Reply); Check(Reply.Status=OK);
      Import_Block(Server,Snapshot_Provider,Revision(Server),41,ACPI_Service.DSDT,Raw,Reply); Check(Reply.Status=OK);
      Handle(Server,Snapshot_Provider,(Label=>Finish_Snapshot,Data=>[Revision(Server),0,0,0],others=><>),Reply); Check(Reply.Status=OK);
   end Load;
   Server : State(1,2048,2048,0);
   C : Client.State;
   Outcome : Client.Result;
   Request,Reply : Packet;
   Token,Old_Token : Client.Token;
   Item : Client.Observation;
   Old_Session : Unsigned_64;
   procedure Exchange is
   begin
      Client.Next_Request(C,Request,Token,Outcome); Check(Outcome=Client.Ready);
      ACPI_Endpoint.Dispatch(Server,Config,11,Request,Reply);
      Client.Accept_Reply(C,Token,Reply,Outcome); Check(Outcome=Client.Applied);
   end Exchange;
   procedure Finish_Read is
   begin
      for I in 1 .. 3 loop Exchange; end loop; -- header, identity, path
      Check(Client.Has_Observation(C));
   end Finish_Read;
   procedure Pending (ID,Count : Unsigned_64) is
      R : Response;
   begin
      Handle(Server,Observer,(Label=>Read_Notification_State,others=><>),R);
      Check(R.Status=OK and then R.Data(2)=Count and then R.Data(3)=ID);
   end Pending;
begin
   Load(Server);
   Client.Start_Session(C,Outcome); Check(Outcome=Client.Applied);
   Exchange; -- synchronize
   Client.Next_Request(C,Request,Token,Outcome); Check(Outcome=Client.Ready and then Request.Label=Claim_Notification);
   ACPI_Endpoint.Dispatch(Server,Config,11,Request,Reply); Pending(1,1);
   Old_Token:=Token; -- lost claim reply
   Client.Transport_Unknown(C,Token,Outcome); Check(Outcome=Client.Applied);
   Client.Accept_Reply(C,Old_Token,Reply,Outcome); Check(Outcome=Client.Ignored);
   Exchange; Exchange; Finish_Read;
   Item:=Client.Copy_Observation(C);
   Check(Item.ID=1 and then Item.Length=1 and then Item.Path(1)=16#30564544# and then
     Item.Number=128 and then Item.Class=Notify_Device_Class and then Item.Kind=Notify_Device and then
     Item.Node>0 and then Item.Incarnation>0 and then Item.Current_At_Claim);
   Client.Next_Request(C,Request,Token,Outcome); Check(Outcome=Client.Idle); Pending(1,1);
   Client.Confirm_Delivery(C,Client.Session(C),0,Outcome); Check(Outcome=Client.Invalid_Delivery); Pending(1,1);
   Client.Confirm_Delivery(C,Client.Session(C),1,Outcome); Check(Outcome=Client.Applied);
   Client.Next_Request(C,Request,Token,Outcome); Check(Outcome=Client.Ready and then Request.Label=Acknowledge_Notification);
   ACPI_Endpoint.Dispatch(Server,Config,11,Request,Reply); Pending(0,1);
   Client.Transport_Unknown(C,Token,Outcome); Check(Outcome=Client.Applied);
   -- Another broker action claims the newer receipt before the lost ACK retry.
   declare R : Response; begin
      Handle(Server,Snapshot_Provider,(Label=>Claim_Notification,Data=>[Revision(Server),0,0,0],others=><>),R);
      Check(R.Status=OK and then R.Data(1)=2);
   end;
   Exchange; Pending(2,0); -- retry exact ACK1 cannot remove receipt2
   Exchange; Exchange; Finish_Read;
   Item:=Client.Copy_Observation(C); Check(Item.ID=2 and then Item.Number=0 and then Item.Class=Notify_Device_Class);
   Old_Session:=Client.Session(C);
   Client.Confirm_Delivery(C,Old_Session,1,Outcome); Check(Outcome=Client.Invalid_Delivery); Pending(2,0);
   Client.Confirm_Delivery(C,Old_Session,2,Outcome); Check(Outcome=Client.Applied);
   Client.Next_Request(C,Request,Old_Token,Outcome); Check(Outcome=Client.Ready);
   -- Reconnect invalidates an unsent old ACK and every old completion.
   Client.Start_Session(C,Outcome); Check(Outcome=Client.Applied and then Client.Session(C)>Old_Session);
   Client.Confirm_Delivery(C,Old_Session,2,Outcome); Check(Outcome=Client.Invalid_Delivery);
   Client.Accept_Reply(C,Old_Token,Reply,Outcome); Check(Outcome=Client.Ignored); Pending(2,0);
   Exchange; Exchange; Finish_Read;
   Client.Confirm_Delivery(C,Client.Session(C),2,Outcome); Check(Outcome=Client.Applied); Exchange; Pending(0,0);
   Exchange;
   Client.Next_Request(C,Request,Token,Outcome); Check(Outcome=Client.Ready);
   ACPI_Endpoint.Dispatch(Server,Config,11,Request,Reply); Client.Accept_Reply(C,Token,Reply,Outcome); Check(Outcome=Client.Empty);
   -- Exact envelope/error shape validation; no malformed response authorizes ACK.
   for Bad in 1 .. 6 loop
      Client.Start_Session(C,Outcome); Check(Outcome=Client.Applied);
      Client.Next_Request(C,Request,Token,Outcome); Check(Outcome=Client.Ready);
      ACPI_Endpoint.Dispatch(Server,Config,11,Request,Reply);
      case Bad is
         when 1 => Reply.Length:=3;
         when 2 => Reply.Flags:=1;
         when 3 => Reply.Reserved:=1;
         when 4 => Reply.Label:=0;
         when 5 => Reply.Data(0):=Unsigned_64'Last;
         when others => Reply.Label:=ACPI_Endpoint.Reply_Error; Reply.Data:=[99,0,0,0];
      end case;
      Client.Accept_Reply(C,Token,Reply,Outcome); Check(Outcome=Client.Invalid_Reply);
      Client.Next_Request(C,Request,Token,Outcome); Check(Outcome=Client.Idle); Pending(0,0);
   end loop;
   -- A real denied response is classified as remote error, not valid event data.
   Client.Start_Session(C,Outcome); Client.Next_Request(C,Request,Token,Outcome);
   ACPI_Endpoint.Dispatch(Server,Config,12,Request,Reply);
   Client.Accept_Reply(C,Token,Reply,Outcome); Check(Outcome=Client.Remote_Error and then Client.Last_Remote_Error(C)=Denied);
   -- Synthetic authenticated transport completions: exercise a valid zero-path
   -- wire shape without claiming that the real Device fixture has an empty path.
   -- Each completion uses the original token of its exact pending request.
   declare
      Consumer : Client.State;
      R : Client.Result;
      P,Q : Packet;
      K : Client.Token;
      O : Client.Observation;
      procedure Complete (Label : Unsigned_32; Data : Words) is
      begin
         Client.Next_Request(Consumer,P,K,R);
         Check(R=Client.Ready and then P.Label=Label);
         Q:=(Label=>ACPI_Endpoint.Reply_OK,Data=>Data,others=><>);
         Client.Accept_Reply(Consumer,K,Q,R); Check(R=Client.Applied);
      end Complete;
   begin
      Client.Start_Session(Consumer,R); Check(R=Client.Applied);
      Complete(Read_Notification_State,[1,3,0,1]);
      Complete(Claim_Notification,[1,1,0,0]);
      Complete(Read_Notification,[1,0,0,0]); -- kind/class/number
      Complete(Read_Notification,[1,0,1,1]); -- root node, incarnation, current
      Check(Client.Has_Observation(Consumer));
      O:=Client.Copy_Observation(Consumer);
      Check(O.ID=1 and then O.Length=0 and then O.Node=0 and then O.Incarnation=1);
      -- No Path request and no ACK before explicit delivery confirmation.
      Client.Next_Request(Consumer,P,K,R); Check(R=Client.Idle);
      Client.Confirm_Delivery(Consumer,Client.Session(Consumer),1,R);
      Check(R=Client.Applied);
      Client.Next_Request(Consumer,P,K,R);
      Check(R=Client.Ready and then P.Label=Acknowledge_Notification and then P.Data(1)=1);
      Q:=(Label=>ACPI_Endpoint.Reply_OK,Data=>[2,1,0,0],others=><>);
      Client.Accept_Reply(Consumer,K,Q,R); Check(R=Client.Applied);
      Check(not Client.Has_Observation(Consumer));
   end;
   -- Retain the same real server receipt across malformed page decodes. No
   -- malformed kind, identity, path extent or NameSeg may trigger automatic ACK.
   declare
      Other : State(1,2048,2048,0);
      Consumer : Client.State;
      R : Client.Result;
      P,Q : Packet;
      K : Client.Token;
      Status_Reply : Response;
      procedure Step is
      begin
         Client.Next_Request(Consumer,P,K,R); Check(R=Client.Ready);
         ACPI_Endpoint.Dispatch(Other,Config,11,P,Q);
         Client.Accept_Reply(Consumer,K,Q,R); Check(R=Client.Applied);
      end Step;
   begin
      Load(Other);
      for Bad in 1 .. 8 loop
         Client.Start_Session(Consumer,R); Check(R=Client.Applied);
         Step; -- synchronize
         if Bad>1 then Step; end if; -- claim
         if Bad in 3 .. 6 then Step; end if; -- header
         if Bad in 4 .. 6 then Step; end if; -- identity
         Client.Next_Request(Consumer,P,K,R); Check(R=Client.Ready);
         ACPI_Endpoint.Dispatch(Other,Config,11,P,Q);
         case Bad is
            when 1 => Q.Data(3):=9; -- claim path exceeds configured8
            when 2 => Q.Data(1):=3; -- unknown target kind
            when 3 => Q.Data(3):=2; -- invalid current-target Boolean
            when 4 => Q.Data(2):=2; -- inconsistent path length
            when 5 => Q.Data(1):=16#30564501#; -- malformed NameSeg byte
            when 6 => Q.Data(0):=0; -- backward authenticated protocol revision
            when 7 => Q.Data(2):=Notification_Class_Code'Pos(Notification_Class_Code'Last)+1;
            when others => Q.Data(3):=Unsigned_64(Unsigned_16'Last)+1;
         end case;
         Client.Accept_Reply(Consumer,K,Q,R); Check(R=Client.Invalid_Reply);
         Client.Next_Request(Consumer,P,K,R); Check(R=Client.Idle);
         Handle(Other,Observer,(Label=>Read_Notification_State,others=><>),Status_Reply);
         Check(Status_Reply.Status=OK and then Status_Reply.Data(3)=1 and then Status_Reply.Data(2)=1);
      end loop;
      -- Stale claim revision: owner configuration changes revision between read
      -- and claim. Client resynchronizes; the staged receipt remains identical.
      Client.Start_Session(Consumer,R); Step;
      Handle(Other,Snapshot_Provider,(Label=>Configure_Notifications,Data=>[Revision(Other),0,0,0],others=><>),Status_Reply);
      Check(Status_Reply.Status=OK);
      Client.Next_Request(Consumer,P,K,R); ACPI_Endpoint.Dispatch(Other,Config,11,P,Q);
      Client.Accept_Reply(Consumer,K,Q,R); Check(R=Client.Remote_Error and then Client.Last_Remote_Error(Consumer)=Stale);
      Step; Step; Step; Step; Step; Check(Client.Has_Observation(Consumer));
      -- Observer can inspect but cannot claim, regardless of packet words.
      Client.Start_Session(Consumer,R); Step;
      Client.Next_Request(Consumer,P,K,R); ACPI_Endpoint.Dispatch(Other,Config,10,P,Q);
      Client.Accept_Reply(Consumer,K,Q,R); Check(R=Client.Remote_Error and then Client.Last_Remote_Error(Consumer)=Denied);
   end;
   declare
      Not_Started : State(1,2048,2048,0);
      Consumer : Client.State;
      R : Client.Result;
      P,Q : Packet;
      K : Client.Token;
   begin
      Client.Start_Session(Consumer,R);
      Client.Next_Request(Consumer,P,K,R); ACPI_Endpoint.Dispatch(Not_Started,Config,11,P,Q); Client.Accept_Reply(Consumer,K,Q,R);
      Check(R=Client.Applied);
      Client.Next_Request(Consumer,P,K,R); ACPI_Endpoint.Dispatch(Not_Started,Config,11,P,Q); Client.Accept_Reply(Consumer,K,Q,R);
      Check(R=Client.Remote_Error and then Client.Last_Remote_Error(Consumer)=Wrong_Order);
   end;
   declare
      T : Tiny.State; K,Old : Tiny.Token; R : Tiny.Result; P : Packet;
      Z : Zero.State; ZR : Zero.Result;
   begin
      Zero.Start_Session(Z,ZR); Check(ZR=Zero.Counter_Exhausted);
      Tiny.Start_Session(T,R); Check(R=Tiny.Applied);
      Tiny.Next_Request(T,P,Old,R); Check(R=Tiny.Ready);
      Tiny.Start_Session(T,R); Check(R=Tiny.Applied and then Tiny.Session(T)=2 and then Tiny.Request_Count(T)=1);
      Tiny.Next_Request(T,P,K,R); Check(R=Tiny.Ready);
      Tiny.Accept_Reply(T,Old,(others=><>),R); Check(R=Tiny.Ignored);
      Tiny.Transport_Unknown(T,K,R); Check(R=Tiny.Applied);
      Tiny.Next_Request(T,P,K,R); Check(R=Tiny.Counter_Exhausted);
      Tiny.Start_Session(T,R); Check(R=Tiny.Counter_Exhausted);
   end;
   declare
      S : Sessions.State; R : Sessions.Result;
      N : No_Requests.State; NR : No_Requests.Result;
      K : No_Requests.Token; P : Packet;
   begin
      Sessions.Start_Session(S,R); Check(R=Sessions.Applied and then Sessions.Session(S)=1);
      Sessions.Start_Session(S,R); Check(R=Sessions.Applied and then Sessions.Session(S)=2);
      Sessions.Start_Session(S,R); Check(R=Sessions.Counter_Exhausted and then Sessions.Session(S)=2);
      No_Requests.Start_Session(N,NR); Check(NR=No_Requests.Applied);
      No_Requests.Next_Request(N,P,K,NR); Check(NR=No_Requests.Counter_Exhausted and then No_Requests.Request_Count(N)=0);
   end;
   Ada.Text_IO.Put_Line("Notification consumer checks" & Checks'Image);
end Notification_Consumer_Tests;
