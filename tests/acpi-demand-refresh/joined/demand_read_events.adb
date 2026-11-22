pragma Ada_2022;
with Interfaces;with AML_Names;
with ACPI_Service;with ACPI_One_Service;with ACPI_Lid;with ACPI_Requests.Lids;
with ACPI_Native_Endpoint;with ACPI_Native_Event_Domain;with ACPI_Native_Battery_Reads;
with ACPI_Protocol;with ACPI_Endpoint;with CuBit.Messages;with Joined_Startup_Boundary;
package body Demand_Read_Events is
 package C renames ACPI_Service;package Q renames ACPI_Requests;package One renames ACPI_One_Service;
 package B renames Joined_Startup_Boundary;package D renames ACPI_Native_Event_Domain;
 package M renames CuBit.Messages;
 Selected:AML_Names.Segment:="GCNT";
 Total:Natural:=0;
 procedure Check(Good:Boolean;Why:String) is
 begin Total:=Total+1;if not Good then raise Program_Error with Why;end if;end;
 procedure Evaluate(Service:aliased in out C.State;Item:C.Node_Descriptor;Budget:Natural;
  Value:out C.Values.Result;Status:out C.Values.Access_Status) is
  Target:C.Node_Descriptor:=Item;Node:C.Namespace.Node_ID;
  use type C.Values.Access_Status;
 begin
  if C.Values.Node_Name(Item)="_LID" then
   C.Find_Child(Service,C.Namespace.Root,Selected,Node,Status);
   if Status/=C.Values.Available then raise Program_Error with "fixed demand producer lookup";end if;
   C.Describe_Node(Service,Node,Target,Status);
   if Status/=C.Values.Available then raise Program_Error with "fixed demand producer binding";end if;
  end if;
  One.Runtime.Evaluate_Sample(Service,Target,Budget,Value,Status);
 end Evaluate;
 package LD is new ACPI_Lid(C,Evaluate);package External is new Q.Lids(LD);
 procedure Run(Server:in out Q.State;Checks:out Natural) is
  use type Interfaces.Unsigned_64,Interfaces.Unsigned_32;
  use type M.MessageWords,One.Runtime.Status.Outcome,ACPI_Native_Server.Owner_Outcome,External.Controller.Sampling_Outcome;
  Request,Reply:M.Message:=M.NULL_MESSAGE;
  G,G2,Calls:Interfaces.Unsigned_64;
  procedure Read(Selector:Interfaces.Unsigned_64:=0;Expected:Interfaces.Unsigned_64:=0;Age:Interfaces.Unsigned_64:=5) is
  begin
   Request:=M.NULL_MESSAGE;Request.tag.length:=4;Request.authorityTag:=Interfaces.Unsigned_64(ACPI_Protocol.Observer_Tag);
   Request.words:=[22,Age,Selector,Expected];B.Current_Read_Reply:=True;
   ACPI_Native_Endpoint.Dispatch(Server,M.Own_Process,Request,Reply);
   if ACPI_Native_Endpoint.Reply_Now then B.Current_Read_Reply:=False;end if;
  end Read;
  function Fixed(Name:AML_Names.Segment) return Interfaces.Unsigned_64 is
   Transition:One.Runtime.Status.Outcome;Report:External.Controller.Sampling_Report;
  begin
   Selected:=Name;One.Runtime_Access.Begin_Event(Server,9_000_000,Transition);
   Check(Transition=One.Runtime.Status.Event_Opened,"fixed model source admitted");
   External.Sample_Ready(Server,100_000,Report);
   Check(Report.Outcome=External.Controller.Sampled,"fixed AML counter/control source");
   One.Runtime_Access.End_Event(Server,True,Transition);
   Check(Transition=One.Runtime.Status.Event_Completed and not Q.Notifications_Pending(Server),"fixed source closes without Notify");
   return Interfaces.Unsigned_64(Report.Query.Raw);
  end Fixed;
  procedure Pump(Until_Reply_Ready:Boolean:=False;Expect_Failure:Boolean:=False) is O:ACPI_Native_Server.Owner_Outcome;
  begin
   for I in 1..128 loop
    Owner_Advance(Server,O);
    if Expect_Failure and O=ACPI_Native_Server.Owner_Refused then return;end if;
    Check(O/=ACPI_Native_Server.Owner_Refused,"original native owner remains healthy");
    if Until_Reply_Ready and ACPI_Native_Battery_Reads.Pending then return;end if;
    if O=ACPI_Native_Server.Owner_Idle and not Until_Reply_Ready then return;end if;
   end loop;
   raise Program_Error with "bounded demand owner pump exhausted";
  end Pump;
 begin
  Total:=0;B.Allow_Read_Save:=True;
  Read;Check(ACPI_Native_Endpoint.Reply_Now and Reply.tag.label=ACPI_Endpoint.Reply_OK and Reply.words(1)=1,"initial battery inventory available");G:=Reply.words(0);
  Check(Fixed("GCNT")=1,"one startup BST sample");Calls:=Fixed("WMOD");Check(Calls=1,"capacity changed without a notification");
  Read(23,G,Interfaces.Unsigned_64'Last);Check(ACPI_Native_Endpoint.Reply_Now and Reply.words(3)=20000,"fresh cache read does not execute changed BST");
  Check(Fixed("GCNT")=1 and B.Read_Saves=0,"fresh requests have no acquisition or saved authority");
  declare
   Foreign:Q.State(1,256,256,1,0,8,128,8);Now:Interfaces.Unsigned_64;Allowed:Boolean;
  begin D.Preview_Battery_Refresh(Foreign,Now,Allowed);Check(not Allowed,"foreign context cannot admit or join demand");end;
  B.Demand_Clock:=(if B.Fresh_Clock_High then Interfaces.Unsigned_64'Last else 2_000_000);
  for I in 1..(if B.Fresh_Expected_Failure then 1 else 8) loop
   Read;Check(not ACPI_Native_Endpoint.Reply_Now and not B.Current_Read_Reply,"expired request moves its original reply");
  end loop;
  Check(B.Read_Answers=0,"no old response before owner collection");
  if B.Fresh_Expected_Failure then
   if B.Fresh_Clock_Lost then B.Demand_Clock_Available:=False;
   elsif B.Fresh_Clock_Back then B.Demand_Clock:=1_000_000;end if;
   Pump(Expect_Failure=>True);Check(B.Read_Answers=0,"failure cannot fake a fresh reply at an unsafe boundary");
   Checks:=Total;return;
  end if;
  Check(Fixed("GCNT")=1,"observer dispatch and coalescing performed no AML acquisition");
  Pump;Check(B.Read_Saves=8 and B.Read_Answers=8 and ACPI_Native_Battery_Reads.Quiet,"actual scheduler drains all coalesced replies before idle");
  Check(B.Last_Read(1)=1 and B.Last_Read(2)=1,"same parked call returns fresh available inventory");G:=B.Last_Read(0);
  Read(23,G,Interfaces.Unsigned_64'Last);Check(Reply.words(3)=18000,"new BST result is in the replied inventory");
  Check(Fixed("GCNT")=2,"eight requests perform exactly one fresh BST");
  B.Demand_Clock:=2_000_001;Read(Age=>0);Check(ACPI_Native_Endpoint.Reply_Now and Reply.words(1)=4 and B.Read_Saves=8,"rate-limited read stays honestly old");
  Pump;Check(Fixed("GCNT")=2,"request storm cannot resample inside cooldown");
  B.Demand_Clock:=3_000_000;Read(Age=>0);Check(not ACPI_Native_Endpoint.Reply_Now,"next interval explicitly requests one sample");
  Pump(Until_Reply_Ready=>True);Check(Fixed("GCNT")=3,"second explicit BST completed");
  B.Demand_Clock:=3_000_001;Pump;Check(B.Read_Answers=9 and B.Last_Read(1)=4 and B.Last_Read(2)=0 and B.Last_Read(3)=0,"age zero never fakes freshness or loops until success");
  Read(Age=>Interfaces.Unsigned_64'Last);G2:=Reply.words(0);Check(G2>G,"completed cache generation moved");
  for I in 1..8 loop Pump;end loop;Check(Fixed("GCNT")=3,"idle owner has no timer or automatic retry");
  -- Leave one full batch pending. The actual outer stop must answer these
  -- without another BST, through the same saved-reply ownership barrier.
  B.Demand_Clock:=4_000_000;
  for I in 1..8 loop Read;Check(not ACPI_Native_Endpoint.Reply_Now,"pending stop batch owns each reply");end loop;
  Check(B.Read_Saves=17 and B.Read_Answers=9,"stop batch pending before actual lifecycle cleanup");
  Checks:=Total;
 end Run;
end Demand_Read_Events;
