pragma Ada_2022;
with Interfaces;with AML_Names;with Joined_Startup_Boundary;with CuBit.Process_IDs;
with ACPI_Service;with ACPI_One_Service;with ACPI_Lid;with ACPI_Requests.Lids;
with ACPI_Native_Event_Domain;with ACPI_Native_Server;with ACPI_Native_Endpoint;
with ACPI_Power_Watch_Protocol;with ACPI_Protocol;with CuBit.Messages;
package body Power_Watch_Events is
 package C renames ACPI_Service;package Q renames ACPI_Requests;package One renames ACPI_One_Service;
 package P renames ACPI_Power_Watch_Protocol;
 Selected:AML_Names.Segment:="WB80";
 Total:Natural:=0;
 procedure Check(Good:Boolean;Why:String) is
 begin Total:=Total+1;if not Good then raise Program_Error with Why;end if;end;
 -- Trusted test source only: the fixed lid-shaped local owner invokes one of
 -- the seven literal fixture producers, never a wire-selected method.
 procedure Evaluate(Service:aliased in out C.State;Item:C.Node_Descriptor;Budget:Natural;
  Value:out C.Values.Result;Status:out C.Values.Access_Status) is
  Target:C.Node_Descriptor:=Item;Node:C.Namespace.Node_ID;
  use type C.Values.Access_Status;
 begin
  if C.Values.Node_Name(Item)="_LID" then
   C.Find_Child(Service,C.Namespace.Root,Selected,Node,Status);
   if Status/=C.Values.Available then raise Program_Error with "fixture producer lookup";end if;
   C.Describe_Node(Service,Node,Target,Status);
   if Status/=C.Values.Available then raise Program_Error with "fixture producer binding";end if;
  end if;
  One.Runtime.Evaluate_Sample(Service,Target,Budget,Value,Status);
 end Evaluate;
 package D is new ACPI_Lid(C,Evaluate);package External is new Q.Lids(D);
 procedure Run(Server:in out Q.State;Checks:out Natural) is
  use type Interfaces.Unsigned_64,Interfaces.Unsigned_32,Interfaces.Unsigned_8;
  use type One.Runtime.Status.Outcome,One.Runtime.Status.Phase;
  use type ACPI_Native_Server.Owner_Outcome,External.Controller.Sampling_Outcome;
  Transition:One.Runtime.Status.Outcome;Report:External.Controller.Sampling_Report;
  Before,After:Interfaces.Unsigned_64;
  function Generation return Interfaces.Unsigned_64 is
   Request,Reply:CuBit.Messages.Message:=CuBit.Messages.NULL_MESSAGE;
  begin
   Request.tag.label:=P.Watch;Request.tag.length:=4;Request.authorityTag:=Interfaces.Unsigned_64(ACPI_Protocol.Observer_Tag);
   Request.words:=[16#FEDC_BA98_7654_3210#,0,0,0];
   ACPI_Native_Endpoint.Dispatch(Server,CuBit.Messages.Own_Process,Request,Reply);
   Check(ACPI_Native_Endpoint.Reply_Now and then Reply.tag.label=P.Watch and then Reply.tag.length=4
    and then Reply.words(0)=Request.words(0) and then Reply.words(1)=P.Status'Enum_Rep((if ACPI_Native_Event_Domain.Diagnostics_Quiet(Server) then P.Changed else P.Unavailable))
    and then Reply.words(3)=P.Schema,"actual endpoint synchronization");
   return Reply.words(2);
  end Generation;
  procedure Pump is Outcome:ACPI_Native_Server.Owner_Outcome;
  begin
   for I in 1..96 loop
    ACPI_Native_Event_Domain.Advance(Server,Outcome);
    Check(Outcome/=ACPI_Native_Server.Owner_Refused,"watch fixture preserves actual owner");
    if Outcome=ACPI_Native_Server.Owner_Idle then
     Check(not Q.Notifications_Pending(Server) and then not Q.Retained_Results_Pending(Server)
      and then One.Runtime.Current=One.Runtime.Status.Idle,"all-consumer receipt and result drain");return;
    end if;
   end loop;
   raise Program_Error with "bounded fixture pump exhausted";
  end Pump;
  type Names is array(Positive range <>) of AML_Names.Segment;
  Producers:constant Names:=["WB80","WB81","WA80","WA81","NU80","BADV","CHPS"];
 begin
  Total:=0;Before:=Generation;Check(Before>1,"initial inventory publications advanced generation");
  for I in Producers'Range loop
   Selected:=Producers(I);Before:=Generation;
   One.Runtime_Access.Begin_Event(Server,500_000,Transition);
   Check(Transition=One.Runtime.Status.Event_Opened,"fixture source admitted under same runtime");
   External.Sample_Ready(Server,100_000,Report);
   Check(Report.Outcome=External.Controller.Sampled,"real AML fixture produced typed value and notification");
   One.Runtime_Access.End_Event(Server,True,Transition);
   Check(Transition=One.Runtime.Status.Event_Completed,"fixture source closed provider window");
   Check(Generation=Before,"callback did not route or reply before broker turn");
   Pump;After:=Generation;
   if I<=4 then Check(After>=Before+2,"battery/AC notice plus completed publication wake");
   elsif I<=6 then Check(After=Before,"unrelated target or wrong number is ignored");
   else Check(After>Before,"observed PSR change wakes without matching power notice");end if;
  end loop;
  declare G:constant Interfaces.Unsigned_64:=Generation;
   Request,Reply:CuBit.Messages.Message:=CuBit.Messages.NULL_MESSAGE;
   package B renames Joined_Startup_Boundary;
  begin
   B.Allow_Watch_Save:=True;
   for I in 1..P.Maximum_Watches loop
    Request.tag.label:=P.Watch;Request.tag.length:=4;Request.authorityTag:=Interfaces.Unsigned_64(ACPI_Protocol.Observer_Tag);
    Request.words:=[Interfaces.Unsigned_64(I),G,0,0];B.Current_Watch_Reply:=True;
    ACPI_Native_Endpoint.Dispatch(Server,CuBit.Process_IDs.From_Word(16#FEDC_BA98_0000_0000#+Interfaces.Unsigned_64(I)),Request,Reply);
    Check(not ACPI_Native_Endpoint.Reply_Now and not B.Current_Watch_Reply,"actual dispatcher saves independent reply authority");
   end loop;
   Check(B.Saved_Watches=P.Maximum_Watches and B.Answered_Watches=0,"all native watches retained until stop");
  end;
  Checks:=Total;
 end Run;
end Power_Watch_Events;
