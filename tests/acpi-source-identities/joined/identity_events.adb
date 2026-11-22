pragma Ada_2022;
with Interfaces;with AML_Names;
with ACPI_Service;with ACPI_One_Service;with ACPI_Lid;with ACPI_Requests.Lids;
with Owner_Test_Control;with ACPI_Native_Endpoint;with ACPI_Native_Event_Domain;with ACPI_Native_Battery_Reads;
with ACPI_Protocol;with ACPI_Endpoint;with CuBit.Messages;with Joined_Startup_Boundary;
package body Identity_Events is
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
  use type One.Runtime.Status.Outcome,ACPI_Native_Server.Owner_Outcome,External.Controller.Sampling_Outcome;
  Request,Reply:M.Message:=M.NULL_MESSAGE;
  Battery_Generation,AC_Generation,Calls,Ignored:Interfaces.Unsigned_64;
  procedure Read(Page,Index,Offset,Expected:Interfaces.Unsigned_64) is
  begin
   Request:=M.NULL_MESSAGE;Request.tag.length:=4;Request.authorityTag:=Interfaces.Unsigned_64(ACPI_Protocol.Observer_Tag);
   Request.words:=[Page,Index,Offset,Expected];
   ACPI_Native_Endpoint.Dispatch(Server,M.Own_Process,Request,Reply);
   Check(ACPI_Native_Endpoint.Reply_Now and Reply.tag.label=ACPI_Endpoint.Reply_OK,"identity is an immediate observer response");
  end Read;
  function Fixed(Name:AML_Names.Segment) return Interfaces.Unsigned_64 is
   Transition:One.Runtime.Status.Outcome;Report:External.Controller.Sampling_Report;
  begin
   Selected:=Name;One.Runtime_Access.Begin_Event(Server,9_000_000,Transition);
   Check(Transition=One.Runtime.Status.Event_Opened,"fixed model source admitted");
   External.Sample_Ready(Server,100_000,Report);
   Check(Report.Outcome=External.Controller.Sampled,"fixed AML counter/control source");
   One.Runtime_Access.End_Event(Server,True,Transition);
   Check(Transition=One.Runtime.Status.Event_Completed,"fixed source closes");
   return Interfaces.Unsigned_64(Report.Query.Raw);
  end Fixed;
  procedure Pump is O:ACPI_Native_Server.Owner_Outcome;
  begin
   for I in 1..128 loop
    Owner_Advance(Server,O);Check(O/=ACPI_Native_Server.Owner_Refused,"original native owner remains healthy");
    if O=ACPI_Native_Server.Owner_Idle then return;end if;
   end loop;
   raise Program_Error with "bounded identity owner pump exhausted";
  end Pump;
  procedure Path(Index,Generation:Interfaces.Unsigned_64;Text:String) is
   Before:constant Natural:=Owner_Test_Control.Clock_Reads;
   Expected:Interfaces.Unsigned_64;
  begin
   for Offset in 0..Text'Length loop
    Expected:=0;
    for Byte in 0..6 loop
     if Offset+Byte<Text'Length then Expected:=Expected+Interfaces.Unsigned_64(Character'Pos(Text(Text'First+Offset+Byte)))*2**(8*Byte);end if;
    end loop;
    Read(23,Index,Interfaces.Unsigned_64(Offset),Generation);
    Check(Reply.words(0)=Generation and Reply.words(1)=0 and Reply.words(2)=Interfaces.Unsigned_64(Text'Length) and Reply.words(3)=Expected,"canonical path index"&Index'Image&" offset"&Offset'Image&" reply"&Reply.words(0)'Image&Reply.words(1)'Image&Reply.words(2)'Image&Reply.words(3)'Image&" expected"&Generation'Image&Text'Length'Image&Expected'Image);
   end loop;
   Check(Owner_Test_Control.Clock_Reads=Before,"identity chunks execute no clock callback");
  end Path;
 begin
  Total:=0;
  Read(22,Interfaces.Unsigned_64'Last,0,0);Battery_Generation:=Reply.words(0);
  Check(Reply.words(1)=1 and Reply.words(2)=3,"three real inventory devices including overlong path");
  Read(21,Interfaces.Unsigned_64'Last,0,0);AC_Generation:=Reply.words(0);
  Check(Reply.words(3)=1,"first real AC source available");
  Calls:=Fixed("GCNT");Check(Calls=3,"one startup BST per battery");
  Path(0,AC_Generation,"\SBUS.POWR.AC00");Path(1,Battery_Generation,"\SBUS.POWR.BAT0");Path(2,Battery_Generation,"\SBUS.POWR.BAT1");
  Read(23,3,0,Battery_Generation);Check(Reply.words(1)=4 and Reply.words(2)=0 and Reply.words(3)=0,"overlong actual Core path returns no prefix");
  Read(23,4,0,Battery_Generation);Check(Reply.words(1)=3 and Reply.words(2)=0 and Reply.words(3)=0,"unused battery slot refused");
  B.Demand_Clock_Available:=False;
  Path(1,Battery_Generation,"\SBUS.POWR.BAT0");
  B.Demand_Clock_Available:=True;B.Demand_Clock:=2_000_000;
  Path(2,Battery_Generation,"\SBUS.POWR.BAT1");
  Check(Fixed("GCNT")=Calls,"clock loss and stale age identity reads execute no BST");
  Ignored:=Fixed("DROP");Check(Ignored=1,"first battery removed from matching HID inventory");Pump;
  Read(23,2,0,Battery_Generation);Check(Reply.words(0)>Battery_Generation and Reply.words(1)=2 and Reply.words(2)=0 and Reply.words(3)=0,"old generation cannot mix reordered path chunks");
  Battery_Generation:=Reply.words(0);
  Read(22,Interfaces.Unsigned_64'Last,0,0);Check(Reply.words(0)=Battery_Generation and Reply.words(1)=1 and Reply.words(2)=2,"actual refreshed inventory compacted");
  Path(1,Battery_Generation,"\SBUS.POWR.BAT1");
  Read(23,2,0,Battery_Generation);Check(Reply.words(1)=4 and Reply.words(2)=0 and Reply.words(3)=0,"long source moved slot without returning a prefix");
  Read(23,1,0,Battery_Generation+1);Check(Reply.words(1)=2,"future family generation refused");
  Read(23,0,0,AC_Generation);Check(Reply.words(1)=2,"Notify also republished the AC family; old generation is stale");
  Read(21,Interfaces.Unsigned_64'Last,0,0);AC_Generation:=Reply.words(0);
  Path(0,AC_Generation,"\SBUS.POWR.AC00");
  Checks:=Total;
 end Run;
end Identity_Events;
