pragma Ada_2022;
with Ada.Command_Line; with Ada.Text_IO; with Ada.Streams; with Ada.Streams.Stream_IO;
with Interfaces; with AML_Objects; with AML_Delays; with AML_Execute;
with ACPI_Service_Core; with Firmware_Tables; with ACPI_Delay_Domains; with ACPI_Local_Runtime;
with ACPI_Lid; with ACPI_Power_Source; with ACPI_Battery; with ACPI_HID_I2C_Discovery;
procedure Runtime_Tests is
   package C is new ACPI_Service_Core
     (AML_Delays.Unavailable_Provider,Namespace_Node_Capacity => 128,Retained_Result_Capacity => 16);
   package D renames ACPI_Delay_Domains; package IO renames Ada.Streams.Stream_IO;
   use type C.Values.Access_Status, C.Stage_Status, C.Freeze_Status;
   use type C.Values.Module_Outcome, C.Namespace.Lookup_Status, AML_Execute.Execution_Status;
   use type Interfaces.Unsigned_64, Ada.Streams.Stream_Element_Offset;
   Mode : constant String := Ada.Command_Line.Argument (1);
   Service, Foreign : aliased C.State (1,4096,4096,1,
     Value_Object_Capacity => AML_Objects.Max_Objects,Value_Byte_Capacity => AML_Objects.Max_Bytes,
     Value_Element_Capacity => AML_Objects.Max_Elements);
   Anchor, Item : C.Node_Descriptor;
   Domain : D.State;
   Claimed, Closed, Idle, Startup_Ready : Boolean := False;
   Calls, Claim_Calls, Close_Calls, Enter_Calls, Begin_Calls, End_Calls, Stop_Calls, Checks : Natural := 0;
   procedure Check (Good : Boolean) is
   begin Checks := Checks+1; if not Good then raise Program_Error with Mode & Checks'Image; end if; end Check;
   function Ready (S : C.State) return Boolean is
     (Startup_Ready and then not Closed and then C.Current_Binding (S,Anchor));
   function Owner_Idle return Boolean is (Idle);
   procedure Claim (S : C.State; Accepted : out Boolean);
   procedure Close_Owner;
   procedure Enter_Delays (Accepted : out Boolean);
   procedure Begin_Delays (Deadline : Interfaces.Unsigned_64; Accepted : out Boolean);
   procedure End_Delays (Accepted : out Boolean);
   procedure Stop_Delays;
   procedure Evaluate (S : aliased in out C.State; Node : C.Node_Descriptor; Budget : Natural;
     Result : out C.Values.Result; Status : out C.Values.Access_Status);
   procedure Invoke (S : aliased in out C.State; Node : C.Namespace.Node_ID;
     Args : AML_Execute.Arguments; Count : C.Values.Argument_Count; Budget : Natural;
     Result : out C.Values.Result; Status : out C.Values.Access_Status);
   procedure Invoke_Values (S : aliased in out C.State; Node : C.Namespace.Node_ID;
     Args : C.Values.Arguments; Count : C.Values.Argument_Count; Budget : Natural;
     Result : out C.Values.Result; Status : out C.Values.Access_Status);
   package R is new ACPI_Local_Runtime (C,Ready,Owner_Idle,Claim,Close_Owner,Enter_Delays,Begin_Delays,End_Delays,Stop_Delays,
     Evaluate,Invoke,Invoke_Values);
   package Other is new ACPI_Local_Runtime (C,Ready,Owner_Idle,Claim,Close_Owner,Enter_Delays,Begin_Delays,End_Delays,Stop_Delays,
     Evaluate,Invoke,Invoke_Values);
   use type R.Phase, R.Outcome;
   package L is new ACPI_Lid (C,R.Evaluate_Named);
   package P is new ACPI_Power_Source (C,R.Evaluate_Named);
   package B is new ACPI_Battery (C,R.Evaluate_Named);
   package H is new ACPI_HID_I2C_Discovery (C,R.Invoke_With_Arguments,R.Evaluate_Named);
   use type L.Query_Outcome, L.Lid_State, L.Identification_Outcome;
   use type P.Query_Outcome, P.Source_State, P.Identification_Outcome;
   use type B.Query_Outcome, B.Identification_Outcome, B.Presence_Outcome;
   use type H.Action_Result, H.Outcome, H.Queries.Descriptor_Register;
   use type Interfaces.Unsigned_32;
   Clock : Interfaces.Unsigned_64 := 11;
   procedure Claim (S : C.State; Accepted : out Boolean) is
      Ignored : R.Outcome;
   begin
      Claim_Calls := Claim_Calls+1; Accepted := Ready (S) and not Claimed and Mode /= "claim-refused";
      if Accepted then Claimed := True; end if;
      if Mode = "stop-during-claim" then R.Request_Stop (Ignored); Check (Ignored = R.Status.Stop_Pending); end if;
   end Claim;
   procedure Close_Owner is begin Close_Calls := Close_Calls+1; Closed := True; end Close_Owner;
   procedure Enter_Delays (Accepted : out Boolean) is
   begin Enter_Calls := Enter_Calls+1; if Mode = "enter-refused" then Accepted := False;
      else D.Enter_Runtime (Domain,11,True,Accepted); end if; end Enter_Delays;
   procedure Begin_Delays (Deadline : Interfaces.Unsigned_64; Accepted : out Boolean) is
   begin Begin_Calls := Begin_Calls+1; if Mode = "begin-refused" then Accepted := False;
      else Clock:=Clock+1; D.Begin_Event (Domain,Clock,Deadline,True,Accepted); end if; end Begin_Delays;
   procedure End_Delays (Accepted : out Boolean) is
   begin End_Calls := End_Calls+1; if Mode = "end-refused" then Accepted := False;
      else Clock:=Clock+1; D.End_Event (Domain,Clock,True,Accepted); end if; end End_Delays;
   procedure Stop_Delays is begin Stop_Calls := Stop_Calls+1; D.Stop (Domain); end Stop_Delays;
   procedure Hook (S : aliased in out C.State) is
      Nested : C.Values.Result; Status : C.Values.Access_Status; Ignored : R.Outcome;
      Before : constant Natural := Calls;
   begin
      if Mode = "reentry" then
         R.Evaluate_Named (S,Item,100,Nested,Status);
         Check (Status = C.Values.Wrong_Phase and Calls = Before);
      elsif Mode = "stop-during-eval" then
         R.Request_Stop (Ignored); Check (Ignored = R.Status.Stop_Pending and Close_Calls = 1 and Stop_Calls = 0);
      end if;
   end Hook;
   procedure Evaluate (S : aliased in out C.State; Node : C.Node_Descriptor; Budget : Natural;
     Result : out C.Values.Result; Status : out C.Values.Access_Status) is
   begin Calls := Calls+1; Hook (S); C.Evaluate_Named (S,Node,Budget,Result,Status); end Evaluate;
   procedure Invoke (S : aliased in out C.State; Node : C.Namespace.Node_ID;
     Args : AML_Execute.Arguments; Count : C.Values.Argument_Count; Budget : Natural;
     Result : out C.Values.Result; Status : out C.Values.Access_Status) is
   begin Calls := Calls+1; Hook (S); C.Invoke_Retained (S,Node,Args,Count,Budget,Result,Status); end Invoke;
   procedure Invoke_Values (S : aliased in out C.State; Node : C.Namespace.Node_ID;
     Args : C.Values.Arguments; Count : C.Values.Argument_Count; Budget : Natural;
     Result : out C.Values.Result; Status : out C.Values.Access_Status) is
   begin Calls := Calls+1; Hook (S); C.Invoke_With_Arguments (S,Node,Args,Count,Budget,Result,Status); end Invoke_Values;
   Result : C.Values.Result; Status : C.Values.Access_Status; Outcome : R.Outcome; Second : Other.Outcome;
   Found : C.Namespace.Lookup_Result; Saved : Natural; Accepted : Boolean;
   procedure Find (Name : String) is
   begin C.Find_Object (Service,C.Namespace.Root,Name,Found,Status); Check (Status=C.Values.Available and Found.Status=C.Namespace.Found);
      C.Describe_Node (Service,Found.Node,Item,Status); Check (Status=C.Values.Available); end Find;
   procedure Finish is
   begin Check (C.Valid (Service) and C.Retained_Results (Service)=0);
      C.Release_Storage (Service,Accepted); Check (Accepted); C.Release_Storage (Foreign,Accepted); Check (Accepted);
      Ada.Text_IO.Put_Line ("LOCAL RUNTIME " & Mode & " checks" & Checks'Image); end Finish;
   File : IO.File_Type;
begin
   IO.Open (File,IO.In_File,Ada.Command_Line.Argument (2));
   declare
      Size : constant Natural := Natural (IO.Size (File)); Raw : Ada.Streams.Stream_Element_Array (1..Ada.Streams.Stream_Element_Offset(Size));
      Last : Ada.Streams.Stream_Element_Offset; Data : Firmware_Tables.Bytes (1..Size);
      Staged : C.Stage_Status; Frozen : C.Freeze_Status; Execution : C.Execution_Report; Members : C.Namespace.Initialization_Report;
   begin
      IO.Read (File,Raw,Last); IO.Close(File); Check (Last=Raw'Last);
      for I in Data'Range loop Data(I):=Firmware_Tables.Byte(Raw(Ada.Streams.Stream_Element_Offset(I))); end loop;
      C.Stage_Table(Service,1,C.DSDT,Data,Staged); Check(Staged=C.Staged);
      C.Freeze_Catalog(Service,Frozen); Check(Frozen=C.Catalog_Frozen_OK);
      C.Execute_Frozen(Service,1000,Execution,Status); Check(Status=C.Values.Available and Execution.Modules.Outcome=C.Values.Modules_Completed);
      C.Initialize_Members(Service,Members,Status); Check(Status=C.Values.Available);
      C.Describe_Node(Service,C.Namespace.Root,Anchor,Status); Check(Status=C.Values.Available);
   end;
   Idle:=True; D.Start(Domain,10,1000,True,Accepted); Check(Accepted);
   Find("LID0"); R.Evaluate_Named(Service,Item,100,Result,Status); Check(Status=C.Values.Wrong_Phase and Calls=0);
   if Mode="fresh-stop" then R.Request_Stop(Outcome); Check(Outcome=R.Status.Stopped_By_Owner and Close_Calls=0 and Stop_Calls=0); Finish; return; end if;
   R.Enter_Runtime(Service,Outcome); Check(Outcome=R.Status.Rejected and Claim_Calls=0);
   Startup_Ready:=True;
   R.Enter_Runtime(Foreign,Outcome); Check(Outcome=R.Status.Rejected and Claim_Calls=0);
   R.Enter_Runtime(Service,Outcome);
   if Mode="claim-refused" then
      Check(Outcome=R.Status.Owner_Refused and Close_Calls=0 and Stop_Calls=0); Finish; return;
   elsif Mode="stop-during-claim" then
      Check(Outcome=R.Status.Stopped_By_Owner and Close_Calls=1 and Stop_Calls=1 and Enter_Calls=0); Finish; return;
   elsif Mode="enter-refused" then
      Check(Outcome=R.Status.Delay_Refused and Close_Calls=1 and Stop_Calls=1); Finish; return;
   end if;
   Check(Outcome=R.Status.Runtime_Entered and R.Current=R.Status.Idle and Enter_Calls=1);
   Other.Enter_Runtime(Service,Second); Check(Second=Other.Status.Owner_Refused and Close_Calls=0 and Stop_Calls=0 and R.Current=R.Status.Idle);
   R.Begin_Event(Service,0,Outcome); Check(Outcome=R.Status.Rejected and Begin_Calls=0);
   R.Begin_Event(Service,100,Outcome);
   if Mode="begin-refused" then Check(Outcome=R.Status.Delay_Refused and Close_Calls=1 and Stop_Calls=1); Finish; return; end if;
   Check(Outcome=R.Status.Event_Opened and R.Current=R.Status.Active);
   R.Begin_Event(Service,200,Outcome); Check(Outcome=R.Status.Rejected and Begin_Calls=1);
   declare
      Lid,Source,Battery,Touchpad : C.Node_Descriptor;
      LR : L.Query_Report; PR : P.Query_Report; BR : B.Query_Report;
      LI : L.Identification_Report; PI : P.Identification_Report;
      BI : B.Identification_Report; BP : B.Presence_Report;
      HR : H.Session; HA : H.Action_Result;
      Bytes : Firmware_Tables.Bytes (1..512);
      Notice : C.Notification_Observation; Present : Boolean;
      procedure Sample (Changed : Boolean) is
      begin
         L.Identify(Service,Lid,1000,LI); Check(LI.Outcome=L.Matched);
         P.Identify(Service,Source,1000,PI); Check(PI.Outcome=P.Matched);
         B.Identify(Service,Battery,1000,BI); Check(BI.Outcome=B.Matched);
         B.Query_Presence(Service,Battery,1000,BP);
         Check(BP.Outcome=B.Observed and BP.Battery_Present and BP.Device_Present);
         L.Query(Service,Lid,1000,LR);
         Check(LR.Outcome=L.Sampled and LR.State=(if Changed then L.Closed else L.Open));
         P.Query(Service,Source,1000,PR);
         Check(PR.Outcome=P.Sampled and PR.State=(if Changed then P.Offline else P.Online));
         B.Query(Service,Battery,1000,BR);
         Check(BR.Outcome=B.Sampled and BR.Value.Remaining_Capacity.Known and
           BR.Value.Remaining_Capacity.Value=(if Changed then 2000 else 3000));
         H.Begin_Discovery(HR,Service,Touchpad,1000,HA);
         if not H.Pending(HR) then
            Ada.Text_IO.Put_Line("HID " & H.Observe(HR).Result'Image & " step=" & H.Observe(HR).Step'Image &
              " execution=" & H.Observe(HR).Execution'Image & " runtime=" & R.Current'Image);
         end if;
         Check(HA=H.Accepted and H.Pending(HR) and C.Retained_Results(Service)=1);
         H.Copy_And_Finish(HR,Service,Bytes,HA);
         Check(HA=H.Accepted and not H.Pending(HR) and H.Observe(HR).Result=H.Observed
           and H.Observe(HR).Descriptor.Address=16#20#);
         Check(C.Retained_Results(Service)=0 and R.Current=R.Status.Active);
      end Sample;
   begin
      Find("LID0"); Lid:=Item; Find("AC00"); Source:=Item;
      Find("BAT0"); Battery:=Item; Find("TPD0"); Touchpad:=Item;
      C.Configure_Notifications(Service,True,Status); Check(Status=C.Values.Available);
      Sample(False);
      R.End_Event(Service,True,Outcome); Check(Outcome=R.Status.Event_Completed);
      Saved:=Calls; L.Query(Service,Lid,1000,LR);
      Check(LR.Outcome=L.Evaluation_Failed and LR.State=L.Unknown and Calls=Saved);
      R.Begin_Event(Service,100,Outcome); Check(Outcome=R.Status.Event_Opened);
      Find("EVT0"); R.Invoke_Retained(Service,Found.Node,[others=>0],0,1000,Result,Status);
      Check(Status=C.Values.Available and Result.Status=AML_Execute.No_Return);
      Check(C.Pending_Notifications(Service)=3);
      R.End_Event(Service,True,Outcome); Check(Outcome=R.Status.Event_Completed);
      for I in 1..3 loop
         C.Take_Notification(Service,Notice,Present,Status);
         Check(Status=C.Values.Available and Present and Notice.Current_Target and Notice.Number=16#80#);
         Check(Notice.Node=(case I is when 1=>C.Values.Position(Lid),when 2=>C.Values.Position(Source),when others=>C.Values.Position(Battery)));
      end loop;
      C.Take_Notification(Service,Notice,Present,Status); Check(Status=C.Values.Available and not Present);
      R.Begin_Event(Service,100,Outcome); Check(Outcome=R.Status.Event_Opened);
      Sample(True);
      R.End_Event(Service,True,Outcome); Check(Outcome=R.Status.Event_Completed);
      Check(Enter_Calls=1 and Begin_Calls=3 and End_Calls=3 and C.Pending_Notifications(Service)=0);
   end;
   R.Request_Stop(Outcome); Check(R.Current=R.Status.Stopped and Close_Calls=1 and Stop_Calls=1);
   Saved:=Calls; R.Evaluate_Named(Service,Item,100,Result,Status); Check(Status=C.Values.Wrong_Phase and Calls=Saved);
   R.Request_Stop(Outcome); Check(Outcome=R.Status.Already_Stopped and Close_Calls=1 and Stop_Calls=1);
   Finish;
end Runtime_Tests;
