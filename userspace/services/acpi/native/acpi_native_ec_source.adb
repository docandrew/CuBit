pragma Ada_2022;
with ACPI_Service;
with ACPI_One_Service;
with ACPI_Native_EC_Discovery;
with ACPI_EC_Methods;
with ACPI_EC_Event_Receiver;
with ACPI_EC_Event_Types;
with ACPI_Requests.EC_Events;
with ACPI_Requests.With_Local_Maintenance;
with ACPI_Clock_Source;
with CuBit.Messages;
with Interfaces;
package body ACPI_Native_EC_Source is
   package C renames ACPI_Service;
   package Q renames ACPI_Requests;
   package One renames ACPI_One_Service;
   package Fields renames One.Root.Fields;
   package Commands renames Fields.EC_Commands;
   package T renames ACPI_EC_Types;
   package Selection renames ACPI_Native_EC_Discovery;
   package E renames ACPI_EC_Event_Types;
   package Methods is new ACPI_EC_Methods(C,One.Runtime.Evaluate_Named);
   package Receiver is new ACPI_EC_Event_Receiver(C,Methods,1);
   package Deliveries is new Q.EC_Events(Receiver);
   use type Interfaces.Unsigned_64,T.Outcome,T.Byte,T.Operation;
   use type Commands.Phase,Commands.Start_Result,Commands.Token_ID;
   use type Selection.Outcome,One.Runtime.Status.Phase,One.Runtime.Status.Outcome;
   use type Receiver.Enrollment,Receiver.Outcome,ACPI_SCI_Events.IO_Outcome;
   use type ACPI_SCI_Quiesce.Outcome;
   Window_Microseconds : constant Interfaces.Unsigned_64 := 500_000;
   AML_Fuel : constant Natural := 100_000;
   Query_Work : constant T.Work_Budget := 10_000;
   Controller : constant E.Controller_ID := 1;
   Epoch : constant E.Epoch_ID := 1;
   Last : Report;
   Run : Receiver.Delivery;
   Fact : Query_Fact;
   Stopping,Working : Boolean := False;
   Rearm_Ready : Boolean := False;
   Until_Time : Interfaces.Unsigned_64 := 0;
   function Observe return Report is (Last);
   function Quiet return Boolean is
     (not Working and then Last.Source in Unarmed|Masked
      and then not Last.Delivery_Pending and then not Last.Fact_Pending
      and then not Receiver.Pending(Run) and then not Selection.Pending
      and then Commands.Quiescent and then Fields.Control_Idle);
   procedure Fail (Why : Failure) is
   begin Last.Stage:=Failed;Last.Why:=Why;end Fail;
   function Admission_Ready (Server : Q.State) return Boolean is
     (Q.Valid(Server) and then not Q.Notifications_Pending(Server)
      and then not Q.Retained_Results_Pending(Server)
      and then One.Runtime.Current=One.Runtime.Status.Idle and then Fields.Control_Idle
      and then Commands.Quiescent);
   procedure Open_Window(Server:in out Q.State;Opened:out Boolean) is
      Now:Interfaces.Unsigned_64;Available:Boolean;Result:One.Runtime.Status.Outcome;
   begin
      Opened:=False;ACPI_Clock_Source.Read(Now,Available);
      if not Available or else Now>Interfaces.Unsigned_64(T.Microseconds'Last)-Window_Microseconds then return;end if;
      Until_Time:=Now+Window_Microseconds;
      One.Runtime_Access.Begin_Event(Server,Until_Time,Result);
      Opened:=Result=One.Runtime.Status.Event_Opened;
   end Open_Window;
   procedure Close_Window(Server:in out Q.State;Completed:Boolean) is
      Result:One.Runtime.Status.Outcome;
   begin
      One.Runtime_Access.End_Event(Server,Completed,Result);
      if Result/=One.Runtime.Status.Event_Completed then Fail(Window_Refused);end if;
   end Close_Window;
   procedure Select_Controller(Service:aliased in out C.State;Completed:out Boolean) is
      Result:Selection.Report;Enrollment:Receiver.Enrollment;
   begin
      Completed:=False;Selection.Configure(Service,AML_Fuel,Until_Time,Result);
      if Result.Status=Selection.Ready and then Selection.Current(Service) then
         Receiver.Install(Service,Controller,Epoch,CuBit.Messages.Own_Process,Result.Description.Device,Enrollment);
         if Enrollment=Receiver.Installed then
            Last.GPE:=ACPI_SCI_Registers.GPE_Number(Result.Description.Event);Last.Stage:=Configured;
         else Fail(Enrollment_Refused);end if;
      else Fail(Selection_Refused);end if;
      -- An unavailable selection is not itself uncertain hardware. End the
      -- finite window cleanly when the actual Core/provider permit it.
      Completed:=not Selection.Pending and then C.Retained_Results(Service)=0
        and then Fields.Control_Idle and then Commands.Quiescent
        and then One.Runtime.Current=One.Runtime.Status.Active;
   end Select_Controller;
   package Configure_Work is new Q.With_Local_Maintenance(Select_Controller);
   procedure Configure(Server:in out Q.State;Accepted:out Boolean) is
      Opened,Completed:Boolean;Allowed:Configure_Work.Controller.Admission;
      use type Configure_Work.Controller.Admission;
   begin
      Accepted:=False;if Working or else Last.Stage/=Fresh or else Stopping then return;end if;
      if not Admission_Ready(Server) then Fail(Admission_Refused);return;end if;
      Working:=True;Open_Window(Server,Opened);
      if not Opened then Fail(Window_Refused);Working:=False;return;end if;
      Configure_Work.Advance(Server,Allowed,Completed);
      if Allowed/=Configure_Work.Controller.Admitted then Fail(Admission_Refused);end if;
      Close_Window(Server,Completed);
      Accepted:=Last.Stage=Configured;Working:=False;
   end Configure;
   procedure Arm(Consumers_Ready:Boolean;Accepted:out Boolean) is
      Result:ACPI_SCI_Events.IO_Outcome;
   begin
      Accepted:=False;
      if Working or else Stopping or else Last.Stage/=Configured or else not Consumers_Ready
        or else not ACPI_SCI_Events.Is_Armed or else not Quiet then return;end if;
      Working:=True;
      ACPI_SCI_Events.Mask_GPE(Last.GPE,Result);
      if Result/=ACPI_SCI_Events.Completed then Last.Source:=Unknown;Fail(Source_Refused);
      else
         Last.Source:=Masked;ACPI_SCI_Events.Enable_GPE(Last.GPE,Result);
         if Result=ACPI_SCI_Events.Completed then Last.Source:=Enabled;Last.Stage:=Armed;Accepted:=True;
         else Last.Source:=Unknown;Fail(Source_Refused);end if;
      end if;
      Working:=False;
   end Arm;
   procedure GPE_Event(GPE:ACPI_SCI_Registers.GPE_Number;Result:out ACPI_SCI_Events.Disposition) is
      IO:ACPI_SCI_Events.IO_Outcome;
   begin
      Result:=ACPI_SCI_Events.Unbound;
      if Working or else Stopping or else Last.Stage not in Armed|Query_Pending|Awaiting_Notifications
        or else GPE/=Last.GPE then return;end if;
      ACPI_SCI_Events.Mask_GPE(GPE,IO);
      if IO/=ACPI_SCI_Events.Completed then Last.Source:=Unknown;Fail(Source_Refused);return;end if;
      Last.Source:=Masked;if Last.Stage=Armed then Last.Stage:=Query_Pending;end if;
      -- ACPI 12.6: the dedicated EC SCI is an EDGE source. Acknowledge
      -- this edge before the query exchange. Never clear status at rearm:
      -- a pulse arriving during masked work must remain latched for delivery.
      ACPI_SCI_Events.Clear_GPE(GPE,IO);
      if IO/=ACPI_SCI_Events.Completed then Fail(Source_Refused);end if;
      -- Mask succeeded, so even clear refusal satisfies the callback's
      -- masked-or-cleared obligation and leaves status retained on failure.
      Result:=ACPI_SCI_Events.Handled;
   end GPE_Event;
   procedure Query_And_Deliver(Service:aliased in out C.State;Completed:out Boolean) is
      Started:Commands.Start_Result;Token:Commands.Token_ID;Done:Commands.Completion;
      Paced,Stopped_Command:Boolean;Outcome:Receiver.Outcome;
      Pause_Failed:Boolean:=False;
   begin
      Completed:=False;
      if not Selection.Current(Service) then Fail(Context_Changed);return;end if;
      Commands.Start(T.Query_Event,0,0,T.Microseconds(Until_Time),Query_Work,Started,Token);
      if Started/=Commands.Started then Fail(Query_Refused);return;end if;
      -- Engine owns the original deadline/work allowance. Pacing failure asks
      -- that same engine to stop and drains it; no new command or deadline.
      for Step in 1..Natural(Query_Work)+1 loop
         exit when Commands.Observe.Stage/=Commands.Running;
         Commands.Advance;
         if Commands.Observe.Stage=Commands.Running then
            Fields.Wait_EC(Commands.Wake_At,Paced);
            if not Paced then Pause_Failed:=True;Commands.Request_Stop(Stopped_Command);end if;
         end if;
      end loop;
      Commands.Collect(Done);
      if not Done.Present or else Done.Token/=Token or else Done.Kind/=T.Query_Event
        or else Done.Recovery or else not Done.Guard_Cleared or else not Commands.Quiescent
      then Fail(Query_Uncertain);return;end if;
      if Pause_Failed or else Done.Stopping then Fail(Query_Refused);return;end if;
      if Done.Hardware=T.No_Event or else (Done.Hardware=T.Succeeded and then Done.Value=0) then
         -- The enclosing owner must successfully close its SAME runtime
         -- window before re-enabling the GPE. New edge status stays latched. A failed close leaves
         -- the original source masked and the exchange attributable.
         Rearm_Ready:=True;Completed:=True;return;
      elsif Done.Hardware/=T.Succeeded then Fail(Query_Refused);return;end if;
      if Last.Queries<Natural'Last then Last.Queries:=Last.Queries+1;end if;Fact:=(Done.Value,False);Last.Fact_Pending:=True;
      Receiver.Dispatch(Run,Service,CuBit.Messages.Own_Process,
         (Controller,Epoch,E.Token_ID(Token),E.Event_Number(Done.Value)),AML_Fuel,Outcome);
      Last.Delivery_Pending:=Receiver.Consumed(Run);
      if Outcome not in Receiver.Completed|Receiver.Handler_Missing then Fail(Handler_Refused);return;end if;
      Fact.Missing_Handler:=Outcome=Receiver.Handler_Missing;Last.Stage:=Awaiting_Notifications;
      Completed:=not Receiver.Pending(Run) and then C.Retained_Results(Service)=0
        and then Fields.Control_Idle and then Commands.Quiescent;
   end Query_And_Deliver;
   package Query_Work_Item is new Q.With_Local_Maintenance(Query_And_Deliver);
   procedure Advance(Server:in out Q.State;Result:out ACPI_Native_Server.Owner_Outcome) is
      Allowed:Deliveries.Controller.Admission;Accepted,Opened,Completed:Boolean;
      Work_Allowed:Query_Work_Item.Controller.Admission;
      IO:ACPI_SCI_Events.IO_Outcome;
      use type Deliveries.Controller.Admission,Query_Work_Item.Controller.Admission;
   begin
      Result:=ACPI_Native_Server.Owner_Idle;
      if Working then Result:=ACPI_Native_Server.Owner_Refused;return;end if;
      if Last.Stage=Failed then Result:=ACPI_Native_Server.Owner_Refused;return;end if;
      if Stopping or else Last.Stage not in Query_Pending|Awaiting_Notifications then return;end if;
      if Q.Notifications_Pending(Server) then Result:=ACPI_Native_Server.Owner_Pending;return;end if;
      if Last.Fact_Pending then Result:=ACPI_Native_Server.Owner_Pending;return;end if;
      if not Admission_Ready(Server) then Fail(Admission_Refused);Result:=ACPI_Native_Server.Owner_Refused;return;end if;
      Working:=True;
      if Last.Delivery_Pending then
         Deliveries.Retire(Server,Run,Allowed,Accepted);
         if Allowed/=Deliveries.Controller.Admitted or else not Accepted then
            Fail(Retirement_Refused);Working:=False;Result:=ACPI_Native_Server.Owner_Refused;return;
         end if;
         Last.Delivery_Pending:=False;Last.Stage:=Query_Pending;
      end if;
      Open_Window(Server,Opened);
      if not Opened then Fail(Window_Refused);Working:=False;Result:=ACPI_Native_Server.Owner_Refused;return;end if;
      Rearm_Ready:=False;Query_Work_Item.Advance(Server,Work_Allowed,Completed);
      if Work_Allowed/=Query_Work_Item.Controller.Admitted then Fail(Admission_Refused);end if;
      Close_Window(Server,Completed);
      if Last.Stage/=Failed and then Rearm_Ready then
         if not Fields.Control_Idle or else not Commands.Quiescent then Fail(Query_Uncertain);
         else
            -- Do not clear an edge that arrived after the callback ACK.
            ACPI_SCI_Events.Enable_GPE(Last.GPE,IO);
            if IO/=ACPI_SCI_Events.Completed then Last.Source:=Unknown;Fail(Source_Refused);
            else Last.Source:=Enabled;Last.Stage:=Armed;end if;
         end if;
      end if;
      Working:=False;
      Result:=(if Last.Stage=Failed then ACPI_Native_Server.Owner_Refused
         elsif Last.Stage=Armed then ACPI_Native_Server.Owner_Idle else ACPI_Native_Server.Owner_Pending);
   end Advance;
   procedure Take_Query(Fact:out Query_Fact;Present:out Boolean) is
   begin
      Fact:=(others=><>);Present:=False;
      if Working or else not Last.Fact_Pending or else not Commands.Quiescent or else not Fields.Control_Idle
        or else Receiver.Pending(Run) or else One.Runtime.Current in One.Runtime.Status.Fresh|One.Runtime.Status.Active then return;end if;
      Fact:=ACPI_Native_EC_Source.Fact;Present:=True;Last.Fact_Pending:=False;
   end Take_Query;
   procedure Stop is
      IO:ACPI_SCI_Events.IO_Outcome;
   begin
      if Working then return;end if;Stopping:=True;
      if Last.Source not in Unarmed|Masked then
         ACPI_SCI_Events.Mask_GPE(Last.GPE,IO);
         Last.Source:=(if IO=ACPI_SCI_Events.Completed then Masked else Unknown);
      end if;
      if Last.Stage/=Failed then Last.Stage:=Stopped;end if;
   end Stop;
   procedure Close(Server:in out Q.State;Completed:out Boolean) is
      Allowed:Deliveries.Controller.Admission;Accepted:Boolean;
      use type Deliveries.Controller.Admission;
   begin
      Completed:=False;if Working then return;end if;Stop;
      if Last.Delivery_Pending and then not Q.Notifications_Pending(Server)
        and then not Q.Retained_Results_Pending(Server) then
         Deliveries.Retire(Server,Run,Allowed,Accepted);
         if Allowed=Deliveries.Controller.Admitted and then Accepted then Last.Delivery_Pending:=False;end if;
      end if;
      Completed:=Quiet;
   end Close;
   procedure Confirm_Disarm(Result:ACPI_SCI_Quiesce.Result) is
   begin
      if not Working and then Stopping and then Result.State=ACPI_SCI_Quiesce.Quiesced then
         Last.Source:=Masked;
      end if;
   end Confirm_Disarm;
end ACPI_Native_EC_Source;
