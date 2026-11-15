pragma Ada_2022;
with ACPI_One_Service;with ACPI_Service;
with ACPI_Native_Consumers;
with ACPI_Native_EC_Source;
with ACPI_Native_Log;
with ACPI_Native_Power_Buttons;
with ACPI_Native_Power_Entry;
with ACPI_Native_Button_Discovery;
with ACPI_Native_Server.With_Notifications;
with Interfaces;
package body ACPI_Native_Event_Domain is
   package Q renames ACPI_Requests;package One renames ACPI_One_Service;
   package S renames ACPI_Native_EC_Source;
   use type Q.Phase,One.Runtime.Status.Phase,S.Phase;
   use type ACPI_Native_Server.Owner_Outcome,Interfaces.Unsigned_64;
   Last:Report;
   Has_Attempted,Working,Captures_Healthy,Consumers_Active,Source_Used:Boolean:=False;
   Consumer_Pending:Boolean:=False;
   type Turn is (Source_Turn,Consumer_Turn);
   Next_Turn:Turn:=Consumer_Turn;
   type Button is (Power_Button,Sleep_Button);
   Presses:array(Button) of Interfaces.Unsigned_64:=[others=>0];
   function Sources_Healthy return Boolean is (Captures_Healthy);
   package Consumers is new ACPI_Native_Consumers(Sources_Healthy);
   function Shutdown_Turns return Positive is
      Query_Fact_Slots:constant Positive:=1;
      Final_Close_Turns:constant Positive:=3;
   begin
      return ACPI_Service.Namespace.Notification_Count'Last+Consumers.Maximum_Facts+
         Query_Fact_Slots+Button'Pos(Button'Last)+1+Final_Close_Turns;
   end Shutdown_Turns;
   function Observe return Report is (Last);
   function Attempted return Boolean is (Has_Attempted);
   function Diagnostics_Quiet(Server:Q.State) return Boolean is
     (not Q.Notifications_Pending(Server) and then not Q.Retained_Results_Pending(Server)
      and then One.Runtime.Current not in One.Runtime.Status.Fresh|One.Runtime.Status.Active
      and then One.Root.Fields.Control_Idle and then One.Root.Fields.EC_Commands.Quiescent);
   procedure Fail(Why:Failure) is begin Last.Stage:=Failed;Last.Why:=Why;end Fail;
   procedure Route_All(Server:Q.State;Routed:out Boolean) is
      Buttons_Routed:Boolean;
   begin
      Routed:=False;if not Consumers_Active then return;end if;
      Consumers.Route(Server,Routed);
      ACPI_Native_Button_Discovery.Route(Server,Buttons_Routed);
      Routed:=Routed and Buttons_Routed;
      if Routed and then Last.Stage/=Stopping then Consumer_Pending:=True;end if;
   end Route_All;
   package Pump is new ACPI_Native_Server.With_Notifications(Route_All);
   procedure Pump_One(Server:in out Q.State;Result:out ACPI_Native_Server.Owner_Outcome) is
   begin
      if not Q.Notifications_Pending(Server) then Result:=ACPI_Native_Server.Owner_Idle;
      elsif Consumers_Active then Pump.Advance(Server,Result);
      else Result:=ACPI_Native_Server.Owner_Refused;end if;
   end Pump_One;
   procedure Start(Server:in out Q.State;Result:out Start_Outcome) is
      Accepted,Source_Ready,Closed_Consumers:Boolean;
      Work:ACPI_Native_Server.Owner_Outcome;
      Button_Start:ACPI_Native_Button_Discovery.Outcome;
   begin
      ACPI_Native_Power_Buttons.Configure_Fixed (ACPI_SCI_Events.Fixed_Power_Button_Available);
      Result:=Refused;if Has_Attempted or Working then return;end if;Has_Attempted:=True;Last.Stage:=Starting;
      if Q.Current(Server)/=Q.Complete or else not Q.Notifications_Ready(Server)
        or else One.Runtime.Current/=One.Runtime.Status.Idle
        or else not One.Root.Fields.Control_Idle or else Q.Retained_Results_Pending(Server) then Fail(Not_Ready);return;end if;
      if not ACPI_SCI_Events.Is_Armed then Last.Stage:=Fixed_Only;Result:=SCI_Unavailable;return;end if;
      Working:=True;
      -- Captured startup Notify values have the original Core's provenance.
      -- Binding/routes run no AML; collect/publish starts only after admission.
      -- Orderly masking/disarm does not revoke already captured receipts.
      Captures_Healthy:=True;Consumers.Start(Server,Accepted);
      Consumers_Active:=Accepted;Last.Consumers_Started:=Accepted;
      if not Accepted then Fail(Consumer_Start_Refused);Working:=False;return;end if;
      for Step in 1..Shutdown_Turns loop
         exit when not Q.Notifications_Pending(Server);
         Pump_One(Server,Work);
         if Work=ACPI_Native_Server.Owner_Refused then Fail(Routing_Refused);Working:=False;return;end if;
      end loop;
      if Q.Notifications_Pending(Server) then Fail(Routing_Refused);Working:=False;return;end if;
      Source_Used:=True;S.Configure(Server,Source_Ready);Last.EC:=ACPI_Native_EC_Discovery.Observe.Status;
      if not Source_Ready then
         -- A safely refused EC is a visible lack of live family support, not
         -- fresh data. Fixed buttons and read-only table/metrics IPC still work.
         for Step in 1..Shutdown_Turns loop
            exit when not Q.Notifications_Pending(Server);
            Pump_One(Server,Work);
            if Work=ACPI_Native_Server.Owner_Refused then Fail(Routing_Refused);Working:=False;return;end if;
         end loop;
         if Diagnostics_Quiet(Server) and then One.Runtime.Current=One.Runtime.Status.Idle and then S.Quiet then
            Consumers.Close(Server,Closed_Consumers);
            if Closed_Consumers then
               Consumers_Active:=False;Last.Stage:=Fixed_Only;Result:=EC_Unavailable;Working:=False;return;
            end if;
         end if;
         Fail(EC_Startup_Refused);Working:=False;return;
      end if;
      S.Arm(Consumers_Active,Accepted);
      if not Accepted then Fail(EC_Startup_Refused);Working:=False;return;end if;
      Last.EC_Armed:=True;
      for Step in 1..Shutdown_Turns loop
         exit when not Q.Notifications_Pending(Server);Pump_One(Server,Work);
         if Work=ACPI_Native_Server.Owner_Refused then Fail(Routing_Refused);Working:=False;return;end if;
      end loop;
      if Q.Notifications_Pending(Server) then Fail(Routing_Refused);Working:=False;return;end if;
      ACPI_Native_Button_Discovery.Start(Server,ACPI_SCI_Events.Fixed_Power_Button_Available,Button_Start);
      if not ACPI_Native_Button_Discovery.Quiet or else One.Runtime.Current/=One.Runtime.Status.Idle then
         Fail(Consumer_Start_Refused);Working:=False;return;
      end if;
      Last.Stage:=Live;Consumer_Pending:=True;Result:=Ready;Working:=False;
   end Start;
   procedure Fixed_Event(Event:ACPI_SCI_Registers.Fixed_Event;Result:out ACPI_SCI_Events.Disposition) is
      procedure Record_Press(Item:Button) is
      begin
         if Presses(Item)<Interfaces.Unsigned_64'Last then Presses(Item):=Presses(Item)+1;end if;
         Result:=ACPI_SCI_Events.Handled;
      end Record_Press;
   begin
      Result:=ACPI_SCI_Events.Unbound;
      -- SCI clears fixed status before this callback. Copy only; no logger,
      -- Core call or policy action here. Stop/disarm prevents further dispatch.
      case Event is
         when ACPI_SCI_Registers.Power_Button=>
            ACPI_Native_Power_Buttons.Fixed_Press;
            Record_Press(Power_Button);
         when ACPI_SCI_Registers.Sleep_Button=>Record_Press(Sleep_Button);
         when others=>null;
      end case;
   end Fixed_Event;
   procedure GPE_Event(GPE:ACPI_SCI_Registers.GPE_Number;Result:out ACPI_SCI_Events.Disposition) is
   begin
      Result:=ACPI_SCI_Events.Unbound;
      if Last.Stage=Live then S.GPE_Event(GPE,Result);end if;
   end GPE_Event;
   function Buttons_Pending return Boolean is (for some B in Button=>Presses(B)/=0);
   function Facts_Pending return Boolean is
     (Buttons_Pending or else (Source_Used and then S.Observe.Fact_Pending)
      or else (Consumers_Active and then Consumers.Facts_Pending));
   function Query_Pending return Boolean is
     (Last.Stage=Live and then S.Observe.Stage in S.Query_Pending|S.Awaiting_Notifications);
   function Runnable return Boolean is
     (Facts_Pending or else Query_Pending or else (Consumers_Active and then Consumer_Pending));
   procedure Publish_One(Server:Q.State;Published:out Boolean) is
      Fact:S.Query_Fact;Present:Boolean;
      Line:Consumers.Fact_Line;Length:Consumers.Fact_Length;
      Hex:constant String:="0123456789ABCDEF";
      Count:Interfaces.Unsigned_64;
   begin
      Published:=False;if not Diagnostics_Quiet(Server) then return;end if;
      if Source_Used then
         S.Take_Query(Fact,Present);
         if Present then
            ACPI_Native_Log.Copied("acpi: query Q"&Hex(Natural(Fact.Number)/16+1)&Hex(Natural(Fact.Number) mod 16+1)&
               (if Fact.Missing_Handler then " UNMAPPED" else ""),not ACPI_Native_Power_Entry.Blocks_Runtime);Published:=True;return;
         end if;
      end if;
      for B in Button loop
         if Presses(B)/=0 then
            Count:=Presses(B);Presses(B):=0;
            ACPI_Native_Log.Copied("acpi: "&(if B=Power_Button then "power-button" else "sleep-button")&" presses="&Count'Image,not ACPI_Native_Power_Entry.Blocks_Runtime);
            Published:=True;return;
         end if;
      end loop;
      if Consumers_Active then
         Consumers.Take_Fact(Server,Line,Length,Present);
         if Present then ACPI_Native_Log.Copied(Line(1..Length),not ACPI_Native_Power_Entry.Blocks_Runtime);Published:=True;end if;
      end if;
   end Publish_One;
   procedure Advance(Server:in out Q.State;Result:out ACPI_Native_Server.Owner_Outcome) is
      Published:Boolean;Work:ACPI_Native_Server.Owner_Outcome;
   begin
      Result:=ACPI_Native_Server.Owner_Refused;
      if Working or Last.Stage not in Live|Fixed_Only then return;end if;
      if Last.Stage=Live and then S.Observe.Stage=S.Failed then Fail(EC_Work_Refused);return;end if;
      Working:=True;Pump_One(Server,Work);
      if Work=ACPI_Native_Server.Owner_Refused then Fail(Routing_Refused);Working:=False;return;end if;
      if Work=ACPI_Native_Server.Owner_Pending then Result:=Work;Working:=False;return;end if;
      Publish_One(Server,Published);
      if Published then
         Result:=(if Runnable then ACPI_Native_Server.Owner_Pending else ACPI_Native_Server.Owner_Idle);Working:=False;return;
      end if;
      if not Diagnostics_Quiet(Server) then Fail(Not_Ready);Working:=False;return;end if;
      Work:=ACPI_Native_Server.Owner_Idle;
      if Last.Stage=Live then
         if Next_Turn=Consumer_Turn and then Consumers_Active then
            Consumers.Advance(Server,Work);Consumer_Pending:=Work=ACPI_Native_Server.Owner_Pending;Next_Turn:=Source_Turn;
            if Work=ACPI_Native_Server.Owner_Refused then Fail(Consumer_Work_Refused);end if;
         else
            S.Advance(Server,Work);Next_Turn:=Consumer_Turn;
            if Work=ACPI_Native_Server.Owner_Refused then Fail(EC_Work_Refused);end if;
         end if;
      end if;
      Result:=(if Last.Stage=Failed then ACPI_Native_Server.Owner_Refused
         elsif Work=ACPI_Native_Server.Owner_Pending or else Runnable then ACPI_Native_Server.Owner_Pending
         else ACPI_Native_Server.Owner_Idle);Working:=False;
   end Advance;
   procedure Begin_Stop is
   begin
      Last.Stage:=Stopping;ACPI_Native_Button_Discovery.Begin_Stop;
      if Source_Used then S.Stop;end if;
      if Consumers_Active then Consumers.Begin_Stop;end if;
   end Begin_Stop;
   procedure Confirm_Disarm(Result:ACPI_SCI_Quiesce.Result) is
   begin if Source_Used then S.Confirm_Disarm(Result);end if;end Confirm_Disarm;
   procedure Drain(Server:in out Q.State;Result:out Drain_Outcome) is
      Work:ACPI_Native_Server.Owner_Outcome;Published,Source_Closed:Boolean;
   begin
      Result:=Retained;if Working then return;end if;
      if Last.Stage/=Stopping then Begin_Stop;end if;
      if not Has_Attempted and then not Buttons_Pending then Result:=Drained;return;end if;
      Working:=True;Pump_One(Server,Work);
      if Work=ACPI_Native_Server.Owner_Refused then Working:=False;return;end if;
      if Work=ACPI_Native_Server.Owner_Pending then Result:=Pending;Working:=False;return;end if;
      if not Diagnostics_Quiet(Server) then Working:=False;return;end if;
      Publish_One(Server,Published);
      if Published then Result:=Pending;Working:=False;return;end if;
      if Facts_Pending then Working:=False;return;end if;
      if Source_Used then
         S.Close(Server,Source_Closed);
         if not Source_Closed then Working:=False;return;end if;
      end if;
      Result:=Drained;Working:=False;
   end Drain;
   procedure Close(Server:in out Q.State;Result:out Drain_Outcome) is
      Consumer_Closed,Buttons_Closed:Boolean;
   begin
      Drain(Server,Result);if Result/=Drained then return;end if;
      if Consumers_Active then
         Consumers.Close(Server,Consumer_Closed);
         if not Consumer_Closed then Result:=Retained;return;end if;
         Consumers_Active:=False;
      end if;
      ACPI_Native_Button_Discovery.Close(Buttons_Closed);
      if not Buttons_Closed then Result:=Retained;return;end if;
      Last.Stage:=Closed;
   end Close;
   function Quiet(Server:Q.State) return Boolean is
     (Last.Stage=Closed and then ACPI_Native_Button_Discovery.Quiet and then not Working and then not Facts_Pending
      and then (not Has_Attempted or else Diagnostics_Quiet(Server))
      and then (not Source_Used or else S.Quiet)
      and then not Consumers_Active);
end ACPI_Native_Event_Domain;
