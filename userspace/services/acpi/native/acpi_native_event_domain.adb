pragma Ada_2022;
with ACPI_One_Service;with ACPI_Service;
with ACPI_Native_Consumers;
with ACPI_Native_EC_Source;
with ACPI_Native_GPE_Source;with ACPI_Native_GPE_Control;
with ACPI_Native_Log;with ACPI_Native_Diagnostic_Output;
with ACPI_Native_Power_Buttons;
with ACPI_Native_Power_Entry;
with ACPI_Native_Button_Discovery;
with ACPI_Native_Server.With_Notifications;
with Interfaces;
package body ACPI_Native_Event_Domain is
   package Q renames ACPI_Requests;package One renames ACPI_One_Service;
   package S renames ACPI_Native_EC_Source;
   use type Q.Phase,One.Runtime.Status.Phase,S.Phase;
   use type ACPI_Native_Server.Owner_Outcome,Interfaces.Unsigned_64,ACPI_SCI_Events.Disposition;
   use type One.Root.Fields.EC_Boot_Status;
   Last:Report;
   Has_Attempted,Working,Captures_Healthy,Consumers_Active,Source_Used:Boolean:=False;
   Consumer_Pending:Boolean:=False;
   GPE_Used,GPE_Active:Boolean:=False;
   GPE_Failure_Noted:Boolean:=False;
   procedure Drain_GPE_Notifications(Server:in out Q.State;Complete:out Boolean);
   package General is new ACPI_Native_GPE_Source
     (ACPI_Native_GPE_Control.Exclusive,Drain_GPE_Notifications);
   use type General.Phase;
   type Turn is (Source_Turn,General_Turn,Consumer_Turn);
   Next_Turn:Turn:=Consumer_Turn;
   type Button is (Power_Button,Sleep_Button);
   Presses:array(Button) of Interfaces.Unsigned_64:=[others=>0];
   function Sources_Healthy return Boolean is (Captures_Healthy);
   package Consumers is new ACPI_Native_Consumers(Sources_Healthy);
   Consumer_Refusals_Noted : Natural := 0;
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
   procedure Fail(Why:Failure) is
   begin
      Last.Stage:=Failed;Last.Why:=Why;
      ACPI_Native_Power_Buttons.Withdraw_Sources;
   end Fail;
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
   procedure Drain_GPE_Notifications(Server:in out Q.State;Complete:out Boolean) is
      Work:ACPI_Native_Server.Owner_Outcome;
   begin
      Complete:=False;
      for Step in 1..Shutdown_Turns loop
         exit when not Q.Notifications_Pending(Server);
         Pump_One(Server,Work);
         if Work=ACPI_Native_Server.Owner_Refused then return;end if;
      end loop;
      Complete:=not Q.Notifications_Pending(Server);
   end Drain_GPE_Notifications;
   procedure Note_GPE(Server:Q.State;Success:Boolean) is
      View:constant General.Report:=General.Observe;
   begin
      if Success then
         ACPI_Native_Diagnostic_Output.Write("acpi: stage general-gpe ok handlers="&View.Sources'Image&
            " ec-armed="&Boolean'Image(Last.EC_Armed),
            Diagnostics_Quiet(Server) and then not ACPI_Native_Power_Entry.Blocks_Runtime,True);
      elsif not GPE_Failure_Noted then
         GPE_Failure_Noted:=True;
         -- Established-ring/serial copy only on failure: never open a channel
         -- while original result/window/control obligations may be retained.
         ACPI_Native_Diagnostic_Output.Write("acpi: stage general-gpe fail "&Last.Why'Image&
            " state="&View.Stage'Image&" reason="&View.Why'Image&
            " source="&(if View.Sources=0 then "NONE" else View.At_Source'Image),False);
      end if;
   end Note_GPE;
   procedure Start_Consumer_Routes(Server:Q.State;Accepted:out Boolean) is
   begin
      if Consumers_Active then Accepted:=Last.Consumers_Started;return;end if;
      Captures_Healthy:=True;Consumers.Start(Server,Accepted);
      Consumers_Active:=Accepted;Last.Consumers_Started:=Accepted;
   end Start_Consumer_Routes;
   procedure Prepare_Consumers(Server:in out Q.State;Accepted:out Boolean) is
   begin
      Accepted:=False;
      if Working or Has_Attempted or Last.Stage/=Fresh then return;end if;
      if Q.Current(Server)/=Q.Complete or else not Q.Notifications_Ready(Server)
        or else One.Runtime.Current/=One.Runtime.Status.Idle or else not One.Root.Fields.Control_Idle
        or else Q.Retained_Results_Pending(Server) then return;end if;
      Working:=True;Start_Consumer_Routes(Server,Accepted);
      if not Accepted then Fail(Consumer_Start_Refused);end if;
      Working:=False;
   end Prepare_Consumers;
   procedure Route_Prepared(Server:in out Q.State;Result:out ACPI_Native_Server.Owner_Outcome) is
   begin
      Result:=ACPI_Native_Server.Owner_Refused;
      if Working or Has_Attempted or Last.Stage/=Fresh or not Consumers_Active then return;end if;
      -- A captured Notify belongs to the original receipt queue. Defer its
      -- fanout while another startup owner holds the runtime/Core boundary;
      -- routing then would refuse and poison an otherwise healthy domain.
      -- This does not acknowledge, drop, or reevaluate the captured method.
      if One.Runtime.Current/=One.Runtime.Status.Idle
        or else Q.Retained_Results_Pending(Server)
        or else not One.Root.Fields.Control_Idle
        or else not One.Root.Fields.EC_Commands.Quiescent then
         Result:=ACPI_Native_Server.Owner_Idle;return;
      end if;
      Working:=True;Pump_One(Server,Result);Working:=False;
      if Result=ACPI_Native_Server.Owner_Refused then Fail(Routing_Refused);end if;
   end Route_Prepared;
   procedure Start(Server:in out Q.State;Result:out Start_Outcome) is
      Accepted,Source_Ready:Boolean;
      Boot:One.Root.Fields.EC_Boot_Report;
      Work:ACPI_Native_Server.Owner_Outcome;
      Button_Start:ACPI_Native_Button_Discovery.Outcome;
   begin
      ACPI_Native_Power_Buttons.Configure_Fixed (ACPI_SCI_Events.Fixed_Power_Button_Available);
      Result:=Refused;if Has_Attempted or Working or Last.Stage/=Fresh then return;end if;Has_Attempted:=True;Last.Stage:=Starting;
      if Q.Current(Server)/=Q.Complete or else not Q.Notifications_Ready(Server)
        or else One.Runtime.Current/=One.Runtime.Status.Idle
        or else not One.Root.Fields.Control_Idle or else Q.Retained_Results_Pending(Server) then Fail(Not_Ready);return;end if;
      if not ACPI_SCI_Events.Is_Armed then
         ACPI_Native_Power_Buttons.Withdraw_Sources;
         Last.Stage:=Fixed_Only;Result:=SCI_Unavailable;return;
      end if;
      if not ACPI_Native_GPE_Control.Exclusive then Fail(Control_Not_Exclusive);Note_GPE(Server,False);return;end if;
      Working:=True;
      -- Captured startup Notify values have the original Core's provenance.
      -- Binding/routes run no AML; collect/publish starts only after admission.
      -- Orderly masking/disarm does not revoke already captured receipts.
      Start_Consumer_Routes(Server,Accepted);
      if not Accepted then Fail(Consumer_Start_Refused);Working:=False;return;end if;
      for Step in 1..Shutdown_Turns loop
         exit when not Q.Notifications_Pending(Server);
         Pump_One(Server,Work);
         if Work=ACPI_Native_Server.Owner_Refused then Fail(Routing_Refused);Working:=False;return;end if;
      end loop;
      if Q.Notifications_Pending(Server) then Fail(Routing_Refused);Working:=False;return;end if;
      Source_Used:=True;S.Configure(Server,Source_Ready);Last.EC:=ACPI_Native_EC_Discovery.Observe.Status;
      if not Source_Ready then
         -- Distinguish safely unavailable EC from retained/uncertain startup.
         -- Consumers and ordinary GPEs remain useful on a system without EC.
         Drain_GPE_Notifications(Server,Accepted);
         if not Accepted or else not Diagnostics_Quiet(Server)
           or else One.Runtime.Current/=One.Runtime.Status.Idle or else not S.Quiet then
            Fail(EC_Startup_Refused);Working:=False;return;
         end if;
      else
         if not ACPI_Native_GPE_Control.Exclusive then Fail(Control_Not_Exclusive);Note_GPE(Server,False);Working:=False;return;end if;
         S.Arm(Consumers_Active,Accepted);
         if not Accepted then Fail(EC_Startup_Refused);Working:=False;return;end if;
         Last.EC_Armed:=True;
      end if;
      for Step in 1..Shutdown_Turns loop
         exit when not Q.Notifications_Pending(Server);Pump_One(Server,Work);
         if Work=ACPI_Native_Server.Owner_Refused then Fail(Routing_Refused);Working:=False;return;end if;
      end loop;
      if Q.Notifications_Pending(Server) then Fail(Routing_Refused);Working:=False;return;end if;
      Boot:=One.Root.Fields.EC_Boot_Observation;
      GPE_Used:=True;
      General.Start(Server,Consumers_Active,Boot.Status=One.Root.Fields.EC_Ready,
         ACPI_SCI_Registers.GPE_Number(Boot.GPE),Accepted);
      if not Accepted then Fail(GPE_Startup_Refused);Note_GPE(Server,False);Working:=False;return;end if;
      GPE_Active:=True;Last.General_GPEs:=General.Observe.Sources;
      ACPI_Native_Button_Discovery.Start(Server,ACPI_SCI_Events.Fixed_Power_Button_Available,Button_Start);
      if not ACPI_Native_Button_Discovery.Quiet or else One.Runtime.Current/=One.Runtime.Status.Idle then
         Fail(Button_Discovery_Refused);Working:=False;return;
      end if;
      Note_GPE(Server,True);
      Last.Stage:=Live;Consumer_Pending:=True;Result:=(if Source_Ready then Ready else EC_Unavailable);Working:=False;
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
      if Last.Stage=Live then
         if Last.EC_Armed then S.GPE_Event(GPE,Result);end if;
         if Result=ACPI_SCI_Events.Unbound and then GPE_Active then General.GPE_Event(GPE,Result);end if;
      end if;
   end GPE_Event;
   function Buttons_Pending return Boolean is (for some B in Button=>Presses(B)/=0);
   function Facts_Pending return Boolean is
     (Buttons_Pending or else (Source_Used and then S.Observe.Fact_Pending)
      or else (Consumers_Active and then Consumers.Facts_Pending));
   function Query_Pending return Boolean is
     (Last.Stage=Live and then S.Observe.Stage in S.Query_Pending|S.Awaiting_Notifications);
   function Runnable return Boolean is
     (Facts_Pending or else Query_Pending or else (GPE_Active and then General.Runnable)
      or else (Consumers_Active and then Consumer_Pending));
   procedure Read_Power (Server : Q.State; Maximum_Age : ACPI_Cached_Power_Values.Microseconds;
      Value : out ACPI_Cached_Power_Values.Snapshot) is
   begin
      Value := (others => <>);
      -- A captured hardware event can precede its AML Notify. Do not certify
      -- cache freshness until both hardware owners and receipt routing settle.
      if Working or else Last.Stage /= Live or else not Consumers_Active
        or else not Captures_Healthy or else not Diagnostics_Quiet (Server)
        or else Query_Pending
        or else (GPE_Active and then (General.Runnable or else General.Observe.Stage /= General.Live))
        or else (Last.EC_Armed and then S.Observe.Stage = S.Failed) then return; end if;
      Consumers.Read_Power (Server, Maximum_Age, Value);
   end Read_Power;
   procedure Read_Batteries (Server : Q.State; Maximum_Age : ACPI_Cached_Power_Values.Microseconds;
      Value : out ACPI_Cached_Battery_Values.View) is
   begin
      Value := (others => <>);
      if Working or else Last.Stage /= Live or else not Consumers_Active
        or else not Captures_Healthy or else not Diagnostics_Quiet (Server)
        or else Query_Pending
        or else (GPE_Active and then (General.Runnable or else General.Observe.Stage /= General.Live))
        or else (Last.EC_Armed and then S.Observe.Stage = S.Failed) then return; end if;
      Consumers.Read_Batteries (Server, Maximum_Age, Value);
   end Read_Batteries;
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
      if Last.Stage=Live and then Last.EC_Armed and then S.Observe.Stage=S.Failed then Fail(EC_Work_Refused);return;end if;
      if Last.Stage=Live and then GPE_Used and then General.Observe.Stage=General.Failed then Fail(GPE_Work_Refused);Note_GPE(Server,False);return;end if;
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
         -- Rotate even across an idle owner so a busy family cannot starve peers.
         case Next_Turn is
            when Consumer_Turn=>
               if Consumers_Active then
                  Consumers.Advance(Server,Work);Consumer_Pending:=Work=ACPI_Native_Server.Owner_Pending;
                  declare View : constant Consumers.Report := Consumers.Observe; begin
                     if View.Refusal_Count /= Consumer_Refusals_Noted then
                        Consumer_Refusals_Noted := View.Refusal_Count;
                        for Line of View.Diagnostics loop
                           if Line.Last /= 0 then
                              ACPI_Native_Diagnostic_Output.Write (Line.Text (1 .. Line.Last), False);
                           end if;
                        end loop;
                     end if;
                  end;
                  if Work=ACPI_Native_Server.Owner_Refused then
                     Fail(Consumer_Work_Refused);
                     declare
                        View:constant Consumers.Report:=Consumers.Observe;
                     begin
                        -- Preserve the copied reason without opening IPC while
                        -- runtime/Field/result ownership may be uncertain.
                        ACPI_Native_Diagnostic_Output.Write
                          ("acpi: stage consumer fail reason="&View.Why'Image&
                           " family="&View.At_Family'Image&
                           " runtime="&One.Runtime.Current'Image,False);
                     end;
                  end if;
               end if;
               Next_Turn:=Source_Turn;
            when Source_Turn=>
               if Last.EC_Armed then
                  S.Advance(Server,Work);
                  if Work=ACPI_Native_Server.Owner_Refused then Fail(EC_Work_Refused);end if;
               end if;
               Next_Turn:=General_Turn;
            when General_Turn=>
               if GPE_Active then
                  General.Advance(Server,Work);
                  if Work=ACPI_Native_Server.Owner_Refused then Fail(GPE_Work_Refused);Note_GPE(Server,False);end if;
               end if;
               Next_Turn:=Consumer_Turn;
         end case;
      end if;
      Result:=(if Last.Stage=Failed then ACPI_Native_Server.Owner_Refused
         elsif Work=ACPI_Native_Server.Owner_Pending or else Runnable then ACPI_Native_Server.Owner_Pending
         else ACPI_Native_Server.Owner_Idle);Working:=False;
   end Advance;
   procedure Begin_Stop is
   begin
      Last.Stage:=Stopping;ACPI_Native_Button_Discovery.Begin_Stop;
      if Source_Used then S.Stop;end if;
      if GPE_Used then General.Stop;end if;
      if Consumers_Active then Consumers.Begin_Stop;end if;
   end Begin_Stop;
   procedure Confirm_Disarm(Result:ACPI_SCI_Quiesce.Result) is
   begin
      if Source_Used then S.Confirm_Disarm(Result);end if;
      if GPE_Used then General.Confirm_Disarm(Result);end if;
   end Confirm_Disarm;
   procedure Drain(Server:in out Q.State;Result:out Drain_Outcome) is
      Work:ACPI_Native_Server.Owner_Outcome;Published,Source_Closed:Boolean;
      General_Closed:Boolean:=False;
   begin
      Result:=Retained;if Working then return;end if;
      if Last.Stage/=Stopping then Begin_Stop;end if;
      if not Has_Attempted and then not Consumers_Active and then not Buttons_Pending then Result:=Drained;return;end if;
      Working:=True;
      -- A known retained result can block both notification routing and the
      -- final quiet gate. Let its original owner release it first. Close may
      -- still be incomplete because notifications have yet to be acknowledged.
      if GPE_Used then General.Close(Server,General_Closed);end if;
      if Q.Retained_Results_Pending(Server) then Working:=False;return;end if;
      Pump_One(Server,Work);
      if Work=ACPI_Native_Server.Owner_Refused then Working:=False;return;end if;
      if Work=ACPI_Native_Server.Owner_Pending then Result:=Pending;Working:=False;return;end if;
      -- Close the known retained GPE result before the global quiet gate.
      -- That gate deliberately rejects retained results; checking it first
      -- would make their original owner's release path unreachable.
      if GPE_Used then
         if not General_Closed then General.Close(Server,General_Closed);end if;
         if not General_Closed then Working:=False;return;end if;
         GPE_Active:=False;
      end if;
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
      and then (not GPE_Used or else General.Observe.Stage=General.Stopped)
      and then not Consumers_Active);
end ACPI_Native_Event_Domain;
