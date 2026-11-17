pragma Ada_2022;
with ACPI_One_Service; with ACPI_One_Service.GPE_Events; with ACPI_Native_GPE_Inventory; with ACPI_GPE_Pending;
with ACPI_GPE_Lifecycle; with ACPI_Clock_Source;
package body ACPI_Native_GPE_Source is
   package Q renames ACPI_Requests; package One renames ACPI_One_Service;
   package G renames One.GPE_Events; package I renames ACPI_Native_GPE_Inventory;
   package R renames ACPI_SCI_Registers; package L renames ACPI_GPE_Lifecycle;
   package Queue is new ACPI_GPE_Pending;
   pragma Compile_Time_Error (Queue.Maximum_Source_Number /= R.Maximum_GPE, "GPE source domains differ");
   use type Interfaces.Unsigned_64, Q.Phase, One.Runtime.Status.Phase, One.Runtime.Status.Outcome;
   use type ACPI_SCI_Events.IO_Outcome, ACPI_SCI_Quiesce.Outcome;
   use type Queue.Capture_Result, Queue.Claim_Result, Queue.Finish_Result, Queue.Retire_Result;
   use type G.Methods.Invocation_Outcome, G.Requests.Controller.Admission;
   use type L.Action_Result, L.Phase;
   Window_Microseconds : constant Interfaces.Unsigned_64 := 500_000;
   Handler_Fuel : constant Positive := 100_000;
   Last : Report;
   Working : Boolean := False;
   Owned : R.GPE_Set := R.No_GPE;
   Controls : array (R.GPE_Number) of Source_State := [others => Unobserved];
   Active_Ticket : Queue.Ticket := Queue.No_Ticket;
   function Observe return Report is
      Value : Report := Last;
   begin
      Value.Pending := Queue.Observe.Pending; Value.Active := Queue.Observe.Active;
      Value.Result_Pending := G.Delivery.Pending_Result; return Value;
   end Observe;
   function Control (Source : R.GPE_Number) return Source_State is (Controls (Source));
   procedure Fail (Why : Failure) is
   begin Last.Stage := Failed; Last.Why := Why; Queue.Begin_Stop; end Fail;
   function Ready (Server : Q.State) return Boolean is
     (Q.Valid (Server) and then Q.Current (Server) = Q.Complete
      and then not Q.Notifications_Pending (Server) and then not Q.Retained_Results_Pending (Server)
      and then One.Runtime.Current = One.Runtime.Status.Idle
      and then One.Root.Fields.Control_Idle and then One.Root.Fields.EC_Commands.Quiescent
      and then ACPI_SCI_Events.Is_Armed);
   procedure Start (Server : Q.State; Consumers_Ready, EC_Present : Boolean;
      EC_GPE : R.GPE_Number; Accepted : out Boolean) is
      Good, Found : Boolean; Item : G.Methods.Binding;
      Sources : Queue.Source_Set := Queue.No_Sources;
      IO : ACPI_SCI_Events.IO_Outcome;
   begin
      Accepted := False; if Working or else Last.Stage /= Fresh then return; end if;
      if not Control_Exclusive then Fail (Control_Not_Exclusive); return; end if;
      if not Consumers_Ready or else not Ready (Server) then Fail (Not_Ready); return; end if;
      Working := True; I.Configure (Server, EC_Present, EC_GPE, Good);
      if not Good then Fail (Inventory_Refused); Working := False; return; end if;
      for Source in R.GPE_Number loop
         I.Lookup (Source, Item, Found);
         if Found and then G.Methods.Is_Bound (Item) then Owned (Source) := True; Sources (Queue.Source_Number (Source)) := True; Last.Sources := Last.Sources + 1; end if;
      end loop;
      Queue.Start (Sources, Good);
      if not Good then Fail (Queue_Refused); Working := False; return; end if;
      for Source in R.GPE_Number loop
         if Owned (Source) then
            Last.At_Source := Source; Controls (Source) := Unknown; ACPI_SCI_Events.Mask_GPE (Source, IO);
            Controls (Source) := (if IO = ACPI_SCI_Events.Completed then Known_Masked else Unknown);
            if IO /= ACPI_SCI_Events.Completed then Fail (Source_IO_Refused); Working := False; return; end if;
            -- Preserve startup status; no clear before a delivered event.
            Controls (Source) := Unknown; ACPI_SCI_Events.Enable_GPE (Source, IO);
            Controls (Source) := (if IO = ACPI_SCI_Events.Completed then Known_Enabled else Unknown);
            if IO /= ACPI_SCI_Events.Completed then Fail (Source_IO_Refused); Working := False; return; end if;
         end if;
      end loop;
      Last.Stage := Live; Accepted := True; Working := False;
   exception when others =>
      for Source in R.GPE_Number loop if Owned (Source) then Controls (Source) := Unknown; end if; end loop;
      Fail (Boundary_Exception); Working := False;
   end Start;
   procedure GPE_Event (Source : R.GPE_Number; Result : out ACPI_SCI_Events.Disposition) is
      IO : ACPI_SCI_Events.IO_Outcome; Captured : Queue.Capture_Result;
   begin
      Result := ACPI_SCI_Events.Unbound;
      if Working or else Last.Stage /= Live or else not Owned (Source) then return; end if;
      Last.At_Source := Source; Controls (Source) := Unknown; ACPI_SCI_Events.Mask_GPE (Source, IO);
      Controls (Source) := (if IO = ACPI_SCI_Events.Completed then Known_Masked else Unknown);
      if IO /= ACPI_SCI_Events.Completed then Fail (Source_IO_Refused); return; end if;
      -- Mask has completed, so callback obligation holds even on refusal.
      Result := ACPI_SCI_Events.Handled;
      Queue.Capture (Queue.Source_Number (Source), Captured);
      if Captured not in Queue.Captured | Queue.Coalesced then Fail (Capture_Refused); end if;
   exception when others => Controls (Source) := Unknown; Fail (Boundary_Exception);
   end GPE_Event;
   function Runnable return Boolean is (Last.Stage = Live and then Queue.Observe.Pending /= 0);
   procedure Advance (Server : in out Q.State; Result : out ACPI_Native_Server.Owner_Outcome) is
      Number : R.GPE_Number := R.GPE_Number'First;
      Item : G.Methods.Binding; Found, Opened, Completed, Drained : Boolean;
      Claimed : Queue.Claim_Result; Finished : Queue.Finish_Result;
      Now : Interfaces.Unsigned_64; Clock_Available : Boolean;
      Window_Result : One.Runtime.Status.Outcome;
      type Source_ID is record Present : Boolean := False; Number : R.GPE_Number := 0; end record;
      No_Source : constant Source_ID := (others => <>);
      procedure Mask (Source : Source_ID; IO_Result : out L.IO_Outcome) is
         IO : ACPI_SCI_Events.IO_Outcome;
      begin
         IO_Result := L.Refused; if not Source.Present then return; end if;
         Controls (Source.Number) := Unknown; ACPI_SCI_Events.Mask_GPE (Source.Number, IO);
         Controls (Source.Number) := (if IO = ACPI_SCI_Events.Completed then Known_Masked else Unknown);
         IO_Result := (if IO = ACPI_SCI_Events.Completed then L.Completed else L.Completion_Unknown);
      end Mask;
      procedure Clear (Source : Source_ID; IO_Result : out L.IO_Outcome) is
         IO : ACPI_SCI_Events.IO_Outcome;
      begin
         IO_Result := L.Refused; if not Source.Present then return; end if;
         ACPI_SCI_Events.Clear_GPE (Source.Number, IO);
         IO_Result := (if IO = ACPI_SCI_Events.Completed then L.Completed else L.Completion_Unknown);
      end Clear;
      procedure Enable (Source : Source_ID; IO_Result : out L.IO_Outcome) is
         IO : ACPI_SCI_Events.IO_Outcome;
      begin
         IO_Result := L.Refused; if not Source.Present then return; end if;
         Controls (Source.Number) := Unknown; ACPI_SCI_Events.Enable_GPE (Source.Number, IO);
         Controls (Source.Number) := (if IO = ACPI_SCI_Events.Completed then Known_Enabled else Unknown);
         IO_Result := (if IO = ACPI_SCI_Events.Completed then L.Completed else L.Completion_Unknown);
      end Enable;
      procedure Invoke (Source : Source_ID; Budget : Natural; Outcome : out L.Handler_Outcome; Charged : out Natural) is
         Allowed : G.Requests.Controller.Admission;
      begin
         Outcome := L.Handler_Failed; Charged := 0;
         if not Source.Present or else Source.Number /= Number then return; end if;
         G.Dispatch (Server, Item, Budget, Allowed, Completed);
         Charged := G.Delivery.Observation.Reported_Charge;
         if Allowed /= G.Requests.Controller.Admitted then return; end if;
         if Completed then Outcome := L.Handler_Succeeded;
         elsif G.Delivery.Observation.Status = G.Methods.Budget_Exhausted then Outcome := L.Handler_Budget_Exhausted;
         elsif G.Delivery.Observation.Status in G.Methods.Indeterminate | G.Methods.Provider_Contract_Error then Outcome := L.Handler_Completion_Unknown;
         end if;
      end Invoke;
      package Driver is new L.Driver (Source_ID, No_Source, Mask, Clear, Enable, Invoke);
      Run : Driver.Session; Action : L.Action_Result;
      procedure Refuse (Why : Failure) is
      begin
         Queue.Finish (Active_Ticket, False, Finished); Fail (Why);
         Working := False; Result := ACPI_Native_Server.Owner_Refused;
      end Refuse;
   begin
      Result := ACPI_Native_Server.Owner_Idle;
      if Working or else Last.Stage = Failed then Result := ACPI_Native_Server.Owner_Refused; return; end if;
      if Last.Stage /= Live or else not Runnable then return; end if;
      if not Control_Exclusive then Fail (Control_Not_Exclusive); Result := ACPI_Native_Server.Owner_Refused; return; end if;
      if not Ready (Server) then Result := ACPI_Native_Server.Owner_Pending; return; end if;
      Working := True; Queue.Claim (Active_Ticket, Claimed);
      if Claimed /= Queue.Claimed then Fail (Queue_Refused); Working := False; Result := ACPI_Native_Server.Owner_Refused; return; end if;
      Number := R.GPE_Number (Queue.Source (Active_Ticket)); Last.At_Source := Number;
      I.Lookup (Number, Item, Found);
      if not Found or else not G.Methods.Is_Bound (Item) then Refuse (Inventory_Refused); return; end if;
      Driver.Start (Run, (True, Number), G.Methods.Trigger (Item), Action);
      if Action /= L.Accepted or else Driver.Observe (Run).Stage /= L.Ready then Refuse (Source_IO_Refused); return; end if;
      ACPI_Clock_Source.Read (Now, Clock_Available);
      if not Clock_Available or else Now > Interfaces.Unsigned_64'Last - Window_Microseconds then Refuse (Clock_Refused); return; end if;
      Last.Window_Open := True;
      One.Runtime_Access.Begin_Event (Server, Now + Window_Microseconds, Window_Result);
      Opened := Window_Result = One.Runtime.Status.Event_Opened; Last.Window_Open := Opened;
      if not Opened then Refuse (Window_Refused); return; end if;
      Completed := False; Driver.Service (Run, Handler_Fuel, Action);
      Completed := Action = L.Accepted and then Driver.Observe (Run).Stage = L.Handler_Done
        and then not G.Delivery.Pending_Result and then not Q.Retained_Results_Pending (Server)
        and then One.Root.Fields.Control_Idle and then One.Root.Fields.EC_Commands.Quiescent;
      One.Runtime_Access.End_Event (Server, Completed, Window_Result);
      Last.Window_Open := One.Runtime.Current = One.Runtime.Status.Active;
      if not Completed then Refuse (Handler_Refused); return; end if;
      if Window_Result /= One.Runtime.Status.Event_Completed then Refuse (Window_Refused); return; end if;
      Drain_Notifications (Server, Drained);
      if not Drained or else Q.Notifications_Pending (Server) then Refuse (Routing_Refused); return; end if;
      if not Ready (Server) then Refuse (Not_Ready); return; end if;
      if not Control_Exclusive then Refuse (Control_Not_Exclusive); return; end if;
      Driver.Finish (Run, Action);
      if Action /= L.Accepted or else Driver.Observe (Run).Stage /= L.Finished then Refuse (Source_IO_Refused); return; end if;
      Queue.Finish (Active_Ticket, True, Finished);
      if Finished /= Queue.Completed then Fail (Queue_Refused); Working := False; Result := ACPI_Native_Server.Owner_Refused; return; end if;
      if Last.Deliveries < Interfaces.Unsigned_64'Last then Last.Deliveries := Last.Deliveries + 1; end if;
      Working := False;
      Result := (if Runnable then ACPI_Native_Server.Owner_Pending else ACPI_Native_Server.Owner_Idle);
   exception when others =>
      for Source in R.GPE_Number loop if Owned (Source) then Controls (Source) := Unknown; end if; end loop;
      if Queue.Observe.Active then Queue.Finish (Active_Ticket, False, Finished); end if;
      Fail (Boundary_Exception); Working := False; Result := ACPI_Native_Server.Owner_Refused;
   end Advance;
   procedure Stop is IO : ACPI_SCI_Events.IO_Outcome;
   begin
      if Working then return; end if;
      Queue.Begin_Stop; if Last.Stage /= Failed then Last.Stage := Stopping; end if;
      for Source in R.GPE_Number loop
         if Owned (Source) and then Controls (Source) /= Known_Masked then
            Controls (Source) := Unknown; ACPI_SCI_Events.Mask_GPE (Source, IO);
            Controls (Source) := (if IO = ACPI_SCI_Events.Completed then Known_Masked else Unknown);
         end if;
      end loop;
   exception when others => Fail (Boundary_Exception);
   end Stop;
   procedure Confirm_Disarm (Result : ACPI_SCI_Quiesce.Result) is
   begin
      if not Working and then Last.Stage in Stopping | Failed and then Result.State = ACPI_SCI_Quiesce.Quiesced then
         for Source in R.GPE_Number loop if Owned (Source) then Controls (Source) := Known_Masked; end if; end loop;
      end if;
   end Confirm_Disarm;
   procedure Close (Server : in out Q.State; Complete : out Boolean) is
      Action : G.Methods.Action_Result; Retired : Queue.Retire_Result; Count : Queue.Pending_Count;
   begin
      Complete := False; if Working then return; end if; Stop;
      if Last.Sources = 0 and then not Queue.Observe.Active and then Queue.Observe.Pending = 0
        and then not G.Delivery.Pending_Result and then not Last.Window_Open then
         Last.Stage := Stopped; Complete := True; return;
      end if;
      if not Q.Valid (Server) then return; end if;
      if G.Delivery.Pending_Result then G.Close_Result (Server, Action); end if;
      if not Q.Valid (Server) then return; end if;
      if G.Delivery.Pending_Result or else Q.Retained_Results_Pending (Server)
        or else Q.Notifications_Pending (Server) or else One.Runtime.Current in One.Runtime.Status.Fresh | One.Runtime.Status.Active
        or else not One.Root.Fields.Control_Idle or else not One.Root.Fields.EC_Commands.Quiescent then return; end if;
      for Source in R.GPE_Number loop if Owned (Source) and then Controls (Source) /= Known_Masked then return; end if; end loop;
      if Queue.Observe.Active then Queue.Retire_Active (Active_Ticket, Retired); if Retired /= Queue.Retired then return; end if; end if;
      Queue.Discard_Queued (Count); Last.Window_Open := False; Last.Stage := Stopped; Complete := True;
   exception when others => Fail (Boundary_Exception);
   end Close;
end ACPI_Native_GPE_Source;
