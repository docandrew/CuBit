pragma Ada_2022;
package body GPIO_Service_Loop with SPARK_Mode is
   package G renames C.G;
   package T renames GPIO_Community_Types;
   use type P.Process_ID, C.Pin_ID, C.Admission, I.Trigger, GPIO_Controller_Resources.Pin_Count;
   use type C.Engine.Phase;
   use type B.Decode_Status, B.Trigger_Mode, B.Polarity, T.Pin_Admission, G.Outcome;
   use type W.Word, G.Location, Channels.Admission, Channels.Binding_Phase, Channels.Lifecycle;
   function Stable return Boolean is
     (not Layout_Fault and then GPIO_Controller_Resources.Pin_Count (Count) = C.Observe.Registered);
   function Operational return Boolean is
   begin
      if not Is_Sealed or else not Stable or else C.Observe.Faulted then return False; end if;
      return True;
   end Operational;
   function Mode_Of (Input : B.Interrupt_Input) return I.Trigger is
     (if Input.Trigger = B.Level_Triggered then
        (if Input.Active = B.Active_Low then I.Level_Low else I.Level_High)
      else (case Input.Active is when B.Active_Low => I.Falling_Edge,
            when B.Active_High => I.Rising_Edge, when B.Active_Both => I.Both_Edges));
   procedure Seal (Accepted : out Boolean) is
   begin
      Accepted := False;
      if Is_Sealed or else Stopping or else not Startup_Parked or else C.Observe.Faulted or else
         C.Observe.Active /= C.No_Pin or else C.Observe.Registered /= 0 then return; end if;
      Is_Sealed := True; Accepted := True;
   end Seal;
   function Find_Pin (Badge : W.Word) return Channel_ID is
      Parsed : constant B.Decoding := B.Decode (Badge);
   begin
      if Parsed.Status /= B.Decoded then return No_Channel; end if;
      for Index in 1 .. Count loop
         if Phases (Index) /= Free and then C.Observe_Pin (C.Pin_ID (Index)).Plan.Selected = G.Locate (G.ACPI_Pin (Parsed.Value.Pin)) then return Index; end if;
      end loop;
      return No_Channel;
   end Find_Pin;
   procedure Enroll (Owner : P.Process_ID; Badge : W.Word; Sink : Event_Sink;
      Setup_Deadline : I.Tick; ID : out Channel_ID; Result : out Admission) is
      Parsed : constant B.Decoding := B.Decode (Badge);
      Selected : Channel_ID := No_Channel;
      Pin : C.Pin_ID;
      Plan : G.Plan_Result;
      Geometry : T.Pin_Admission;
      Registered : C.Admission;
      Engine_Result : C.Engine.Admission;
   begin
      ID := No_Channel;
      if not Operational or else Stopping then Result := Not_Ready;
      elsif not P.Is_Process (Owner) then Result := Invalid_Owner;
      elsif Parsed.Status /= B.Decoded then Result := Invalid_Badge;
      else
         Selected := Find_Pin (Badge);
         if Selected /= No_Channel then
            Result := (if C.Observe_Pin (C.Pin_ID (Selected)).Mode /= Mode_Of (Parsed.Value)
               then Trigger_Mismatch else Already_Bound); return;
         end if;
         Admit_Pin (G.ACPI_Pin (Parsed.Value.Pin), Geometry, Plan);
         if Geometry not in T.Admitted | T.Already_Admitted or else Plan.Status /= G.Ready or else
            Plan.Selected /= G.Locate (G.ACPI_Pin (Parsed.Value.Pin))
         then Result := Unknown_Pin; return; end if;
         for Index in 1 .. Count loop
            if Phases (Index) = Free and then C.Released (C.Pin_ID (Index)) then Selected := Index; exit; end if;
         end loop;
         if Selected = No_Channel then
            if Count = Channel_Count (Capacity) then Result := Busy; return; end if;
            C.Register_Parked (Plan, Mode_Of (Parsed.Value), Setup_Deadline, Channels.Operation_Budget,
               Pin, Registered, Engine_Result);
            if Registered /= C.Accepted then
               Result := (if Registered = C.Busy then Busy else Bind_Refused); return;
            end if;
            Selected := Count + 1; Count := Selected;
            if Pin /= C.Pin_ID (Selected) then Layout_Fault := True; Result := Bind_Refused; Phases (Selected) := Unresolved; return; end if;
         else
            C.Retarget_Parked (C.Pin_ID (Selected), Plan, Mode_Of (Parsed.Value), Setup_Deadline,
               Channels.Operation_Budget, Registered, Engine_Result);
            if Registered /= C.Accepted then
               Result := (if Registered = C.Busy then Busy else Bind_Refused); return;
            end if;
         end if;
         Owners (Selected) := Owner; Badges (Selected) := Badge; Sinks (Selected) := Sink;
         Setup_Deadlines (Selected) := Setup_Deadline; Stop_Pending (Selected) := False;
         Phases (Selected) := Registering; ID := Selected; Result := Accepted;
      end if;
   end Enroll;
   function Match (Owner : P.Process_ID; Badge : W.Word) return Channel_ID is
   begin
      if not Is_Sealed or else not Stable or else not P.Is_Process (Owner) then return No_Channel; end if;
      for Index in 1 .. Count loop
         if Phases (Index) /= Free and then Owners (Index) = Owner and then Badges (Index) = Badge then return Index; end if;
      end loop;
      return No_Channel;
   end Match;
   procedure Retire (Owner : P.Process_ID; Badge : W.Word; Cleanup_Deadline : I.Tick;
      ID : out Channel_ID; Result : out Admission) is
   begin
      ID := Match (Owner, Badge);
      if ID = No_Channel then Result := Wrong_Binding;
      elsif Phases (ID) = Registering then
         if Stop_Pending (ID) then Result := Busy;
         else Stop_Pending (ID) := True; Cleanup_Deadlines (ID) := Cleanup_Deadline; Result := Accepted; end if;
      elsif Phases (ID) not in Binding | Live then Result := Busy;
      else Channels.Request_Stop (States (ID), Cleanup_Deadline); Phases (ID) := Retiring; Result := Accepted;
      end if;
   end Retire;
   procedure Acknowledge_Retirement (ID : Channel_ID; Accepted : out Boolean) is
   begin
      Accepted := False;
      if ID = No_Channel or else ID > Count or else Phases (ID) /= Retired then return; end if;
      Owners (ID) := P.No_Process; Badges (ID) := 0; Phases (ID) := Free; Accepted := True;
   end Acknowledge_Retirement;
   function Observe return Summary is
      Info : Summary := (Registered => Count, Sealed => Is_Sealed, Operational => Operational,
         Stopping => Stopping, Layout_Changed => Is_Sealed and then not Stable,
         Next_Channel => (if Count = 0 then No_Channel else Cursor), others => <>);
   begin
      for Index in 1 .. Count loop
         case Phases (Index) is
            when Registering | Binding | Live | Retiring => Info.Bound := Info.Bound + 1;
            when Retired => Info.Retired := Info.Retired + 1;
            when Close_Failed | Unresolved => Info.Unresolved := Info.Unresolved + 1;
            when Free => null;
         end case;
      end loop;
      return Info;
   end Observe;
   function Observe_Channel (ID : Channel_ID) return Binding_Report is
     ((Phases (ID), Owners (ID), Badges (ID), Channels.Observe (States (ID))));
   function Lookup (Caller : P.Process_ID; Badge : W.Word) return Channel_ID is
      ID : constant Channel_ID := Match (Caller, Badge);
   begin
      if not Operational or else Stopping or else ID = No_Channel or else Phases (ID) /= Live then return No_Channel; end if;
      return ID;
   end Lookup;
   function Authorized (Caller : P.Process_ID; Badge : W.Word) return Boolean is (Lookup (Caller, Badge) /= No_Channel);
   procedure Offer (Caller : P.Process_ID; Badge : W.Word; Item : W.Request;
      Reply : Reply_Authority; Taken : out Boolean) is
      ID : constant Channel_ID := Lookup (Caller, Badge);
      Result : Channels.Admission;
   begin
      Taken := False; if ID = No_Channel then return; end if;
      Channels.Offer (States (ID), Caller, Item, Reply, Result); Taken := Result = Channels.Accepted;
   end Offer;
   function Polling_Needed return Boolean is
   begin
      if not Operational or else Stopping then return False; end if;
      for Index in 1 .. Count loop
         if Phases (Index) = Live and then Channels.Polling_Needed (States (Index)) then return True; end if;
      end loop;
      return False;
   end Polling_Needed;
   procedure Poll_All (Deadline : I.Tick) is
      Accepted : Boolean;
   begin
      if not Operational or else Stopping then return; end if;
      for Index in 1 .. Count loop
         if Phases (Index) = Live then Channels.Poll (States (Index), Deadline, Accepted); end if;
      end loop;
   end Poll_All;
   procedure Request_Stop (Cleanup_Deadline : I.Tick) is
   begin
      if Stopping then return; end if;
      Stopping := True;
      for Index in 1 .. Count loop
         if Phases (Index) = Registering then
            if not Stop_Pending (Index) then Stop_Pending (Index) := True; Cleanup_Deadlines (Index) := Cleanup_Deadline; end if;
         elsif Phases (Index) in Binding | Live then
            Channels.Request_Stop (States (Index), Cleanup_Deadline); Phases (Index) := Retiring;
         end if;
      end loop;
   end Request_Stop;
   procedure Step (Controller_Worked : out Boolean) is
      Closed, Detached, Configured : Boolean;
      Released : C.Admission;
      Info : Channels.Report;
   begin
      Controller_Worked := False;
      if not Is_Sealed or else not Stable then return; end if;
      C.Advance (Controller_Worked);
      if Count = 0 then return; end if;
      case Phases (Cursor) is
         when Registering =>
            if C.Observe.Faulted then Phases (Cursor) := Unresolved;
            else
               declare Pin : constant C.Pin_Observation := C.Observe_Pin (C.Pin_ID (Cursor)); begin
                  if not Pin.Present or else Pin.Report.At_Phase = C.Engine.Faulted then Phases (Cursor) := Unresolved;
                  elsif Pin.Report.At_Phase = C.Engine.Held_Masked and then not Pin.Report.Pin_May_Be_Enabled then
                     Channels.Configure (States (Cursor), C.Pin_ID (Cursor), Owners (Cursor), Sinks (Cursor),
                        Setup_Deadlines (Cursor), Configured);
                     if not Configured then Phases (Cursor) := Unresolved;
                     elsif Stop_Pending (Cursor) then
                        Channels.Request_Stop (States (Cursor), Cleanup_Deadlines (Cursor)); Phases (Cursor) := Retiring;
                     else Phases (Cursor) := Binding; end if;
                  end if;
               end;
            end if;
         when Binding | Live | Retiring =>
            Channels.Step (States (Cursor)); Info := Channels.Observe (States (Cursor));
            if Phases (Cursor) = Binding then
               if C.Observe.Faulted or else Info.Binding = Channels.Failed then Phases (Cursor) := Unresolved;
               elsif Info.Binding = Channels.Ready then Phases (Cursor) := Live; end if;
            elsif Phases (Cursor) = Retiring then
               if C.Observe.Faulted or else Info.Life = Channels.Unresolved then Phases (Cursor) := Unresolved;
               elsif Info.Life = Channels.Stopped and then Channels.Quiescent (States (Cursor)) and then
                  C.Observe.Active = C.No_Pin then
                  Close_Sink (Sinks (Cursor), Closed);
                  if not Closed then Phases (Cursor) := Close_Failed;
                  else
                     Channels.Detach (States (Cursor), Detached);
                     if not Detached then Phases (Cursor) := Unresolved;
                     else
                        C.Release_Parked (C.Pin_ID (Cursor), Released);
                        Phases (Cursor) := (if Released = C.Accepted then Retired else Unresolved);
                     end if;
                  end if;
               end if;
            end if;
         when Free | Retired | Close_Failed | Unresolved => null;
      end case;
      Cursor := (if Cursor = Count then Slot'First else Cursor + 1);
   end Step;
   function Terminal return Boolean is
   begin
      if not Stopping then return False; end if;
      for Index in 1 .. Count loop
         if Phases (Index) in Registering | Binding | Live | Retiring then return False; end if;
      end loop;
      return True;
   end Terminal;
   function Quiescent return Boolean is
   begin
      if not Operational or else C.Observe.Active /= C.No_Pin then return False; end if;
      for Index in 1 .. Count loop
         if Phases (Index) in Registering | Binding | Retiring | Close_Failed | Unresolved then return False; end if;
         if Phases (Index) = Live then
            if not Channels.Quiescent (States (Index)) then return False; end if;
         else
            declare Pin : constant C.Pin_Observation := C.Observe_Pin (C.Pin_ID (Index)); begin
               if not Pin.Present or else Pin.Report.At_Phase /= C.Engine.Held_Masked or else Pin.Report.Pin_May_Be_Enabled then return False; end if;
            end;
         end if;
      end loop;
      return True;
   end Quiescent;
   function Needs_Step return Boolean is
   begin
      if not Is_Sealed or else not Stable then return False; end if;
      if C.Observe.Active /= C.No_Pin then return True; end if;
      for Index in 1 .. Count loop
         if Phases (Index) in Registering | Binding | Retiring then return True; end if;
         if Phases (Index) = Live and then Channels.Needs_Step (States (Index)) then return True; end if;
      end loop;
      return False;
   end Needs_Step;
end GPIO_Service_Loop;
