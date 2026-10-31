pragma Ada_2022;
package body Intel_GPIO_Controller with SPARK_Mode is
   use type GPIO_Controller_Resources.Pin_Count;
   use type G.Outcome;
   use type G.Pad_Number;
   use type G.Memory_Resource_Ordinal;
   use type Engine.Phase;
   use type Engine.Admission;
   subtype Slot is Pin_ID range 1 .. Pin_ID (Capacity);
   type Engine_Array is array (Slot) of Engine.State;
   States : Engine_Array;
   Plans : array (Slot) of G.Plan_Result;
   Modes : array (Slot) of I.Trigger := [others => I.Level_High];
   Info : Summary;
   Free : array (Slot) of Boolean := [others => False];
   In_Operation, In_Step : Boolean := False;
   Current : G.Plan_Result;
   function Observe return Summary is (Info);
   function Known (Pin : Pin_ID) return Boolean is
     (Pin /= No_Pin and then GPIO_Controller_Resources.Pin_Count (Pin) <= Info.Registered);
   function Released (Pin : Pin_ID) return Boolean is (Known (Pin) and then Free (Pin));
   function Observe_Pin (Pin : Pin_ID) return Pin_Observation is
   begin
      if not Known (Pin) then return (Present => False); end if;
      return (Present => True, Plan => Plans (Pin), Mode => Modes (Pin), Report => Engine.Observe (States (Pin)));
   end Observe_Pin;
   procedure Dispatched_Read
     (ID : I.Read_ID; Value : out I.Word; Status : out I.Access_Status) is
   begin
      Value := 0; Status := I.Unavailable;
      if not In_Step or else Current.Status /= G.Ready then return; end if;
      Read_Register (Current, ID, Value, Status);
   end Dispatched_Read;
   procedure Dispatched_Write (ID : I.Write_ID; Value : I.Word; Status : out I.Access_Status) is
   begin
      Status := I.Unavailable;
      if not In_Step or else Current.Status /= G.Ready then return; end if;
      Write_Register (Current, ID, Value, Status);
   end Dispatched_Write;
   function Gate return Admission is
     (if Info.Faulted then Controller_Faulted
      elsif In_Operation or else Info.Active /= No_Pin then Busy else Accepted);
   procedure Register_Internal
     (Plan : G.Plan_Result; Mode : I.Trigger; Deadline : I.Tick; Budget : I.Limit;
      Pin : out Pin_ID; Result : out Admission; Owner_Result : out Engine.Admission;
      Parked : Boolean) is
      Next : Pin_ID;
   begin
      Pin := No_Pin; Owner_Result := Engine.Busy; Result := Gate;
      if Result /= Accepted then return; end if;
      if Plan.Status /= G.Ready then Result := Invalid_Plan; return; end if;
      for Previous in 1 .. Pin_ID (Info.Registered) loop
         if not Free (Previous) and then Plans (Previous).Selected.Pad = Plan.Selected.Pad and then
           Plans (Previous).Selected.Resource = Plan.Selected.Resource
         then Result := Duplicate_Pin; return; end if;
      end loop;
      if Info.Registered = GPIO_Controller_Resources.Pin_Count (Capacity)
      then Result := Capacity_Refused; return; end if;
      Next := Pin_ID (Info.Registered) + 1;
      In_Operation := True;
      if Parked then Engine.Start_Parked (States (Next), Plan, Mode, Deadline, Budget, Owner_Result);
      else Engine.Start (States (Next), Plan, Mode, Deadline, Budget, Owner_Result); end if;
      In_Operation := False;
      if Owner_Result /= Engine.Accepted then Result := Owner_Refused; return; end if;
      Plans (Next) := Plan; Modes (Next) := Mode; Info.Registered := Info.Registered + 1;
      Info.Active := Next; Pin := Next; Result := Accepted;
   end Register_Internal;
   procedure Register_Pin
     (Plan : G.Plan_Result; Mode : I.Trigger; Deadline : I.Tick; Budget : I.Limit;
      Pin : out Pin_ID; Result : out Admission; Owner_Result : out Engine.Admission) is
   begin Register_Internal (Plan, Mode, Deadline, Budget, Pin, Result, Owner_Result, False); end Register_Pin;
   procedure Register_Parked
     (Plan : G.Plan_Result; Mode : I.Trigger; Deadline : I.Tick; Budget : I.Limit;
      Pin : out Pin_ID; Result : out Admission; Owner_Result : out Engine.Admission) is
   begin Register_Internal (Plan, Mode, Deadline, Budget, Pin, Result, Owner_Result, True); end Register_Parked;
   procedure Release_Parked (Pin : Pin_ID; Result : out Admission) is
      Item : Pin_Observation;
   begin
      Result := Gate; if Result /= Accepted then return; end if;
      if not Known (Pin) or else Free (Pin) then Result := Unknown_Pin; return; end if;
      Item := Observe_Pin (Pin);
      if Item.Report.At_Phase /= Engine.Held_Masked or else Item.Report.Pin_May_Be_Enabled or else
         Item.Report.Event_Pending then Result := Owner_Refused; return; end if;
      Free (Pin) := True;
   end Release_Parked;
   procedure Retarget_Parked
     (Pin : Pin_ID; Plan : G.Plan_Result; Mode : I.Trigger;
      Deadline : I.Tick; Budget : I.Limit; Result : out Admission; Owner_Result : out Engine.Admission) is
   begin
      Owner_Result := Engine.Busy; Result := Gate; if Result /= Accepted then return; end if;
      if not Released (Pin) then Result := Owner_Refused; return; end if;
      if Plan.Status /= G.Ready then Result := Invalid_Plan; return; end if;
      for Previous in 1 .. Pin_ID (Info.Registered) loop
         if not Free (Previous) and then Plans (Previous).Selected.Pad = Plan.Selected.Pad and then
            Plans (Previous).Selected.Resource = Plan.Selected.Resource
         then Result := Duplicate_Pin; return; end if;
      end loop;
      In_Operation := True;
      Engine.Retarget_Parked (States (Pin), Plan, Mode, Deadline, Budget, Owner_Result);
      In_Operation := False;
      if Owner_Result /= Engine.Accepted then Result := Owner_Refused; return; end if;
      Plans (Pin) := Plan; Modes (Pin) := Mode; Free (Pin) := False; Info.Active := Pin;
   end Retarget_Parked;
   procedure Poll
     (Pin : Pin_ID; Deadline : I.Tick; Budget : I.Limit;
      Result : out Admission; Owner_Result : out Engine.Admission) is
   begin
      Owner_Result := Engine.Busy; Result := Gate;
      if Result /= Accepted then return; end if;
      if not Known (Pin) or else Free (Pin) then Result := Unknown_Pin; return; end if;
      In_Operation := True; Engine.Poll (States (Pin), Deadline, Budget, Owner_Result);
      In_Operation := False;
      if Owner_Result /= Engine.Accepted then Result := Owner_Refused; return; end if;
      Info.Active := Pin;
   end Poll;
   procedure Pause (Pin : Pin_ID; Deadline : I.Tick; Budget : I.Limit;
      Result : out Admission; Owner_Result : out Engine.Admission) is
   begin
      Owner_Result := Engine.Busy; Result := Gate;
      if Result /= Accepted then return; end if;
      if not Known (Pin) or else Free (Pin) then Result := Unknown_Pin; return; end if;
      In_Operation := True; Engine.Pause (States (Pin), Deadline, Budget, Owner_Result);
      In_Operation := False;
      if Owner_Result /= Engine.Accepted then Result := Owner_Refused; return; end if;
      Info.Active := Pin;
   end Pause;
   procedure Resume (Pin : Pin_ID; Deadline : I.Tick; Budget : I.Limit;
      Result : out Admission; Owner_Result : out Engine.Admission) is
   begin
      Owner_Result := Engine.Busy; Result := Gate;
      if Result /= Accepted then return; end if;
      if not Known (Pin) or else Free (Pin) then Result := Unknown_Pin; return; end if;
      In_Operation := True; Engine.Resume (States (Pin), Deadline, Budget, Owner_Result);
      In_Operation := False;
      if Owner_Result /= Engine.Accepted then Result := Owner_Refused; return; end if;
      Info.Active := Pin;
   end Resume;
   procedure Retire_Event (Pin : Pin_ID; Event : I.Ticket;
      Result : out Admission; Owner_Result : out Engine.Admission) is
   begin
      Owner_Result := Engine.Busy; Result := Gate;
      if Result /= Accepted then return; end if;
      if not Known (Pin) or else Free (Pin) then Result := Unknown_Pin; return; end if;
      In_Operation := True; Engine.Retire_Event (States (Pin), Event, Owner_Result); In_Operation := False;
      if Owner_Result /= Engine.Accepted then Result := Owner_Refused; return; end if;
   end Retire_Event;
   procedure Rearm
     (Pin : Pin_ID; Event : I.Ticket; Deadline : I.Tick; Budget : I.Limit;
      Result : out Admission; Owner_Result : out Engine.Admission) is
   begin
      Owner_Result := Engine.Busy; Result := Gate;
      if Result /= Accepted then return; end if;
      if not Known (Pin) or else Free (Pin) then Result := Unknown_Pin; return; end if;
      In_Operation := True;
      Engine.Rearm (States (Pin), Event, Deadline, Budget, Owner_Result);
      In_Operation := False;
      if Owner_Result /= Engine.Accepted then Result := Owner_Refused; return; end if;
      Info.Active := Pin;
   end Rearm;
   procedure Advance (Worked : out Boolean) is
      Pin : Pin_ID;
      Report : Engine.Report;
   begin
      Worked := False;
      if In_Operation or else Info.Active = No_Pin or else Info.Faulted then return; end if;
      Pin := Info.Active; In_Operation := True; In_Step := True; Current := Plans (Pin);
      Engine.Advance (States (Pin)); Worked := True;
      In_Step := False; Current := (Status => G.No_GPIO_Pin); In_Operation := False;
      Report := Engine.Observe (States (Pin));
      if Report.At_Phase = Engine.Faulted then
         Info.Faulted := True; Info.Last_Fault := Pin; Info.Active := No_Pin;
      elsif Report.At_Phase in Engine.Listening | Engine.Event_Ready | Engine.Held_Masked then Info.Active := No_Pin;
      end if;
   end Advance;
end Intel_GPIO_Controller;
