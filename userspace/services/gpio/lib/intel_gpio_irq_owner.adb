pragma Ada_2022;
package body Intel_GPIO_IRQ_Owner with SPARK_Mode is
   use type G.Outcome;
   use type I.Tick;
   use type I.Count;
   use type I.Word;
   use type I.Access_Status;
   function Observe (S : State) return Report is (S.Info);
   procedure Fail (S : in out State; Why : Failure) is
   begin
      S.Info.Failed_At := S.Info.At_Phase; S.Info.Reason := Why; S.Info.At_Phase := Faulted;
   end Fail;
   procedure Bound
     (S : in out State; Deadline : I.Tick; Budget : I.Limit; Result : out Admission) is
      Now : I.Tick;
      Available : Boolean;
   begin
      Clock (Now, Available);
      if not Available or else Now < S.Previous then Result := Clock_Refused; return; end if;
      if Now >= Deadline then Result := Expired; return; end if;
      S.Previous := Now; S.Deadline := Deadline; S.Remaining := Budget; Result := Accepted;
   end Bound;
   procedure Start
     (S : in out State; Plan : G.Plan_Result; Mode : I.Trigger;
      Deadline : I.Tick; Budget : I.Limit; Result : out Admission) is
   begin
      if S.Info.At_Phase /= Cold then Result := Busy; return; end if;
      if Plan.Status /= G.Ready then Result := Invalid_Plan; return; end if;
      Bound (S, Deadline, Budget, Result); if Result /= Accepted then return; end if;
      S.Plan := Plan; S.Mode := Mode; S.Info.At_Phase := Read_Owner;
   end Start;
   procedure Start_Parked
     (S : in out State; Plan : G.Plan_Result; Mode : I.Trigger;
      Deadline : I.Tick; Budget : I.Limit; Result : out Admission) is
   begin
      Start (S, Plan, Mode, Deadline, Budget, Result);
      if Result = Accepted then S.Initial_Masked := True; end if;
   end Start_Parked;
   procedure Retarget_Parked
     (S : in out State; Plan : G.Plan_Result; Mode : I.Trigger;
      Deadline : I.Tick; Budget : I.Limit; Result : out Admission) is
      Previous_Event : constant I.Ticket := S.Info.Event;
   begin
      if S.Info.At_Phase /= Held_Masked or else S.Info.Pin_May_Be_Enabled or else S.Info.Event_Pending
      then Result := Busy; return; end if;
      if Plan.Status /= G.Ready then Result := Invalid_Plan; return; end if;
      Bound (S, Deadline, Budget, Result); if Result /= Accepted then return; end if;
      S.Plan := Plan; S.Mode := Mode;
      S.Info := (At_Phase => Read_Owner, Event => Previous_Event, others => <>);
      S.Owner := 0; S.Host := 0; S.Desired_Config := 0; S.Desired_Enable := 0;
      S.Rearming := False; S.Pausing := False; S.Initial_Masked := True;
   end Retarget_Parked;
   procedure Poll
     (S : in out State; Deadline : I.Tick; Budget : I.Limit; Result : out Admission) is
   begin
      if S.Info.At_Phase /= Listening then Result := Busy; return; end if;
      Bound (S, Deadline, Budget, Result); if Result = Accepted then S.Info.At_Phase := Poll_Status; end if;
   end Poll;
   procedure Pause (S : in out State; Deadline : I.Tick; Budget : I.Limit; Result : out Admission) is
   begin
      if S.Info.At_Phase /= Listening then Result := Busy; return; end if;
      Bound (S, Deadline, Budget, Result); if Result /= Accepted then return; end if;
      S.Pausing := True; S.Rearming := False; S.Info.At_Phase := Read_Owner;
   end Pause;
   procedure Resume (S : in out State; Deadline : I.Tick; Budget : I.Limit; Result : out Admission) is
   begin
      if S.Info.At_Phase /= Held_Masked then Result := Busy; return; end if;
      Bound (S, Deadline, Budget, Result); if Result /= Accepted then return; end if;
      S.Pausing := False; S.Rearming := True; S.Info.At_Phase := Read_Owner;
   end Resume;
   procedure Retire_Event (S : in out State; Event : I.Ticket; Result : out Admission) is
   begin
      if S.Info.At_Phase /= Event_Ready then Result := Busy; return; end if;
      if Event = I.No_Ticket or else Event /= S.Info.Event then Result := Wrong_Ticket; return; end if;
      S.Info.Event_Pending := False; S.Pausing := True; S.Rearming := False;
      S.Info.At_Phase := Held_Masked; Result := Accepted;
   end Retire_Event;
   procedure Rearm
     (S : in out State; Event : I.Ticket;
      Deadline : I.Tick; Budget : I.Limit; Result : out Admission) is
   begin
      if S.Info.At_Phase /= Event_Ready then Result := Busy; return; end if;
      if Event = I.No_Ticket or else Event /= S.Info.Event then Result := Wrong_Ticket; return; end if;
      Bound (S, Deadline, Budget, Result); if Result /= Accepted then return; end if;
      S.Rearming := True; S.Pausing := False; S.Info.Event_Pending := False; S.Info.At_Phase := Read_Owner;
   end Rearm;
   procedure Time_OK (S : in out State; OK : out Boolean) is
      Now : I.Tick;
      Available : Boolean;
   begin
      Clock (Now, Available); OK := False;
      if not Available then Fail (S, Clock_Unavailable);
      elsif Now < S.Previous then Fail (S, Clock_Reversed);
      elsif Now >= S.Deadline then Fail (S, Deadline_Expired);
      else S.Previous := Now; OK := True;
      end if;
   end Time_OK;
   procedure Advance (S : in out State) is
      OK : Boolean;
      Value : I.Word := 0;
      Status : I.Access_Status;
      ID : I.Read_ID := I.Pad_Owner;
      Write_ID : I.Write_ID := I.Write_Enable;
      Writing : Boolean := False;
      Mask : I.Word;
      Access_Phase : Phase;
   begin
      if S.Info.At_Phase in Cold | Listening | Event_Ready | Held_Masked | Faulted then return; end if;
      Access_Phase := S.Info.At_Phase;
      S.Info.Access_Attempted := False;
      if S.Remaining = 0 then Fail (S, Budget_Exhausted); return; end if;
      S.Remaining := S.Remaining - 1;
      Time_OK (S, OK); if not OK then return; end if;
      Mask := S.Plan.Selected.Interrupt_Mask;
      case S.Info.At_Phase is
         when Read_Owner => ID := I.Pad_Owner;
         when Read_Host => ID := I.Host_Owner;
         when Read_Lock => ID := I.Configuration_Lock;
         when Read_Config | Config_Confirm => ID := I.Pad_Configuration;
         when Read_Mask | Mask_Confirm | Enable_Read | Enable_Confirm |
           Event_Mask_Read | Event_Mask_Confirm | Park_Mask_Read | Park_Mask_Confirm => ID := I.Group_Enable;
         when Poll_Status => ID := I.Group_Status;
         when Mask_Write | Enable_Write | Event_Mask_Write | Park_Mask_Write =>
            Writing := True; Value := S.Desired_Enable;
         when Config_Write => Writing := True; Write_ID := I.Write_Configuration; Value := S.Desired_Config;
         when Clear_Stale | Acknowledge =>
            if S.Info.At_Phase = Acknowledge and then S.Info.Event = I.Ticket'Last then
               Fail (S, Tickets_Exhausted); return;
            end if;
            Writing := True; Write_ID := I.Clear_Status; Value := Mask;
         when others => return;
      end case;
      if Writing then
         Write (Write_ID, Value, Status); S.Info.Last_Write := Write_ID;
         if Status in I.Access_OK | I.Unknown_Effect then S.Info.Effects_Possible := True; end if;
         if S.Info.At_Phase = Enable_Write and then Status in I.Access_OK | I.Unknown_Effect then
            S.Info.Pin_May_Be_Enabled := True;
         end if;
      else Read (ID, Value, Status); S.Info.Last_Read := ID;
      end if;
      S.Info.Was_Write := Writing; S.Info.Last_Value := Value;
      S.Info.Last_Status := Status; S.Info.Access_Attempted := True;
      if Status /= I.Access_OK then
         Fail (S, (if Status = I.Unknown_Effect then Register_Indeterminate else Register_Refused)); return;
      end if;
      case S.Info.At_Phase is
         when Read_Owner => S.Owner := Value; S.Info.At_Phase := Read_Host;
         when Read_Host => S.Host := Value; S.Info.At_Phase := Read_Lock;
         when Read_Lock =>
            if not G.Interrupt_Eligible (S.Plan.Selected, S.Owner, S.Host, Value)
            then Fail (S, Ownership_Refused);
            else S.Info.At_Phase := (if S.Pausing then Park_Mask_Read elsif S.Rearming then Enable_Read else Read_Config);
            end if;
         when Park_Mask_Read => S.Desired_Enable := Value and not Mask; S.Info.At_Phase := Park_Mask_Write;
         when Park_Mask_Write => S.Info.At_Phase := Park_Mask_Confirm;
         when Park_Mask_Confirm =>
            if (Value and Mask) /= 0 then Fail (S, Readback_Refused);
            else S.Info.Pin_May_Be_Enabled := False; S.Info.At_Phase := Held_Masked;
            end if;
         when Read_Config => S.Desired_Config := I.Input_Configuration (Value, S.Mode); S.Info.At_Phase := Read_Mask;
         when Read_Mask => S.Desired_Enable := Value and not Mask; S.Info.At_Phase := Mask_Write;
         when Mask_Write => S.Info.At_Phase := Mask_Confirm;
         when Mask_Confirm | Event_Mask_Confirm =>
            if (Value and Mask) /= 0 then Fail (S, Readback_Refused);
            else
               S.Info.Pin_May_Be_Enabled := False;
               S.Info.At_Phase := (if S.Info.At_Phase = Mask_Confirm then Config_Write else Acknowledge);
            end if;
         when Config_Write => S.Info.At_Phase := Config_Confirm;
         when Config_Confirm =>
            if ((Value xor S.Desired_Config) and I.Changed_Fields) /= 0 then Fail (S, Readback_Refused);
            else S.Info.At_Phase := Clear_Stale;
            end if;
         when Clear_Stale => S.Info.At_Phase := (if S.Initial_Masked then Held_Masked else Enable_Read);
         when Enable_Read => S.Desired_Enable := Value or Mask; S.Info.At_Phase := Enable_Write;
         when Enable_Write => S.Info.At_Phase := Enable_Confirm;
         when Enable_Confirm =>
            if (Value and Mask) = 0 then Fail (S, Readback_Refused);
            else S.Rearming := False; S.Info.At_Phase := Listening;
            end if;
         when Poll_Status =>
            S.Info.At_Phase := (if (Value and Mask) = 0 then Listening else Event_Mask_Read);
         when Event_Mask_Read => S.Desired_Enable := Value and not Mask; S.Info.At_Phase := Event_Mask_Write;
         when Event_Mask_Write => S.Info.At_Phase := Event_Mask_Confirm;
         when Acknowledge =>
            S.Info.Event := S.Info.Event + 1; S.Info.Event_Pending := True; S.Info.At_Phase := Event_Ready;
         when others => null;
      end case;
      if S.Info.At_Phase /= Faulted then
         Time_OK (S, OK);
         if not OK then S.Info.Failed_At := Access_Phase; end if;
      end if;
   end Advance;
end Intel_GPIO_IRQ_Owner;
