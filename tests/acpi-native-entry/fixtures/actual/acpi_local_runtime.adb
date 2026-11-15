pragma Ada_2022;
package body ACPI_Local_Runtime is
   use type Core.Values.Access_Status;
   function Current return Phase is (State);
   function Observation return Outcome is (Last);
   procedure Close_Admission is
   begin
      if Claimed and then not Owner_Closed then
         Owner_Closed := True; Close_Owner;
      end if;
   end Close_Admission;
   procedure Stop (Reason : Outcome; Result : out Outcome) is
   begin
      State := Stopped; Close_Admission;
      if Claimed and then not Delays_Stopped then
         Delays_Stopped := True; Stop_Delays;
      end if;
      Stop_Requested := False; Last := Reason; Result := Reason;
   end Stop;
   procedure Enter_Runtime (Service : Core.State; Result : out Outcome) is
      Accepted : Boolean;
   begin
      Result := Rejected;
      if Busy or else State /= Fresh or else not Owner_Ready (Service)
        or else not Owner_Idle or else Core.Retained_Results (Service) /= 0 then return; end if;
      Busy := True; Claim_Owner (Service,Accepted);
      if not Accepted then
         -- No claim means no authority to close another facade's owner/delays.
         State := Stopped; Last := Owner_Refused; Result := Last;
      else
         Claimed := True;
         if Stop_Requested then Stop (Stopped_By_Owner,Result);
         else
            Enter_Delays (Accepted);
            if Stop_Requested then Stop (Stopped_By_Owner,Result);
            elsif not Accepted or else not Owner_Ready (Service) or else not Owner_Idle then Stop (Delay_Refused,Result);
            else State := Idle; Last := Runtime_Entered; Result := Last;
            end if;
         end if;
      end if;
      Busy := False;
   end Enter_Runtime;
   procedure Begin_Event (Service : Core.State; Deadline_Microseconds : Tick; Result : out Outcome) is
      Accepted : Boolean;
   begin
      Result := Rejected;
      if Busy or else State /= Idle or else not Owner_Ready (Service) or else not Owner_Idle
        or else Core.Retained_Results (Service) /= 0 or else Deadline_Microseconds in 0 | Tick'Last then return; end if;
      Busy := True; Begin_Delays (Deadline_Microseconds,Accepted);
      if Stop_Requested then Stop (Stopped_By_Owner,Result);
      elsif not Accepted or else not Owner_Ready (Service) or else not Owner_Idle then Stop (Delay_Refused,Result);
      else State := Active; Last := Event_Opened; Result := Last;
      end if;
      Busy := False;
   end Begin_Event;
   procedure End_Event (Service : Core.State; Completed : Boolean; Result : out Outcome) is
      Accepted : Boolean;
   begin
      Result := Rejected;
      if Busy or else State /= Active or else not Owner_Ready (Service) then return; end if;
      Busy := True;
      if not Completed then Stop (Evaluation_Failed,Result);
      elsif not Owner_Idle then Stop (IO_Pending,Result);
      elsif Core.Retained_Results (Service) /= 0 then Stop (Retained_Results_Pending,Result);
      else
         End_Delays (Accepted);
         if Stop_Requested then Stop (Stopped_By_Owner,Result);
         elsif not Accepted or else not Owner_Ready (Service) or else not Owner_Idle then Stop (Delay_Refused,Result);
         else State := Idle; Last := Event_Completed; Result := Last;
         end if;
      end if;
      Busy := False;
   end End_Event;
   procedure Request_Stop (Result : out Outcome) is
   begin
      if Busy then
         Stop_Requested := True; State := Stopped; Close_Admission; Last := Stop_Pending; Result := Last;
      elsif State = Stopped then Result := Already_Stopped;
      else
         Busy := True; Stop (Stopped_By_Owner,Result); Busy := False;
      end if;
   end Request_Stop;
   function Admits (Service : Core.State) return Boolean is
     (not Busy and then State = Active and then Claimed and then Owner_Ready (Service) and then Owner_Idle);
   procedure Finish_Evaluation (Service : Core.State; Succeeded : Boolean) is
      Ignored : Outcome;
   begin
      if Stop_Requested or else State = Stopped then Stop (Stopped_By_Owner,Ignored);
      elsif not Succeeded or else not Owner_Ready (Service) or else not Owner_Idle then Stop (Evaluation_Failed,Ignored);
      end if;
      Busy := False;
   end Finish_Evaluation;
   procedure Evaluate_Named
     (Service : aliased in out Core.State; Item : Core.Node_Descriptor; Budget : Natural;
      Result : out Core.Values.Result; Status : out Core.Values.Access_Status) is
   begin
      Result := (AML_Execute.Unsupported_Value,0); Status := Core.Values.Wrong_Phase;
      if not Admits (Service) then return; end if;
      Busy := True; Evaluate (Service,Item,Budget,Result,Status);
      Finish_Evaluation (Service,Status = Core.Values.Available and then Result.Charged <= Budget and then
        Result.Status in AML_Execute.Returned | AML_Execute.Object_Returned | AML_Execute.Reference_Returned |
          AML_Execute.Metadata_Returned | AML_Execute.No_Return);
   end Evaluate_Named;
   procedure Invoke_Retained
     (Service : aliased in out Core.State; Node : Core.Namespace.Node_ID;
      Args : AML_Execute.Arguments; Argument_Count : Core.Values.Argument_Count; Budget : Natural;
      Result : out Core.Values.Result; Status : out Core.Values.Access_Status) is
   begin
      Result := (AML_Execute.Unsupported_Value,0); Status := Core.Values.Wrong_Phase;
      if not Admits (Service) then return; end if;
      Busy := True; Invoke (Service,Node,Args,Argument_Count,Budget,Result,Status);
      Finish_Evaluation (Service,Status = Core.Values.Available and then Result.Charged <= Budget and then
        Result.Status in AML_Execute.Returned | AML_Execute.Object_Returned | AML_Execute.Reference_Returned |
          AML_Execute.Metadata_Returned | AML_Execute.No_Return);
   end Invoke_Retained;
   function Preparation_Result_Safe (Value : Core.Values.Result;
      Status : Core.Values.Access_Status; Budget : Natural) return Boolean is
     (Status = Core.Values.Available and then Value.Charged <= Budget
      and then Value.Status not in AML_Execute.Field_Indeterminate | AML_Execute.Fatal_Requested | AML_Execute.Module_Completed);
   procedure Evaluate_Preparation
     (Service : aliased in out Core.State; Item : Core.Node_Descriptor; Budget : Natural;
      Result : out Core.Values.Result; Status : out Core.Values.Access_Status) is
   begin
      Result := (AML_Execute.Unsupported_Value,0); Status := Core.Values.Wrong_Phase;
      if not Admits (Service) then return; end if;
      Busy := True; Evaluate (Service,Item,Budget,Result,Status);
      Finish_Evaluation (Service,Preparation_Result_Safe (Result,Status,Budget));
   end Evaluate_Preparation;
   procedure Invoke_Preparation
     (Service : aliased in out Core.State; Node : Core.Namespace.Node_ID;
      Args : AML_Execute.Arguments; Argument_Count : Core.Values.Argument_Count; Budget : Natural;
      Result : out Core.Values.Result; Status : out Core.Values.Access_Status) is
   begin
      Result := (AML_Execute.Unsupported_Value,0); Status := Core.Values.Wrong_Phase;
      if not Admits (Service) then return; end if;
      Busy := True; Invoke (Service,Node,Args,Argument_Count,Budget,Result,Status);
      Finish_Evaluation (Service,Preparation_Result_Safe (Result,Status,Budget));
   end Invoke_Preparation;
   procedure Invoke_With_Arguments
     (Service : aliased in out Core.State; Node : Core.Namespace.Node_ID;
      Args : Core.Values.Arguments; Argument_Count : Core.Values.Argument_Count; Budget : Natural;
      Result : out Core.Values.Result; Status : out Core.Values.Access_Status) is
   begin
      Result := (AML_Execute.Unsupported_Value,0); Status := Core.Values.Wrong_Phase;
      if not Admits (Service) then return; end if;
      Busy := True; Invoke_Values (Service,Node,Args,Argument_Count,Budget,Result,Status);
      Finish_Evaluation (Service,Status = Core.Values.Available and then Result.Charged <= Budget and then
        Result.Status in AML_Execute.Returned | AML_Execute.Object_Returned | AML_Execute.Reference_Returned |
          AML_Execute.Metadata_Returned | AML_Execute.No_Return);
   end Invoke_With_Arguments;
end ACPI_Local_Runtime;
