pragma Ada_2022;
package body ACPI_Local_Hardware.Fields is
   use type Core.Values.Access_Status, Core.Execution_Phase_Result, Core.Values.Module_Outcome;
   function Scalar_Ready (IO : Scalar.Context) return Boolean is
     (Field_State = Owner_Ready and then Scope.Ready (Transaction)
      and then (Scope.Observe (Transaction).Held or else ACPI_Local_Hardware.Admitting)
      and then Scalar.Is_Ready (IO));
   function Scalar_Pending (IO : Scalar.Context) return Boolean is
      S : constant Scope.Statistics := Scope.Observe (Transaction);
      use type Scope.Depth_Count;
   begin
      return Scalar.Pending (IO) or else S.Working or else S.Faulted
        or else (S.Depth = 0 and then S.Cleanup_Required);
   end Scalar_Pending;
   procedure Begin_Field (IO : in out Scalar.Context; Requires_Lock, For_Write : Boolean; Accepted : in out Boolean) is
   begin
      -- Core deliberately supplies False for LockRule. A real local lease
      -- permits the trusted hook to explicitly accept after acquisition.
      Accepted := False;
      if Scalar_Ready (IO) then Scope.Begin_Field (Transaction,Requires_Lock,For_Write,Accepted); end if;
   end Begin_Field;
   procedure End_Field (IO : in out Scalar.Context; Accepted : in out Boolean) is
   begin
      if Scalar.Pending (IO) then Scope.Mark_Fault (Transaction); end if;
      Scope.End_Field (Transaction,Accepted);
      Accepted := Accepted and then not Scalar.Pending (IO);
   end End_Field;
   procedure Dispatch (IO : in out Scalar.Context; Item : AML_Field_Access.Request;
      Reply : out AML_Field_Access.Response) is
      Accepted : Boolean;
   begin
      Reply := (others => <>);
      if not Scalar_Ready (IO) then return; end if;
      Scope.Check_Datum (Transaction,Accepted);
      if not Accepted then return; end if;
      Scalar.Dispatch (IO,Item,Reply);
      if Scalar.Pending (IO) then Scope.Mark_Fault (Transaction); end if;
   end Dispatch;
   function Observe return Field_Report is
     ((Field_State,Attempted,Completed,Finished,Scalar_Pending (IO) or else ACPI_Local_Hardware.Pending,Scalar.Register_Count (IO)));
   function Owns (Service : Core.State) return Boolean is
     (Completed and then Has_Anchor and then Core.Valid (Service) and then Core.Current_Binding (Service,Anchor));
   procedure Begin_Startup (Symbol : AML_Field_Access.Endpoint_ID; Accepted : out Boolean) is
   begin
      Accepted := False;
      if Field_State /= Owner_Fresh or else Attempted then return; end if;
      Field_State := Owner_Failed;
      if not ACPI_Local_Hardware.Admitting then return; end if;
      Scalar.Initialize (IO,Symbol,Accepted);
      if Accepted then Field_State := Owner_Ready; end if;
   end Begin_Startup;
   procedure Execute_Module_Startup (Service : in out Core.State; Budget : Natural;
      Result : out Core.Execution_Report; Status : out Core.Values.Access_Status) is
   begin
      Result := (others => <>); Status := Core.Values.Wrong_Phase;
      if Attempted or else Finished then return; end if;
      Attempted := True;
      if not Core.Valid (Service) or else not Scalar_Ready (IO) then Field_State := Owner_Failed; return; end if;
      Evaluation.Bindings.Startup.Execute_Frozen (Service,IO,Budget,Result,Status);
      Completed := Status = Core.Values.Available and then Result.Phase = Core.Module_Execution_Reported
        and then Result.Modules.Outcome = Core.Values.Modules_Completed;
      if not Completed then Field_State := Owner_Failed; return; end if;
      Core.Describe_Node (Service,Core.Namespace.Root,Anchor,Status);
      Has_Anchor := Status = Core.Values.Available;
      if not Has_Anchor then Field_State := Owner_Failed; end if;
   end Execute_Module_Startup;
   procedure Prepare_Fields (Service : Core.State; Accepted : out Boolean) is
   begin
      Accepted := False;
      if not Owns (Service) or else not Scalar_Ready (IO) then return; end if;
      Evaluation.Prepare (Cache,Service,IO,Accepted);
   end Prepare_Fields;
   procedure Evaluate_Named (Service : aliased in out Core.State; Item : Core.Node_Descriptor;
      Budget : Natural; Result : out Core.Values.Result; Status : out Core.Values.Access_Status) is
   begin
      Result := (AML_Execute.Unsupported_Value,0); Status := Core.Values.Wrong_Phase;
      if not Owns (Service) or else not Scalar_Ready (IO) then return; end if;
      Evaluation.Evaluate_Named (Cache,Service,IO,Item,Budget,Result,Status);
   end Evaluate_Named;
   procedure Invoke_Retained (Service : aliased in out Core.State; Node : Core.Namespace.Node_ID;
      Args : AML_Execute.Arguments; Argument_Count : Core.Values.Argument_Count;
      Budget : Natural; Result : out Core.Values.Result; Status : out Core.Values.Access_Status) is
   begin
      Result := (AML_Execute.Unsupported_Value,0); Status := Core.Values.Wrong_Phase;
      if not Owns (Service) or else not Scalar_Ready (IO) then return; end if;
      Evaluation.Invoke_Retained (Cache,Service,IO,Node,Args,Argument_Count,Budget,Result,Status);
   end Invoke_Retained;
   procedure Invoke_With_Arguments (Service : aliased in out Core.State; Node : Core.Namespace.Node_ID;
      Args : Core.Values.Arguments; Argument_Count : Core.Values.Argument_Count;
      Budget : Natural; Result : out Core.Values.Result; Status : out Core.Values.Access_Status) is
   begin
      Result := (AML_Execute.Unsupported_Value,0); Status := Core.Values.Wrong_Phase;
      if not Owns (Service) or else not Scalar_Ready (IO) then return; end if;
      Evaluation.Invoke_With_Arguments (Cache,Service,IO,Node,Args,Argument_Count,Budget,Result,Status);
   end Invoke_With_Arguments;
   procedure Finish_Startup (Accepted : out Boolean) is
   begin
      Accepted := Completed and then Has_Anchor and then not Finished and then Scalar_Ready (IO);
      if Accepted then Finished := True; end if;
   end Finish_Startup;
   procedure Close is
   begin Field_State := Owner_Closed; end Close;
   procedure Cleanup (Completed : out Boolean) is
   begin
      Field_State := Owner_Closed;
      Scope.Cleanup (Transaction,Completed);
      Completed := Completed and then not Scalar.Pending (IO);
   end Cleanup;
   function Uncertain return AML_Field_Access.Observation is (Scalar.Uncertain (IO));
end ACPI_Local_Hardware.Fields;
