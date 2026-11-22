pragma Ada_2022;
package body ACPI_Startup_Evaluation is
   use type Registers.Phase, Core.Values.Access_Status, Core.Execution_Phase_Result, Core.Values.Module_Outcome;
   function Current return Mode is (Selected);
   procedure Select_Observer (Accepted : out Boolean) is
   begin
      Accepted := Selected = Unselected and then not Module_Attempted;
      if Accepted then Selected := Observer; end if;
   end Select_Observer;
   procedure Select_Registers (Accepted : out Boolean) is
      State : constant Registers.Report := Registers.Observe;
   begin
      Accepted := False;
      if Selected /= Unselected or else Module_Attempted then return; end if;
      Selected := Refused;
      if State.Stage /= Registers.Ready or State.Module_Attempted or State.IO_Pending then return; end if;
      Selected := Register_Provider; Accepted := True;
   end Select_Registers;
   procedure Close_Startup is
   begin Registers.Close_Startup; Selected := Closed; end Close_Startup;
   procedure Execute_Module_Startup
     (Service : in out Core.State; Budget : Natural;
      Result : out Core.Execution_Report; Status : out Core.Values.Access_Status) is
   begin
      Result := (others => <>); Status := Core.Values.Wrong_Phase;
      if Module_Attempted then return; end if;
      Module_Attempted := True;
      case Selected is
         when Observer => Core.Execute_Frozen (Service, Budget, Result, Status);
         when Register_Provider => Registers.Execute_Module_Startup (Service, Budget, Result, Status);
         when others => return;
      end case;
      Module_Completed := Status = Core.Values.Available
        and then Result.Phase = Core.Module_Execution_Reported
        and then Result.Modules.Outcome = Core.Values.Modules_Completed;
      if not Module_Completed then Selected := Refused; end if;
   end Execute_Module_Startup;
   procedure Prepare_Fields (Service : Core.State; Accepted : out Boolean) is
   begin
      Accepted := False;
      if not Module_Completed then return; end if;
      case Selected is
         when Observer => Accepted := True;
         when Register_Provider => Registers.Prepare_Fields (Service, Accepted);
         when others => null;
      end case;
   end Prepare_Fields;
   procedure Evaluate_Named
     (Service : aliased in out Core.State; Item : Core.Node_Descriptor;
      Budget : Natural; Result : out Core.Values.Result; Status : out Core.Values.Access_Status) is
   begin
      Result := (AML_Execute.Unsupported_Value, 0); Status := Core.Values.Wrong_Phase;
      if not Module_Completed then return; end if;
      case Selected is
         when Observer => Core.Evaluate_Named (Service, Item, Budget, Result, Status);
         when Register_Provider => Registers.Evaluate_Named (Service, Item, Budget, Result, Status);
         when others => null;
      end case;
   end Evaluate_Named;
   procedure Invoke_Retained
     (Service : aliased in out Core.State; Node : Core.Namespace.Node_ID;
      Args : AML_Execute.Arguments; Argument_Count : Core.Values.Argument_Count;
      Budget : Natural; Result : out Core.Values.Result; Status : out Core.Values.Access_Status) is
   begin
      Result := (AML_Execute.Unsupported_Value, 0); Status := Core.Values.Wrong_Phase;
      if not Module_Completed then return; end if;
      case Selected is
         when Observer => Core.Invoke_Retained (Service, Node, Args, Natural (Argument_Count), Budget, Result, Status);
         when Register_Provider => Registers.Invoke_Retained (Service, Node, Args, Argument_Count, Budget, Result, Status);
         when others => null;
      end case;
   end Invoke_Retained;
   procedure Invoke_With_Arguments
     (Service : aliased in out Core.State; Node : Core.Namespace.Node_ID;
      Args : Core.Values.Arguments; Argument_Count : Core.Values.Argument_Count;
      Budget : Natural; Result : out Core.Values.Result; Status : out Core.Values.Access_Status) is
   begin
      Result := (AML_Execute.Unsupported_Value, 0); Status := Core.Values.Wrong_Phase;
      if not Module_Completed then return; end if;
      case Selected is
         when Observer => Core.Invoke_With_Arguments (Service, Node, Args, Argument_Count, Budget, Result, Status);
         when Register_Provider => Registers.Invoke_With_Arguments (Service, Node, Args, Argument_Count, Budget, Result, Status);
         when others => null;
      end case;
   end Invoke_With_Arguments;
end ACPI_Startup_Evaluation;
