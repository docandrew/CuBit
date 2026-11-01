pragma Ada_2022;
with AML_Execute;
package body ACPI_Field_Evaluation is
   use type Bindings.Outcome;
   function Observe (Run : Context) return Bindings.Report is
     (Bindings.Observe (Run.Cache));
   procedure Export
     (Run : in out Context; Service : Core.State; IO : in out Provider.Context;
      Items : out Bindings.Fields.Binding_Set; Accepted : out Boolean)
   is
      Report : Bindings.Report;
   begin
      if Observe (Run).Status = Bindings.Not_Attempted then
         Bindings.Build (Run.Cache, Service, IO, Report);
      end if;
      -- Revalidate original namespace/provider identities on every export.
      -- Cached metadata never substitutes for the provider's live checks.
      Bindings.Copy_Bindings (Run.Cache, Service, IO, Items, Accepted);
   end Export;
   procedure Prepare
     (Run : in out Context; Service : Core.State; IO : in out Provider.Context;
      Accepted : out Boolean)
   is
      Items : Bindings.Fields.Binding_Set;
   begin
      Export (Run, Service, IO, Items, Accepted);
      Accepted := Accepted and then not Provider.Pending (IO);
   end Prepare;
   procedure Evaluate_Named
     (Run : in out Context; Service : aliased in out Core.State;
      IO : in out Provider.Context; Item : Core.Node_Descriptor;
      Budget : Natural; Result : out Core.Values.Result;
      Status : out Core.Values.Access_Status)
   is
      Items : Bindings.Fields.Binding_Set;
      Accepted : Boolean;
   begin
      Result := (AML_Execute.Unsupported_Value, 0);
      Status := Core.Values.Wrong_Phase;
      Export (Run, Service, IO, Items, Accepted);
      if not Accepted then return; end if;
      if Provider.Pending (IO) then Status := Core.Values.Busy; return; end if;
      Bindings.Fields.Evaluate_Named (Service, IO, Items, Item, Budget, Result, Status);
   end Evaluate_Named;
   procedure Invoke_With_Arguments
     (Run : in out Context; Service : aliased in out Core.State;
      IO : in out Provider.Context; Node : Core.Namespace.Node_ID;
      Args : Core.Values.Arguments; Argument_Count : Core.Values.Argument_Count;
      Budget : Natural; Result : out Core.Values.Result;
      Status : out Core.Values.Access_Status)
   is
      Items : Bindings.Fields.Binding_Set;
      Accepted : Boolean;
   begin
      Result := (AML_Execute.Unsupported_Value, 0);
      Status := Core.Values.Wrong_Phase;
      Export (Run, Service, IO, Items, Accepted);
      if not Accepted then return; end if;
      if Provider.Pending (IO) then Status := Core.Values.Busy; return; end if;
      Bindings.Fields.Invoke_With_Arguments
        (Service, IO, Items, Node, Args, Argument_Count, Budget, Result, Status);
   end Invoke_With_Arguments;
end ACPI_Field_Evaluation;
