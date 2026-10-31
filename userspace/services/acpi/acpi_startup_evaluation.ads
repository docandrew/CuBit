pragma Ada_2022;
with ACPI_Service_Core;
with ACPI_Field_Startup_Owner;
generic
   with package Core is new ACPI_Service_Core (<>);
   with package Registers is new ACPI_Field_Startup_Owner (Core => Core, others => <>);
package ACPI_Startup_Evaluation is
   type Mode is (Unselected, Observer, Register_Provider, Refused, Closed);
   function Current return Mode;
   -- One lifetime selection, before module execution. A present but refused
   -- provider cannot fall back to observer or be retargeted/retried.
   procedure Select_Observer (Accepted : out Boolean);
   procedure Select_Registers (Accepted : out Boolean);
   procedure Close_Startup;
   procedure Execute_Module_Startup
     (Service : in out Core.State; Budget : Natural;
      Result : out Core.Execution_Report; Status : out Core.Values.Access_Status);
   procedure Prepare_Fields (Service : Core.State; Accepted : out Boolean);
   procedure Evaluate_Named
     (Service : aliased in out Core.State; Item : Core.Node_Descriptor;
      Budget : Natural; Result : out Core.Values.Result; Status : out Core.Values.Access_Status)
     with Pre => Core.Valid (Service) and then not Result'Constrained;
   procedure Invoke_With_Arguments
     (Service : aliased in out Core.State; Node : Core.Namespace.Node_ID;
      Args : Core.Values.Arguments; Argument_Count : Core.Values.Argument_Count;
      Budget : Natural; Result : out Core.Values.Result; Status : out Core.Values.Access_Status)
     with Pre => Core.Valid (Service) and then not Result'Constrained;
private
   Selected : Mode := Unselected;
   Module_Attempted, Module_Completed : Boolean := False;
end ACPI_Startup_Evaluation;
