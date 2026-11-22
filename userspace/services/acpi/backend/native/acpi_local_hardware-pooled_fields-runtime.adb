pragma Ada_2022;
package body ACPI_Local_Hardware.Pooled_Fields.Runtime is
   function Current return Status.Phase renames Owner.Current;
   function Observation return Status.Outcome renames Owner.Observation;
   procedure Enter_Runtime (Service : Core.State; Result : out Status.Outcome) renames Owner.Enter_Runtime;
   procedure Begin_Event (Service : Core.State; Deadline_Microseconds : Word; Result : out Status.Outcome) renames Owner.Begin_Event;
   procedure End_Event (Service : Core.State; Completed : Boolean; Result : out Status.Outcome) renames Owner.End_Event;
   procedure Request_Stop (Result : out Status.Outcome) renames Owner.Request_Stop;
   procedure Evaluate_Named
     (Service : aliased in out Core.State; Item : Core.Node_Descriptor; Budget : Natural;
      Result : out Core.Values.Result; Access_Result : out Core.Values.Access_Status) renames Owner.Evaluate_Named;
   procedure Evaluate_Discovery
     (Service : aliased in out Core.State; Item : Core.Node_Descriptor; Budget : Natural;
      Result : out Core.Values.Result; Status : out Core.Values.Access_Status) renames Owner.Evaluate_Discovery;
   procedure Invoke_Retained
     (Service : aliased in out Core.State; Node : Core.Namespace.Node_ID;
      Args : AML_Execute.Arguments; Argument_Count : Core.Values.Argument_Count; Budget : Natural;
      Result : out Core.Values.Result; Access_Result : out Core.Values.Access_Status) renames Owner.Invoke_Retained;
   procedure Evaluate_Sample
     (Service : aliased in out Core.State; Item : Core.Node_Descriptor; Budget : Natural;
      Result : out Core.Values.Result; Status : out Core.Values.Access_Status) renames Owner.Evaluate_Sample;
   procedure Invoke_Sample
     (Service : aliased in out Core.State; Node : Core.Namespace.Node_ID;
      Args : Core.Values.Arguments; Argument_Count : Core.Values.Argument_Count; Budget : Natural;
      Result : out Core.Values.Result; Status : out Core.Values.Access_Status) renames Owner.Invoke_Sample;
   procedure Evaluate_Preparation
     (Service : aliased in out Core.State; Item : Core.Node_Descriptor; Budget : Natural;
      Result : out Core.Values.Result; Status : out Core.Values.Access_Status) renames Owner.Evaluate_Preparation;
   procedure Invoke_Preparation
     (Service : aliased in out Core.State; Node : Core.Namespace.Node_ID;
      Args : AML_Execute.Arguments; Argument_Count : Core.Values.Argument_Count; Budget : Natural;
      Result : out Core.Values.Result; Status : out Core.Values.Access_Status) renames Owner.Invoke_Preparation;
   procedure Invoke_With_Arguments
     (Service : aliased in out Core.State; Node : Core.Namespace.Node_ID;
      Args : Core.Values.Arguments; Argument_Count : Core.Values.Argument_Count; Budget : Natural;
      Result : out Core.Values.Result; Access_Result : out Core.Values.Access_Status) renames Owner.Invoke_With_Arguments;
end ACPI_Local_Hardware.Pooled_Fields.Runtime;
