pragma Ada_2022;
with ACPI_Local_Runtime;
with ACPI_Local_Runtime_Status;
generic
   with procedure Enter_Delays (Accepted : out Boolean);
   with procedure Begin_Delays (Deadline_Microseconds : Word; Accepted : out Boolean);
   with procedure End_Delays (Accepted : out Boolean);
   with procedure Stop_Delays;
-- Bind the SAME delay owner used by the root's IO-window inspector. This child
-- owns no Core, cache, mapping, lease or result storage: all operations use the
-- exact parent instance. Only one child facade can claim it. Native Main still
-- authenticates events and owns shutdown/backing retirement separately.
package ACPI_Local_Hardware.Pooled_Fields.Runtime is
   package Status renames ACPI_Local_Runtime_Status;
   function Current return Status.Phase;
   function Observation return Status.Outcome;
   procedure Enter_Runtime (Service : Core.State; Result : out Status.Outcome);
   procedure Begin_Event (Service : Core.State; Deadline_Microseconds : Word; Result : out Status.Outcome);
   procedure End_Event (Service : Core.State; Completed : Boolean; Result : out Status.Outcome);
   procedure Request_Stop (Result : out Status.Outcome);
   procedure Evaluate_Named
     (Service : aliased in out Core.State; Item : Core.Node_Descriptor; Budget : Natural;
      Result : out Core.Values.Result; Access_Result : out Core.Values.Access_Status);
   -- Enumeration only: preserve the exact failure result without stopping a
   -- quiet same-owner window. A failed method may be isolated only with no new
   -- retained roots; Fatal/module completion, budget/access contract violations
   -- or actual provider uncertainty still close admission. No retry/reset.
   procedure Evaluate_Discovery
     (Service : aliased in out Core.State; Item : Core.Node_Descriptor; Budget : Natural;
      Result : out Core.Values.Result; Status : out Core.Values.Access_Status);
   procedure Invoke_Retained
     (Service : aliased in out Core.State; Node : Core.Namespace.Node_ID;
      Args : AML_Execute.Arguments; Argument_Count : Core.Values.Argument_Count; Budget : Natural;
      Result : out Core.Values.Result; Access_Result : out Core.Values.Access_Status);
   procedure Evaluate_Sample
     (Service : aliased in out Core.State; Item : Core.Node_Descriptor; Budget : Natural;
      Result : out Core.Values.Result; Status : out Core.Values.Access_Status);
   procedure Invoke_Sample
     (Service : aliased in out Core.State; Node : Core.Namespace.Node_ID;
      Args : Core.Values.Arguments; Argument_Count : Core.Values.Argument_Count; Budget : Natural;
      Result : out Core.Values.Result; Status : out Core.Values.Access_Status);
   procedure Invoke_With_Arguments
     (Service : aliased in out Core.State; Node : Core.Namespace.Node_ID;
      Args : Core.Values.Arguments; Argument_Count : Core.Values.Argument_Count; Budget : Natural;
      Result : out Core.Values.Result; Access_Result : out Core.Values.Access_Status);
   -- Trusted sleep-preparation controller only. Ordinary bounded AML failure
   -- may leave the runtime active ONLY when access/budget contracts and the
   -- existing same-owner Ready/Idle checks still hold. Fatal, indeterminate
   -- Field IO and invalid module-only completion stop the owner. No retry,
   -- deadline renewal or failure suppression; the original result is returned.
   procedure Evaluate_Preparation
     (Service : aliased in out Core.State; Item : Core.Node_Descriptor; Budget : Natural;
      Result : out Core.Values.Result; Status : out Core.Values.Access_Status);
   procedure Invoke_Preparation
     (Service : aliased in out Core.State; Node : Core.Namespace.Node_ID;
      Args : AML_Execute.Arguments; Argument_Count : Core.Values.Argument_Count; Budget : Natural;
      Result : out Core.Values.Result; Status : out Core.Values.Access_Status);
private
   package Owner is new ACPI_Local_Runtime
     (Core,Runtime_Ready,Runtime_Idle,Claim_Runtime,Close,Enter_Delays,Begin_Delays,End_Delays,Stop_Delays,
      Runtime_Evaluate,Runtime_Invoke,Runtime_Invoke_Values);
end ACPI_Local_Hardware.Pooled_Fields.Runtime;
