pragma Ada_2022;
with Interfaces;
with ACPI_Service_Core;
with AML_Execute;
with ACPI_Local_Runtime_Status;
generic
   with package Core is new ACPI_Service_Core (<>);
   with function Owner_Ready (Service : Core.State) return Boolean;
   with function Owner_Idle return Boolean;
   -- False must leave startup/another facade unchanged. Success transfers
   -- this one provider exactly once and closes its startup-only entry points.
   with procedure Claim_Owner (Service : Core.State; Accepted : out Boolean);
   with procedure Close_Owner;
   with procedure Enter_Delays (Accepted : out Boolean);
   with procedure Begin_Delays (Deadline_Microseconds : Interfaces.Unsigned_64; Accepted : out Boolean);
   with procedure End_Delays (Accepted : out Boolean);
   with procedure Stop_Delays;
   with procedure Evaluate
     (Service : aliased in out Core.State; Item : Core.Node_Descriptor; Budget : Natural;
      Result : out Core.Values.Result; Status : out Core.Values.Access_Status);
   with procedure Invoke
     (Service : aliased in out Core.State; Node : Core.Namespace.Node_ID;
      Args : AML_Execute.Arguments; Argument_Count : Core.Values.Argument_Count; Budget : Natural;
      Result : out Core.Values.Result; Status : out Core.Values.Access_Status);
   with procedure Invoke_Values
     (Service : aliased in out Core.State; Node : Core.Namespace.Node_ID;
      Args : Core.Values.Arguments; Argument_Count : Core.Values.Argument_Count; Budget : Natural;
      Result : out Core.Values.Result; Status : out Core.Values.Access_Status);
-- Serialized process-local facade. Actual callbacks are bounded, normally
-- returning and non-mutating except for their declared owner/evaluation work.
-- A stop closes admission; it never cancels an in-flight call, frees backing,
-- releases a result or resets/replays AML. Authenticate event sources outside.
package ACPI_Local_Runtime is
   subtype Tick is Interfaces.Unsigned_64;
   package Status renames ACPI_Local_Runtime_Status;
   subtype Phase is Status.Phase;
   subtype Outcome is Status.Outcome;
   function Current return Phase;
   function Observation return Outcome;
   procedure Enter_Runtime (Service : Core.State; Result : out Outcome)
     with Pre => Core.Valid (Service);
   procedure Begin_Event (Service : Core.State; Deadline_Microseconds : Tick; Result : out Outcome)
     with Pre => Core.Valid (Service);
   procedure End_Event (Service : Core.State; Completed : Boolean; Result : out Outcome)
     with Pre => Core.Valid (Service);
   procedure Request_Stop (Result : out Outcome);
   procedure Evaluate_Named
     (Service : aliased in out Core.State; Item : Core.Node_Descriptor; Budget : Natural;
      Result : out Core.Values.Result; Status : out Core.Values.Access_Status)
     with Pre => Core.Valid (Service) and then not Result'Constrained;
   -- Enumeration only: preserve the exact failure result without stopping a
   -- quiet same-owner window. A failed method may be isolated only with no new
   -- retained roots; Fatal/module completion, budget/access contract violations
   -- or actual provider uncertainty still close admission. No retry/reset.
   procedure Evaluate_Discovery
     (Service : aliased in out Core.State; Item : Core.Node_Descriptor; Budget : Natural;
      Result : out Core.Values.Result; Status : out Core.Values.Access_Status)
     with Pre => Core.Valid (Service) and then not Result'Constrained;
   -- Fixed consumer samples: an ordinary bounded AML error may be isolated
   -- only at the same idle provider, with no newly retained error result.
   -- Fatal/module completion, indeterminate Field IO, access/budget violations
   -- and owner/stop failures still permanently close the shared runtime.
   procedure Evaluate_Sample
     (Service : aliased in out Core.State; Item : Core.Node_Descriptor; Budget : Natural;
      Result : out Core.Values.Result; Status : out Core.Values.Access_Status)
     with Pre => Core.Valid (Service) and then not Result'Constrained;
   procedure Invoke_Sample
     (Service : aliased in out Core.State; Node : Core.Namespace.Node_ID;
      Args : Core.Values.Arguments; Argument_Count : Core.Values.Argument_Count; Budget : Natural;
      Result : out Core.Values.Result; Status : out Core.Values.Access_Status)
     with Pre => Core.Valid (Service) and then not Result'Constrained;
   procedure Invoke_Retained
     (Service : aliased in out Core.State; Node : Core.Namespace.Node_ID;
      Args : AML_Execute.Arguments; Argument_Count : Core.Values.Argument_Count; Budget : Natural;
      Result : out Core.Values.Result; Status : out Core.Values.Access_Status)
     with Pre => Core.Valid (Service) and then not Result'Constrained;
   procedure Invoke_With_Arguments
     (Service : aliased in out Core.State; Node : Core.Namespace.Node_ID;
      Args : Core.Values.Arguments; Argument_Count : Core.Values.Argument_Count; Budget : Natural;
      Result : out Core.Values.Result; Status : out Core.Values.Access_Status)
     with Pre => Core.Valid (Service) and then not Result'Constrained;
   -- Trusted sleep-preparation controller only. Ordinary bounded AML failure
   -- may leave the runtime active ONLY when access/budget contracts and the
   -- existing same-owner Ready/Idle checks still hold. Fatal, indeterminate
   -- Field IO and invalid module-only completion stop the owner. No retry,
   -- deadline renewal or failure suppression; the original result is returned.
   procedure Evaluate_Preparation
     (Service : aliased in out Core.State; Item : Core.Node_Descriptor; Budget : Natural;
      Result : out Core.Values.Result; Status : out Core.Values.Access_Status)
     with Pre => Core.Valid (Service) and then not Result'Constrained;
   procedure Invoke_Preparation
     (Service : aliased in out Core.State; Node : Core.Namespace.Node_ID;
      Args : AML_Execute.Arguments; Argument_Count : Core.Values.Argument_Count; Budget : Natural;
      Result : out Core.Values.Result; Status : out Core.Values.Access_Status)
     with Pre => Core.Valid (Service) and then not Result'Constrained;
private
   use ACPI_Local_Runtime_Status;
   State : Phase := Fresh;
   Last : Outcome := Not_Attempted;
   Busy, Claimed, Owner_Closed, Delays_Stopped, Stop_Requested : Boolean := False;
   procedure Close_Admission;
   procedure Stop (Reason : Outcome; Result : out Outcome);
   function Admits (Service : Core.State) return Boolean;
   procedure Finish_Evaluation (Service : Core.State; Succeeded : Boolean);
end ACPI_Local_Runtime;
