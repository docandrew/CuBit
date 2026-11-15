pragma Ada_2022;
with ACPI_Service_Core;
with AML_Execute;
generic
   with package Core is new ACPI_Service_Core (<>);
   -- Trusted owner callback: retain Core/catalog identity, budget and result
   -- ownership. Native use must already hold its finite local runtime window.
   -- No reset, reentry, deadline renewal or automatic retry is permitted.
   with procedure Invoke
     (Service : aliased in out Core.State; Node : Core.Namespace.Node_ID;
      Args : AML_Execute.Arguments; Argument_Count : Natural; Budget : Natural;
      Result : out Core.Values.Result; Status : out Core.Values.Access_Status)
        is Core.Invoke_Retained;
package ACPI_Sleep_Preparation with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);
   type Sleep_State is (S1, S2, S3, S4, S5);
   for Sleep_State use (S1 => 1, S2 => 2, S3 => 3, S4 => 4, S5 => 5);
   type Method_Phase is (Transition_To_State, Prepare_To_Sleep);
   type Outcome is (Not_Declared, Method_Completed, Lookup_Failed,
      Wrong_Object_Type, Invocation_Failed, Identity_Changed,
      Budget_Exhausted, Cleanup_Failed, Provider_Contract_Error);
   type Preparation_Report is record
      Target : Sleep_State := S5;
      Phase : Method_Phase := Prepare_To_Sleep;
      Status : Outcome := Lookup_Failed;
      Present, Invoked : Boolean := False;
      Charged, AML_Charged : Natural := 0;
      Access_Result : Core.Values.Access_Status := Core.Values.Available;
      Cleanup_Result : Core.Values.Access_Status := Core.Values.Available;
      Execution : AML_Execute.Execution_Status := AML_Execute.No_Return;
   end record;
   -- Invoke ROOT _TTS or _PTS with one integer state argument. An absent
   -- method is explicit; its acceptability is the controller's policy.
   -- Ignore a normal return value after releasing its retained root once.
   -- Completion certifies this invocation only, not readiness to power off.
   -- No _Sx evaluation, autonomous startup action, register IO or IPC here.
   procedure Prepare (Service : aliased in out Core.State; Target : Sleep_State;
      Budget : Natural; Report : out Preparation_Report;
      Phase : Method_Phase := Prepare_To_Sleep)
     with Pre => Core.Valid (Service),
       Post => Core.Valid (Service) and then Report.Target = Target and then Report.Phase = Phase
         and then Report.Charged <= Budget
         and then (Report.Status in Cleanup_Failed | Provider_Contract_Error
           or else Core.Retained_Results (Service) = Core.Retained_Results (Service)'Old);
end ACPI_Sleep_Preparation;
