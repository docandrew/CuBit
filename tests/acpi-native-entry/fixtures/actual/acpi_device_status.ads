pragma Ada_2022;
with Interfaces;
with ACPI_Service_Core;
with AML_Execute;
generic
   with package Core is new ACPI_Service_Core (<>);
   -- Fixed owner evaluator, same preservation/charge/no-reset/no-reentry
   -- contract as Core.Evaluate_Named; not supplied by an IPC caller.
   with procedure Evaluate_Named
     (Service : aliased in out Core.State; Item : Core.Node_Descriptor;
      Budget : Natural; Result : out Core.Values.Result;
      Status : out Core.Values.Access_Status) is Core.Evaluate_Named;
package ACPI_Device_Status with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);
   use type Interfaces.Unsigned_64;
   type Status_Outcome is
     (Status_Available, Invalid_Device, Lookup_Failed, Evaluation_Failed,
      Wrong_Result_Type, Identity_Changed, Budget_Exhausted,
      Cleanup_Failed, Provider_Contract_Error);
   type Status_Report is record
      Outcome : Status_Outcome := Invalid_Device;
      Flags : Interfaces.Unsigned_64 := 0;
      Present, Enabled, Functioning, Defaulted : Boolean := False;
      Charged, Last_Charge : Natural := 0;
      Access_Result : Core.Values.Access_Status := Core.Values.Available;
      Cleanup_Result : Core.Values.Access_Status := Core.Values.Available;
      Execution : AML_Execute.Execution_Status := AML_Execute.No_Return;
   end record;
   -- Observe only this device's canonical direct _STA, including named values
   -- and aliases. Missing _STA uses the existing device-policy default. Keep
   -- the full flags; presence, enablement and functioning are separate facts.
   -- Neither the flags nor this observation establish initialization/readiness.
   procedure Observe_Status
     (Service : aliased in out Core.State; Device : Core.Node_Descriptor;
      Budget : Natural; Report : out Status_Report)
     with Pre => Core.Valid (Service),
       Post => Core.Valid (Service) and then Report.Charged <= Budget
         and then (Report.Outcome in Cleanup_Failed | Provider_Contract_Error
           or else Core.Retained_Results (Service) = Core.Retained_Results (Service)'Old)
         and then (if Report.Outcome /= Status_Available then
           Report.Flags = 0 and not Report.Present and not Report.Enabled
             and not Report.Functioning and not Report.Defaulted);

   -- Conservative query eligibility only; no initialization or IO authority.
   function Usable (Item : Status_Report) return Boolean is
     (Item.Outcome = Status_Available and then Item.Present
      and then Item.Enabled and then Item.Functioning)
     with Global => null;
end ACPI_Device_Status;
