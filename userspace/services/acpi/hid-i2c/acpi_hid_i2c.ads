pragma Ada_2022;
with Interfaces;
with ACPI_Service_Core;
with AML_Execute;
generic
   with package Core is new ACPI_Service_Core (<>);
   -- Trusted controller invocation, optionally closing over admitted Field IO.
   -- Must preserve Core validity, catalog and retained-result ownership, charge
   -- at most Budget, and never reset/reenter Service. Not selected by wire data.
   with procedure Invoke
     (Service : aliased in out Core.State; Node : Core.Namespace.Node_ID;
      Args : Core.Values.Arguments; Argument_Count : Core.Values.Argument_Count;
      Budget : Natural; Result : out Core.Values.Result;
      Status : out Core.Values.Access_Status) is Core.Invoke_With_Arguments;
   with procedure Evaluate_Named
     (Service : aliased in out Core.State; Item : Core.Node_Descriptor;
      Budget : Natural; Result : out Core.Values.Result;
      Status : out Core.Values.Access_Status) is Core.Evaluate_Named;
package ACPI_HID_I2C with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);
   use type Interfaces.Unsigned_64;
   type Descriptor_Register is new Interfaces.Unsigned_16;
   type Call_Count is range 0 .. 2;
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
   type Identification_Outcome is
     (Compatible, Not_Compatible, Invalid_Device, Missing_CID, Lookup_Failed,
      Evaluation_Failed, Wrong_Result_Type, Identity_Changed, Budget_Exhausted,
      Cleanup_Failed, Provider_Contract_Error);
   type Identification_Report is record
      Outcome : Identification_Outcome := Invalid_Device;
      Charged, AML_Charged, IDs_Examined : Natural := 0;
      Access_Result : Core.Values.Access_Status := Core.Values.Available;
      Cleanup_Result : Core.Values.Access_Status := Core.Values.Available;
      Execution : AML_Execute.Execution_Status := AML_Execute.No_Return;
   end record;
   -- Observe the direct _CID as a candidate HID-I2C match. Accept PNP0C50
   -- string/compressed integer, or a flat package of integer/string IDs.
   -- Every package element is checked, including those after a match. One
   -- budget unit per examined ID bounds host work as well as AML execution.
   -- No ancestor fallback. _HID, _STA, resources and dependencies are separate;
   -- Compatible is an identity observation, not authorization or readiness.
   procedure Identify
     (Service : aliased in out Core.State; Device : Core.Node_Descriptor;
      Budget : Natural; Report : out Identification_Report)
     with Pre => Core.Valid (Service),
       Post => Core.Valid (Service)
         and then Report.Charged <= Budget
         and then (Report.Outcome in Cleanup_Failed | Provider_Contract_Error
           or else Core.Retained_Results (Service) = Core.Retained_Results (Service)'Old);
   type Query_Outcome is
     (Descriptor_Available, Invalid_Device, Missing_DSM, Not_A_Method,
      Lookup_Failed, Allocation_Failed, Function_Unsupported, Wrong_Result_Type,
      Invalid_Register, Evaluation_Failed, Identity_Changed, Budget_Exhausted,
      Cleanup_Failed, Provider_Contract_Error);
   type Query_Report is record
      Outcome : Query_Outcome := Invalid_Device;
      Address : Descriptor_Register := 0;
      Functions : Interfaces.Unsigned_8 := 0;
      Calls : Call_Count := 0;
      Charged, Last_Charge : Natural := 0;
      Access_Result : Core.Values.Access_Status := Core.Values.Available;
      Cleanup_Result : Core.Values.Access_Status := Core.Values.Available;
      Execution : AML_Execute.Execution_Status := AML_Execute.No_Return;
   end record;
   -- Device owner separately establishes HID-I2C identity/readiness/resources.
   -- Only this device's direct _DSM is queried, with the HID-I2C UUID, revision1,
   -- function0 then function1 and an empty argument package. No arbitrary call.
   -- Address is an I2C descriptor-register number, never a physical address or
   -- authority. Zero is representable: success is determined by Outcome.
   -- _DSM can have effects; caller needs controller authority. No retry occurs.
   procedure Query_Descriptor
     (Service : aliased in out Core.State; Device : Core.Node_Descriptor;
      Budget : Natural; Report : out Query_Report)
     with Pre => Core.Valid (Service),
       Post => Core.Valid (Service)
         and then (if Report.Outcome /= Provider_Contract_Error then Report.Charged <= Budget)
         and then (Report.Outcome in Cleanup_Failed | Provider_Contract_Error
           or else Core.Retained_Results (Service) = Core.Retained_Results (Service)'Old)
         and then (if Report.Outcome /= Descriptor_Available then Report.Address = 0);
end ACPI_HID_I2C;
