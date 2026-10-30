pragma Ada_2022;
with ACPI_Service_Core;
with AML_Decode;
with AML_Execute;
generic
   with package Core is new ACPI_Service_Core (<>);
   -- Trusted controller evaluator: preserve Core/catalog/retained ownership,
   -- charge within Budget, and do not reset/reenter Service. AML may have effects.
   with procedure Evaluate_Named
     (Service : aliased in out Core.State; Item : Core.Node_Descriptor;
      Budget : Natural; Result : out Core.Values.Result;
      Status : out Core.Values.Access_Status) is Core.Evaluate_Named;
package ACPI_Power_Source with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);
   use type AML_Decode.Integer_Value;
   Offline_Code : constant AML_Decode.Integer_Value := 0;
   Online_Code : constant AML_Decode.Integer_Value := 1;
   type Source_State is (Unknown, Offline, Online);
   type Query_Outcome is (Sampled, Invalid_Device, Missing_PSR, Lookup_Failed,
      Evaluation_Failed, Wrong_Result_Type, Invalid_Status_Value,
      Identity_Changed, Budget_Exhausted, Cleanup_Failed, Provider_Contract_Error);
   type Query_Report is record
      Outcome : Query_Outcome := Invalid_Device;
      State : Source_State := Unknown;
      Raw : AML_Decode.Integer_Value := 0;
      Charged, Last_Charge : Natural := 0;
      Access_Result : Core.Values.Access_Status := Core.Values.Available;
      Cleanup_Result : Core.Values.Access_Status := Core.Values.Available;
      Execution : AML_Execute.Execution_Status := AML_Execute.No_Return;
   end record;
   -- Observe only this device's canonical direct _PSR. Only integers0/1 are
   -- accepted; missing/error is Unknown. Online means this SOURCE is in use,
   -- not physical cable presence or a whole-system battery/AC decision.
   -- Owner separately establishes ACPI0003 identity, _STA and initialization.
   -- Observers consume cached samples; they do not get AML invocation authority.
   procedure Query (Service : aliased in out Core.State;
      Device : Core.Node_Descriptor; Budget : Natural; Report : out Query_Report)
     with Pre => Core.Valid (Service),
       Post => Core.Valid (Service) and then Report.Charged <= Budget
         and then (Report.Outcome in Cleanup_Failed | Provider_Contract_Error
           or else Core.Retained_Results (Service) = Core.Retained_Results (Service)'Old)
         and then (if Report.Outcome = Sampled then Report.Raw <= Online_Code
           and then Report.State = (if Report.Raw = Offline_Code then Offline else Online)
           else Report.State = Unknown)
         and then (if Report.Outcome not in Sampled | Invalid_Status_Value then Report.Raw = 0);
   type Identification_Outcome is (Matched, Not_Power_Source, Invalid_Device,
      Missing_HID, Lookup_Failed, Evaluation_Failed, Wrong_HID_Type,
      Identity_Changed, Budget_Exhausted, Cleanup_Failed, Provider_Contract_Error);
   type Identification_Report is record
      Outcome : Identification_Outcome := Invalid_Device;
      Charged, Last_Charge : Natural := 0;
      Access_Result : Core.Values.Access_Status := Core.Values.Available;
      Cleanup_Result : Core.Values.Access_Status := Core.Values.Available;
      Execution : AML_Execute.Execution_Status := AML_Execute.No_Return;
   end record;
   -- Exact string _HID ACPI0003, with named/aliased/method forms. Integer EISA
   -- IDs are valid nonmatches; Buffer/Package returns are wrong types. No
   -- coercion, prefix match, ancestor fallback or authorization from an ID.
   procedure Identify (Service : aliased in out Core.State;
      Device : Core.Node_Descriptor; Budget : Natural; Report : out Identification_Report)
     with Pre => Core.Valid (Service),
       Post => Core.Valid (Service) and then Report.Charged <= Budget
         and then (Report.Outcome in Cleanup_Failed | Provider_Contract_Error
           or else Core.Retained_Results (Service) = Core.Retained_Results (Service)'Old);
end ACPI_Power_Source;
