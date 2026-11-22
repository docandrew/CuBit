with ACPI_Service_Core;
with ACPI_Battery_Information;
with AML_Execute;
with Interfaces;
generic
   with package Core is new ACPI_Service_Core (<>);
   with package Format is new ACPI_Battery_Information (<>);
   -- Owner callback preserves Core/retained ownership and charges within
   -- Budget. It must not reset/reenter Service; AML may perform admitted IO.
   with procedure Evaluate_Named
     (Service : aliased in out Core.State; Item : Core.Node_Descriptor;
      Budget : Natural; Result : out Core.Values.Result;
      Status : out Core.Values.Access_Status) is Core.Evaluate_Named;
package ACPI_Battery_Information_Reader with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);
   use type Format.Information;
   type Query_Outcome is (Sampled, Invalid_Device, Missing_BIX, Lookup_Failed,
      Evaluation_Failed, Wrong_Result_Type, Wrong_Element_Count,
      Wrong_Element_Type, Text_Too_Long, Invalid_Package_Value,
      Identity_Changed, Budget_Exhausted, Cleanup_Failed, Provider_Contract_Error);
   subtype Field_Count is Natural range 0 .. Format.Revision_One_Elements;
   type Query_Report is record
      Outcome : Query_Outcome := Invalid_Device;
      Value : Format.Information := Format.Empty_Information;
      Raw : Format.Raw_Numbers := [others => 0];
      Raw_Swap : Interfaces.Unsigned_64 := 0;
      Elements : Natural := 0;
      Fields_Read : Field_Count := 0;
      Failure_Field : Format.Field := Format.No_Field;
      Decode_Status : Format.Outcome := Format.Unsupported_Revision;
      Charged, Last_Charge : Natural := 0;
      Access_Result : Core.Values.Access_Status := Core.Values.Available;
      Cleanup_Result : Core.Values.Access_Status := Core.Values.Available;
      Execution : AML_Execute.Execution_Status := AML_Execute.No_Return;
   end record;
   -- Direct canonical _BIX only. Owner establishes battery identity, presence,
   -- initialization and admitted region context separately. No fallback to _BIF
   -- after a failed evaluation, no hardware authority and no observer invocation.
   -- One work unit per element plus each copied text byte; retained handles are
   -- released on every normal exit, including malformed/budget/overcharge paths.
   procedure Query (Service : aliased in out Core.State;
      Device : Core.Node_Descriptor; Budget : Natural; Report : out Query_Report)
     with Pre => Core.Valid (Service),
       Post => Core.Valid (Service) and then Report.Charged <= Budget
         and then (if Report.Outcome /= Sampled then Report.Value = Format.Empty_Information)
         and then (Report.Outcome in Cleanup_Failed | Provider_Contract_Error
           or else Core.Retained_Results (Service) = Core.Retained_Results (Service)'Old);
end ACPI_Battery_Information_Reader;
