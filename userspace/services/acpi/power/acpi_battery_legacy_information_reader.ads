with ACPI_Service_Core;
with ACPI_Battery_Legacy_Information;
with AML_Execute;
generic
   with package Core is new ACPI_Service_Core (<>);
   with package Format is new ACPI_Battery_Legacy_Information (<>);
   -- Owner callback preserves Core/retained ownership and charges within
   -- Budget. It must not reset/reenter Service; AML may perform admitted IO.
   with procedure Evaluate_Named
     (Service : aliased in out Core.State; Item : Core.Node_Descriptor;
      Budget : Natural; Result : out Core.Values.Result;
      Status : out Core.Values.Access_Status) is Core.Evaluate_Named;
package ACPI_Battery_Legacy_Information_Reader with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);
   use type Format.Information;
   type Query_Outcome is (Sampled, Invalid_Device, Missing_BIF, Lookup_Failed,
      Evaluation_Failed, Wrong_Result_Type, Wrong_Element_Count,
      Wrong_Element_Type, Text_Too_Long, Invalid_Package_Value,
      Identity_Changed, Budget_Exhausted, Cleanup_Failed, Provider_Contract_Error);
   subtype Field_Count is Natural range 0 .. Format.Expected_Elements;
   type Query_Report is record
      Outcome : Query_Outcome := Invalid_Device;
      Value : Format.Information := Format.Empty_Information;
      Raw : Format.Raw_Numbers := [others => 0];
      Elements : Natural := 0;
      Fields_Read : Field_Count := 0;
      Failure_Field : Format.Field := Format.No_Field;
      Decode_Status : Format.Outcome := Format.Invalid_Units;
      Charged, Last_Charge : Natural := 0;
      Access_Result : Core.Values.Access_Status := Core.Values.Available;
      Cleanup_Result : Core.Values.Access_Status := Core.Values.Available;
      Execution : AML_Execute.Execution_Status := AML_Execute.No_Return;
   end record;
   -- Explicit canonical _BIF only; no retry/fallback after failed _BIX.
   -- Owner establishes identity, presence, initialization and admitted regions.
   -- Legacy information does not contain or fabricate extended BIX fields.
   -- One work unit per element plus each copied text byte; retained handles are
   -- each release is attempted once on normal malformed/budget/overcharge exits.
   -- Cleanup failure is reported explicitly, never as successful reclamation.
   procedure Query (Service : aliased in out Core.State;
      Device : Core.Node_Descriptor; Budget : Natural; Report : out Query_Report)
     with Pre => Core.Valid (Service),
       Post => Core.Valid (Service) and then Report.Charged <= Budget
         and then (if Report.Outcome /= Sampled then Report.Value = Format.Empty_Information)
         and then (Report.Outcome in Cleanup_Failed | Provider_Contract_Error
           or else Core.Retained_Results (Service) = Core.Retained_Results (Service)'Old);
end ACPI_Battery_Legacy_Information_Reader;
