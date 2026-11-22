pragma Ada_2022;
with ACPI_Service_Core;
with ACPI_Battery_Status;
with AML_Execute;
with AML_Decode;
generic
   with package Core is new ACPI_Service_Core (<>);
   -- Trusted owner callback: preserve Core/catalog/retained-result ownership,
   -- charge within Budget, and do not reset/reenter Service. AML may do IO.
   with procedure Evaluate_Named
     (Service : aliased in out Core.State; Item : Core.Node_Descriptor;
      Budget : Natural; Result : out Core.Values.Result;
      Status : out Core.Values.Access_Status) is Core.Evaluate_Named;
package ACPI_Battery with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);
   use type ACPI_Battery_Status.Sample;
   use type ACPI_Battery_Status.Outcome;
   type Query_Outcome is (Sampled, Invalid_Device, Missing_BST, Lookup_Failed,
      Evaluation_Failed, Wrong_Result_Type, Wrong_Element_Count,
      Wrong_Element_Type, Invalid_Package_Value, Identity_Changed,
      Budget_Exhausted, Cleanup_Failed, Provider_Contract_Error);
   subtype Field_Count is Natural range 0 .. ACPI_Battery_Status.Expected_Elements;
   type Query_Report is record
      Outcome : Query_Outcome := Invalid_Device;
      Value : ACPI_Battery_Status.Sample := ACPI_Battery_Status.Empty_Sample;
      Raw : ACPI_Battery_Status.Raw_Package
        (1 .. ACPI_Battery_Status.Expected_Elements) := [others => 0];
      Fields_Read : Field_Count := 0;
      Elements : Natural := 0;
      Failure_Field : ACPI_Battery_Status.Field := ACPI_Battery_Status.No_Field;
      Decode_Status : ACPI_Battery_Status.Outcome := ACPI_Battery_Status.Wrong_Element_Count;
      Charged, Last_Charge : Natural := 0;
      Access_Result : Core.Values.Access_Status := Core.Values.Available;
      Cleanup_Result : Core.Values.Access_Status := Core.Values.Available;
      Execution : AML_Execute.Execution_Status := AML_Execute.No_Return;
   end record;
   -- Read only the canonical direct _BST of this current Device. Retained
   -- handles are released on every path. Charge one host operation per element
   -- in addition to AML execution. Partial raw values are diagnostic only.
   -- The owner separately establishes identity, _STA presence, initialization,
   -- and _BIX units/technology. Failed observation never means absent battery.
   procedure Query (Service : aliased in out Core.State;
      Device : Core.Node_Descriptor; Budget : Natural; Report : out Query_Report)
     with Pre => Core.Valid (Service),
       Post => Core.Valid (Service) and then Report.Charged <= Budget
         and then (if Report.Outcome = Sampled then
           Report.Fields_Read = ACPI_Battery_Status.Expected_Elements
           and then Report.Decode_Status = ACPI_Battery_Status.Decoded
           else Report.Value = ACPI_Battery_Status.Empty_Sample)
         and then (Report.Outcome in Cleanup_Failed | Provider_Contract_Error
           or else Core.Retained_Results (Service) = Core.Retained_Results (Service)'Old);
   type Identification_Outcome is (Matched, Not_Battery, Invalid_Device,
      Missing_HID, Lookup_Failed, Evaluation_Failed, Wrong_HID_Type,
      Identity_Changed, Budget_Exhausted, Cleanup_Failed, Provider_Contract_Error);
   type Identification_Report is record
      Outcome : Identification_Outcome := Invalid_Device;
      Charged, Last_Charge : Natural := 0;
      Access_Result : Core.Values.Access_Status := Core.Values.Available;
      Cleanup_Result : Core.Values.Access_Status := Core.Values.Available;
      Execution : AML_Execute.Execution_Status := AML_Execute.No_Return;
   end record;
   -- Exact PNP0C0A string or compressed EISA integer, including named, alias
   -- and method forms. Identity does not grant hardware authority.
   procedure Identify (Service : aliased in out Core.State;
      Device : Core.Node_Descriptor; Budget : Natural; Report : out Identification_Report)
     with Pre => Core.Valid (Service),
       Post => Core.Valid (Service) and then Report.Charged <= Budget
         and then (Report.Outcome in Cleanup_Failed | Provider_Contract_Error
           or else Core.Retained_Results (Service) = Core.Retained_Results (Service)'Old);
   type Presence_Outcome is (Observed, Invalid_Device, Lookup_Failed,
      Evaluation_Failed, Wrong_Result_Type, Identity_Changed, Budget_Exhausted,
      Cleanup_Failed, Provider_Contract_Error);
   type Presence_Report is record
      Outcome : Presence_Outcome := Invalid_Device;
      Flags : AML_Decode.Integer_Value := 0;
      Device_Present, Battery_Present, Enabled, Functioning, Defaulted : Boolean := False;
      Charged, Last_Charge : Natural := 0;
      Access_Result : Core.Values.Access_Status := Core.Values.Available;
      Cleanup_Result : Core.Values.Access_Status := Core.Values.Available;
      Execution : AML_Execute.Execution_Status := AML_Execute.No_Return;
   end record;
   -- Observe direct _STA. Bit4 reports the battery, independently of device
   -- presence/enabled/functioning. Missing _STA defaults per ACPI; evaluation
   -- failure leaves presence unknown (Outcome /= Observed), never absent.
   -- Owner establishes PNP0C0A identity first. Full-width flags are preserved.
   procedure Query_Presence (Service : aliased in out Core.State;
      Device : Core.Node_Descriptor; Budget : Natural; Report : out Presence_Report)
     with Pre => Core.Valid (Service),
       Post => Core.Valid (Service) and then Report.Charged <= Budget
         and then (Report.Outcome in Cleanup_Failed | Provider_Contract_Error
           or else Core.Retained_Results (Service) = Core.Retained_Results (Service)'Old)
         and then (if Report.Outcome /= Observed then
           not Report.Device_Present and not Report.Battery_Present
           and not Report.Enabled and not Report.Functioning and not Report.Defaulted);
end ACPI_Battery;
