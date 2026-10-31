pragma Ada_2022;
with ACPI_Service_Core;
with ACPI_Device_Status;
with AML_Decode;
with ACPI_Observation_Values;
with AML_Execute;
generic
   with package Core is new ACPI_Service_Core (<>);
   -- Trusted service-owner evaluator, closing over an admitted Field context.
   -- Must preserve the Core arena/catalog and the named-evaluation result and
   -- cleanup contracts. No callback, node or address comes from wire words.
   with procedure Evaluate_Named
     (Service : aliased in out Core.State; Item : Core.Node_Descriptor;
      Budget : Natural; Result : out Core.Values.Result;
      Status : out Core.Values.Access_Status) is Core.Evaluate_Named;
package ACPI_Lid with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);
   package Device_Status is new ACPI_Device_Status (Core, Evaluate_Named);
   use type AML_Decode.Integer_Value;
   subtype Lid_State is ACPI_Observation_Values.Lid_State;
   use type Lid_State;
   Unknown : constant Lid_State := ACPI_Observation_Values.Unknown;
   Closed : constant Lid_State := ACPI_Observation_Values.Closed;
   Open : constant Lid_State := ACPI_Observation_Values.Open;
   type Query_Outcome is (Sampled, Invalid_Device, Missing_Lid, Lookup_Failed,
      Evaluation_Failed, Wrong_Result_Type, Identity_Changed, Cleanup_Failed,
      Budget_Exhausted, Provider_Contract_Error);
   type Query_Report is record
      Outcome : Query_Outcome := Invalid_Device;
      State : Lid_State := Unknown;
      Raw : AML_Decode.Integer_Value := 0;
      Charged : Natural := 0;
      Access_Result : Core.Values.Access_Status := Core.Values.Available;
      Execution : AML_Execute.Execution_Status := AML_Execute.No_Return;
   end record;
   -- Device is a current descriptive binding selected by the trusted device
   -- owner (which separately establishes PNP0C0D identity). It is not authority.
   -- Only the fixed _LID member is resolved; no arbitrary method/arguments API.
   -- AML _LID can itself have effects: invocation requires controller authority,
   -- even though the returned sample is suitable for observers. No CCL endpoint
   -- or hardware capability is granted here. Unknown is never a closed sample.
   procedure Query (Service : aliased in out Core.State;
      Device : Core.Node_Descriptor; Budget : Natural; Report : out Query_Report)
     with Pre => Core.Valid (Service),
       Post => Core.Valid (Service)
         and then (if Report.Outcome /= Provider_Contract_Error then Report.Charged <= Budget)
         and then (Report.Outcome in Cleanup_Failed | Provider_Contract_Error
           or else Core.Retained_Results (Service) = Core.Retained_Results (Service)'Old)
         and then (if Report.Outcome = Sampled then
           Report.State = (if Report.Raw = 0 then Closed else Open)
         else Report.State = Unknown and then Report.Raw = 0);
   type Identification_Outcome is (Matched, Not_Lid, Not_Device, No_HID,
      Lookup_Error, Evaluation_Error, Wrong_HID_Type, Binding_Changed,
      Cleanup_Error, No_Budget, Evaluator_Error);
   type Identification_Report is record
      Outcome : Identification_Outcome := Not_Device;
      Charged : Natural := 0;
      Access_Result : Core.Values.Access_Status := Core.Values.Available;
      Execution : AML_Execute.Execution_Status := AML_Execute.No_Return;
   end record;
   -- Registration-time identity observation, not callable authority. Accepts
   -- the exact PNP0C0D compressed integer or seven-byte string _HID forms.
   -- Evaluation and result cleanup use the same trusted controller context as
   -- Query. Device enumeration/_STA readiness and registration are separate.
   procedure Identify (Service : aliased in out Core.State;
      Device : Core.Node_Descriptor; Budget : Natural;
      Report : out Identification_Report)
     with Pre => Core.Valid (Service),
       Post => Core.Valid (Service)
         and then (if Report.Outcome /= Evaluator_Error then Report.Charged <= Budget)
         and then (Report.Outcome in Cleanup_Error | Evaluator_Error
           or else Core.Retained_Results (Service) = Core.Retained_Results (Service)'Old);

end ACPI_Lid;
