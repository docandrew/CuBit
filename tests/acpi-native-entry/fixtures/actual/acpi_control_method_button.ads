pragma Ada_2022;
with ACPI_Service_Core;
with ACPI_Device_Status;
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
package ACPI_Control_Method_Button with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);
   package Device_Status is new ACPI_Device_Status (Core, Evaluate_Named);
   type Identification_Outcome is (Matched, Not_Button, Not_Device, No_HID,
      Lookup_Error, Evaluation_Error, Wrong_HID_Type, Binding_Changed,
      Cleanup_Error, No_Budget, Evaluator_Error);
   type Identification_Report is record
      Outcome : Identification_Outcome := Not_Device;
      Charged : Natural := 0;
      Access_Result : Core.Values.Access_Status := Core.Values.Available;
      Execution : AML_Execute.Execution_Status := AML_Execute.No_Return;
   end record;
   -- Registration-time identity observation, not callable authority. Accepts
   -- the exact PNP0C0C compressed integer or seven-byte string _HID forms.
   -- Evaluation and result cleanup use the same trusted controller context. Device enumeration/_STA readiness and registration are separate.
   procedure Identify (Service : aliased in out Core.State;
      Device : Core.Node_Descriptor; Budget : Natural;
      Report : out Identification_Report)
     with Pre => Core.Valid (Service),
       Post => Core.Valid (Service)
         and then (if Report.Outcome /= Evaluator_Error then Report.Charged <= Budget)
         and then (Report.Outcome in Cleanup_Error | Evaluator_Error
           or else Core.Retained_Results (Service) = Core.Retained_Results (Service)'Old);

end ACPI_Control_Method_Button;
