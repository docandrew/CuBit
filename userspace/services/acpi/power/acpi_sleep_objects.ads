pragma Ada_2022;
with ACPI_Service_Core;
with AML_Decode;
with AML_Execute;
generic
   with package Core is new ACPI_Service_Core (<>);
   -- Trusted controller context; preserve Core/catalog/retained ownership and
   -- budget, no reset/reentry. Evaluating present objects may have effects.
   with procedure Evaluate_Named
     (Service : aliased in out Core.State; Item : Core.Node_Descriptor;
      Budget : Natural; Result : out Core.Values.Result;
      Status : out Core.Values.Access_Status) is Core.Evaluate_Named;
package ACPI_Sleep_Objects with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);
   type System_State is (S0, S1, S2, S3, S4, S5);
   type Register_Model is (No_Sleep_Registers, PM1A_Only, PM1A_And_B, Hardware_Reduced);
   type Support_Indication is (Unknown, Required_State, Declared_State, No_Declaration);
   type Sleep_Type is range 0 .. 7;
   type Sleep_Field is (No_Field, Field_A, Field_B);
   type Query_Outcome is (Not_Declared, Return_Ignored, Types_Available,
      Lookup_Failed, Evaluation_Failed, Wrong_Result_Type, Missing_Elements,
      Wrong_Element_Type, Invalid_Type_Value, Identity_Changed,
      Budget_Exhausted, Cleanup_Failed, Provider_Contract_Error);
   type Query_Report is record
      Kind : System_State := S0;
      Registers : Register_Model := No_Sleep_Registers;
      Support : Support_Indication := Unknown;
      Outcome : Query_Outcome := Lookup_Failed;
      Present, Package_Inspected : Boolean := False;
      Elements, Ignored_Elements : Natural := 0;
      Type_A, Type_B : Sleep_Type := 0;
      A_Available, B_Available : Boolean := False;
      Raw_A, Raw_B : AML_Decode.Integer_Value := 0;
      Failure_Field : Sleep_Field := No_Field;
      Charged, AML_Charged : Natural := 0;
      Access_Result : Core.Values.Access_Status := Core.Values.Available;
      Cleanup_Result : Core.Values.Access_Status := Core.Values.Available;
      Execution : AML_Execute.Execution_Status := AML_Execute.No_Return;
   end record;
   -- Register model describes admitted FADT metadata, never access authority.
   -- Evaluate a present canonical ROOT _Sx even for No_Sleep_Registers, then
   -- ignore its return. Otherwise inspect only used fields (A or A/B); unused
   -- B/reserved elements are not interpreted. One host work unit per field is
   -- charged in addition to AML execution. No truncation/masking to three bits.
   -- Missing S0/S5 remain Required_State, not evidence that ACPI type data or
   -- another transition mechanism is ready. No _PTS/_TTS/_WAK or register IO.
   procedure Query (Service : aliased in out Core.State; Kind : System_State;
      Registers : Register_Model; Budget : Natural; Report : out Query_Report)
     with Pre => Core.Valid (Service),
       Post => Core.Valid (Service) and then Report.Charged <= Budget
         and then (Report.Outcome in Cleanup_Failed | Provider_Contract_Error
           or else Core.Retained_Results (Service) = Core.Retained_Results (Service)'Old)
         and then (if Report.Outcome /= Types_Available then
           not Report.A_Available and not Report.B_Available
             and Report.Type_A = 0 and Report.Type_B = 0);
end ACPI_Sleep_Objects;
