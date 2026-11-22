pragma Ada_2022;
with ACPI_Service_Core;
with ACPI_HID_I2C;
with ACPI_HID_I2C_Resources;
with ACPI_Resource_Reader;
with Firmware_Tables;
with AML_Execute;
generic
   with package Core is new ACPI_Service_Core (<>);
   with procedure Invoke
     (Service : aliased in out Core.State; Node : Core.Namespace.Node_ID;
      Args : Core.Values.Arguments; Argument_Count : Core.Values.Argument_Count;
      Budget : Natural; Result : out Core.Values.Result;
      Status : out Core.Values.Access_Status) is Core.Invoke_With_Arguments;
   with procedure Evaluate_Named
     (Service : aliased in out Core.State; Item : Core.Node_Descriptor;
      Budget : Natural; Result : out Core.Values.Result;
      Status : out Core.Values.Access_Status) is Core.Evaluate_Named;
package ACPI_HID_I2C_Discovery with SPARK_Mode is
   package Queries is new ACPI_HID_I2C (Core, Invoke, Evaluate_Named);
   package Reader is new ACPI_Resource_Reader (Core);
   package Resources renames ACPI_HID_I2C_Resources;
   type Stage is (Not_Started, Identity, Device_Status, Current_Resources,
      Resource_Copy, Resource_Analysis, Descriptor_Query, Finished);
   type Outcome is (Not_Attempted, Awaiting_Buffer, Observed,
      Invalid_Device, Identity_Failed, Status_Failed, Missing_Resources,
      Lookup_Failed, Evaluation_Failed, Identity_Changed, Wrong_Result_Type,
      Copy_Failed, Analysis_Failed, Descriptor_Failed, Budget_Exhausted,
      Cleanup_Failed, Provider_Contract_Error, Cancelled);
   type Action_Result is (Accepted, Busy, Wrong_Context, Not_Pending);
   type Report is record
      Step : Stage := Not_Started;
      Result : Outcome := Not_Attempted;
      Charged, Resource_Charge : Natural := 0;
      Candidate : Queries.Identification_Report;
      Status : Queries.Status_Report;
      Copy : Reader.Report;
      Analysis : Resources.Analysis_Report;
      Descriptor : Queries.Query_Report;
      Access_Result : Core.Values.Access_Status := Core.Values.Available;
      Cleanup_Result : Core.Values.Access_Status := Core.Values.Available;
      Execution : AML_Execute.Execution_Status := AML_Execute.No_Return;
   end record;
   type Session is limited private;
   function Observe (Run : Session) return Report;
   function Pending (Run : Session) return Boolean;
   -- Trusted owner callbacks have the same preservation/no-reset/no-reentry
   -- obligations as Queries. The owner must also exclude AML execution/reset/
   -- catalog mutation BETWEEN calls while Pending: retention does not freeze
   -- mutable AML objects. Session lifetime is enclosed by its Core generation.
   -- No initialization, dependency completion or hardware authority is implied.
   -- These are ordered observations: later firmware methods can change facts
   -- observed earlier. Observed does not promise an atomic readiness snapshot.
   procedure Begin_Discovery
     (Run : in out Session; Service : aliased in out Core.State;
      Device : Core.Node_Descriptor; Budget : Natural;
      Action : out Action_Result;
      Maximum_Items : Resources.R.Item_Count := Resources.R.Default_Item_Limit)
     with Pre => Core.Valid (Service), Post => Core.Valid (Service);
   -- Capacity failure preserves the SAME retained _CRS and accepted charge.
   -- Retry only this operation with larger storage. No AML is rerun for sizing.
   -- Copied spans refer to Data; retain its exact prefix while consuming facts.
   -- Wrong-context calls zero Data and leave the original session unchanged.
   procedure Copy_And_Finish
     (Run : in out Session; Service : aliased in out Core.State;
      Data : out Firmware_Tables.Bytes; Action : out Action_Result)
     with Pre => Core.Valid (Service), Post => Core.Valid (Service);
   procedure Cancel
     (Run : in out Session; Service : aliased in out Core.State;
      Action : out Action_Result)
     with Pre => Core.Valid (Service), Post => Core.Valid (Service);
private
   type Session is limited record
      Info : Report;
      Device : Core.Node_Descriptor := Core.Values.No_Node;
      Resource_Node : Core.Node_Descriptor := Core.Values.No_Node;
      Handle : Core.Values.Value_Handle := Core.Values.No_Value;
      Budget : Natural := 0;
      Maximum_Items : Resources.R.Item_Count := Resources.R.Default_Item_Limit;
      Release_Tried : Boolean := False;
   end record;
end ACPI_HID_I2C_Discovery;
