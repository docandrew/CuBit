pragma Ada_2022;
with ACPI_Service_Core;
with ACPI_Dependency_Reader;
with AML_Execute;
generic
   with package Core is new ACPI_Service_Core (<>);
   -- Trusted controller evaluator with Core's validity/catalog/retained-root
   -- and budget contracts, optionally closing over admitted Field providers.
   -- No reset, namespace replacement or reentry; not selected by wire words.
   with procedure Evaluate_Named
     (Service : aliased in out Core.State; Item : Core.Node_Descriptor;
      Budget : Natural; Result : out Core.Values.Result;
      Status : out Core.Values.Access_Status) is Core.Evaluate_Named;
package ACPI_Device_Dependencies with SPARK_Mode is
   package Reader is new ACPI_Dependency_Reader (Core);
   type Outcome is (Not_Started, Retained, Missing, Invalid_Device, Lookup_Failed,
      Evaluation_Failed, Wrong_Result_Type, Identity_Changed, Budget_Exhausted,
      Provider_Contract_Error, Cleanup_Failed, Closed);
   type Action_Result is (Accepted, Busy, Wrong_Context, Not_Pending);
   type Report is record
      Status : Outcome := Not_Started;
      Charged, Reported_Charge : Natural := 0;
      Required : Reader.Element_Count := 0;
      Access_Result, Cleanup_Result : Core.Values.Access_Status := Core.Values.Available;
      Execution : AML_Execute.Execution_Status := AML_Execute.No_Return;
      Last_Copy : Reader.Report;
   end record;
   type Session is limited private;
   function Pending (Run : Session) return Boolean;
   function Observe (Run : Session) return Report;
   -- Controller-only in-process operation: evaluate only this device's direct
   -- canonical _DEP once. Methods can have effects; providers/authority and
   -- readiness must already be established by the owning controller. No new
   -- CCL authority, scheduler or hardware capability is created here.
   procedure Begin_Observation
     (Run : in out Session; Service : aliased in out Core.State;
      Device : Core.Node_Descriptor; Budget : Natural; Action : out Action_Result)
     with Pre => Core.Valid (Service), Post => Core.Valid (Service);
   -- Caller excludes AML execution/reset/catalog mutation while Pending and
   -- keeps this session within the Core generation. Retention does not freeze
   -- mutable objects. Copy can be retried with larger output/quota without AML
   -- replay; even successful copy keeps the original root until explicit Close.
   -- Wrong-context copies clear Items and preserve the original session.
   procedure Copy
     (Run : in out Session; Service : in out Core.State;
      Maximum_Elements : Reader.Element_Count; Items : out Reader.Device_Array;
      Result : out Reader.Report; Action : out Action_Result)
     with Pre => Core.Valid (Service), Post => Core.Valid (Service);
   -- One release attempt; failed release retains the original handle. Retrying
   -- Close never evaluates AML. Effects of Begin_Observation are not rolled back.
   procedure Close (Run : in out Session; Service : in out Core.State; Action : out Action_Result)
     with Pre => Core.Valid (Service), Post => Core.Valid (Service);
private
   type Session is limited record
      Info : Report;
      Device, Member : Core.Node_Descriptor := Core.Values.No_Node;
      Handle : Core.Values.Value_Handle := Core.Values.No_Value;
      Can_Copy : Boolean := False;
   end record;
end ACPI_Device_Dependencies;
