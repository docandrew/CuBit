pragma Ada_2022;
with ACPI_Service_Core;
with ACPI_PCI_Observation_Reader;
with ACPI_Resource_Reader;
with ACPI_Resource_Data;
with Firmware_Tables;
with AML_Execute;
generic
   with package Core is new ACPI_Service_Core (<>);
   with procedure Evaluate_Named
     (Service : aliased in out Core.State; Item : Core.Node_Descriptor;
      Budget : Natural; Result : out Core.Values.Result;
      Status : out Core.Values.Access_Status) is Core.Evaluate_Named;
package ACPI_Host_Bridge_Query with SPARK_Mode is
   package Roots is new ACPI_PCI_Observation_Reader (Core, Evaluate_Named);
   package Reader is new ACPI_Resource_Reader (Core);
   type Outcome is (Not_Started, Not_A_Root, Root_Unresolved,
      Coordinate_Failed, Missing_Resources, Lookup_Failed, Evaluation_Failed,
      Wrong_Result_Type, Budget_Exhausted, Provider_Contract_Error,
      Identity_Changed, Awaiting_Buffer, Copied, Copy_Failed,
      Cleanup_Failed, Cancelled);
   type Action_Result is (Accepted, Already_Consumed, Not_Pending, Wrong_Context);
   type Report is record
      Status : Outcome := Not_Started;
      Charged : Natural := 0;
      Identity : Roots.Root_Report;
      Segment, Base_Bus : Roots.Report;
      Copy : Reader.Report;
      Access_Result, Cleanup_Result : Core.Values.Access_Status := Core.Values.Available;
      Execution : AML_Execute.Execution_Status := AML_Execute.No_Return;
      Recovery_Needed : Boolean := False;
   end record;
   type Session is limited private;
   function Observe (Run : Session) return Report;
   function Consumed (Run : Session) return Boolean;
   -- Includes failed cleanup handles exposed by the root/coordinate reader.
   -- Those have already had a release attempt; Cancel never retries them.
   function Pending (Run : Session) return Boolean;
   -- One attempt per session, including failures and cancellation. Identity,
   -- _SEG, _BBN and _CRS share one operation budget. Absent coordinates remain
   -- explicit Absent facts; no PCI default, narrowing or authority is invented.
   -- These are ordered AML observations, not an atomic firmware-state claim;
   -- later queried methods may change earlier firmware facts. Publication
   -- atomicity means the completed copied report is transferred all-or-none.
   -- Caller serializes this Core and excludes reset/catalog/AML mutation until
   -- the retained copy completes. Unexpected callback violations remain fatal.
   procedure Begin_Query (Run : in out Session;
      Service : aliased in out Core.State; Device : Core.Node_Descriptor;
      Budget : Natural; Maximum_Items : ACPI_Resource_Data.Item_Count;
      Action : out Action_Result)
      with Pre => Core.Valid (Service), Post => Core.Valid (Service);
   -- Insufficient_Capacity preserves the same retained result and charge.
   -- Grow Data and retry this copy, never Begin_Query or AML evaluation.
   -- Wrong-context calls zero Data and preserve the original session.
   procedure Copy_And_Finish (Run : in out Session;
      Service : aliased in out Core.State; Data : out Firmware_Tables.Bytes;
      Action : out Action_Result)
      with Pre => Core.Valid (Service), Post => Core.Valid (Service);
   procedure Cancel (Run : in out Session;
      Service : aliased in out Core.State; Action : out Action_Result)
      with Pre => Core.Valid (Service), Post => Core.Valid (Service);
private
   type Session is limited record
      Info : Report;
      Started, Release_Tried : Boolean := False;
      Device, Resource_Node : Core.Node_Descriptor := Core.Values.No_Node;
      Handle : Core.Values.Value_Handle := Core.Values.No_Value;
      Maximum_Items : ACPI_Resource_Data.Item_Count := 0;
   end record;
end ACPI_Host_Bridge_Query;
