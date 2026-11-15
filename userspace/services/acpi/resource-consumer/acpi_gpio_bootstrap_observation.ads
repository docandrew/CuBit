pragma Ada_2022;
with ACPI_Service_Core;
with ACPI_GPIO_Controller_Resources;
with ACPI_Resource_Data;
with AML_Execute;
with AML_Fatal;
with Firmware_Tables;
with Interfaces;
-- One-shot copied GPIO description, for a trusted pre-device-_INI owner.
-- This neither runs _INI nor admits memory, pins, interrupts or a driver.
generic
   with package Core is new ACPI_Service_Core(<>);
   with procedure Evaluate_Named
     (Service:aliased in out Core.State;Item:Core.Node_Descriptor;Budget:Natural;
      Result:out Core.Values.Result;Status:out Core.Values.Access_Status) is Core.Evaluate_Named;
   with procedure Release_Result
     (Service:in out Core.State;Handle:in out Core.Values.Value_Handle;
      Status:out Core.Values.Access_Status) is Core.Release_Result;
   -- Actual lower provider: no active/pending/uncertain Field datum or command.
   -- This is NOT an assertion that the outer startup/runtime IO window closed.
   with function Hardware_Quiet(Service:Core.State) return Boolean;
package ACPI_GPIO_Bootstrap_Observation with SPARK_Mode is
   package Resources renames ACPI_GPIO_Controller_Resources;
   type Member_Kind is (Device_Status,Hardware_ID,Unique_ID,Current_Resources);
   type Outcome is (Not_Started,Described,Capacity_Required,Invalid_Device,
      Device_Unusable,Unsupported_Identity,Missing_Member,Lookup_Failed,
      Wrong_Result_Type,Unsupported_UID,Evaluation_Failed,Budget_Exhausted,Identity_Changed,Indeterminate,Fatal_Requested,
      Provider_Contract_Error,Hardware_Not_Quiet,Resource_Error,Cleanup_Failed,Closed);
   subtype Hardware_Identifier is String(1..8);
   -- The complete bounded _UID, observed fresh; missing is distinct from zero.
   -- The admission wire permits at most eight printable ASCII bytes.
   type UID_Kind is (No_UID,Integer_UID,String_UID);
   subtype UID_Text is String(1..8);
   type Unique_Identifier is record
      Kind:UID_Kind:=No_UID;
      Number:Interfaces.Unsigned_64:=0;
      Length:Natural range 0..UID_Text'Length:=0;
      Text:UID_Text:=[others=>Character'Val(0)];
   end record;
   type Metadata is record
      Device:Core.Node_Descriptor:=Core.Values.No_Node;
      Identifier:Hardware_Identifier:=[others=>' '];
      UID:Unique_Identifier;
      Status_Flags:Interfaces.Unsigned_64:=0;
      Status_Defaulted:Boolean:=False;
      Resources:ACPI_GPIO_Controller_Resources.Description;
   end record;
   type Report is record
      Status,Before_Cleanup:Outcome:=Not_Started;
      At_Member:Member_Kind:=Device_Status;
      Initial_Budget,Remaining,Charged,Last_Reported_Charge:Natural:=0;
      Required:ACPI_Resource_Data.Byte_Count:=0;
      Projection:Resources.Outcome:=Resources.Invalid_Template;
      Access_Result,Cleanup_Result:Core.Values.Access_Status:=Core.Values.Available;
      Execution:AML_Execute.Execution_Status:=AML_Execute.No_Return;
      Fatal:AML_Fatal.Observation;
      Field_Uncertainty:Boolean:=False;
   end record;
   type Session is limited private;
   type Action_Result is (Accepted,Already_Consumed,Wrong_Context,Not_Awaiting_Capacity,No_Pending_Result);
   function Observe(Run:Session) return Report;
   function Consumed(Run:Session) return Boolean;
   function Pending(Run:Session) return Boolean;
   function Description(Run:Session) return Metadata;
   function Is_Current(Run:Session;Service:Core.State) return Boolean with Pre=>Core.Valid(Service);
   -- Same Core, already initialized namespace, handlers/bounds/deadline owned
   -- by caller. No second interpreter. Fresh STA/HID/UID/CRS share ONE fuel budget.
   -- Caller supplies the potential device; this is not a hard-coded path scan.
   -- STA absent defaults explicitly. Unsupported/disabled devices never run CRS.
   -- Full copied raw bytes remain caller-owned. A small buffer retains only the
   -- same CRS result for Copy_Resources, never reevaluating AML for a larger copy.
   -- All other normal returns release roots before exposing metadata; failed
   -- cleanup is explicit and must retain the Core/provider. No reset/retry AML.
   procedure Describe(Run:in out Session;Service:aliased in out Core.State;
      Device:Core.Node_Descriptor;Budget:Natural;Data:out Firmware_Tables.Bytes;
      Action:out Action_Result) with Pre=>Core.Valid(Service),Post=>Core.Valid(Service);
   procedure Copy_Resources(Run:in out Session;Service:in out Core.State;
      Data:out Firmware_Tables.Bytes;Action:out Action_Result)
      with Pre=>Core.Valid(Service),Post=>Core.Valid(Service);
   procedure Close(Run:in out Session;Service:in out Core.State;Action:out Action_Result)
      with Pre=>Core.Valid(Service),Post=>Core.Valid(Service);
   -- Before IPC/admission the outer owner MUST additionally close its actual
   -- finite IO window, check runtime/provider/notification/result quiescence,
   -- and revalidate identity. Copied facts alone never establish that boundary.
private
   type Member_Array is array(Member_Kind) of Core.Node_Descriptor;
   type Presence_Array is array(Member_Kind) of Boolean;
   type Session is limited record
      Used:Boolean:=False;
      Root,Device:Core.Node_Descriptor:=Core.Values.No_Node;
      Members:Member_Array:=[others=>Core.Values.No_Node];
      Examined,Present:Presence_Array:=[others=>False];
      Handle:Core.Values.Value_Handle:=Core.Values.No_Value;
      Info:Report;
      Cache:Metadata;
   end record;
end ACPI_GPIO_Bootstrap_Observation;
