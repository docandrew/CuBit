pragma Ada_2022;
with ACPI_Service_Core;
with ACPI_EC_Resources;
with ACPI_Resource_Data;
with Firmware_Tables;
with AML_Execute;
with AML_Fatal;
--  Trusted local discovery owner. Firmware descriptions are not authority.
--  Callback preserves Core ownership/catalog, reports actual charges, does not
--  reset/reenter and obeys named-evaluation/result cleanup contracts.
generic
   with package Core is new ACPI_Service_Core (<>);
   with procedure Evaluate_Named
     (Service : aliased in out Core.State; Item : Core.Node_Descriptor;
      Budget : Natural; Result : out Core.Values.Result;
      Status : out Core.Values.Access_Status) is Core.Evaluate_Named;
   with procedure Release_Result
     (Service : in out Core.State; Handle : in out Core.Values.Value_Handle;
      Status : out Core.Values.Access_Status) is Core.Release_Result;
package ACPI_EC_Discovery with SPARK_Mode is
   type Member_Kind is (Hardware_ID, Global_Lock, Event_Source, Resource_Set);
   type Outcome is (Not_Started, Described, Capacity_Required, Invalid_Device,
      Not_An_EC, Missing_Member, Lookup_Failed, Wrong_Result_Type,
      Unsupported_Value, Evaluation_Failed, Budget_Exhausted, Indeterminate,
      Identity_Changed, Provider_Contract_Error, Resource_Error,
      Unsupported_Platform, Cleanup_Failed, Closed);
   type Source_Kind is (No_Source, FADT_GPE, Device_GPE, GPIO_Interrupt);
   type Event_Number is range 0 .. 255;
   type Metadata is record
      Device : Core.Node_Descriptor := Core.Values.No_Node;
      Lock_Declared, Lock_Required : Boolean := False;
      Source : Source_Kind := No_Source;
      Event : Event_Number := 0;
      Block_Device : Core.Node_Descriptor := Core.Values.No_Node;
      Registers : ACPI_EC_Resources.Claims;
      GPIO_Present : Boolean := False;
      GPIO_Offset : ACPI_Resource_Data.Byte_Offset := 0;
   end record;
   type Report is record
      Status : Outcome := Not_Started;
      Before_Cleanup : Outcome := Not_Started;
      At_Member : Member_Kind := Hardware_ID;
      Initial_Budget, Remaining, Charged, Last_Reported_Charge : Natural := 0;
      Required : ACPI_Resource_Data.Byte_Count := 0;
      Projection : ACPI_EC_Resources.Status := ACPI_EC_Resources.Bad_Template;
      Access_Result, Cleanup_Result : Core.Values.Access_Status := Core.Values.Available;
      Execution : AML_Execute.Execution_Status := AML_Execute.No_Return;
      Fatal : AML_Fatal.Observation;
      Field_Uncertainty : Boolean := False;
   end record;
   type Session is limited private;
   type Action_Result is (Accepted, Already_Consumed, Wrong_Context,
                          Not_Awaiting_Capacity, No_Pending_Result);
   function Observe (Run : Session) return Report;
   --  Returns only a fully completed description; all other outcomes return
   --  default metadata. No partial tuple becomes a usable controller binding.
   function Description (Run : Session) return Metadata;
   --  Historical metadata must be revalidated before later use; this checks
   --  saved namespace bindings, not unchanged firmware values or hardware rights.
   function Is_Current (Run : Session; Service : Core.State) return Boolean
     with Pre => Core.Valid (Service);
   function Consumed (Run : Session) return Boolean;
   --  Retained result roots only. Report.Field_Uncertainty independently latches
   --  lower Field/provider obligations; no roots does not establish quiescence.
   function Pending (Run : Session) return Boolean;
   --  One attempt per Session. HW_Reduced is trusted FADT/platform metadata,
   --  not a caller-provided wire bit. Device/provider readiness and _STA/_INI
   --  sequencing are established by the outer controller. All four evaluations
   --  share Budget; optional missing _GLK defaults to no firmware lock request.
   procedure Discover
     (Run : in out Session; Service : aliased in out Core.State;
      Device : Core.Node_Descriptor; HW_Reduced : Boolean; Budget : Natural;
      Data : out Firmware_Tables.Bytes; Action : out Action_Result)
     with Pre => Core.Valid (Service), Post => Core.Valid (Service);
   --  Same retained _CRS result, no AML evaluation or budget reset. Caller owns
   --  the copied buffer and must retain it for GPIO-offset revalidation/admission.
   procedure Copy_Resources
     (Run : in out Session; Service : in out Core.State;
      Data : out Firmware_Tables.Bytes; Action : out Action_Result)
     with Pre => Core.Valid (Service), Post => Core.Valid (Service);
   --  Cleanup only in the owning Core, even if device/member bindings changed.
   --  Reattempts retained releases only; never repeats AML or publishes metadata.
   procedure Close
     (Run : in out Session; Service : in out Core.State; Action : out Action_Result)
     with Pre => Core.Valid (Service), Post => Core.Valid (Service);
private
   type Member_Array is array (Member_Kind) of Core.Node_Descriptor;
   type Presence_Array is array (Member_Kind) of Boolean;
   type Session is limited record
      Used : Boolean := False;
      Root : Core.Node_Descriptor := Core.Values.No_Node;
      Device : Core.Node_Descriptor := Core.Values.No_Node;
      Members : Member_Array := [others => Core.Values.No_Node];
      Examined, Present : Presence_Array := [others => False];
      Parent, Child : Core.Values.Value_Handle := Core.Values.No_Value;
      Reduced : Boolean := False;
      Cache : Metadata;
      Info : Report;
   end record;
end ACPI_EC_Discovery;
