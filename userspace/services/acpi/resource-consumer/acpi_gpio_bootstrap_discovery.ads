pragma Ada_2022;
with ACPI_Service_Core;with ACPI_GPIO_Bootstrap_Observation;
with AML_Execute;with AML_Fatal;with Firmware_Tables;
-- Trusted serialized pre-device-_INI observation owner. No IO admission/IPC.
generic
   with package Core is new ACPI_Service_Core(<>);
   with procedure Evaluate_Named
     (Service:aliased in out Core.State;Item:Core.Node_Descriptor;Budget:Natural;
      Result:out Core.Values.Result;Status:out Core.Values.Access_Status) is Core.Evaluate_Named;
   with procedure Release_Result
     (Service:in out Core.State;Handle:in out Core.Values.Value_Handle;
      Status:out Core.Values.Access_Status) is Core.Release_Result;
   with function Hardware_Quiet(Service:Core.State) return Boolean;
package ACPI_GPIO_Bootstrap_Discovery with SPARK_Mode is
   package Observation is new ACPI_GPIO_Bootstrap_Observation
     (Core,Evaluate_Named,Release_Result,Hardware_Quiet);
   type Outcome is (Not_Started,Described,Capacity_Required,
      No_Supported_Identity_Observed,Multiple_Observed_Candidates,
      Lookup_Failed,Identity_Changed,Identity_Evaluation_Failed,
      Wrong_Identity_Type,Budget_Exhausted,Provider_Contract_Error,
      Hardware_Not_Quiet,Observation_Failed,Cleanup_Failed,Closed);
   subtype Candidate_Count is Natural range 0..2;
   subtype Probe_Count is Natural range 0..Core.Max_Namespace_Nodes;
   type Report is record
      Status:Outcome:=Not_Started;
      Candidates:Candidate_Count:=0;
      Probes:Probe_Count:=0;
      At_Device:Core.Node_Descriptor:=Core.Values.No_Node;
      Initial_Budget,Remaining,Charged,Last_Reported_Charge:Natural:=0;
      Access_Result,Cleanup_Result:Core.Values.Access_Status:=Core.Values.Available;
      Execution:AML_Execute.Execution_Status:=AML_Execute.No_Return;
      Fatal:AML_Fatal.Observation;
      Field_Uncertainty:Boolean:=False;
      Observation_Result:Observation.Report;
   end record;
   type Session is limited private;
   type Action_Result is (Accepted,Already_Consumed,Wrong_Context,
      Not_Awaiting_Capacity,No_Pending_Result,Busy);
   function Observe(Run:Session) return Report;
   function Consumed(Run:Session) return Boolean;
   function Pending(Run:Session) return Boolean;
   function Description(Run:Session) return Observation.Metadata;
   function Is_Current(Run:Session;Service:Core.State) return Boolean with Pre=>Core.Valid(Service);
   -- Each direct device HID is observed at most once during this bounded scan.
   -- No STA/CRS of unrelated devices is evaluated. Missing HID/nonmatching scalar
   -- IDs are skipped; failed/unsupported compound observations are NOT absence.
   -- Exactly one observed supported HID permits a FRESH STA/HID/CRS observation
   -- after the scan. Both phases share Budget; no hard-coded device path/base.
   -- These are sequential AML observations, not a claim of immutable/global
   -- hardware identity: methods may have side effects. The final copied tuple
   -- comes from the fresh observation, never the prefilter. Two observed matches
   -- refuse selection before any STA/CRS. No-match means no supported HID was
   -- observed, not that the machine physically has no GPIO hardware.
   procedure Discover(Run:in out Session;Service:aliased in out Core.State;
      Budget:Natural;Data:out Firmware_Tables.Bytes;Action:out Action_Result)
      with Pre=>Core.Valid(Service),Post=>Core.Valid(Service);
   -- Same retained CRS, no scan/AML replay or fuel reset. No description escapes
   -- until all attributable roots are released. Failed cleanup is retried only
   -- on explicit Close, in the same Core; lower uncertainty remains independent.
   procedure Copy_Resources(Run:in out Session;Service:in out Core.State;
      Data:out Firmware_Tables.Bytes;Action:out Action_Result)
      with Pre=>Core.Valid(Service),Post=>Core.Valid(Service);
   procedure Close(Run:in out Session;Service:in out Core.State;Action:out Action_Result)
      with Pre=>Core.Valid(Service),Post=>Core.Valid(Service);
private
   type Session is limited record
      Used,Working:Boolean:=False;
      Root,Candidate,Identity:Core.Node_Descriptor:=Core.Values.No_Node;
      Last_Node:Core.Namespace.Node_ID:=Core.Namespace.Root;
      Handle:Core.Values.Value_Handle:=Core.Values.No_Value;
      Selected:Observation.Session;
      Info:Report;
   end record;
end ACPI_GPIO_Bootstrap_Discovery;
