pragma Ada_2022;
with ACPI_Service_Core;
with ACPI_Device_Policy;
with AML_Execute;
with AML_Fatal;
with AML_Decode;
with AML_Names;
generic
   with package Core is new ACPI_Service_Core (<>);
   -- Synchronous trusted barrier. The actual must perform the real provider
   -- phase, preserve catalog/arena identity, and report charged fuel <= Budget.
   -- No_Regions_Required is an established empty requirement, not a bypass.
   with procedure Initialize_Regions
     (Service : aliased in out Core.State; Budget : Natural;
      Charged : out Natural; Outcome : out ACPI_Device_Policy.Region_Outcome);
   -- Optional trusted evaluator closes over an explicitly provisioned context.
   -- It retains full result/cleanup semantics; provisioning and _REG are separate.
   with procedure Evaluate_Named
     (Service : aliased in out Core.State; Item : Core.Node_Descriptor;
      Budget : Natural; Result : out Core.Values.Result;
      Status : out Core.Values.Access_Status) is Core.Evaluate_Named;
   -- Optional staged registration: Prepared is physical/descriptor readiness,
   -- never a connected claim. The callback follows consideration of an initializer scope, including
   -- policy skips; its controller activates eligible descendant groups whose
   -- ancestor ordering steps are complete. Known ordinary AML failures count
   -- as considered scopes, never successful method results; fatal/uncertain
   -- states stop before this callback. Finalization performs no new AML.
   with procedure After_Initializer
     (Service : aliased in out Core.State; Scope : Core.Namespace.Node_ID; Initialization_Completed : Boolean;
      Budget : Natural; Charged : in out Natural;
      Outcome : in out ACPI_Device_Policy.Region_Outcome) is null;
   with procedure Finish_Regions
     (Service : aliased in out Core.State; Budget : Natural;
      Charged : in out Natural;
      Outcome : in out ACPI_Device_Policy.Region_Outcome) is null;
   -- Copied invocation facts only, immediately after a real initializer
   -- evaluator call returns, including failed/unavailable/overcharged results.
   -- Missing/skipped initializers and STA evaluations do not call this hook.
   -- Trusted observer must not evaluate AML, retain Core aliases, perform IO
   -- or mutate any owner. Raw facts do not attest cleanup or device readiness.
   with procedure Note_Initializer
     (Service : Core.State; Scope : Core.Namespace.Node_ID; Method : Core.Node_Descriptor;
      Access_Result : Core.Values.Access_Status; Execution : AML_Execute.Execution_Status;
      Reported_Charge : Natural) is null;
   -- Opt-in provider-prefix/consumer barrier. Classification is trusted copied
   -- lifecycle metadata, not static analysis of arbitrary AML. The walker
   -- closes selected scopes over their ancestors and runs each at most once.
   Consumer_Preparation_Enabled : Boolean := False;
   with procedure Classify_Prerequisite
     (Service : Core.State; Scope : Core.Namespace.Node_ID;
      Required, Accepted : in out Boolean) is null;
   -- Called once after the prerequisite prefix and successful region finish,
   -- before any remaining initializer. False/overcharge stops the walk.
   with procedure Prepare_Consumers
     (Service : aliased in out Core.State; Budget : Natural;
      Charged : in out Natural; Accepted : in out Boolean) is null;
package ACPI_Device_Initialization with SPARK_Mode is
   type Phase is (Not_Started, Preparing, Awaiting_Regions, Walking,
      Completed, Completed_With_Errors, Incomplete, Failed);
   type Step_Result is (Not_Attempted, Missing, Succeeded,
      Evaluation_Failed, Wrong_Result_Type, Identity_Changed, Budget_Exhausted);
   type Node_Report is record
      Status_Evaluation : Step_Result := Not_Attempted;
      Initialization : Step_Result := Not_Attempted;
      Flags : AML_Decode.Integer_Value := 0;
      Decision : ACPI_Device_Policy.Status_Decision := ACPI_Device_Policy.Descend_Only;
   end record;
   type Report is record
      Stage : Phase := Not_Started;
      Charged : Natural := 0;
      Status_Methods, Initialization_Methods, Errors : Natural := 0;
      Regions_Attempted : Boolean := False;
      Consumer_Preparation_Attempted, Consumers_Prepared : Boolean := False;
      Region_Status : ACPI_Device_Policy.Region_Outcome := ACPI_Device_Policy.Regions_Unavailable;
      First_Error_Node : Core.Namespace.Node_ID := Core.Namespace.Root;
      First_Error : AML_Execute.Execution_Status := AML_Execute.No_Return;
      Fatal : AML_Fatal.Observation;
      Fatal_Initializer : Core.Namespace.Node_ID := Core.Namespace.Root;
   end record;
   type Session is limited private;
   Maximum_Captured_Errors : constant Positive := 8;
   type Error_Count is range 0 .. Maximum_Captured_Errors;
   subtype Error_Position is Error_Count range 1 .. Error_Count'Last;
   type Error_Report is record
      Scope, Method : Core.Namespace.Node_ID := Core.Namespace.Root;
      Member : AML_Names.Segment := "____";
      Failure : AML_Execute.Execution_Status := AML_Execute.No_Return;
      Access_Result : Core.Values.Access_Status := Core.Values.Wrong_Phase;
      Evaluation : AML_Execute.Execution_Status := AML_Execute.No_Return;
      Reported_Charge : Natural := 0;
   end record;
   function Captured_Errors (Run : Session) return Error_Count;
   function Omitted_Errors (Run : Session) return Natural;
   function Error_At (Run : Session; Position : Error_Position) return Error_Report
     with Pre => Position <= Captured_Errors (Run);
   function Observe (Run : Session) return Report;
   function Work_Limit (Run : Session) return Natural;
   -- Process-lifetime controller admission, separate from the device walk.
   -- Claim before module evaluation; no reset/rearm operation exists.
   function Startup_Claimed (Run : Session) return Boolean;
   procedure Claim_Startup (Run : in out Session; Accepted : out Boolean)
     with Post => Startup_Claimed (Run)
       and Accepted = (not Startup_Claimed (Run)'Old)
       and Observe (Run) = Observe (Run)'Old;
   function Observe_Node (Run : Session; Node : Core.Namespace.Node_ID) return Node_Report;
   type Path_Outcome is (Initialization_Ready, Initialization_With_Errors, History_Unknown,
      Initialization_Not_Complete, Stale_History, Pruned_Path);
   -- Historical same-Core initialization evidence only. No AML or hardware
   -- access, no current status assertion and no authority for enumeration.
   -- A separate enumerator must still apply current ancestor/leaf STA policy.
   -- Pruned scopes and uncompleted required initializers cannot certify a
   -- descendant merely because the aggregate walk reported completion.
   function Initialization_Path
     (Run : Session; Service : Core.State; Item : Core.Node_Descriptor) return Path_Outcome
     with Pre => Core.Valid (Service);
   -- Snapshot topology and claim the one initialization attempt. No AML is
   -- evaluated here; root and system-bus compatibility initializers run in
   -- Complete_Initialization after physical provider preparation.
   procedure Begin_Initialization (Run : in out Session;
      Service : aliased in out Core.State; Budget : Natural; Accepted : out Boolean)
     with Pre => Core.Valid (Service), Post => Core.Valid (Service);
   -- Invokes the trusted region barrier once, then the bounded device walk.
   -- Unavailable/failed barrier cannot enter Walking. No automatic retry;
   -- preceding AML side effects and charged fuel are retained on failure.
   procedure Complete_Initialization (Run : in out Session;
      Service : aliased in out Core.State; Accepted : out Boolean)
     with Pre => Core.Valid (Service), Post => Core.Valid (Service);
private
   -- Terminal control outcome only. The arena owns the independent uncertainty
   -- observation; this session does not duplicate its authority or ticket.
   procedure Stop_Field_Indeterminate
     (Run : in out Session; Node : Core.Namespace.Node_ID);
   subtype Node_ID is Core.Namespace.Node_ID;
   type Descriptions is array (Node_ID) of Core.Node_Descriptor;
   type Node_Set is array (Node_ID) of Boolean;
   type Links is array (Node_ID) of Node_ID;
   type Node_Reports is array (Node_ID) of Node_Report;
   type Error_Reports is array (Error_Position) of Error_Report;
   type Session is limited record
      Info : Report;
      Error_Used : Error_Count := 0;
      Saved_Errors : Error_Reports := [others => <>];
      Claimed : Boolean := False;
      Budget : Natural := 0;
      Last : Node_ID := Core.Namespace.Root;
      Nodes : Descriptions := [others => Core.Values.No_Node];
      Live, Has_Ini : Node_Set := [others => False];
      Activated : Node_Set := [others => False];
      Prerequisites, Walked : Node_Set := [others => False];
      Initialize_Subtree : Node_Set := [others => True];
      First_Child, Next_Sibling : Links := [others => Core.Namespace.Root];
      Items : Node_Reports := [others => <>];
      System_Bus : Node_ID := Core.Namespace.Root;
   end record;
end ACPI_Device_Initialization;
