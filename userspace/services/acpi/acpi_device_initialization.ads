pragma Ada_2022;
with ACPI_Service_Core;
with ACPI_Device_Policy;
with AML_Execute;
with AML_Fatal;
with AML_Decode;
generic
   with package Core is new ACPI_Service_Core (<>);
   -- Synchronous trusted barrier. The actual must perform the real provider
   -- phase, preserve catalog/arena identity, and report charged fuel <= Budget.
   -- No_Regions_Required is an established empty requirement, not a bypass.
   with procedure Initialize_Regions
     (Service : aliased in out Core.State; Budget : Natural;
      Charged : out Natural; Outcome : out ACPI_Device_Policy.Region_Outcome);
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
      Region_Status : ACPI_Device_Policy.Region_Outcome := ACPI_Device_Policy.Regions_Unavailable;
      First_Error_Node : Core.Namespace.Node_ID := Core.Namespace.Root;
      First_Error : AML_Execute.Execution_Status := AML_Execute.No_Return;
      Fatal : AML_Fatal.Observation;
      Fatal_Initializer : Core.Namespace.Node_ID := Core.Namespace.Root;
   end record;
   type Session is limited private;
   function Observe (Run : Session) return Report;
   function Observe_Node (Run : Session; Node : Core.Namespace.Node_ID) return Node_Report;
   -- Runs the compatibility root _INI and then \_SB._INI exactly once.
   -- Neither success here nor namespace Ready means region/device initialization.
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
   type Session is limited record
      Info : Report;
      Budget : Natural := 0;
      Last : Node_ID := Core.Namespace.Root;
      Nodes : Descriptions := [others => Core.Values.No_Node];
      Live, Has_Ini : Node_Set := [others => False];
      First_Child, Next_Sibling : Links := [others => Core.Namespace.Root];
      Items : Node_Reports := [others => <>];
      System_Bus : Node_ID := Core.Namespace.Root;
   end record;
end ACPI_Device_Initialization;
