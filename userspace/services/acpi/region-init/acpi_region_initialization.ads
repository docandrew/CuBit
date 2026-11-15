pragma Ada_2022;
with ACPI_Service_Core;
with ACPI_Device_Policy;
with AML_Execute;
with AML_Fatal;
generic
   with package Core is new ACPI_Service_Core (<>);
   type Controller is limited private;
   -- All handlers/bindings must already be installed. This observation grants
   -- nothing and must not capture or mutate the Service passed to Connect.
   with function Handler_Ready
     (Context : Controller; Region : Core.Node_Descriptor;
      Data : Core.Namespace.Operation_Region_Attributes) return Boolean;
   -- True only for a provider-established PCI root bus with a local _BBN.
   -- The namespace metadata alone cannot establish this platform property.
   with function Permanent_PCI_Root
     (Context : Controller; Region : Core.Node_Descriptor) return Boolean;
   with procedure Invoke
     (Service : aliased in out Core.State; Context : in out Controller;
      Node : Core.Namespace.Node_ID; Args : AML_Execute.Arguments;
      Argument_Count : Natural; Budget : Natural; Result : out Core.Values.Result;
      Status : out Core.Values.Access_Status);
   -- Trusted callbacks preserve Core/result ownership, never reset/reenter it,
   -- and obey Core's release contract (including retaining a refused handle).
   with procedure Release_Result
     (Service : in out Core.State; Handle : in out Core.Values.Value_Handle;
      Status : out Core.Values.Access_Status) is Core.Release_Result;
   -- Native root handlers can be installed at different times. The default
   -- retains all-handler admission. False connects only available groups and
   -- records unsupported regions without ever sending them _REG(space,1).
   Require_Every_Handler : Boolean := True;
   -- _REG describes a parent-scope/address-space group, not an individual
   -- region. Existing callers keep per-region behavior until opting in.
   Coalesce_Local_Notifications : Boolean := False;
package ACPI_Region_Initialization with SPARK_Mode is
   type Phase is (Not_Started, Preparing, Prepared, Connecting, Connected,
      Disconnecting, Disconnected, Failed);
   type Notice_Result is (Not_Attempted, Not_Required, Handler_Unavailable, Missing_Method,
      Completed, Evaluation_Failed);
   subtype Region_Count is Natural range 0 .. Natural (Core.Namespace.Node_ID'Last);
   type Report is record
      Stage : Phase := Not_Started;
      Regions, Methods, Unavailable : Region_Count := 0;
      Charged : Natural := 0;
      Reported_Charge : Natural := 0;
      Cleanup_Result : Core.Values.Access_Status := Core.Values.Available;
      Error_Node : Core.Namespace.Node_ID := Core.Namespace.Root;
      Error : AML_Execute.Execution_Status := AML_Execute.No_Return;
      Fatal : AML_Fatal.Observation;
   end record;
   type Session is limited private;
   type Cleanup_Action is (Attempted, Wrong_Context, No_Pending_Result);
   function Pending (Run : Session) return Boolean;
   function Observe (Run : Session) return Report;
   function Observe_Region (Run : Session; Node : Core.Namespace.Node_ID) return Notice_Result;
   type Registration_Observation is record
      Present, Handler_Present, Group_Available, Suppressed : Boolean := False;
   end record;
   -- Detached preparation facts only, never current hardware authority.
   function Observe_Registration (Run : Session; Node : Core.Namespace.Node_ID)
     return Registration_Observation;
   -- True only when this prepared scope contains an actual REG invocation.
   -- Snapshot membership still includes unsupported/suppressed/no-method groups.
   function Has_Notification_Work (Run : Session; Scope : Core.Namespace.Node_ID) return Boolean;

   -- Snapshot membership only; does not attest current handler availability.
   function Has_Prepared_Group (Run : Session; Scope : Core.Namespace.Node_ID) return Boolean;
   function Outcome (Run : Session) return ACPI_Device_Policy.Region_Outcome;
   type Scope_Action is
     (Scope_Completed, No_Prepared_Group, Already_Completed,
      Wrong_Phase, Wrong_Context, Scope_Failed);
   -- Snapshot descriptors/bindings only. No AML or REG notification occurs.
   -- Accepted consumes preparation; inspect Stage=Prepared for success.
   procedure Prepare
     (Run : in out Session; Service : Core.State;
      Context : Controller; Accepted : out Boolean)
     with Pre => Core.Valid (Service), Post => Core.Valid (Service);
   -- Activate only recorded local groups at Scope, after its ancestor
   -- prerequisites. Each group is consumed before entering AML and never
   -- replayed. Budget/Charged apply to this call; Observe.Charged is cumulative.
   -- No_Prepared_Group describes the snapshot, not current hardware absence.
   procedure Connect_Scope
     (Run : in out Session; Service : aliased in out Core.State;
      Context : in out Controller; Scope : Core.Namespace.Node_ID;
      Budget : Natural; Charged : out Natural; Action : out Scope_Action)
     with Pre => Core.Valid (Service), Post => Core.Valid (Service);
   -- Complete only after every prepared entry has been visited. No AML here.
   procedure Finish_Connect
     (Run : in out Session; Service : Core.State;
      Context : Controller; Accepted : out Boolean)
     with Pre => Core.Valid (Service), Post => Core.Valid (Service);
   -- Convenience for controllers whose prerequisites already hold for ALL
   -- groups: Prepare and activate the whole snapshot in one bounded call.
   -- Single attempt per direction. Failed/partial work cannot be replayed.
   -- A failed notification does not undo earlier firmware/hardware effects.
   procedure Connect
     (Run : in out Session; Service : aliased in out Core.State;
      Context : in out Controller; Budget : Natural; Accepted : out Boolean)
     with Pre => Core.Valid (Service), Post => Core.Valid (Service);
   -- Call while handlers remain installed. Withdrawing backing/authority is a
   -- separate controller operation, and must wait for attributable completion.
   -- Prepared sessions may disconnect only recorded successful notifications;
   -- unvisited groups never receive REG(0). Failed/unknown work stays blocked.
   procedure Disconnect
     (Run : in out Session; Service : aliased in out Core.State;
      Context : in out Controller; Budget : Natural; Accepted : out Boolean)
     with Pre => Core.Valid (Service), Post => Core.Valid (Service);
   -- A failed result release remains owned by this session. Cleanup may run
   -- after region/method changes, but only in the original Core lifetime.
   -- Close never reruns AML, restores connectivity or resets a failed session.
   -- Preserve the session and its Core/backing while Pending is true.
   procedure Close
     (Run : in out Session; Service : in out Core.State;
      Action : out Cleanup_Action)
     with Pre => Core.Valid (Service), Post => Core.Valid (Service);
private
   subtype Node_ID is Core.Namespace.Node_ID;
   type Entry_State is record
      Present : Boolean := False;
      Handler_Present, Available : Boolean := False;
      Region, Method : Core.Node_Descriptor := Core.Values.No_Node;
      Data : Core.Namespace.Operation_Region_Attributes;
      Notice : Notice_Result := Not_Attempted;
      Suppressed : Boolean := False;
      Was_Connected : Boolean := False;
   end record;
   type Entries is array (Node_ID) of Entry_State;
   type Scope_Set is array (Node_ID) of Boolean;
   type Session is limited record
      Info : Report;
      Owner : Core.Node_Descriptor := Core.Values.No_Node;
      Handle : Core.Values.Value_Handle := Core.Values.No_Value;
      Pending_Node : Node_ID := Core.Namespace.Root;
      Items : Entries;
      Scopes : Scope_Set := [others => False];
      Notification_Scopes : Scope_Set := [others => False];
   end record;
end ACPI_Region_Initialization;
