pragma Ada_2022;
with ACPI_Service_Core;
with AML_Names;
with AML_Execute;
with AML_Fatal;
generic
   with package Core is new ACPI_Service_Core (<>);
   -- Trusted provisioned evaluator/releaser satisfy Core's owner, catalog,
   -- retained-root and charge contracts, do not raise/reset/reenter, and do not
   -- change ownership of the admitted event's hardware control state.
   with procedure Evaluate_Named
     (Service : aliased in out Core.State; Item : Core.Node_Descriptor;
      Budget : Natural; Result : out Core.Values.Result;
      Status : out Core.Values.Access_Status) is Core.Evaluate_Named;
   with procedure Release_Result
     (Service : in out Core.State; Handle : in out Core.Values.Value_Handle;
      Status : out Core.Values.Access_Status) is Core.Release_Result;
package ACPI_EC_Methods with SPARK_Mode is
   type Event_Number is range 1 .. 255;
   function Method_Name (Number : Event_Number) return AML_Names.Segment;
   type Binding is private;
   No_Binding : constant Binding;
   function Is_Bound (Item : Binding) return Boolean;
   function Number (Item : Binding) return Event_Number with Pre => Is_Bound (Item);
   type Resolution_Outcome is (Found, Missing, Invalid_Scope,
      Unsupported_Handler, Lookup_Failed);
   type Resolution_Report is record
      Status : Resolution_Outcome := Missing;
      Access_Result : Core.Values.Access_Status := Core.Values.Available;
   end record;
   -- Read-only direct canonical lookup in an independently admitted EC Device.
   -- No ancestor fallback or AML evaluation; nonmethod members are unsupported.
   -- Number excludes zero: query value zero never denotes an AML handler.
   procedure Resolve
     (Service : Core.State; Scope : Core.Node_Descriptor; Event : Event_Number;
      Item : out Binding; Result : out Resolution_Report)
     with Pre => Core.Valid (Service);
   -- Re-resolve the exact local name; a rebound alias invalidates the binding.
   -- Metadata describes a method, not provider/source authority.
   function Current (Service : Core.State; Item : Binding) return Boolean
     with Pre => Core.Valid (Service);
   type Invocation_Outcome is (Not_Started, Succeeded, Invalid_Binding,
      Budget_Exhausted, Evaluation_Failed, Indeterminate, Identity_Changed,
      Provider_Contract_Error, Cleanup_Failed, Closed);
   type Invocation_Report is record
      Status : Invocation_Outcome := Not_Started;
      Execution : AML_Execute.Execution_Status := AML_Execute.No_Return;
      Charged, Reported_Charge : Natural := 0;
      Access_Result, Cleanup_Result : Core.Values.Access_Status := Core.Values.Available;
      Fatal : AML_Fatal.Observation;
   end record;
   type Action_Result is (Accepted, Already_Consumed, Wrong_Context, No_Pending_Result);
   type Invocation is limited private;
   function Consumed (Run : Invocation) return Boolean;
   function Pending (Run : Invocation) return Boolean;
   function Observe (Run : Invocation) return Invocation_Report;
   -- Trusted local event controller only, after attributable query completion
   -- and provider readiness. Consumes invocation before evaluation; never replays.
   -- Zero budget consumes this invocation without executing. Returned objects
   -- are discarded with one release attempt; failed cleanup stays owned here.
   -- One fresh Invocation per independently admitted event delivery. No wire
   -- method selector, argument injection, source registration or reset API.
   procedure Invoke
     (Run : in out Invocation; Service : aliased in out Core.State;
      Item : Binding; Budget : Natural; Action : out Action_Result)
     with Pre => Core.Valid (Service), Post => Core.Valid (Service);
   -- Cleanup may proceed after a method/scope identity change. The saved Core
   -- root authenticates ownership; never reset/replace Core while Pending.
   procedure Close
     (Run : in out Invocation; Service : in out Core.State; Action : out Action_Result)
     with Pre => Core.Valid (Service), Post => Core.Valid (Service);
private
   type Binding is record
      Bound : Boolean := False;
      Scope, Method : Core.Node_Descriptor := Core.Values.No_Node;
      Event : Event_Number := Event_Number'First;
   end record;
   No_Binding : constant Binding := (others => <>);
   type Invocation is limited record
      Used : Boolean := False;
      Owner : Core.Node_Descriptor := Core.Values.No_Node;
      Handle : Core.Values.Value_Handle := Core.Values.No_Value;
      Info : Invocation_Report;
   end record;
end ACPI_EC_Methods;
