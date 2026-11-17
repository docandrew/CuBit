pragma Ada_2022;
with ACPI_Service_Core;
with AML_Decode;
with AML_Execute;
generic
   with package Core is new ACPI_Service_Core (<>);
   -- Fixed owner evaluator, optionally closing over admitted Field handlers.
   -- Preserve Core/catalog/result ownership; no reentry or wire-selected call.
   with procedure Evaluate_Named
     (Service : aliased in out Core.State; Item : Core.Node_Descriptor;
      Budget : Natural; Result : out Core.Values.Result;
      Status : out Core.Values.Access_Status) is Core.Evaluate_Named;
package ACPI_PCI_Observation_Reader with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);
   use type AML_Decode.Integer_Value;
   use type Core.Values.Value_Handle;
   type Member is (Segment, Base_Bus, Address);
   type Outcome is (Observed, Absent, Invalid_Device, Lookup_Failed,
      Evaluation_Failed, Wrong_Result_Type, Identity_Changed,
      Budget_Exhausted, Cleanup_Failed, Provider_Contract_Error);
   type Report is record
      Status : Outcome := Invalid_Device;
      Raw : AML_Decode.Integer_Value := 0;
      Charged : Natural := 0;
      Access_Result, Cleanup_Result : Core.Values.Access_Status := Core.Values.Available;
      Execution : AML_Execute.Execution_Status := AML_Execute.No_Return;
      Pending : Core.Values.Value_Handle := Core.Values.No_Value;
      Recovery_Needed : Boolean := False;
   end record;
   -- Read only the selected device's explicit _SEG, _BBN or _ADR, including
   -- aliases/named integers and zero-argument methods. Never search ancestors
   -- or apply PCI defaults. Absent differs from an observed zero.
   -- The owner selects the correct host bridge/controller and PCI topology;
   -- this primitive does not infer a bus or admit a device. Keep full-width
   -- numbers for later checked PCI validation. AML can have effects.
   -- A recovery result blocks further owner AML; retain Pending for cleanup.
   procedure Read (Service : aliased in out Core.State;
      Device : Core.Node_Descriptor; Selected : Member; Budget : Natural;
      Result : out Report)
     with Pre => Core.Valid (Service),
       Post => Core.Valid (Service)
         and then (if Result.Status /= Provider_Contract_Error then Result.Charged <= Budget)
         and then (if Result.Status /= Observed then Result.Raw = 0)
         and then (Result.Status in Cleanup_Failed | Provider_Contract_Error
           or else Core.Retained_Results (Service) = Core.Retained_Results (Service)'Old);
   type Root_Kind is (Unknown, PCI, PCI_Express);
   type Root_Report is record
      Status : Outcome := Invalid_Device;
      Kind : Root_Kind := Unknown;
      Charged : Natural := 0;
      Access_Result, Cleanup_Result : Core.Values.Access_Status := Core.Values.Available;
      Execution : AML_Execute.Execution_Status := AML_Execute.No_Return;
      Pending : Core.Values.Value_Handle := Core.Values.No_Value;
      Pending_Element, Pending_Dereference : Core.Values.Value_Handle := Core.Values.No_Value;
      Recovery_Needed : Boolean := False;
   end record;
   -- Recognize the direct _HID, then _CID when an observed scalar HID does not match.
   -- Compatible IDs may be scalar or a package of integer/string IDs; scans
   -- and reference resolution share the total evaluation/inspection budget.
   -- Unknown is unresolved identity, not proof that this is not a root.
   -- No ancestor search, PCI defaults or hardware authority. Caller validates
   -- topology. On recovery, all three Pending handles require cleanup before
   -- further AML. Evaluators preserve retained ownership and never reset/reenter.
   procedure Identify_Root (Service : aliased in out Core.State;
      Device : Core.Node_Descriptor; Budget : Natural; Result : out Root_Report)
     with Pre => Core.Valid (Service), Post => Core.Valid (Service)
       and then Result.Charged <= Budget
       and then (if Result.Status /= Observed then Result.Kind = Unknown)
       and then (if Result.Status not in Cleanup_Failed | Provider_Contract_Error then
         Core.Retained_Results (Service) = Core.Retained_Results (Service)'Old)
       and then (if not Result.Recovery_Needed then Result.Pending = Core.Values.No_Value
         and then Result.Pending_Element = Core.Values.No_Value
         and then Result.Pending_Dereference = Core.Values.No_Value);
   Default_PCI_Segment : constant AML_Decode.Integer_Value := 0;
   type Topology_Outcome is (Topology_Observed, Topology_Incomplete,
      Invalid_Controller, Parent_Unresolved, Root_Unresolved,
      Topology_Observation_Failed, Topology_Budget_Exhausted,
      Topology_Recovery_Required, Topology_Identity_Changed);
   type Member_Reports is array (Member) of Report;
   type Topology_Report is record
      Status : Topology_Outcome := Invalid_Controller;
      Controller, Host_Bridge : Core.Node_Descriptor := Core.Values.No_Node;
      Identity : Root_Report;
      -- Only complete topology may apply the standard absent-SEG default.
      -- Members(Segment) remains the original Absent/zero observation.
      Segment_Defaulted : Boolean := False;
      Members : Member_Reports;
      Charged : Natural := 0;
      Recovery_Needed, Provider_Charge_Refused : Boolean := False;
   end record;
   -- Observe a controller immediately below a positively identified PCI host
   -- bridge. Intermediate bridges remain unresolved: no ancestor search or
   -- implicit bus. An absent SEG resolves to standard segment0, recorded in
   -- Segment_Defaulted; errors or absent BBN/ADR never default. Raw member
   -- observations remain intact. One total fuel budget, with identity revalidation
   -- after each effectful query. Raw numbers are never narrowed or clamped.
   -- Topology_Observed means all PCI values are resolved, not that they form
   -- a valid/admitted BDF. The receiver still validates ranges/hardware.
   -- Missing members retain Absent, distinct from observed zero; only SEG
   -- has this explicit default. Topology_Observed denotes resolved numbers,
   -- including standard segment0, not that every member was present.
   -- Recovery retains the nested pending result; caller must block further AML.
   procedure Observe_Controller (Service : aliased in out Core.State;
      Controller : Core.Node_Descriptor; Budget : Natural;
      Result : out Topology_Report)
     with Pre => Core.Valid (Service), Post => Core.Valid (Service)
       and then Result.Charged <= Budget
       and then (if Result.Segment_Defaulted then Result.Status = Topology_Observed
         and then Result.Members (Segment).Status = Absent
         and then Result.Members (Segment).Raw = Default_PCI_Segment);
end ACPI_PCI_Observation_Reader;
