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
      Recovery_Needed : Boolean := False;
   end record;
   -- Recognize only the device's direct _HID, integer/string PNP0A03/PNP0A08.
   -- Unknown is unresolved identity, not proof that a device is not a root.
   -- No ancestor/_CID fallback, PCI defaults or hardware authority. Caller
   -- must separately validate the controller's relationship/topology.
   procedure Identify_Root (Service : aliased in out Core.State;
      Device : Core.Node_Descriptor; Budget : Natural; Result : out Root_Report)
     with Pre => Core.Valid (Service), Post => Core.Valid (Service)
       and then (if Result.Status /= Provider_Contract_Error then Result.Charged <= Budget)
       and then (if Result.Status /= Observed then Result.Kind = Unknown);
   type Topology_Outcome is (Topology_Observed, Topology_Incomplete,
      Invalid_Controller, Parent_Unresolved, Root_Unresolved,
      Topology_Observation_Failed, Topology_Budget_Exhausted,
      Topology_Recovery_Required, Topology_Identity_Changed);
   type Member_Reports is array (Member) of Report;
   type Topology_Report is record
      Status : Topology_Outcome := Invalid_Controller;
      Controller, Host_Bridge : Core.Node_Descriptor := Core.Values.No_Node;
      Identity : Root_Report;
      Members : Member_Reports;
      Charged : Natural := 0;
      Recovery_Needed, Provider_Charge_Refused : Boolean := False;
   end record;
   -- Observe a controller immediately below a positively identified PCI host
   -- bridge. Intermediate bridges remain unresolved: no ancestor search or
   -- implicit bus/segment. One total fuel budget, with identity revalidation
   -- after each effectful query. Raw numbers are never narrowed or clamped.
   -- Topology_Observed means all three numbers were observed, not that they
   -- form a valid/admitted BDF. The receiver still validates ranges/hardware.
   -- Missing members retain their Absent status, distinct from observed zero.
   -- Recovery retains the nested pending result; caller must block further AML.
   procedure Observe_Controller (Service : aliased in out Core.State;
      Controller : Core.Node_Descriptor; Budget : Natural;
      Result : out Topology_Report)
     with Pre => Core.Valid (Service), Post => Core.Valid (Service)
       and then Result.Charged <= Budget;
end ACPI_PCI_Observation_Reader;
