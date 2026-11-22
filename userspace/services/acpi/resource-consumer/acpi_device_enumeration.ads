pragma Ada_2022;
with ACPI_Service_Core;
generic
   with package Core is new ACPI_Service_Core (<>);
package ACPI_Device_Enumeration with SPARK_Mode is
   package N renames Core.Namespace;
   type Phase is (Not_Started, Active, Initial_Range_Exhausted,
      Budget_Exhausted, Topology_Changed, Metadata_Failed, Not_Ready);
   type Step_Result is (No_Start, Wrong_Context, Yielded, Device_Found,
      Range_Exhausted, Out_Of_Budget, Source_Changed, Failed);
   type Report is record
      Stage : Phase := Not_Started;
      Attempted : Boolean := False;
      Initial_Last, Last_Visited : N.Node_ID := N.Root;
      Budget, Charged, Devices, Holes : Natural := 0;
      Last_Access : Core.Values.Access_Status := Core.Values.Available;
   end record;
   type Session is limited private;
   function Observe (Run : Session) return Report;
   -- Captures an equality-only Core context and initial high-water node range.
   -- One attempt, no restart or renewed budget. Budget counts node descriptions,
   -- including one initial Root anchor. No AML/evaluator or hardware.
   procedure Start (Run : in out Session; Service : Core.State; Budget : Natural)
      with Pre => Core.Valid (Service);
   -- At most Max_Nodes node descriptions and one detached Device per call,
   -- plus constant-time context/Root/count checks.
   -- Holes in the namespace high-water range are skipped, aliases are not
   -- duplicate Devices, and wrong-Core calls preserve the original session.
   -- Context/count changes refuse continuation, never silently restart.
   procedure Advance (Run : in out Session; Service : Core.State;
      Max_Nodes : Positive; Device : out Core.Node_Descriptor;
      Reads : out Natural; Result : out Step_Result)
      with Pre => Core.Valid (Service), Post => Reads <= Max_Nodes;
   -- Initial_Range_Exhausted describes visited positions, not an atomic
   -- topology snapshot or positive PCI-root identification. To build a stable
   -- candidate list, the native owner must exclude namespace/binding mutation
   -- across metadata batches; afterward revalidate each Device before AML.
   -- Node-count/context checks alone cannot detect same-count binding changes.
private
   type Session is limited record
      Info : Report;
      Context : Core.Context_Key := Core.No_Context;
      Anchor : Core.Node_Descriptor := Core.Values.No_Node;
   end record;
end ACPI_Device_Enumeration;
