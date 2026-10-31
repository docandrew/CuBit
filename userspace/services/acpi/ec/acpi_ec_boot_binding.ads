pragma Ada_2022;
with ACPI_Service_Core;
with ACPI_EC_Boot_Table;
with Firmware_Tables;
with AML_Names;
with AML_Execute;
with AML_Fatal;
generic
   with package Core is new ACPI_Service_Core (<>);
   --  Trusted provisioned callbacks preserve catalog/arena ownership,report
   --  truthful charges/results,and do not raise,reset or reenter the owner.
   with procedure Evaluate_Named
     (Service : aliased in out Core.State; Item : Core.Node_Descriptor;
      Budget : Natural; Result : out Core.Values.Result;
      Status : out Core.Values.Access_Status) is Core.Evaluate_Named;
   with procedure Release_Result
     (Service : in out Core.State; Handle : in out Core.Values.Value_Handle;
      Status : out Core.Values.Access_Status) is Core.Release_Result;
package ACPI_EC_Boot_Binding with SPARK_Mode is
   type Outcome is (Not_Started, UID_Matched, Invalid_Table,
      Unsupported_Registers, Invalid_Path, Missing_Path, Lookup_Failed,
      Not_A_Device, Missing_UID, Wrong_UID_Type, UID_Mismatch,
      Evaluation_Failed, Budget_Exhausted, Indeterminate, Identity_Changed,
      Provider_Contract_Error, Cleanup_Failed, Closed);
   type Report is record
      Status, Before_Cleanup : Outcome := Not_Started;
      Table_Status : ACPI_EC_Boot_Table.Decode_Status := ACPI_EC_Boot_Table.Invalid_Table;
      Path_Status : ACPI_EC_Boot_Table.Path_Status := ACPI_EC_Boot_Table.Invalid_Description;
      Registers : ACPI_EC_Boot_Table.Register_Status := ACPI_EC_Boot_Table.Invalid_Description;
      Initial_Budget, Charged, Last_Reported_Charge : Natural := 0;
      Access_Result, Cleanup_Result : Core.Values.Access_Status := Core.Values.Available;
      Execution : AML_Execute.Execution_Status := AML_Execute.No_Return;
      Fatal : AML_Fatal.Observation;
      Field_Uncertainty : Boolean := False;
   end record;
   type Session is limited private;
   function Observe (Run : Session) return Report;
   function Consumed (Run : Session) return Boolean;
   function Pending (Run : Session) return Boolean;
   function Device (Run : Session) return Core.Node_Descriptor;
   --  Revalidates the exact root/path/alias/_UID bindings,not the numerical
   --  value of a mutable _UID or hardware ownership. Never evaluates AML.
   function Is_Current (Run : Session; Service : Core.State) return Boolean
     with Pre => Core.Valid (Service);
   type Action_Result is (Accepted, Already_Consumed, Wrong_Context, No_Pending_Result);
   --  Post-namespace association only. Revalidates copied ECDT bytes and
   --  byte-register compatibility; follows its exact rooted path to a Device,
   --  then evaluates only the canonical local _UID once. Requires an integer
   --  exactly equal to the full32-bit ECDT UID; no missing-value default,string
   --  coercion,ancestor fallback or search for a substitute device.
   --  Matching does not establish PNP0C09,_STA/_INI/_REG,resource equivalence,
   --  GlobalLock,early provider availability or any hardware/event authority.
   procedure Match
     (Run : in out Session; Service : aliased in out Core.State;
      Table : Firmware_Tables.Bytes; Budget : Natural; Action : out Action_Result)
     with Pre => Core.Valid (Service), Post => Core.Valid (Service);
   --  Owning-Core cleanup only; never repeats AML or publishes a late match.
   --  Pending counts roots only; Field uncertainty remains independent.
   procedure Close
     (Run : in out Session; Service : in out Core.State; Action : out Action_Result)
     with Pre => Core.Valid (Service), Post => Core.Valid (Service);
private
   type Nodes is array (Positive range 1 .. AML_Names.Segment_Array'Length) of Core.Node_Descriptor;
   type Session is limited record
      Used, UID_Saved : Boolean := False;
      Root, Target, UID_Node : Core.Node_Descriptor := Core.Values.No_Node;
      Parts : AML_Names.Segment_Array := [others => "____"];
      Count : ACPI_EC_Boot_Table.Segment_Count := 0;
      Path_Nodes : Nodes := [others => Core.Values.No_Node];
      Handle : Core.Values.Value_Handle := Core.Values.No_Value;
      Info : Report;
   end record;
end ACPI_EC_Boot_Binding;
