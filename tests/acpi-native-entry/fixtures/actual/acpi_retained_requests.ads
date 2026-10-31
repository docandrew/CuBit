pragma Ada_2022;
with ACPI_Service_Core;
with ACPI_Module_Paths;
with AML_Execute;
with AML_Names;
with Firmware_Tables.Identifiers;
generic
   with package Core is new ACPI_Service_Core (<>);
package ACPI_Retained_Requests with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);
   package Paths is new ACPI_Module_Paths (Core);
   package Values renames Core.Values;
   use type Core.Catalog_State, Core.State_Model, Core.Import_Phase;
   use type Values.Access_Status;
   type Request_Outcome is (Not_Started, Table_Not_Found, Paths_Refused,
      Source_Refused, Already_Present, Completed, Module_Failed,
      Parameter_Store_Failed, Recovery_Failed);
   type Report is record
      Outcome : Request_Outcome := Not_Started;
      Index : Natural := 0;
      Charged : Natural := 0;
      Paths_Checked : Boolean := False;
      Path_Status : Paths.Selection_Status := Paths.Caller_Rejected;
      Syntax : AML_Names.Parse_Status := AML_Names.Truncated;
      Lookup : Core.Namespace.Lookup_Status := Core.Namespace.Not_Found;
      Load : Core.Retained_Load_Report;
      Store_Attempted : Boolean := False;
      Store_Outcome : AML_Execute.Execution_Status := AML_Execute.No_Return;
      Store_Access : Values.Access_Status := Values.Wrong_Phase;
      Recovery_Access : Values.Access_Status := Values.Wrong_Phase;
   end record;
   -- Exclusive idle-owner operation over existing immutable retained tables.
   -- Strings/data have already been evaluated by the caller. Budget covers
   -- module execution and the ordinary named Store; path scans are bounded.
   -- This explicitly composes ordinary Store policy, not complete LoadTable
   -- semantics or opcode admission. Parameter targets must already exist.
   procedure Append_With_Store (Service : in out Core.State;
      Requested : Firmware_Tables.Identifiers.Selection;
      Caller : Values.Node_Descriptor; Root_Path, Parameter_Path : String;
      Item : Values.Argument; Budget : Natural; Result : out Report;
      Status : out Values.Access_Status)
     with Pre => Core.Valid (Service), Post => Core.Valid (Service)
       and then Result.Charged <= Budget
       and then Core.Catalog_Snapshot (Service) = Core.Catalog_Snapshot (Service)'Old
       and then Core.Retained_Results (Service) = Core.Retained_Results (Service)'Old
       and then (if Result.Outcome in Not_Started | Table_Not_Found | Paths_Refused |
          Source_Refused | Already_Present then Core.Model (Service) = Core.Model (Service)'Old)
       and then (if Result.Outcome = Parameter_Store_Failed then
          Result.Recovery_Access = Values.Available
          and then Core.Current_Import_Phase (Service) = Core.Catalog_Failed);
end ACPI_Retained_Requests;
