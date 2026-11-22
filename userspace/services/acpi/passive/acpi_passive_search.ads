pragma Ada_2022;
with ACPI_Service_Core;
with ACPI_Passive_Members;
with ACPI_Passive_Query.Pages;
with Firmware_Tables;
generic
   with package Core is new ACPI_Service_Core (<>);
   with package Reader is new ACPI_Passive_Members (Core => Core);
package ACPI_Passive_Search with SPARK_Mode is
   package Wire renames ACPI_Passive_Query;
   package Pages renames ACPI_Passive_Query.Pages;
   type Scope_State is record
      Ready : Boolean := False;
      Instance : Wire.Instance_ID := 0;
      Revision : Wire.Generation := 0;
   end record;
   type Node_Array is array (Natural range <>) of Wire.Node_Ref;
   No_Reference : constant Wire.Node_Ref := (0, 0, 0);
   type Outcome is
     (Page_Ready, No_Static_Match, Namespace_Not_Ready, Stale_Reference,
      Bad_Continuation, Invalid_Parent, Invalid_Request, Output_Required,
      Scratch_Required, Observation_Failed);
   type Report is record
      Status : Outcome := Invalid_Request;
      Key : Wire.Query_Key;
      Next_Cursor : Wire.Cursor_Option;
      Copied_Nodes, Visited_Nodes : Natural := 0;
      Required_Atoms, Required_Bytes : Natural := 0;
      Byte_Size_Known : Boolean := False;
      Read_Status : Reader.Outcome := Reader.Not_Observed;
      Access_Result : Core.Values.Access_Status := Core.Values.Available;
   end record;
   use type Wire.Node_Ref;
   -- Scope is owner state (Own_Process/Requests.Revision/readiness), never input
   -- claimed by the client. Request_Data must be an owned stable private copy.
   -- One visit is one namespace slot; member work is additionally bounded by
   -- caller scratch capacity. No device AML, parent search, IO or IPC is run.
   -- A failure rolls back this entire page, never returning partial matches or
   -- advancing past a value that could not fit. Owner decides scratch budgeting
   -- and maps these transport-independent outcomes to the observer reply.
   procedure Search
     (Service : in out Core.State; Scope : Scope_State; Request_Data : Pages.Byte_Array;
      Maximum_Node_Visits : Positive; Nodes : out Node_Array;
      Scratch_Data : out Firmware_Tables.Bytes; Scratch_Atoms : out Reader.Atoms;
      Result : out Report)
     with Pre => Core.Valid (Service), Post => Core.Valid (Service)
       and then Result.Copied_Nodes <= Nodes'Length
       and then Result.Visited_Nodes <= Maximum_Node_Visits
       and then (if Result.Status not in Page_Ready | No_Static_Match then
         Result.Copied_Nodes = 0 and then not Result.Next_Cursor.Present
         and then (for all Node of Nodes => Node = No_Reference));
end ACPI_Passive_Search;
