pragma Ada_2022;
with ACPI_Protocol;
with ACPI_Bootstrap.Passive_Queries;
-- Internal owner entry, not a new wire selector or a loan-mapping boundary.
package ACPI_Requests.Passive_Queries with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);
   package Data renames ACPI_Bootstrap.Passive_Queries;
   use type Data.Dispatcher.Outcome;
   -- Derives readiness and generation from this exact Server. The owner
   -- supplies its authenticated process lifetime; raw packet words cannot
   -- override any of those facts. Static values only, no AML execution.
   procedure Handle
     (Server : in out State; Owner_Instance : Data.Wire.Instance_ID;
      Stamp : ACPI_Protocol.Role_Tag; Request_Data : Data.Pages.Byte_Array;
      Maximum_Node_Visits : Positive;
      Maximum_Resource_Items : Data.Dispatcher.Resources.Data_IO.Item_Count;
      Nodes : out Data.Search.Node_Array; Parents : out Data.Paths.Node_Array;
      Reader_Data : out Firmware_Tables.Bytes; Reader_Atoms : out Data.Reader.Atoms;
      Text_Source : out Firmware_Tables.Bytes;
      Filter, Payload, Response : out Data.Pages.Byte_Array;
      Result : out Data.Dispatcher.Report)
     with Pre => Valid (Server), Post => Valid (Server)
       and then Revision (Server) = Revision (Server)'Old
       and then Current (Server) = Current (Server)'Old
       and then Table_Open (Server) = Table_Open (Server)'Old
       and then Received (Server) = Received (Server)'Old
       and then (if Result.Status /= Data.Dispatcher.Reply_Ready then
         Result.Written = 0 and then (for all B of Response => B = 0));
end ACPI_Requests.Passive_Queries;
