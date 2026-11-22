pragma Ada_2022;
package body ACPI_Bootstrap.Passive_Queries with SPARK_Mode is
   procedure Handle
     (Session : in out State; Owner_Instance : Wire.Instance_ID;
      Snapshot_Generation : Wire.Generation;
      Stamp : ACPI_Protocol.Role_Tag; Request_Data : Pages.Byte_Array;
      Maximum_Node_Visits : Positive;
      Maximum_Resource_Items : Dispatcher.Resources.Data_IO.Item_Count;
      Nodes : out Search.Node_Array; Parents : out Paths.Node_Array;
      Reader_Data : out Firmware_Tables.Bytes; Reader_Atoms : out Reader.Atoms;
      Text_Source : out Firmware_Tables.Bytes;
      Filter, Payload, Response : out Pages.Byte_Array;
      Result : out Dispatcher.Report) is
      Scope : constant Search.Scope_State :=
        (Ready => Current (Session) = Complete and then Session.Initialized
           and then Runtime_Status.Namespace_Ready (Session.Core),
         Instance => Owner_Instance, Revision => Snapshot_Generation);
   begin
      Dispatcher.Handle
        (Session.Core, Stamp, Scope, Request_Data, Maximum_Node_Visits,
         Maximum_Resource_Items, Nodes, Parents, Reader_Data, Reader_Atoms,
         Text_Source, Filter, Payload, Response, Result);
   end Handle;
end ACPI_Bootstrap.Passive_Queries;
