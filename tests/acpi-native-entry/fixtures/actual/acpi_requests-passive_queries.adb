pragma Ada_2022;
package body ACPI_Requests.Passive_Queries with SPARK_Mode is
   procedure Handle
     (Server : in out State; Owner_Instance : Data.Wire.Instance_ID;
      Stamp : ACPI_Protocol.Role_Tag; Request_Data : Data.Pages.Byte_Array;
      Maximum_Node_Visits : Positive;
      Maximum_Resource_Items : Data.Dispatcher.Resources.Data_IO.Item_Count;
      Nodes : out Data.Search.Node_Array; Parents : out Data.Paths.Node_Array;
      Reader_Data : out Firmware_Tables.Bytes; Reader_Atoms : out Data.Reader.Atoms;
      Text_Source : out Firmware_Tables.Bytes;
      Filter, Payload, Response : out Data.Pages.Byte_Array;
      Result : out Data.Dispatcher.Report) is
   begin
      Data.Handle
        (Server.Boot, Owner_Instance, Data.Wire.Generation (Revision (Server)),
         Stamp, Request_Data, Maximum_Node_Visits, Maximum_Resource_Items,
         Nodes, Parents, Reader_Data, Reader_Atoms, Text_Source,
         Filter, Payload, Response, Result);
   end Handle;
end ACPI_Requests.Passive_Queries;
