pragma Ada_2022;
package body ACPI_Requests.Device_Enumeration is
   procedure Start (Item : in out Run; Server : State; Budget : Natural; Allowed : out Controller.Admission) is
   begin Controller.Start (Item, Server.Boot, Budget, Allowed); end Start;
   procedure Advance (Item : in out Run; Server : State; Max_Nodes : Positive;
      Device : out ACPI_Service.Node_Descriptor; Reads : out Natural;
      Result : out Controller.Cursor.Step_Result; Allowed : out Controller.Admission) is
   begin Controller.Advance (Item, Server.Boot, Max_Nodes, Device, Reads, Result, Allowed); end Advance;
end ACPI_Requests.Device_Enumeration;
