pragma Ada_2022;
with ACPI_Bootstrap.Device_Enumeration;
generic
   with function Metadata_Owner_Admitted return Boolean;
package ACPI_Requests.Device_Enumeration is
   package Controller is new ACPI_Bootstrap.Device_Enumeration (Metadata_Owner_Admitted);
   subtype Run is Controller.Run;
   procedure Start (Item : in out Run; Server : State; Budget : Natural; Allowed : out Controller.Admission)
      with Pre => Valid (Server);
   procedure Advance (Item : in out Run; Server : State; Max_Nodes : Positive;
      Device : out ACPI_Service.Node_Descriptor; Reads : out Natural;
      Result : out Controller.Cursor.Step_Result; Allowed : out Controller.Admission)
      with Pre => Valid (Server);
end ACPI_Requests.Device_Enumeration;
