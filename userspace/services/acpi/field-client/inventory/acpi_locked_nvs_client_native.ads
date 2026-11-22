pragma Ada_2022;
with CuBit.Handles;
with ACPI_Field_Transport;
with ACPI_Global_Lock_Client_Native;
with ACPI_Locked_NVS_Client;
package ACPI_Locked_NVS_Client_Native is
   package Client is new ACPI_Locked_NVS_Client
     (ACPI_Field_Transport.State, CuBit.Handles.Endpoint, CuBit.Handles.No_Endpoint,
      ACPI_Field_Transport.Read_Clock, ACPI_Field_Transport.Exchange,
      ACPI_Global_Lock_Client_Native.Client);
end ACPI_Locked_NVS_Client_Native;
