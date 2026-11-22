pragma Ada_2022;
with CuBit.Handles;
with ACPI_Field_Transport;
with ACPI_Global_Lock_Client_Native;
with ACPI_Locked_NVS_Client_Native;
with ACPI_Field_Lease_Scope;
package ACPI_Field_Lease_Scope_Native is
   -- The native AML namespace currently permits this many field frames.
   Maximum_Field_Depth : constant Positive := 4_096;
   package Scope is new ACPI_Field_Lease_Scope
     (ACPI_Field_Transport.State, CuBit.Handles.Endpoint, CuBit.Handles.No_Endpoint,
      ACPI_Field_Transport.Read_Clock, ACPI_Field_Transport.Exchange,
      ACPI_Global_Lock_Client_Native.Client, ACPI_Locked_NVS_Client_Native.Client,
      Maximum_Field_Depth);
end ACPI_Field_Lease_Scope_Native;
