pragma Ada_2022;
with CuBit.Handles;
with ACPI_Field_Transport;
with ACPI_NVS_Client;
-- Reuses the existing finite-deadline typed endpoint transport. No new peer
-- routing, implicit Wait_Forever, callback to an upper layer or retry path.
package ACPI_NVS_Client_Native is
   package Owner is new ACPI_NVS_Client
     (ACPI_Field_Transport.State, CuBit.Handles.Endpoint, CuBit.Handles.No_Endpoint,
      ACPI_Field_Transport.Read_Clock, ACPI_Field_Transport.Exchange);
end ACPI_NVS_Client_Native;
