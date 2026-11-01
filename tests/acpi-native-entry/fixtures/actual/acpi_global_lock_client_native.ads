pragma Ada_2022;
with Interfaces;
with CuBit.Handles;
with ACPI_Field_Transport;
with ACPI_Global_Lock_Client;
package ACPI_Global_Lock_Client_Native is
   package Callbacks is
      procedure Clock (Now : out Interfaces.Unsigned_64; Available : out Boolean);
   end Callbacks;
   package Client is new ACPI_Global_Lock_Client
     (ACPI_Field_Transport.State, CuBit.Handles.Endpoint, CuBit.Handles.No_Endpoint,
      Callbacks.Clock, ACPI_Field_Transport.Read_Clock, ACPI_Field_Transport.Exchange);
end ACPI_Global_Lock_Client_Native;
