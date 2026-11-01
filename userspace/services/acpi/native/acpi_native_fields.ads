pragma Ada_2022;
with ACPI_Service;
with ACPI_Field_Startup_Owner;
with ACPI_Field_Transport;
with ACPI_Global_Lock_Client_Native;
with CuBit.Handles;
-- Process-lifetime native frontend. The main must choose this controlled
-- path only with Startup.Registers, before module AML. No hardware handles.
-- Startup only: Close_Startup refuses subsequent callbacks and retains state.
package ACPI_Native_Fields is new ACPI_Field_Startup_Owner
  (ACPI_Service, ACPI_Field_Transport.State,
   CuBit.Handles.Endpoint, CuBit.Handles.No_Endpoint,
   ACPI_Field_Transport.Read_Clock, ACPI_Field_Transport.Exchange,
   Read_Lease_Clock => ACPI_Global_Lock_Client_Native.Callbacks.Clock);
