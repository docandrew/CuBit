pragma Ada_2022;
with ACPI_Service;
with ACPI_Field_Startup_Owner;
with ACPI_Field_Transport;
with ACPI_Global_Lock_Client_Native;
with CuBit.Handles;
with ACPI_Native_Data;
with ACPI_Firmware_Data_Activation;
with ACPI_Native_PCI_Aliases;
-- Process-lifetime native frontend. The main must choose this controlled
-- path only with Startup.Registers, before module AML. No hardware handles.
-- Startup only: Close_Startup refuses subsequent callbacks and retains state.
package ACPI_Native_Fields is new ACPI_Field_Startup_Owner
  (ACPI_Service, ACPI_Field_Transport.State,
   CuBit.Handles.Endpoint, CuBit.Handles.No_Endpoint,
   ACPI_Field_Transport.Read_Clock, ACPI_Field_Transport.Exchange,
   Read_Lease_Clock => ACPI_Global_Lock_Client_Native.Callbacks.Clock, Enable_PCI => True,
   Resolve_PCI_Alias => ACPI_Native_PCI_Aliases.Resolve,
   Enable_Firmware_Data => ACPI_Firmware_Data_Activation.Enabled,
   Initialize_Firmware => ACPI_Native_Data.Session.Initialize,
   Inspect_Firmware => ACPI_Native_Data.Session.Inspect,
   Describe_Firmware => ACPI_Native_Data.Session.Describe,
   Authorize_Firmware => ACPI_Native_Data.Session.Authorize,
   Dispatch_Firmware => ACPI_Native_Data.Session.Dispatch,
   Resolve_Firmware => ACPI_Native_Data.Session.Resolve,
   Begin_Firmware => ACPI_Native_Data.Session.Begin_Field,
   End_Firmware => ACPI_Native_Data.Session.End_Field,
   Cleanup_Firmware => ACPI_Native_Data.Session.Cleanup);
