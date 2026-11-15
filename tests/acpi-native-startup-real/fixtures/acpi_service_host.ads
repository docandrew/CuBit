with ACPI_Native_Delays;with ACPI_Service_Core;
with ACPI_Native_Firmware_Profile;with ACPI_Native_AML_Profile;
with ACPI_Clock_Source;with ACPI_Native_Startup_Timing;
-- Hosted capacities are smaller; native service callback selection is exact.
package ACPI_Service is new ACPI_Service_Core
 (Perform_Delay=>ACPI_Native_Delays.Perform,
  Read_Microseconds=>ACPI_Clock_Source.Read,
  Supported_Interfaces=>ACPI_Native_Firmware_Profile.Interfaces,
  Observe_Interface_Query=>ACPI_Native_Firmware_Profile.Observe_Query,
  Observe_Work=>ACPI_Native_Startup_Timing.Observe_Work,
  Profile_Enabled=>ACPI_Native_AML_Profile.Enabled,
  Observe_Profile=>ACPI_Native_AML_Profile.Observe,
  Namespace_Node_Capacity=>128,Retained_Result_Capacity=>16,
  Aggregate_Method_Capacity=>1_048_576,Enable_PCI_Context=>True);
