pragma SPARK_Mode (On);
with ACPI_Native_Delays;
with ACPI_Service_Core;
with ACPI_Native_Firmware_Profile;
with ACPI_Native_AML_Profile;
with ACPI_Clock_Source; with ACPI_Native_Startup_Timing;
package ACPI_Service is new ACPI_Service_Core (Perform_Delay => ACPI_Native_Delays.Perform,
   Read_Microseconds => ACPI_Clock_Source.Read,
   Supported_Interfaces => ACPI_Native_Firmware_Profile.Interfaces,
   Observe_Interface_Query => ACPI_Native_Firmware_Profile.Observe_Query,
   Observe_Work => ACPI_Native_Startup_Timing.Observe_Work,
   Profile_Enabled => ACPI_Native_AML_Profile.Enabled,
   Observe_Profile => ACPI_Native_AML_Profile.Observe,
   -- The captured Lenovo DSDT plus 24 SSDTs has 8,134 ACPICA namespace
   -- nodes. Keep bounded headroom for other tables and temporary declarations;
   -- the aggregate method store is independent of the single-method limit.
   -- Snapshot startup constructs this large owner once on the CuAlloc heap.
   Namespace_Node_Capacity => 16_384,
   Aggregate_Method_Capacity => 1_048_576, Enable_PCI_Context => True);
