pragma SPARK_Mode (On);
with ACPI_Native_Delays;
with ACPI_Service_Core;
with ACPI_Clock_Source;
package ACPI_Service is new ACPI_Service_Core (Perform_Delay => ACPI_Native_Delays.Perform,
   Read_Microseconds => ACPI_Clock_Source.Read,
   -- The captured Lenovo DSDT plus 24 SSDTs has 8,134 ACPICA namespace
   -- nodes. Keep bounded headroom for other tables and temporary declarations;
   -- the aggregate method store is independent of the single-method limit.
   -- Snapshot startup constructs this large owner once on the CuAlloc heap.
   Namespace_Node_Capacity => 16_384,
   Aggregate_Method_Capacity => 1_048_576, Enable_PCI_Context => True);
