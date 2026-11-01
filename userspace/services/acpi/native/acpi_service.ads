pragma SPARK_Mode (On);
with ACPI_Native_Delays;
with ACPI_Service_Core;
with ACPI_Clock_Source;
package ACPI_Service is new ACPI_Service_Core (Perform_Delay => ACPI_Native_Delays.Perform,
   Read_Microseconds => ACPI_Clock_Source.Read,
   -- The Lenovo DSDT's initial Field declaration exceeds 2,048 nodes.
   -- This bounded native capacity clears that declaration; hardware-region
   -- admission and complete firmware startup remain independent requirements.
   Namespace_Node_Capacity => 4_096);
