pragma SPARK_Mode (On);
with ACPI_Native_Delays;
with ACPI_Service_Core;
with ACPI_Clock_Source;
package ACPI_Service is new ACPI_Service_Core (Perform_Delay => ACPI_Native_Delays.Perform,
   Read_Microseconds => ACPI_Clock_Source.Read);
