pragma SPARK_Mode (On);
with ACPI_Service_Core;
with ACPI_Clock_Source;
package ACPI_Service is new ACPI_Service_Core (ACPI_Clock_Source.Read);
