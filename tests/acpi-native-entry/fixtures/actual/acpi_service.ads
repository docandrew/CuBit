pragma SPARK_Mode (On);
with AML_Delays;
with ACPI_Service_Core;
with ACPI_Clock_Source;
package ACPI_Service is new ACPI_Service_Core (AML_Delays.Unavailable_Provider, ACPI_Clock_Source.Read);
