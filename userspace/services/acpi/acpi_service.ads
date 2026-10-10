pragma SPARK_Mode (On);
with AML_Delays;
with ACPI_Service_Core;
package ACPI_Service is new ACPI_Service_Core (Perform_Delay => AML_Delays.Unavailable_Provider);
