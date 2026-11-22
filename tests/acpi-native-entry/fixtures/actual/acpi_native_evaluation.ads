pragma Ada_2022;
with ACPI_Service;
with ACPI_Native_Fields;
with ACPI_Startup_Evaluation;
package ACPI_Native_Evaluation is new ACPI_Startup_Evaluation (ACPI_Service, ACPI_Native_Fields);
